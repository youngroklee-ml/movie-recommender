library(tidyverse)

movies <- readr::read_csv("ml-25m/movies.csv")
links <- readr::read_csv("ml-25m/links.csv")

genome_tags <- readr::read_csv("ml-25m/genome-tags.csv")
genome_scores <- arrow::read_parquet("ml-25m/genome-scores.parquet")

tag_selected <- genome_scores |>
  group_by(tagId) |>
  summarize(sd = sd(relevance)) |>
  slice_max(sd, n = 20) |>
  select(tagId)

write_csv(tag_selected, "tag_selected.csv")


movies <- movies |>
  semi_join(genome_scores, by = "movieId")

movies_watched <- readr::read_csv("2022-08-28 16_44_10.csv")

tag_selected <- genome_scores |>
  group_by(tagId) |>
  summarize(sd = sd(relevance)) |>
  slice_max(sd, n = 100) |>
  select(tagId)

genome_scores_tags <- genome_scores |>
  inner_join(tag_selected, by = "tagId") |>
  inner_join(
    genome_tags |>
      mutate(tag = janitor::make_clean_names(tag)),
    by = "tagId"
  )

genome_scores_wide <- genome_scores_tags |>
  pivot_wider(
    id_cols = movieId,
    names_from = tag,
    values_from = relevance
  )

train_df <- genome_scores_wide |>
  mutate(
    watched = as.factor(if_else(movieId %in% movies_watched$movieId, 1L, 0L))
  )

library(tidymodels)

train_rec <- recipe(watched ~ ., data = train_df) |>
  update_role(movieId, new_role = "ID")

wf <- workflow() %>%
  add_recipe(train_rec)

train_df_boot <- bootstraps(train_df, strata = watched)

tune_spec <- logistic_reg(penalty = tune(), mixture = 1) |>
  set_engine("glmnet")

lambda_grid <- grid_regular(penalty(), levels = 50)

lasso_grid <- tune_grid(
  wf %>% add_model(tune_spec),
  resamples = train_df_boot,
  grid = lambda_grid
)

lasso_grid %>%
  collect_metrics()

lowest_auc <- lasso_grid %>%
  select_best("roc_auc")

final_lasso <- finalize_workflow(
  wf %>% add_model(tune_spec),
  lowest_auc
)

library(vip)

feature_selected <- final_lasso %>%
  fit(train_df) %>%
  extract_fit_parsnip() %>%
  vi(lambda = lowest_auc$penalty) |>
  filter(!near(Importance, 0))

tag_selected <- genome_tags |>
  mutate(tag = janitor::make_clean_names(tag)) |>
  semi_join(feature_selected, by = c("tag" = "Variable")) |>
  select(tagId)

write_csv(tag_selected, "tag_selected_2.csv")


