library(tidyverse)
library(mlogit)

movies <- readr::read_csv("ml-25m/movies.csv")
links <- readr::read_csv("ml-25m/links.csv")

genome_tags <- readr::read_csv("ml-25m/genome-tags.csv")
genome_scores <- readr::read_cav("ml-25m/genome-scores.csv")

movies <- movies |>
  semi_join(genome_scores, by = "movieId")

movies_watched <- readr::read_csv("2022-08-28 16_44_10.csv")

tag_selected <- read_csv("tag_selected.csv")


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

movies_unwatched <- genome_scores_wide |>
  anti_join(movies_watched, by = "movieId")

movies_watched <- genome_scores_wide |>
  semi_join(movies_watched, by = "movieId")

set.seed(1234)

generate_choice <- function(watched, unwatched, n_unwatched = 1L, weight = 1) {
  pos <- watched |>
    slice_sample(n = 1L) |>
    mutate(
      choice = TRUE,
      w = weight
    )

  neg <- unwatched |>
    slice_sample(n = n_unwatched) |>
    mutate(
      choice = FALSE,
      w = weight
    )

  res <- bind_rows(pos, neg)
  res
}

df_choice <- replicate(
  100,
  generate_choice(movies_watched, movies_unwatched, n_unwatched = 10L),
  simplify = FALSE
)

tag_names <- setdiff(names(genome_scores_wide), "movieId")

str_formula <- as.formula(str_c(
  "choice ~ ",
  str_c(tag_names, collapse = " + "),
  " + 0 | 0"
))

fit_mlogit <- mlogit(
  str_formula,
  data = bind_rows(df_choice, .id = "chid"),
  alt.var = "movieId",
  chid.var = "chid",
  weights = bind_rows(df_choice)[["w"]]
)

broom::tidy(fit_mlogit) |>
  arrange(desc(estimate))

top_choices <- genome_scores_tags |>
  semi_join(movies_unwatched, by = "movieId") |>
  mutate(score = fit_mlogit$coefficients[tag] * relevance) |>
  group_by(movieId) |>
  summarize(score = sum(score)) |>
  slice_max(score, n = 500L)

movies_unwatched <- movies_unwatched |>
  semi_join(top_choices, by = "movieId")


#
# predicted |>
#   slice_head(n = 20)
#
# predicted |>
#   slice_tail(n = 20)
