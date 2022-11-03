library(shiny)
library(DT)

source("preliminary-analysis.R")

movies_watched_metadata <- movies |> 
  semi_join(movies_watched, by = "movieId")

create_choice_data <- function(selected, rejected, weight = 1) {
  bind_rows(
    selected |>
      mutate(
        choice = TRUE,
        w = weight
      ),
    rejected |>
      mutate(
        choice = FALSE,
        w = weight
      )
  )
}

sample_movies <- function(df) {
  df |>
    slice_sample(n = 2) |>
    rowwise() |>
    group_split()
}

plot_tags <- function(df, id, n = 10) {
  df_most <- df |>
    filter(movieId == id) |>
    slice_max(relevance, n = n, with_ties = FALSE)

  df_most |>
    mutate(tag = fct_reorder(tag, relevance)) |>
    ggplot(aes(x = tag, y = relevance)) +
    geom_col(width = 0.5) +
    scale_y_continuous(limits = c(0, 1)) +
    coord_flip()
}

plot_coef <- function(fit) {
  broom::tidy(fit) |>
    mutate(
      xmin = estimate - 1.96 * std.error,
      xmax = estimate + 1.96 * std.error,
      direction = case_when(
        xmin > 0 ~ "Positive",
        xmax < 0 ~ "Negative",
        TRUE ~ "Inconclusive"
      )
    ) |>
    ggplot(aes(x = estimate, y = reorder(term, estimate))) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_pointrange(
      aes(
        xmin = xmin,
        xmax = xmax,
        color = direction
      )
    ) +
    scale_color_manual(
      values = c("Positive" = "steelblue", "Negative" = "firebrick", "Inconclusive" = "grey70")
    ) +
    labs(
      x = NULL, y = NULL
    ) +
    theme(
      legend.position = "none"
    )
}

predict_utilities <- function(fit, data) {
  data |>
    mutate(score = fit$coefficients[tag] * relevance) |>
    group_by(movieId) |>
    summarize(score = sum(score)) |>
    arrange(desc(score)) |>
    inner_join(movies, by = "movieId")
}


make_ui <- function(x, var) {
  if (is.numeric(x)) {
    # rng <- range(x, na.rm = TRUE)
    sliderInput(var, var, min = 0, max = 1, value = c(0, 1))
  } else if (is.factor(x)) {
    levs <- levels(x)
    selectInput(var, var, choices = levs, selected = levs, multiple = TRUE)
  } else {
    # Not supported
    NULL
  }
}

filter_var <- function(x, val) {
  if (is.numeric(x)) {
    !is.na(x) & x >= val[1] & x <= val[2]
  } else if (is.factor(x)) {
    x %in% val
  } else {
    # No control, so don't filter
    TRUE
  }
}




ui <- fluidPage(
  waiter::use_waiter(),
  tabsetPanel(
    id = "main",
    type = "pills",
    tabPanel(
      "Add choices",
      value = "add",
      column(4,
             plotOutput("tag1"),
             actionButton("choose1", label = "Choose")
      ),
      column(4,
             plotOutput("tag2"),
             actionButton("choose2", label = "Choose")
      )
    ),
    tabPanel(
      "Preference",
      value = "fit",
      plotOutput("coef")
    ),
    tabPanel(
      "Recommendation",
      value = "recommend",
      column(4,
        map(tag_names, ~ make_ui(movies_unwatched[[.x]], .x))
      ),
      column(8,
             dataTableOutput("table")
      )
    )
  )
)

server <- function(input, output, session) {
  values <- reactiveValues(
    choice_df = df_choice,
    n = length(df_choice) + 1L
  )

  choice_set <- eventReactive(values$n, {
    res <- sample_movies(movies_watched)
  }, ignoreNULL = FALSE, ignoreInit = FALSE)

  choice_set_meta <- reactive({
    req(choice_set())

    choice_set() |>
      bind_rows() |>
      inner_join(movies_watched_metadata, by = "movieId")
  })

  observeEvent(choice_set_meta(), {
    updateActionButton(inputId = "choose1", label = choice_set_meta()[["title"]][1])
    updateActionButton(inputId = "choose2", label = choice_set_meta()[["title"]][2])
  })

  observeEvent(input$choose1, {
    req(choice_set())

    new_df <- create_choice_data(choice_set()[[1]], choice_set()[[2]], weight = 5)

    values$choice_df[[length(values$choice_df) + 1L]] <- new_df
    values$n <- values$n + 1L
  })

  observeEvent(input$choose2, {
    req(choice_set())

    new_df <- create_choice_data(choice_set()[[2]], choice_set()[[1]], weight = 5)

    values$choice_df[[length(values$choice_df) + 1L]] <- new_df
    values$n <- values$n + 1L
  })

  output$tag1 <- renderPlot({
    req(choice_set())
    plot_tags(genome_scores_tags, id = choice_set()[[1]][["movieId"]])
  }, res = 96)

  output$tag2 <- renderPlot({
    req(choice_set())
    plot_tags(genome_scores_tags, id = choice_set()[[2]][["movieId"]])
  }, res = 96)


  model <- reactive({
    req(values$choice_df)

    waiter::Waiter$new(id = "coef")$show()

    train_df <- bind_rows(values$choice_df, .id = "chid")
    w <- train_df |> pull(w)

    mlogit(str_formula, data = train_df,
           alt.var = "movieId", chid.var = "chid", weights = w)
  })

  output$coef <- renderPlot({
    req(model())

    plot_coef(model())
  }, res = 96)

  pred <- reactive(
    predict_utilities(fit = model(), data = genome_scores_tags)
  )

  pred_w_feature <- reactive(
    pred() |>
      inner_join(movies_unwatched, by = "movieId") |>
      arrange(desc(score))
  )

  selected <- reactive({
    each_var <- map(
      tag_names,
      ~ filter_var(pred_w_feature()[[.x]], input[[.x]])
    )
    reduce(each_var, ~ .x & .y)
  })

  output$table <- renderDataTable(
    pred_w_feature()[selected(), c("movieId", "title", "score")]
  )

}

shinyApp(ui, server)
