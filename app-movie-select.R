library(shiny)
library(tidyverse)
library(DT)

movies <- readr::read_csv("ml-25m/movies.csv")

ui <- fluidPage(
  dataTableOutput("movies_all"),

  downloadButton("download_list"),
  dataTableOutput("movies_watched")
)

server <- function(input, output, session) {
  watched <- reactive({
    req(input$movies_all_rows_selected)

    movies |>
      slice(input$movies_all_rows_selected)
  })

  output$movies_all <- renderDataTable(
    movies,
    filter = 'top'
  )
  output$movies_watched <- renderDataTable(
    watched(),
    extensions = 'Buttons',
    options = list(
      dom = 'Bfrtip',
      buttons = c('csv')
    )
  )

  output$download_list <- downloadHandler(
    filename = function() {
      stringr::str_c(as.character(lubridate::now()), ".csv")
    },
    content = function(file) {
      readr::write_csv(watched(), file)
    }
  )
}

shinyApp(ui, server)
