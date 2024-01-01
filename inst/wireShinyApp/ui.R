library(shiny)

ui <- fluidPage(
  shinyjs::useShinyjs(),  # Set up shinyjs

  titlePanel("R Shiny App for wire"),

  sidebarLayout(
    sidebarPanel(
      fileInput("fileInput1", "Choose file (Max size: 5MB, Acceptable formats: .x3p, .rda)", accept = c(".x3p", ".rda")),
      shinyjs::hidden(  # Hide the second file input initially
        div(id = "secondFileInput",  # Add an id to the div so it can be referenced
          fileInput("fileInput2", "Choose file (Max size: 5MB, Acceptable format: .x3p)", accept = c(".x3p"))
        )
      )
    ),

    mainPanel(
      verbatimTextOutput("strOutput")
    )
  )
)
