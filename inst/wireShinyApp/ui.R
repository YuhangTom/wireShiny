library(shiny)

ui <- fluidPage(
  shinyjs::useShinyjs(),

  ### App title
  titlePanel("R Shiny App for wire"),

  ### Sidebar layout with input and output definitions
  sidebarLayout(
    ### Sidebar panel for inputs
    sidebarPanel(
      ### Input file
      fileInput("fileInput",
        "Choose x3p files",
        multiple = FALSE,
        accept = c(".x3p", ".rda")
      )
    ),

    ### Main panel for displaying outputs
    mainPanel(
      verbatimTextOutput("strOutput")
    )
  )
)
