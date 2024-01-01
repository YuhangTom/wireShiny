library(shiny)
library(x3ptools)

server <- function(input, output) {
  x3p <- reactive({
    if (is.null(input$fileInput)) {
      x3p <- x3p_subsamples[[1]]
    } else {
      x3p <- x3p_read(input$fileInput$datapath)
    }
    x3p
  })

  output$strOutput <- renderPrint({
    str(x3p())
  })
}
