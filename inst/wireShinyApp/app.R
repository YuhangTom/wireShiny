library(shiny)
library(shinyjs)
library(shinyWidgets) # showNotification

library(tidyverse)

library(x3ptools)
library(wire)

ui <- fluidPage(
  useShinyjs(),
  titlePanel("R Shiny App for wire"),
  sidebarLayout(
    sidebarPanel(
      tags$head(
        tags$style(HTML("
        input[type=number] {
              -moz-appearance:textfield;
        }
        input[type=number]::-webkit-outer-spin-button,
        input[type=number]::-webkit-inner-spin-button {
              -webkit-appearance: none;
              margin: 0;
        }
    "))
      ),
      titlePanel("File inputs"),
      fileInput("fileInput1", "Choose file (Max size: 5MB, Acceptable formats: .x3p, .rda)", accept = c(".x3p", ".rda")),
      hidden(
        div(
          id = "secondFileInput",
          fileInput("fileInput2", "Choose file (Max size: 5MB, Acceptable format: .x3p)", accept = c(".x3p"))
        )
      ),
      actionButton("clear", "Clear files"),
      conditionalPanel(
        condition = "output.strOutput !== ''",
        titlePanel("Inner polygon parameters"),
        numericInput("concavity", "Concavity (positive number)", value = 1.5, min = 1e-12),
        numericInput("b", "Block size b (positive integer)", value = 1, min = 1, step = 1),
        titlePanel("Rotation parameters"),
        numericInput("red_cutoff", "Red cutoff", value = 0.3, min = 0, max = 1),
        numericInput("blue_cutoff", "Blue cutoff", value = 0.7, min = 0, max = 1),
        numericInput("min_score_cut", "Min score cut", value = 0.1, min = 0),
        numericInput("loess_span", "Loess span", value = 0.2, min = 0),
        titlePanel("Shifting parameters"),
        numericInput("delta_lower", "Delta lower", value = -5, min = -Inf, max = -1, step = 1),
        numericInput("delta_upper", "Delta upper", value = 5, min = 1, max = Inf, step = 1),
        actionButton("run_button", "Run")
      )
    ),
    mainPanel(
      verbatimTextOutput("strOutput"),
      plotOutput("signals_plot")
    )
  )
)




server <- function(input, output) {
  x3ps <- reactiveValues(x3p1 = NULL, x3p2 = NULL)

  observeEvent(input$fileInput1, {
    inFile1 <- input$fileInput1

    ext1 <- tools::file_ext(inFile1$datapath)

    if (ext1 == "x3p") {
      show("secondFileInput")

      x3p1 <- x3p_read(inFile1$datapath)
      x3p2 <- NULL
    } else if (ext1 == "rda") {
      hide("secondFileInput")

      x3prda <- get(load(inFile1$datapath))
      if (!is.list(x3prda) || length(x3prda) < 2 || !all(map_lgl(x3prda, ~ "x3p" %in% class(.)))) {
        showNotification("The uploaded file is not a valid rda file containing at least 2 x3p objects.",
          type = "error"
        )
        return(NULL)
      }
      if (length(x3prda) > 2) {
        showNotification("More than two x3p objects detected in the uploaded file. Only the first two will be used.",
          type = "warning"
        )
      }
      x3p1 <- x3prda[1]
      x3p2 <- x3prda[2]
    }

    x3ps$x3p1 <- x3p1
    x3ps$x3p2 <- x3p2
  })

  observeEvent(input$fileInput2, {
    inFile2 <- input$fileInput2

    x3ps$x3p2 <- x3p_read(inFile2$datapath)
  })

  observeEvent(input$clear, {
    reset("fileInput1")
    reset("fileInput2")
    x3ps$x3p1 <- NULL
    x3ps$x3p2 <- NULL
    hide("secondFileInput")
    output$signals_plot <- NULL
  })

  output$strOutput <- renderPrint({
    if (!is.null(x3ps$x3p1) && !is.null(x3ps$x3p2)) {
      cat("Structure of the first x3p object:\n")
      str(x3ps$x3p1)
      cat("\nStructure of the second x3p object:\n")
      str(x3ps$x3p2)
    }
  })

  observeEvent(input$run_button, {
    if (is.na(input$concavity)) {
      showNotification("Concavity not provided, using default value 1.5.", type = "warning")
      concavity <- 1.5
    } else {
      concavity <- input$concavity
    }
    if (is.na(input$b)) {
      showNotification("Block size b not provided, using default value 1.", type = "warning")
      b <- 1
    } else {
      b <- input$b
    }
    if (is.na(input$red_cutoff)) {
      showNotification("Red cutoff not provided, using default value 0.3.", type = "warning")
      red_cutoff <- 0.3
    } else {
      red_cutoff <- input$red_cutoff
    }
    if (is.na(input$blue_cutoff)) {
      showNotification("Blue cutoff not provided, using default value 0.7.", type = "warning")
      blue_cutoff <- 0.7
    } else {
      blue_cutoff <- input$blue_cutoff
    }
    if (is.na(input$min_score_cut)) {
      showNotification("Min score cut not provided, using default value 0.1.", type = "warning")
      min_score_cut <- 0.1
    } else {
      min_score_cut <- input$min_score_cut
    }
    if (is.na(input$loess_span)) {
      showNotification("Loess span not provided, using default value 0.2.", type = "warning")
      loess_span <- 0.2
    } else {
      loess_span <- input$loess_span
    }
    if (is.na(input$delta_lower)) {
      showNotification("Delta lower not provided, using default value -5.", type = "warning")
      delta_lower <- -5
    } else {
      delta_lower <- input$delta_lower
    }
    if (is.na(input$delta_upper)) {
      showNotification("Delta upper not provided, using default value 5.", type = "warning")
      delta_upper <- 5
    } else {
      delta_upper <- input$delta_upper
    }

    shift_sigs <- map(list(x3ps$x3p1[[1]], x3ps$x3p2[[1]]), function(x3p) {
      insidepoly_df <- x3p_insidepoly_df(x3p, concavity = concavity, b = b)
      x3p_inner_nomiss_res <- df_rmtrend_x3p(insidepoly_df)
      x3p_inner_impute <- x3p_impute(x3p_inner_nomiss_res)
      x3p_bin_rotate <- x3p_vertical(x3p_inner_impute,
        freqs = c(0, red_cutoff, blue_cutoff, 1),
        min_score_cut = min_score_cut,
        loess_span = loess_span
      )
      x3p_shift_sig_vec(x3p_bin_rotate, delta = delta_lower:delta_upper)
    })

    aligned <- bulletxtrctr::sig_align(shift_sigs[[1]]$sig, shift_sigs[[2]]$sig)

    p_signals <- aligned$lands %>%
      pivot_longer(sig1:sig2, names_to = "x3p", names_prefix = "sig") %>%
      ggplot(aes(x = x, y = value)) +
      geom_line(aes(colour = x3p)) +
      theme_bw() +
      scale_colour_brewer(palette = "Paired") +
      xlab("x") +
      ylab("signal value")

    output$signals_plot <- renderPlot({
      p_signals
    })
  })
}

shinyApp(ui = ui, server = server)
