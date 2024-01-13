library(shiny)
library(shinyjs)
library(shinyWidgets) # showNotification

library(tidyverse)
library(plotly)
library(rgl) # rglwidgetOutput, rglwidget

library(x3ptools)
if (!require(wire)) devtools::install_github("YuhangTom/wire")
library(wire)

# Increase the limit to 20MB
options(shiny.maxRequestSize = 20 * 1024^2)

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
      fileInput("fileInput1", "Choose file (Max size: 20MB, Acceptable formats: .x3p, .rda)", accept = c(".x3p", ".rda")),
      hidden(
        div(
          id = "secondFileInput",
          fileInput("fileInput2", "Choose file (Max size: 20MB, Acceptable format: .x3p)", accept = c(".x3p"))
        )
      ),
      actionButton("clear", "Clear files"),
      hidden(
        div(
          id = "parametersInput",
          titlePanel("Inner polygon parameters"),
          numericInput("concavity", "Concavity (positive number)", value = 1.5, min = 1e-12),
          numericInput("b", "Block size b (positive integer)", value = 1, min = 1, step = 1),
          titlePanel("Rotation parameters"),
          sliderInput("colour_cutoff", "Colour cutoff", value = c(0.3, 0.7), min = 0, max = 1, step = 0.1),
          numericInput("min_score_cut", "Min score cut (non-negative number)", value = 0.1, min = 0),
          numericInput("loess_span", "Loess span (positive number)", value = 0.2, min = 0),
          titlePanel("Shifting parameters"),
          numericInput("delta_lower", "Delta lower (negative integer)", value = -5, max = -1, step = 1),
          numericInput("delta_upper", "Delta upper (positive integer)", value = 5, min = 1, step = 1),
          actionButton("run_button", "Run")
        )
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Signals after aligning",
          plotlyOutput("signals_plot")
        ),
        tabPanel(
          "Original x3p images",
          rglwidgetOutput("x3p1_plot"),
          rglwidgetOutput("x3p2_plot")
        ),
        tabPanel(
          "x3p images after detrending",
          rglwidgetOutput("x3p_inner_nomiss_res_1_plot"),
          rglwidgetOutput("x3p_inner_nomiss_res_2_plot")
        ),
        tabPanel(
          "x3p images after imputing",
          rglwidgetOutput("x3p_inner_impute_1_plot"),
          rglwidgetOutput("x3p_inner_impute_2_plot")
        ),
        tabPanel(
          "x3p images after rotating",
          rglwidgetOutput("x3p_bin_rotate_1_plot"),
          rglwidgetOutput("x3p_bin_rotate_2_plot")
        ),
        tabPanel(
          "x3p images after shifting",
          rglwidgetOutput("x3p_bin_shift_1_plot"),
          rglwidgetOutput("x3p_bin_shift_2_plot")
        )
      )
    )
  )
)




server <- function(input, output) {
  x3ps <- reactiveValues(x3p1 = NULL, x3p2 = NULL)

  observeEvent(input$fileInput1, {
    output$signals_plot <- NULL

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
      x3p1 <- x3prda[1][[1]]
      x3p2 <- x3prda[2][[1]]
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

  observe({
    if (is.null(x3ps$x3p2)) {
      hide("parametersInput")
    } else {
      show("parametersInput")
    }
  })

  observeEvent(input$run_button, {
    withProgress(message = "Processing", value = 0, {
      n_step <- 10

      incProgress(1 / n_step,
        detail = sprintf("Step 1 of %d: Checking parameters...", n_step)
      )

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
      if (near(input$colour_cutoff[1], input$colour_cutoff[2])) {
        showNotification("Colour cutoffs cannot be the same, using default value 0.3 and 0.7.", type = "warning")
        colour_cutoff <- c(0.3, 0.7)
      } else {
        colour_cutoff <- input$colour_cutoff
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

      incProgress(1 / n_step,
        detail = sprintf("Step 2 of %d: Computing summaries for scans...", n_step)
      )

      output$x3p1_plot <- renderRglwidget({
        x3ps$x3p1 %>%
          x3p_image_autosize(zoom = 0.8)
        rglwidget()
      })
      output$x3p2_plot <- renderRglwidget({
        x3ps$x3p2 %>%
          x3p_image_autosize(zoom = 0.8)
        rglwidget()
      })

      insidepoly_df_1 <- x3p_insidepoly_df(x3ps$x3p1, concavity = concavity, b = b)
      insidepoly_df_2 <- x3p_insidepoly_df(x3ps$x3p2, concavity = concavity, b = b)

      incProgress(1 / n_step,
        detail = sprintf("Step 3 of %d: Detrending...", n_step)
      )

      x3p_inner_nomiss_res_1 <- df_rmtrend_x3p(insidepoly_df_1)
      x3p_inner_nomiss_res_2 <- df_rmtrend_x3p(insidepoly_df_2)

      output$x3p_inner_nomiss_res_1_plot <- renderRglwidget({
        x3p_inner_nomiss_res_1 %>%
          x3p_delete_mask() %>%
          x3p_image_autosize(zoom = 0.8)
        rglwidget()
      })
      output$x3p_inner_nomiss_res_2_plot <- renderRglwidget({
        x3p_inner_nomiss_res_2 %>%
          x3p_delete_mask() %>%
          x3p_image_autosize(zoom = 0.8)
        rglwidget()
      })

      incProgress(1 / n_step,
        detail = sprintf("Step 4 of %d: Imputing for missing values...", n_step)
      )

      x3p_inner_impute_1 <- x3p_impute(x3p_inner_nomiss_res_1)
      x3p_inner_impute_2 <- x3p_impute(x3p_inner_nomiss_res_2)

      output$x3p_inner_impute_1_plot <- renderRglwidget({
        x3p_inner_impute_1 %>%
          x3p_image_autosize(zoom = 0.8)
        rglwidget()
      })
      output$x3p_inner_impute_2_plot <- renderRglwidget({
        x3p_inner_impute_2 %>%
          x3p_image_autosize(zoom = 0.8)
        rglwidget()
      })

      incProgress(1 / n_step,
        detail = sprintf("Step 5 of %d: Rotating scans...", n_step)
      )

      x3p_bin_rotate_1 <- x3p_vertical(x3p_inner_impute_1,
        freqs = c(0, colour_cutoff, 1),
        min_score_cut = min_score_cut,
        loess_span = loess_span
      )
      x3p_bin_rotate_2 <- x3p_vertical(x3p_inner_impute_2,
        freqs = c(0, colour_cutoff, 1),
        min_score_cut = min_score_cut,
        loess_span = loess_span
      )

      output$x3p_bin_rotate_1_plot <- renderRglwidget({
        x3p_bin_rotate_1 %>%
          x3p_image_autosize(zoom = 0.8)
        rglwidget()
      })
      output$x3p_bin_rotate_2_plot <- renderRglwidget({
        x3p_bin_rotate_2 %>%
          x3p_image_autosize(zoom = 0.8)
        rglwidget()
      })

      incProgress(1 / n_step,
        detail = sprintf("Step 6 of %d: Shifting scans...", n_step)
      )

      x3p_bin_shift_1 <- x3p_shift(x3p_bin_rotate_1, delta = delta_lower:delta_upper)
      x3p_bin_shift_2 <- x3p_shift(x3p_bin_rotate_2, delta = delta_lower:delta_upper)

      output$x3p_bin_shift_1_plot <- renderRglwidget({
        x3p_bin_shift_1 %>%
          x3p_image_autosize(zoom = 0.8)
        rglwidget()
      })
      output$x3p_bin_shift_2_plot <- renderRglwidget({
        x3p_bin_shift_2 %>%
          x3p_image_autosize(zoom = 0.8)
        rglwidget()
      })

      incProgress(1 / n_step,
        detail = sprintf("Step 7 of %d: Extracting signals...", n_step)
      )

      shift_sig_1 <- x3p_raw_sig_vec(x3p_bin_shift_1)
      shift_sig_2 <- x3p_raw_sig_vec(x3p_bin_shift_2)

      incProgress(1 / n_step,
        detail = sprintf("Step 8 of %d: Aligning signals...", n_step)
      )

      aligned <- bulletxtrctr::sig_align(shift_sig_1$sig, shift_sig_2$sig)

      incProgress(1 / n_step,
        detail = sprintf("Step 9 of %d: Plotting...", n_step)
      )

      p_signals <- aligned$lands %>%
        pivot_longer(sig1:sig2, names_to = "x3p", names_prefix = "sig") %>%
        ggplot(aes(x = x, y = value)) +
        geom_line(aes(colour = x3p)) +
        theme_bw() +
        scale_colour_brewer(palette = "Paired") +
        xlab("x") +
        ylab("signal value") +
        ggtitle(paste0("CCF: ", round(aligned$ccf, 4)))

      output$signals_plot <- renderPlotly({
        p_signals %>%
          ggplotly()
      })

      incProgress(1 / n_step,
        detail = sprintf("Step 10 of %d: Complete.", n_step)
      )

      Sys.sleep(1)
    })
  })
}

shinyApp(ui = ui, server = server)
