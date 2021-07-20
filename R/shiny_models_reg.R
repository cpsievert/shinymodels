#' @export
#' @rdname shiny_models
shiny_models.reg_shiny_data <-
  function(x,
           hover_cols = NULL,
           hover_only = FALSE,
           ...) {
    preds <- x$predictions
    num_columns <- x$num_cols
    fac_columns <- x$fac_cols
    ui <- shiny::fluidPage(
      shinyjs::useShinyjs(),
      theme = "style.css",
      # Navbar structure for UI
      navbarPage(
        "Welcome to Shinymodels!",
        theme = shinythemes::shinytheme("sandstone"),
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            HTML("For all plots"),
            shiny::actionButton("p1Button", "Push"),

            shiny::helpText("Select column(s) to create plots"),
            if (length(num_columns) == 0) {
              shiny::helpText("No numeric column to display")
            }
            else {
              shiny::selectInput(inputId = "num_value_col",
                                 label = "Numeric Columns",
                                 choices = unique(c("None Selected" = "", num_columns)))
            },
            if (length(fac_columns) == 0) {
              shiny::helpText("No factor column to display")
            }
            else {
              shiny::selectInput(inputId = "factor_value_col",
                                 label = "Factor Columns",
                                 choices = unique(c("None Selected" = "", fac_columns)))
            },
            width = 2.5,
            inline = TRUE
          ),
          shiny::mainPanel(
            shinyBS::bsCollapse(
              id = "collapseExample",
              multiple = TRUE,
              open = NULL,
              shinyBS::bsCollapsePanel(
                "Observed vs. Predicted",
                plotly::plotlyOutput("obs_vs_pred")
              ),
              shinyBS::bsCollapsePanel(
                "Residuals vs Predicted",
                plotly::plotlyOutput("resid_vs_pred")
              ),
              shinyBS::bsCollapsePanel(
                "Residuals vs A numeric column",
                plotly::plotlyOutput("resid_vs_numcol")
              ),
              # conditionalPanel(
              #   condition = "length(fac_columns) > 0",
              shinyBS::bsCollapsePanel(
                "Residuals vs A factor column",
                plotly::plotlyOutput("resid_vs_factorcol")
                # )
              )
            )
          )
        )
      )
    )
    server <- function(input, output, session) {
      selected_rows <- shiny::reactiveVal()
      if (hover_only) {
        selected_rows(NULL)
      }
      else {
        shiny::observe({
          new <- c(
            plotly::event_data("plotly_click")$customdata,
            plotly::event_data("plotly_selected")$customdata
          )
          if (length(new)) {
            current <- shiny::isolate(selected_rows())
            selected_rows(unique(c(current, new)))
          }
          else {
            # clear the selected rows when a double-click occurs
            selected_rows(NULL)
          }
        })
      }
      preds_dat <- shiny::reactive({
        dplyr::mutate(preds, .color = ifelse(.row %in% selected_rows(), "red", "black"))
      })
      output$obs_vs_pred <- plotly::renderPlotly({
        plot_numeric_obs_pred(preds_dat(), x$y_name)
      })
      output$resid_vs_pred <- plotly::renderPlotly({
        plot_numeric_res_pred(preds_dat(), x$y_name)
      })
      output$resid_vs_numcol <- plotly::renderPlotly({
        req(input$num_value_col)
        plot_numeric_res_numcol(preds_dat(), x$y_name, input$num_value_col)
      })
      output$resid_vs_factorcol <- plotly::renderPlotly({
        req(input$factor_value_col)
        plot_numeric_res_factorcol(preds_dat(), x$y_name, input$factor_value_col)
      })
      observeEvent(input$p1Button, ({
        updateCollapse(
          session,
          "collapseExample",
          open = c(
            "Observed vs. Predicted",
            "Residuals vs Predicted",
            "Residuals vs A numeric column",
            "Residuals vs A factor column"
          )
        )
      }))
    }
    shiny::shinyApp(ui, server)
  }
