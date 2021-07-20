#'  [shiny_models()] for an object of `two_cls_shiny_data` class
#'
#' @export
#' @rdname shiny_models
shiny_models.two_cls_shiny_data <-
  function(x,
           hover_cols = NULL,
           hover_only = FALSE,
           ...) {
    preds <- x$predictions
    num_columns <- x$num_cols
    fac_columns <- x$fac_cols
    ui <- shiny::fluidPage(
      theme = "style.css",
      shinyjs::useShinyjs(),
      # Navbar structure for UI
      navbarPage(
        "Welcome to Shinymodels!",
        theme = shinythemes::shinytheme("sandstone"),
        tabPanel(
          "Static Plots",
          fluid = TRUE,
          icon = icon("chart-bar"),
          # Sidebar layout with a input and output definitions
          sidebarLayout(
              shiny::sidebarPanel(
                HTML("For all plots"),
                shiny::actionButton("p1Button", "Here"),
                width = 2.5
            ),
            shiny::mainPanel(
              shinyBS::bsCollapse(
                id = "collapse1",
                multiple = TRUE,
                open = NULL,
                shinyBS::bsCollapsePanel(
                  "Predicted probabilities vs True class",
                  plotly::plotlyOutput("obs_vs_pred")
                ),
                shinyBS::bsCollapsePanel("Confusion Matrix",
                                         plotly::plotlyOutput("conf_mat")),
                shinyBS::bsCollapsePanel("ROC curve",
                                         plotly::plotlyOutput("roc")),
                # conditionalPanel(
                #   condition = "length(fac_columns) > 0",
                shinyBS::bsCollapsePanel("PR curve",
                                         plotly::plotlyOutput("pr")
                                         # ))
                )
              )
            )
          )
          ),
          tabPanel(
            "Interactive Plots",
            fluid = TRUE,
            icon = icon("chart-line"),
            # Sidebar layout with a input and output definitions
            shiny::sidebarLayout(
              shiny::sidebarPanel(
                HTML("For all plots"),
                shiny::actionButton("p2Button", "Here"),
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
                  id = "collapse2",
                  multiple = TRUE,
                  open = NULL,
                  shinyBS::bsCollapsePanel(
                    "Predicted probabilities vs A numeric column",
                    plotly::plotlyOutput("pred_vs_numcol")
                  ),
                  shinyBS::bsCollapsePanel(
                    "Predicted probabilities vs A factor column",
                    plotly::plotlyOutput("pred_vs_factorcol")
                  )
                )
              )
            ))
          )
        )

        server <- function(input, output, session) {
          observeEvent(input$toggleSidebar, {
            shinyjs::toggle(id = "Sidebar")
          })
          selected_rows <- shiny::reactiveVal()
          if (hover_only) {
            selected_rows(NULL)
          }
          else {
            shiny::observe({
              new <-
                c(
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
              plot_twoclass_obs_pred(preds_dat(), x$y_name)
          })
          output$conf_mat <- plotly::renderPlotly({
              plot_twoclass_conf_mat(preds_dat())
          })
          output$pred_vs_numcol <- plotly::renderPlotly({
            req(input$num_value_col)
              plot_twoclass_pred_numcol(preds_dat(), x$y_name, input$num_value_col)
          })
          output$pred_vs_factorcol <- plotly::renderPlotly({
            req(input$factor_value_col)
              plot_twoclass_pred_factorcol(preds_dat(), x$y_name, input$factor_value_col)
          })

          output$roc <- plotly::renderPlotly({
              plot_twoclass_roc(preds_dat(), x$y_name)
          })
          output$pr <- plotly::renderPlotly({
              plot_twoclass_pr(preds_dat(), x$y_name)
          })
          observeEvent(input$p1Button, ({
            updateCollapse(
              session,
              "collapse1",
              open = c(
                "Predicted probabilities vs True class",
                "Confusion Matrix",
                "ROC curve",
                "PR curve"
              )
            )
          }))
          observeEvent(input$p2Button, ({
            updateCollapse(
              session,
              "collapse2",
              open = c(
                "Predicted probabilities vs A numeric column",
                "Predicted probabilities vs A factor column"
              )
            )
          }))
        }
        shiny::shinyApp(ui, server)
  }
