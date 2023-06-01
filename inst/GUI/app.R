require("shiny")

# UI ----
ui <- fluidPage(
  tag("link", list(
    rel = "stylesheet",
    href = "https://fonts.googleapis.com/css?family=Raleway"
  )),
  includeCSS("./www/styles.css"),
  titlePanel("Multidimentional Top Scoring for Creativity Research"),
  ## Sidebar ----
  sidebarLayout(
    sidebarPanel(
      width = 3,
      # hex sticker ----
      img(
        id = "hex-sticker",
        src = "https://raw.githubusercontent.com/jakub-jedrusiak/mtscr/main/man/figures/mtscr-hex.svg",
        alt = "mtscr hex sticker",
      ),
      hr(),
      actionButton("import_window", "Import data"),
      uiOutput("args_dropdowns"),
      uiOutput("download_buttons"),
      uiOutput("wide_warning")
    ),
    ## Main panel ----
    mainPanel(
      width = 9,
      fluidRow(
        ### Model info ----
        uiOutput("models_summary_header"),
        tableOutput("models_summary"),
        ### Loading message ----
        conditionalPanel(
          condition = "$('html').hasClass('shiny-busy')",
          tags$div("Loading...", id = "loadmessage")
        )
      ),
      fluidRow(
        uiOutput("scored_data_header"),
        DT::dataTableOutput("scored_data", width = "95%")
      )
    )
  ),
  hr(),
  div(class = "footer", includeHTML("./www/article_citation.html")),
)


# Server ----
server <- function(input, output, session) {
  ## Import data when run ----
  datamods::import_modal(
    id = "data_main",
    from = c("env", "file", "copypaste", "googlesheets", "url"),
    title = "Import data to be used in application"
  )

  ## Import button ----
  observeEvent(input$import_window, {
    datamods::import_modal(
      id = "data_main",
      from = c("env", "file", "copypaste", "googlesheets", "url"),
      title = "Import data to be used in application"
    )
  })

  imported <- datamods::import_server("data_main", return_class = "tbl_df")

  ## Dropdown lists with arguments for `mtscr_model()` ----
  output$args_dropdowns <- renderUI({
    req(imported$data())
    list(
      br(),
      selectInput("id_column", "Select ID column:", choices = colnames(imported$data())),
      selectInput("item_column", "Select item column:", choices = colnames(imported$data())),
      selectInput("score_column", "Select score column:", choices = colnames(
        dplyr::select(
          imported$data(),
          dplyr::where(is.numeric)
        )
      )),
      sliderInput("top", "Max number of top answers to be included:", value = 1, min = 1, max = 10),
      actionButton("generate_model", "Generate model →")
    )
  })

  ## Generate model button ----
  observeEvent(input$generate_model, {
    ### Create model ----
    data <- imported$data()
    id_col <- input$id_column
    item_col <- input$item_column
    score_col <- input$score_column
    top <- seq(1, input$top)
    model <- mtscr::mtscr_model(data, !!id_col, !!item_col, !!score_col, top = top)
    if (length(top) == 1) {
      model <- list(model)
    }
    models_summary <- mtscr::mtscr_model_summary(model)

    ### Make UI for summaries ----
    output$models_summary_header <- renderUI(tags$b("Models summary:"))
    output$models_summary <- renderTable(models_summary)

    ### Make UI for scored data ----
    scored_data <- mtscr::mtscr_score(data, !!id_col, !!item_col, !!score_col, top = top, format = "minimal")
    scored_data_whole <- mtscr::mtscr_score(data, !!id_col, !!item_col, !!score_col, top = top, format = "full")
    output$scored_data_header <- renderUI(tags$b("Scored data:"))
    output$scored_data <- DT::renderDataTable(scored_data,
      extensions = "Buttons",
      options = list(
        dom = "Bfrtip",
        buttons = c("csv", "excel")
      )
    )

    ### Download buttons ----
    output$download_buttons <- renderUI(
      list(
        br(),
        tags$b("Dowload scores:"),
        br(),
        downloadButton("scores_csv", ".csv"),
        downloadButton("scores_xlsx", ".xlsx"),
        br(),
        br(),
        tags$b("Dowload the whole database with scores:"),
        br(),
        downloadButton("whole_csv", ".csv"),
        downloadButton("whole_xlsx", ".xlsx")
      )
    )

    ## Download handlers ----
    output$scores_csv <- downloadHandler(
      filename = "scores.csv",
      content = function(file) {
        write.csv(scored_data, file)
      }
    )

    output$scores_xlsx <- downloadHandler(
      filename = "scores.xlsx",
      content = function(file) {
        writexl::write_xlsx(scored_data, file)
      }
    )

    output$whole_csv <- downloadHandler(
      filename = "whole.csv",
      content = function(file) {
        write.csv(scored_data_whole, file)
      }
    )

    output$whole_xlsx <- downloadHandler(
      filename = "whole.xlsx",
      content = function(file) {
        writexl::write_xlsx(scored_data_whole, file)
      }
    )
  })
}

# App function ----
shinyApp(ui, server)
