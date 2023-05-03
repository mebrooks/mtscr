require("shiny")
if (Sys.getenv("RSTUDIO") == "1") {
  options(shiny.launch.browser = .rs.invokeShinyWindowViewer) # new window for RStudio
}

# UI ----
ui <- fluidPage(
  titlePanel("Multidimentional Top Scoring for Creativity Research"),
  ## Sidebar ----
  sidebarLayout(
    sidebarPanel(
      width = 3,
      actionButton("import_window", "Import data"),
      uiOutput("args_dropdowns"),
      uiOutput("download_buttons")
    ),
    ## Main panel ----
    mainPanel(
      fluidRow(
        img(src = "https://raw.githubusercontent.com/jakub-jedrusiak/mtscr/main/man/figures/mtscr-hex.svg", width = 200, vspace = 10, hspace = 10, align = "right"),
        ### Loading message ----
        tags$style(type = "text/css", "
           #loadmessage {
             position: fixed;
             top: 0px;
             left: 0px;
             width: 100%;
             padding: 5px 0px 5px 0px;
             text-align: center;
             font-weight: bold;
             font-size: 100%;
             color: #ffffff;
             background-color: #0163A2;
             z-index: 105;
           }"),
        conditionalPanel(
          condition = "$('html').hasClass('shiny-busy')",
          tags$div("Loading...", id = "loadmessage")
        ),
        ### Model info ----
        uiOutput("all_max_header"),
        tableOutput("all_max_glance"),
        uiOutput("all_top2_header"),
        tableOutput("all_top2_glance"),
        uiOutput("scored_data_header"),
        DT::dataTableOutput("scored_data")
      )
    )
  )
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
      selectInput("score_column", "Select score column:", choices = colnames(imported$data())),
      selectInput("summarise_for", "Summarise for:", choices = c("person", "item", "both")),
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
    model <- mtscr::mtscr_model(data, !!id_col, !!item_col, !!score_col)
    all_max_summary <- broom.mixed::glance(model[["all_max"]])
    all_top2_summary <- broom.mixed::glance(model[["all_top2"]])

    ### Make UI for summaries ----
    output$all_max_header <- renderUI(tags$b("All Maximum model summary:"))
    output$all_max_glance <- renderTable(all_max_summary)
    output$all_top2_header <- renderUI(tags$b("All Top 2 model summary:"))
    output$all_top2_glance <- renderTable(all_top2_summary)

    ### Make UI for scored data ----
    summarise_for <- input$summarise_for
    scored_data <- mtscr::mtscr_score(data, !!id_col, !!item_col, !!score_col, summarise_for = summarise_for)
    scored_data_whole <- mtscr::mtscr_score(data, !!id_col, !!item_col, !!score_col, summarise_for = summarise_for, append = TRUE)
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
