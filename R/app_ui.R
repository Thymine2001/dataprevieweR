#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    #golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      fluidPage(
        theme = shinytheme("flatly"),
        tags$head(
          tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Roboto+Slab:wght@700&display=swap');

      /* 标题 */
      .title-panel {
        text-align: center;
        margin-top: 20px;
        margin-bottom: 30px;
        background-color: #CEB888; /* Purdue Gold background for title bar */
        padding: 15px; /* Add padding for a polished look */
        border-radius: 5px; /* Optional: slight rounding for aesthetics */
      }
      .title-panel h1 {
        font-family: 'Roboto Slab', serif;
        font-size: 48px;
        font-weight: 700;
        color: #000000; /* 黑色文字 */
        margin: 0; /* Remove default margin for better alignment */
      }
      /* 背景 & 全局文字 */
      body {
        background-color: #FFFFFF; /* 白底 */
        color: #000000; /* 黑字 */
      }
      /* Sidebar / 面板 */
      .sidebar {
        background-color: #F9F9F9; /* 浅灰背景 */
        border: 1px solid #E0E0E0;
      }
      .well {
        background-color: #F9F9F9;
        border: 1px solid #E0E0E0;
      }
      /* Purdue Gold 按钮 */
      .btn-primary {
        background-color: #CEB888; /* Purdue Gold */
        border-color: #CEB888;
        color: #000000;
        font-weight: bold;
      }
      .btn-primary:hover {
        background-color: #B89D5D;
        border-color: #B89D5D;
        color: #000000;
      }
      .btn-success {
        background-color: #CEB888;
        border-color: #CEB888;
        color: #000000;
        font-weight: bold;
      }
      .btn-success:hover {
        background-color: #B89D5D;
        border-color: #B89D5D;
      }
      /* 选项卡 */
      .nav-tabs > li > a {
        color: #000000;
        font-weight: bold;
      }
      .nav-tabs > li.active > a {
        background-color: #FFFFFF;
        color: #000000;
        border-color: #CEB888;
        border-bottom: 2px solid #CEB888;
      }
      /* 表格 */
      .dataTable th {
        background-color: #F5F5F5;
        color: #000000;
      }
      .dataTable td {
        color: #000000;
      }
      .dataTables_wrapper .dataTables_paginate .paginate_button {
        color: #000000 !important;
      }
      .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
        background: #CEB888 !important;
        color: #000000 !important;
      }
      /* 输入框 */
      select, input[type='number'], input[type='file'] {
        background-color: #FFFFFF;
        color: #000000;
        border: 1px solid #CEB888;
      }
      select option {
        background-color: #FFFFFF;
        color: #000000;
      }
    "))
        ),

        # Title panel with custom class for styling
        div(class = "title-panel",
            h1("Data Preview and QC Tool")
        ),

        sidebarLayout(
          sidebarPanel(
            wellPanel(
              fileInput("file", "Upload CSV File (First row as header)")
            ),
            selectInput("columns", "🗂️ Select Column Names (Multi-select supported)", choices = NULL, multiple = TRUE),
            selectInput("plotType", "📊 Plot Type", choices = c("Histogram" = "histogram", "Boxplot" = "boxplot"), selected = "histogram"),
            conditionalPanel(
              condition = "input.plotType == 'histogram'",
              numericInput("bins", "Histogram Bin Size", value = 30, min = 1, step = 1)
            ),
            h4("QC Filter Options"),
            radioButtons("filterType", "Filter Type",
                         choices = c("Threshold Range" = "threshold",
                                     "Standard Deviation Multiplier (± Mean)" = "sd",
                                     "IQR Multiplier" = "iqr")),
            conditionalPanel(
              condition = "input.filterType == 'threshold'",
              numericInput("minVal", "Minimum Threshold", value = NA),
              numericInput("maxVal", "Maximum Threshold", value = NA)
            ),
            conditionalPanel(
              condition = "input.filterType == 'sd'",
              numericInput("sdMultiplier", "Standard Deviation Multiplier", value = 2, min = 0.1, step = 0.1)
            ),
            conditionalPanel(
              condition = "input.filterType == 'iqr'",
              numericInput("iqrMultiplier", "IQR Multiplier", value = 1.5, min = 0.1, step = 0.1)
            ),
            actionButton("applyFilter", "Apply Filter", class = "btn-primary"),
            downloadButton("downloadData", "Download Filtered Data", class = "btn-success")
          ),

          mainPanel(
            tabsetPanel(
              tabPanel("Data Preview",
                       plotOutput("preDistPlot"),
                       DT::dataTableOutput("previewTable")),
              tabPanel("QC Results",
                       DT::dataTableOutput("filterStats"),
                       plotOutput("comparisonPlots", height = "600px"))
            )
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "dataprevieweR"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
