#' Application User Interface
#'
#' @param request Internal parameter for {shiny}. DO NOT REMOVE.
#' @noRd
app_ui <- function(request) {

  shiny::tagList(
    # If you later need external resources, re-enable this:
    # golem_add_external_resources(),

    shiny::fluidPage(
      theme = shinythemes::shinytheme("flatly"),

      # -------------------- Head: CSS --------------------
      shiny::tags$head(
        shiny::tags$style(shiny::HTML("
          /* Title bar */
          .title-panel {
            text-align: center;
            margin-top: 20px;
            margin-bottom: 30px;
            background-color: #CEB888; /* Purdue Gold background */
            padding: 15px;
            border-radius: 5px;
          }
          .title-panel h1 {
            font-family: 'Roboto Slab', serif;
            font-size: 48px;
            font-weight: 700;
            color: #000000;
            margin: 0;
          }

          /* Page background & global text */
          body {
            background-color: #FFFFFF; /* white background */
            color: #000000;            /* black text */
          }

          /* Sidebar */
          .well {
            background-color: #F9F9F9; /* light gray background */
            border: 1px solid #E0E0E0;
          }

          /* Buttons */
          .btn-primary, .btn-success {
            background-color: #CEB888;
            border-color: #CEB888;
            color: #000000;
            font-weight: bold;
          }
          .btn-primary:hover, .btn-success:hover {
            background-color: #B89D5D;
            border-color: #B89D5D;
            color: #000000;
          }

          /* Tabs */
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

          /* Tables */
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

          /* Inputs */
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

      # -------------------- Title --------------------
      shiny::div(class = "title-panel",
                 shiny::h1("datapreviewR: A data review and QC tool")
      ),

      # -------------------- Layout --------------------
      shiny::sidebarLayout(

        # ---- LEFT: controls ----
        shiny::sidebarPanel(
         ## div(
          ##  style = "text-align: center;",
          ##  radioButtons(
          ##    "language", "Language / 语言",
          ##    choices = c("English" = "en", "中文" = "zh"),
          ##    selected = "en",
          ##    inline = TRUE
          #  )
          #),
          shiny::wellPanel(
            shiny::fileInput(
              "file",
              "Upload Data File (csv, txt, xlsx, rds, etc.)",
              accept = c(".csv", ".txt", ".tsv", ".xlsx", ".xls", ".rds")
            ),
            shiny::HTML("<span style='color: #444;'> Supported file types: <strong>.csv</strong>, <strong>.tsv</strong>, <strong>.txt</strong>, <strong>.xlsx</strong>, <strong>.xls</strong>, <strong>.rds</strong><br>
 <em>Note: First row must be column headers.</em></span>")
          ),

          shiny::selectInput(
            "columns",
            shiny::HTML("&#x1F5C2;&#xFE0F; Select Column Names (Multi-select supported)"),
            choices = NULL, multiple = TRUE
          ),

          shiny::selectInput(
            "plotType",
            shiny::HTML("&#x1F4CA; Plot Type"),
            choices = c("Histogram" = "histogram", "Boxplot" = "boxplot"),
            selected = "histogram"
          ),

          shiny::conditionalPanel(
            condition = "input.plotType == 'histogram'",
            shiny::numericInput("bins", "Histogram Bin Size", value = 30, min = 1, step = 1)
          ),

          shiny::h4("QC Filter Options"),

          shiny::radioButtons(
            "filterType", "Filter Type",
            choices = c(
              "Threshold Range" = "threshold",
              "Mean +/- Times Standard Deviation Multiplier" = "sd",
              "IQR Multiplier" = "iqr"
            )
          ),

          shiny::conditionalPanel(
            condition = "input.filterType == 'threshold'",
            shiny::numericInput("minVal", "Minimum Threshold", value = NA),
            shiny::numericInput("maxVal", "Maximum Threshold", value = NA)
          ),

          shiny::conditionalPanel(
            condition = "input.filterType == 'sd'",
            shiny::numericInput("sdMultiplier", "Standard Deviation Multiplier", value = 2, min = 0.1, step = 0.1)
          ),

          shiny::conditionalPanel(
            condition = "input.filterType == 'iqr'",
            shiny::numericInput("iqrMultiplier", "IQR Multiplier", value = 1.5, min = 0.1, step = 0.1)
          ),

          shiny::actionButton("applyFilter", "Apply Filter", class = "btn btn-primary"),
          shiny::downloadButton("downloadData", "Download Filtered Data", class = "btn btn-success")
        ),

        # ---- RIGHT: outputs ----
        shiny::mainPanel(
          shiny::tabsetPanel(
            id = "mainTabs",
            shiny::tabPanel(
              "Data Preview",
              br(), br(),
              shiny::uiOutput("plotDownloadUI"),
              shiny::plotOutput("preDistPlot"),
              DT::dataTableOutput("previewTable")
            ),
            shiny::tabPanel(
              "QC Results",
              tags$div(
                "Summary of Removed Records per Column",
                style = "text-align:center; font-weight:bold; font-family:'Times New Roman', Times, serif; font-size:18px; margin-top:15px; margin-bottom:10px;"
              ),
              DT::dataTableOutput("filterStats"),

              br(),

              tags$div(
                "Comparison of Means and Standard Deviations (Pre vs Post QC)",
                style = "text-align:center; font-weight:bold; font-family:'Times New Roman', Times, serif; font-size:18px; margin-top:15px; margin-bottom:10px;"
              ),
              DT::DTOutput("qcSummaryTable"),
              shiny::plotOutput("comparisonPlots", height = "600px"),
              br(),
              shiny::downloadButton("downloadComparisonPlot", "Download Comparison Plot (PNG)", class = "btn btn-success",
                                    style = "float: right;")
            )
          )
        )
      )
    )
  )
}

#' Add external resources to the application
#' @noRd
golem_add_external_resources <- function() {
  www_path <- system.file("app/www", package = "dataprevieweR")

  # 正确的 Shiny 函数名（驼峰）
  shiny::addResourcePath("www", www_path)

  shiny::tags$head(
    golem::favicon(),
    golem::bundle_resources(
      path = www_path,
      app_title = "dataprevieweR"
    )
  )
}
