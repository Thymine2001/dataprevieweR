#' Application User Interface
#'
#' @param request Internal parameter for {shiny}. DO NOT REMOVE.
#' @noRd
app_ui <- function(request) {
  options(shiny.maxRequestSize = 10 * 1024^3)
  shiny::tagList(
    # If you later need external resources, re-enable this:
    # golem_add_external_resources(),

    shiny::fluidPage(
      theme = shinythemes::shinytheme("flatly"),
      shinyjs::useShinyjs(),
      
      # JavaScript for CSV download
      shiny::tags$script("
        Shiny.addCustomMessageHandler('downloadCSV', function(message) {
          var blob = new Blob([message.content], { type: 'text/csv;charset=utf-8;' });
          var link = document.createElement('a');
          var url = URL.createObjectURL(blob);
          link.setAttribute('href', url);
          link.setAttribute('download', message.filename);
          link.style.visibility = 'hidden';
          document.body.appendChild(link);
          link.click();
          document.body.removeChild(link);
        });
      "),

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
            font-family: 'Times New Roman', 'SimSun', serif;
            font-size: 48px;
            font-weight: 700;
            color: #000000;
            margin: 0;
          }

          /* Page background & global text */
          body {
            background-color: #FFFFFF; /* white background */
            color: #000000;            /* black text */
            font-family: 'Times New Roman', 'SimSun', serif;
          }
          
          /* Chinese text styling */
          .chinese-text {
            font-family: 'SimSun', 'FangSong', serif;
          }
          
          /* English text styling */
          .english-text {
            font-family: 'Times New Roman', serif;
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

          /* Individual QC controls */
          #individualQC .well {
            background-color: #F0F8FF;
            border: 1px solid #B0C4DE;
            border-radius: 5px;
            margin-bottom: 10px;
          }
          #individualQC h6 {
            color: #2E8B57;
            margin-bottom: 10px;
          }
          #individualQC .radio-group {
            margin-bottom: 10px;
          }
          #individualQC .form-group {
            margin-bottom: 8px;
          }

          /* Categorical filter controls */
          .categorical-filter {
            background-color: #FFF8DC;
            border: 1px solid #DAA520;
            border-radius: 5px;
            margin-bottom: 8px;
            padding: 6px;
          }
          .categorical-filter h6 {
            color: #8B4513;
            margin-bottom: 4px;
            font-weight: bold;
            font-size: 12px;
          }
          .categorical-filter .checkbox-group {
            max-height: 80px;
            overflow-y: auto;
            border: 1px solid #DDD;
            padding: 3px;
            background-color: #FAFAFA;
            font-size: 11px;
          }
          .categorical-filter .checkbox-group .form-check {
            display: inline-block;
            margin-right: 8px;
            margin-bottom: 2px;
          }
          .categorical-filter .checkbox-group .form-check-input {
            margin-right: 3px;
            transform: scale(0.8);
          }
          .categorical-filter .checkbox-group .form-check-label {
            font-size: 10px;
            margin-bottom: 0;
          }
        "))
      ),

      # -------------------- Title --------------------
      shiny::div(class = "title-panel",
                 shiny::h1(shiny::textOutput("appTitle"))
      ),

      # -------------------- Layout --------------------
      shiny::sidebarLayout(

        # ---- LEFT: controls ----
        shiny::sidebarPanel(
          shiny::div(
            style = "text-align: center; margin-bottom: 15px;",
            shiny::radioButtons(
               "language", "Language / \u8bed\u8a00 / Idioma",
              choices = c("English" = "en", "\u4e2d\u6587" = "zh", "Portugu\u00eas" = "pt"),
              selected = "en",
              inline = TRUE
            )
          ),
          shiny::wellPanel(
            shiny::uiOutput("fileUploadUI")
          ),

          # Visualization Options Panel
          shiny::wellPanel(
            shiny::h4("📊 Visualization Options", style = "margin-top: 0; color: #2c3e50; font-weight: bold;"),
            
            shiny::uiOutput("columnSelectionUI"),

            shiny::h5(shiny::textOutput("categoricalFilterTitle"), style = "margin-top: 20px; margin-bottom: 10px; color: #34495e;"),
            shiny::uiOutput("categoricalSelectionUI"),
            shiny::uiOutput("categoricalFilters"),

            shiny::h5(shiny::textOutput("plotTypeTitle"), style = "margin-top: 20px; margin-bottom: 10px; color: #34495e;"),
            shiny::uiOutput("plotTypeUI"),
            shiny::uiOutput("binsUI"),

            # Color customization panel
            shiny::div(
              style = "margin-top: 20px; padding: 15px; background-color: #f8f9fa; border-radius: 5px; border: 1px solid #e9ecef;",
              shiny::div(
                style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
                shiny::h5(shiny::textOutput("colorCustomizationTitle"), style = "margin: 0; color: #495057; font-weight: bold;"),
                shiny::actionButton("toggleColorOptions", 
                                   shiny::textOutput("showColorOptionsText", inline = TRUE),
                                   class = "btn btn-sm btn-outline-primary",
                                   style = "margin-left: 10px;")
              ),
              shiny::conditionalPanel(
                condition = "input.toggleColorOptions % 2 == 1",
                shiny::uiOutput("colorCustomizationUI")
              )
            )
          ),

          # Quality Control Options Panel
          shiny::wellPanel(
            shiny::h4("🔍 Quality Control Options", style = "margin-top: 0; color: #2c3e50; font-weight: bold;"),
            shiny::h5(shiny::textOutput("qcFilterTitle"), style = "margin-top: 0; margin-bottom: 15px; color: #34495e;"),

            shiny::uiOutput("qcModeUI"),

            # Uniform QC mode (original behavior)
            shiny::conditionalPanel(
              condition = "input.qcMode == 'uniform'",
              shiny::uiOutput("uniformQCControls")
            ),

            # Individual QC mode (new feature)
            shiny::conditionalPanel(
              condition = "input.qcMode == 'individual'",
              shiny::div(
                id = "individualQC",
                shiny::h5(shiny::textOutput("individualQCTitle")),
                shiny::uiOutput("individualQCControls")
              )
            ),

            # Action buttons section
            shiny::div(
              style = "margin-top: 20px; padding: 15px; background-color: #e8f5e8; border-radius: 5px; border: 1px solid #c3e6c3;",
              shiny::h5("📊 Apply & Download", style = "margin-top: 0; margin-bottom: 15px; color: #2d5a2d; font-weight: bold;"),
              shiny::uiOutput("actionButtonsUI")
            )
          )
        ),

        # ---- RIGHT: outputs ----
        shiny::mainPanel(
          shiny::tabsetPanel(
            id = "mainTabs",
            shiny::tabPanel(
              shiny::textOutput("dataPreviewTabTitle"),
              value = "data_preview",
              br(), br(),
              shiny::uiOutput("dataSummaryUI"),
              shiny::uiOutput("plotDownloadUI"),
              shiny::plotOutput("preDistPlot"),
              DT::dataTableOutput("previewTable")
            ),
            shiny::tabPanel(
              shiny::textOutput("qcResultsTabTitle"),
              value = "qc_results",
              shiny::uiOutput("qcResultsContent")
            )
          )
        )
      ),

      # ---- Hidden Download Link ----
      shiny::uiOutput("hiddenDownloadLink")
    )
  )
}

#' Add external resources to the application
#' @noRd
golem_add_external_resources <- function() {
  www_path <- system.file("app/www", package = "dataprevieweR")

  # Correct Shiny function names (camelCase)
  shiny::addResourcePath("www", www_path)

  shiny::tags$head(
    golem::favicon(),
    golem::bundle_resources(
      path = www_path,
      app_title = "dataprevieweR"
    )
  )
}
