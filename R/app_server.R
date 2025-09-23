# -*- coding: UTF-8 -*-
#' Application server-side logic
#'
#' @param input,output,session Internal parameters for {shiny}.
#'   DO NOT REMOVE.
#' @noRd
plot_ready <- reactiveVal(FALSE)
labels <- list(
  file_upload = list(
    en = "Upload Data File (csv, txt, xlsx, rds, etc.)",
    zh = "\u4e0a\u4f20\u6570\u636e\u6587\u4ef6\uff08csv\u3001txt\u3001xlsx\u3001rds \u7b49\uff09"
  ),
  supported_types = list(
    en = " Supported file types: <strong>.csv</strong>, <strong>.tsv</strong>, <strong>.txt</strong>, <strong>.xlsx</strong>, <strong>.xls</strong>, <strong>.rds</strong><br> <em>Note: First row must contain column headers.</em>",
    zh = " \u652f\u6301\u7684\u6587\u4ef6\u7c7b\u578b\uff1a<strong>.csv</strong>\u3001<strong>.tsv</strong>\u3001<strong>.txt</strong>\u3001<strong>.xlsx</strong>\u3001<strong>.xls</strong>\u3001<strong>.rds</strong><br> <em>\u6ce8\u610f\uff1a\u7b2c\u4e00\u884c\u5fc5\u987b\u5305\u542b\u5217\u540d\uff08header\uff09\u3002</em>"
  )
)

# Function to convert specified values to NA
convert_values_to_na <- function(data, missing_values) {
  if (!is.data.frame(data)) {
    return(data)
  }
  
  # Create a copy to avoid modifying original data
  result <- data
  
  for (i in seq_along(result)) {
    col <- result[[i]]
    
    # Handle different data types
    if (is.numeric(col)) {
      # For numeric columns, convert specified values to NA
      for (val in missing_values) {
        if (val == "NA") {
          # NA is already handled by R
          next
        } else if (val == "0") {
          result[[i]][col == 0] <- NA
        } else if (val == "-999") {
          result[[i]][col == -999] <- NA
        } else {
          # Try to convert to numeric and compare
          num_val <- tryCatch(as.numeric(val), error = function(e) NA)
          if (!is.na(num_val)) {
            result[[i]][col == num_val] <- NA
          }
        }
      }
    } else if (is.character(col)) {
      # For character columns, convert specified values to NA
      for (val in missing_values) {
        if (val == "NA") {
          # NA is already handled by R
          next
        } else if (val == "") {
          result[[i]][col == ""] <- NA
        } else {
          result[[i]][col == val] <- NA
        }
      }
    }
  }
  
  return(result)
}

app_server <- function(input, output, session) {

  # Color reactive values
  preFilterColor <- reactiveVal("#DB3124")
  postFilterColor <- reactiveVal("#4B74B2")
  
  # 16 color palette (4 colors x 4 shades: light to dark)
  # This palette is used in the UI generation for color buttons
  # Note: The actual color values are hardcoded in the UI for better performance
  color_palette <- list(
    red = c("#FFB3B3", "#FF6666", "#FF0000", "#CC0000"),
    green = c("#B3FFB3", "#66FF66", "#00FF00", "#00CC00"),
    blue = c("#B3CCFF", "#6699FF", "#0066FF", "#0033CC"),
    yellow = c("#FFFFB3", "#FFFF66", "#FFFF00", "#CCCC00")
  )
  
  # Suppress unused variable warning - palette is used for reference
  if (FALSE) {
    color_palette
  }

  data <- reactive({
    req(input$file)

    ext <- tools::file_ext(input$file$name)
    filepath <- input$file$datapath

    tryCatch({
      res <- switch(tolower(ext),
                    "csv"  = readr::read_csv(filepath, col_names = TRUE),
                    "tsv"  = readr::read_tsv(filepath, col_names = TRUE),
                    "txt"  = read.table(filepath, header = TRUE, sep = "", stringsAsFactors = FALSE),
                    "xlsx" = readxl::read_excel(filepath),
                    "xls"  = readxl::read_excel(filepath),
                    "rds"  = readRDS(filepath),
                    NULL   # fallback
      )

      if (is.null(res)) {
        lang <- current_lang()
        showNotification(
          HTML(paste0("\u1f6ab <strong>", get_label("unsupported_file", lang), "</strong> <code>", ext, "</code>")),
          type = "error"
        )
        return(NULL)
      } else {
        # Convert user-defined missing values to NA
        missing_values <- input$missingValueOptions %||% "NA"
        if (!is.null(missing_values) && length(missing_values) > 0) {
          res <- convert_values_to_na(res, missing_values)
        }
        return(res)
      }
    }, error = function(e) {
      lang <- current_lang()
      showNotification(
        HTML(paste0("\u26a0\ufe0f <strong>", get_label("file_error", lang), "</strong>")),
        type = "error"
      )
      return(NULL)
    })
  })


  observe({
    req(data())
    updateSelectInput(session, "columns", choices = names(data()))

    # Identify potential categorical variables
    df <- data()
    categorical_candidates <- names(df)[sapply(df, function(x) {
      is.character(x) || is.factor(x) ||
      (is.numeric(x) && length(unique(x)) <= 20 && length(unique(x)) < nrow(df) * 0.5)
    })]
    updateSelectInput(session, "categoricalColumns", choices = categorical_candidates)
  })

  # Get current language
  current_lang <- reactive({
    input$language %||% "en"
  })

  # App title
  output$appTitle <- renderText({
    get_label("app_title", current_lang())
  })

  # File upload UI
  output$fileUploadUI <- renderUI({
    lang <- current_lang()
    shiny::div(
      shiny::fileInput(
        "file",
        get_label("file_upload", lang),
        accept = c(".csv", ".txt", ".tsv", ".xlsx", ".xls", ".rds")
      ),
      shiny::HTML(paste0("<span style='color: #444;'>", get_label("supported_types", lang), "</span>")),
      br(),
      shiny::div(
        style = "margin-top: 10px; padding: 10px; background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 5px;",
        shiny::h6(shiny::textOutput("missingValueFormatLabel")),
        shiny::checkboxGroupInput("missingValueOptions", 
                                 shiny::textOutput("missingValueFormatLabel"), 
                                 choices = list("NA" = "NA", "0" = "0", "-999" = "-999", "Empty" = ""),
                                 selected = "NA",
                                 inline = TRUE),
        shiny::helpText(shiny::textOutput("missingValueFormatHelp"))
      )
    )
  })

  # Column selection UI
  output$columnSelectionUI <- renderUI({
    lang <- current_lang()
    shiny::selectInput(
      "columns",
      shiny::HTML(paste0("&#x1F5C2;&#xFE0F; ", get_label("select_columns", lang))),
      choices = NULL, multiple = TRUE
    )
  })

  # Categorical filter title
  #output$categoricalFilterTitle <- renderText({
   # get_label("categorical_vars", current_lang())
  #})

  # Categorical selection UI
  output$categoricalSelectionUI <- renderUI({
    lang <- current_lang()
    shiny::selectInput(
      "categoricalColumns",
      shiny::HTML(paste0("&#x1F4CA; ", get_label("categorical_vars", lang))),
      choices = NULL, multiple = TRUE
    )
  })

  # Plot type title
  output$plotTypeTitle <- renderText({
    get_label("plot_type_title", current_lang())
  })

  # Plot type UI
  output$plotTypeUI <- renderUI({
    lang <- current_lang()
    shiny::selectInput(
      "plotType",
      shiny::HTML(paste0("&#x1F4CA; ", get_label("plot_type", lang))),
      choices = stats::setNames(c("histogram", "boxplot"),
                        c(get_label("histogram", lang), get_label("boxplot", lang))),
      selected = "histogram"
    )
  })

  # Bins UI
  output$binsUI <- renderUI({
    lang <- current_lang()
    shiny::conditionalPanel(
      condition = "input.plotType == 'histogram'",
      shiny::numericInput("bins", get_label("hist_bin", lang), value = 30, min = 1, step = 1)
    )
  })

  # Color customization title
  output$colorCustomizationTitle <- renderText({
    get_label("color_customization", current_lang())
  })

  # Show/Hide color options text
  output$showColorOptionsText <- renderText({
    lang <- current_lang()
    if (is.null(input$toggleColorOptions) || input$toggleColorOptions %% 2 == 0) {
      get_label("show_color_options", lang)
    } else {
      get_label("hide_color_options", lang)
    }
  })

  # Color customization UI
  output$colorCustomizationUI <- renderUI({
    lang <- current_lang()
    shiny::div(
      # Pre-filter color
      shiny::div(
        style = "margin-bottom: 20px;",
        shiny::h5(get_label("pre_filter_color", lang)),
        
        # Color palette for pre-filter
        shiny::div(
          style = "margin-bottom: 10px;",
          shiny::h6(get_label("color_palette_label", lang), style = "margin-bottom: 8px; font-weight: bold;"),
          shiny::div(
            style = "display: grid; grid-template-columns: repeat(4, 1fr); gap: 5px; margin-bottom: 10px;",
            shiny::div(style = "text-align: center; font-size: 10px; color: #666;", "Red"),
            shiny::div(style = "text-align: center; font-size: 10px; color: #666;", "Green"),
            shiny::div(style = "text-align: center; font-size: 10px; color: #666;", "Blue"),
            shiny::div(style = "text-align: center; font-size: 10px; color: #666;", "Yellow"),
            # Red shades
            shiny::actionButton("preFilterRed1", "", style = "width: 30px; height: 30px; background-color: #FFB3B3; border: 1px solid #ccc;"),
            shiny::actionButton("preFilterGreen1", "", style = "width: 30px; height: 30px; background-color: #B3FFB3; border: 1px solid #ccc;"),
            shiny::actionButton("preFilterBlue1", "", style = "width: 30px; height: 30px; background-color: #B3CCFF; border: 1px solid #ccc;"),
            shiny::actionButton("preFilterYellow1", "", style = "width: 30px; height: 30px; background-color: #FFFFB3; border: 1px solid #ccc;"),
            # Row 2
            shiny::actionButton("preFilterRed2", "", style = "width: 30px; height: 30px; background-color: #FF6666; border: 1px solid #ccc;"),
            shiny::actionButton("preFilterGreen2", "", style = "width: 30px; height: 30px; background-color: #66FF66; border: 1px solid #ccc;"),
            shiny::actionButton("preFilterBlue2", "", style = "width: 30px; height: 30px; background-color: #6699FF; border: 1px solid #ccc;"),
            shiny::actionButton("preFilterYellow2", "", style = "width: 30px; height: 30px; background-color: #FFFF66; border: 1px solid #ccc;"),
            # Row 3
            shiny::actionButton("preFilterRed3", "", style = "width: 30px; height: 30px; background-color: #FF0000; border: 1px solid #ccc;"),
            shiny::actionButton("preFilterGreen3", "", style = "width: 30px; height: 30px; background-color: #00FF00; border: 1px solid #ccc;"),
            shiny::actionButton("preFilterBlue3", "", style = "width: 30px; height: 30px; background-color: #0066FF; border: 1px solid #ccc;"),
            shiny::actionButton("preFilterYellow3", "", style = "width: 30px; height: 30px; background-color: #FFFF00; border: 1px solid #ccc;"),
            # Row 4
            shiny::actionButton("preFilterRed4", "", style = "width: 30px; height: 30px; background-color: #CC0000; border: 1px solid #ccc;"),
            shiny::actionButton("preFilterGreen4", "", style = "width: 30px; height: 30px; background-color: #00CC00; border: 1px solid #ccc;"),
            shiny::actionButton("preFilterBlue4", "", style = "width: 30px; height: 30px; background-color: #0033CC; border: 1px solid #ccc;"),
            shiny::actionButton("preFilterYellow4", "", style = "width: 30px; height: 30px; background-color: #CCCC00; border: 1px solid #ccc;")
          )
        ),
        
        # Manual input for pre-filter
        shiny::div(
          style = "display: flex; align-items: center; gap: 10px;",
          shiny::textInput(
            "preFilterColorInput", 
            get_label("color_input_label", lang), 
            value = preFilterColor(),
            width = "150px"
          ),
          shiny::div(
            style = "display: flex; align-items: center; gap: 5px;",
            shiny::textOutput("color_picker_label", inline = TRUE),
            shiny::actionButton("preFilterColorPicker", "", 
                               style = paste0("width: 30px; height: 30px; background-color: ", preFilterColor(), "; border: 1px solid #ccc;"))
          )
        )
      ),
      
      # Post-filter color
      shiny::div(
        style = "margin-bottom: 20px;",
        shiny::h5(get_label("post_filter_color", lang)),
        
        # Color palette for post-filter
        shiny::div(
          style = "margin-bottom: 10px;",
          shiny::h6(get_label("color_palette_label", lang), style = "margin-bottom: 8px; font-weight: bold;"),
          shiny::div(
            style = "display: grid; grid-template-columns: repeat(4, 1fr); gap: 5px; margin-bottom: 10px;",
            shiny::div(style = "text-align: center; font-size: 10px; color: #666;", "Red"),
            shiny::div(style = "text-align: center; font-size: 10px; color: #666;", "Green"),
            shiny::div(style = "text-align: center; font-size: 10px; color: #666;", "Blue"),
            shiny::div(style = "text-align: center; font-size: 10px; color: #666;", "Yellow"),
            # Red shades
            shiny::actionButton("postFilterRed1", "", style = "width: 30px; height: 30px; background-color: #FFB3B3; border: 1px solid #ccc;"),
            shiny::actionButton("postFilterGreen1", "", style = "width: 30px; height: 30px; background-color: #B3FFB3; border: 1px solid #ccc;"),
            shiny::actionButton("postFilterBlue1", "", style = "width: 30px; height: 30px; background-color: #B3CCFF; border: 1px solid #ccc;"),
            shiny::actionButton("postFilterYellow1", "", style = "width: 30px; height: 30px; background-color: #FFFFB3; border: 1px solid #ccc;"),
            # Row 2
            shiny::actionButton("postFilterRed2", "", style = "width: 30px; height: 30px; background-color: #FF6666; border: 1px solid #ccc;"),
            shiny::actionButton("postFilterGreen2", "", style = "width: 30px; height: 30px; background-color: #66FF66; border: 1px solid #ccc;"),
            shiny::actionButton("postFilterBlue2", "", style = "width: 30px; height: 30px; background-color: #6699FF; border: 1px solid #ccc;"),
            shiny::actionButton("postFilterYellow2", "", style = "width: 30px; height: 30px; background-color: #FFFF66; border: 1px solid #ccc;"),
            # Row 3
            shiny::actionButton("postFilterRed3", "", style = "width: 30px; height: 30px; background-color: #FF0000; border: 1px solid #ccc;"),
            shiny::actionButton("postFilterGreen3", "", style = "width: 30px; height: 30px; background-color: #00FF00; border: 1px solid #ccc;"),
            shiny::actionButton("postFilterBlue3", "", style = "width: 30px; height: 30px; background-color: #0066FF; border: 1px solid #ccc;"),
            shiny::actionButton("postFilterYellow3", "", style = "width: 30px; height: 30px; background-color: #FFFF00; border: 1px solid #ccc;"),
            # Row 4
            shiny::actionButton("postFilterRed4", "", style = "width: 30px; height: 30px; background-color: #CC0000; border: 1px solid #ccc;"),
            shiny::actionButton("postFilterGreen4", "", style = "width: 30px; height: 30px; background-color: #00CC00; border: 1px solid #ccc;"),
            shiny::actionButton("postFilterBlue4", "", style = "width: 30px; height: 30px; background-color: #0033CC; border: 1px solid #ccc;"),
            shiny::actionButton("postFilterYellow4", "", style = "width: 30px; height: 30px; background-color: #CCCC00; border: 1px solid #ccc;")
          )
        ),
        
        # Manual input for post-filter
        shiny::div(
          style = "display: flex; align-items: center; gap: 10px;",
          shiny::textInput(
            "postFilterColorInput", 
            get_label("color_input_label", lang), 
            value = postFilterColor(),
            width = "150px"
          ),
          shiny::div(
            style = "display: flex; align-items: center; gap: 5px;",
            shiny::textOutput("color_picker_label", inline = TRUE),
            shiny::actionButton("postFilterColorPicker", "", 
                               style = paste0("width: 30px; height: 30px; background-color: ", postFilterColor(), "; border: 1px solid #ccc;"))
          )
        )
      ),
      
      # Reset button
      shiny::div(
        style = "text-align: center; margin-top: 10px;",
        shiny::actionButton("resetColors", get_label("reset_colors", lang), 
                           class = "btn btn-warning btn-sm")
      )
    )
  })

  # Color input handlers
  observeEvent(input$preFilterColorInput, {
    if (!is.null(input$preFilterColorInput) && nchar(input$preFilterColorInput) > 0) {
      # Validate hex color format
      if (grepl("^#[0-9A-Fa-f]{6}$", input$preFilterColorInput)) {
        preFilterColor(input$preFilterColorInput)
      }
    }
  })

  observeEvent(input$postFilterColorInput, {
    if (!is.null(input$postFilterColorInput) && nchar(input$postFilterColorInput) > 0) {
      # Validate hex color format
      if (grepl("^#[0-9A-Fa-f]{6}$", input$postFilterColorInput)) {
        postFilterColor(input$postFilterColorInput)
      }
    }
  })

  # Color picker handlers (using shinyjs for color picker)
  observeEvent(input$preFilterColorPicker, {
    shinyjs::runjs("
      const color = prompt('Enter color (e.g., #FF0000):', '#DB3124');
      if (color && /^#[0-9A-Fa-f]{6}$/.test(color)) {
        Shiny.setInputValue('preFilterColorInput', color);
      }
    ")
  })

  observeEvent(input$postFilterColorPicker, {
    shinyjs::runjs("
      const color = prompt('Enter color (e.g., #FF0000):', '#4B74B2');
      if (color && /^#[0-9A-Fa-f]{6}$/.test(color)) {
        Shiny.setInputValue('postFilterColorInput', color);
      }
    ")
  })

  # Color palette button handlers for pre-filter
  observeEvent(input$preFilterRed1, { preFilterColor("#FFB3B3"); updateTextInput(session, "preFilterColorInput", value = "#FFB3B3") })
  observeEvent(input$preFilterRed2, { preFilterColor("#FF6666"); updateTextInput(session, "preFilterColorInput", value = "#FF6666") })
  observeEvent(input$preFilterRed3, { preFilterColor("#FF0000"); updateTextInput(session, "preFilterColorInput", value = "#FF0000") })
  observeEvent(input$preFilterRed4, { preFilterColor("#CC0000"); updateTextInput(session, "preFilterColorInput", value = "#CC0000") })
  
  observeEvent(input$preFilterGreen1, { preFilterColor("#B3FFB3"); updateTextInput(session, "preFilterColorInput", value = "#B3FFB3") })
  observeEvent(input$preFilterGreen2, { preFilterColor("#66FF66"); updateTextInput(session, "preFilterColorInput", value = "#66FF66") })
  observeEvent(input$preFilterGreen3, { preFilterColor("#00FF00"); updateTextInput(session, "preFilterColorInput", value = "#00FF00") })
  observeEvent(input$preFilterGreen4, { preFilterColor("#00CC00"); updateTextInput(session, "preFilterColorInput", value = "#00CC00") })
  
  observeEvent(input$preFilterBlue1, { preFilterColor("#B3CCFF"); updateTextInput(session, "preFilterColorInput", value = "#B3CCFF") })
  observeEvent(input$preFilterBlue2, { preFilterColor("#6699FF"); updateTextInput(session, "preFilterColorInput", value = "#6699FF") })
  observeEvent(input$preFilterBlue3, { preFilterColor("#0066FF"); updateTextInput(session, "preFilterColorInput", value = "#0066FF") })
  observeEvent(input$preFilterBlue4, { preFilterColor("#0033CC"); updateTextInput(session, "preFilterColorInput", value = "#0033CC") })
  
  observeEvent(input$preFilterYellow1, { preFilterColor("#FFFFB3"); updateTextInput(session, "preFilterColorInput", value = "#FFFFB3") })
  observeEvent(input$preFilterYellow2, { preFilterColor("#FFFF66"); updateTextInput(session, "preFilterColorInput", value = "#FFFF66") })
  observeEvent(input$preFilterYellow3, { preFilterColor("#FFFF00"); updateTextInput(session, "preFilterColorInput", value = "#FFFF00") })
  observeEvent(input$preFilterYellow4, { preFilterColor("#CCCC00"); updateTextInput(session, "preFilterColorInput", value = "#CCCC00") })

  # Color palette button handlers for post-filter
  observeEvent(input$postFilterRed1, { postFilterColor("#FFB3B3"); updateTextInput(session, "postFilterColorInput", value = "#FFB3B3") })
  observeEvent(input$postFilterRed2, { postFilterColor("#FF6666"); updateTextInput(session, "postFilterColorInput", value = "#FF6666") })
  observeEvent(input$postFilterRed3, { postFilterColor("#FF0000"); updateTextInput(session, "postFilterColorInput", value = "#FF0000") })
  observeEvent(input$postFilterRed4, { postFilterColor("#CC0000"); updateTextInput(session, "postFilterColorInput", value = "#CC0000") })
  
  observeEvent(input$postFilterGreen1, { postFilterColor("#B3FFB3"); updateTextInput(session, "postFilterColorInput", value = "#B3FFB3") })
  observeEvent(input$postFilterGreen2, { postFilterColor("#66FF66"); updateTextInput(session, "postFilterColorInput", value = "#66FF66") })
  observeEvent(input$postFilterGreen3, { postFilterColor("#00FF00"); updateTextInput(session, "postFilterColorInput", value = "#00FF00") })
  observeEvent(input$postFilterGreen4, { postFilterColor("#00CC00"); updateTextInput(session, "postFilterColorInput", value = "#00CC00") })
  
  observeEvent(input$postFilterBlue1, { postFilterColor("#B3CCFF"); updateTextInput(session, "postFilterColorInput", value = "#B3CCFF") })
  observeEvent(input$postFilterBlue2, { postFilterColor("#6699FF"); updateTextInput(session, "postFilterColorInput", value = "#6699FF") })
  observeEvent(input$postFilterBlue3, { postFilterColor("#0066FF"); updateTextInput(session, "postFilterColorInput", value = "#0066FF") })
  observeEvent(input$postFilterBlue4, { postFilterColor("#0033CC"); updateTextInput(session, "postFilterColorInput", value = "#0033CC") })
  
  observeEvent(input$postFilterYellow1, { postFilterColor("#FFFFB3"); updateTextInput(session, "postFilterColorInput", value = "#FFFFB3") })
  observeEvent(input$postFilterYellow2, { postFilterColor("#FFFF66"); updateTextInput(session, "postFilterColorInput", value = "#FFFF66") })
  observeEvent(input$postFilterYellow3, { postFilterColor("#FFFF00"); updateTextInput(session, "postFilterColorInput", value = "#FFFF00") })
  observeEvent(input$postFilterYellow4, { postFilterColor("#CCCC00"); updateTextInput(session, "postFilterColorInput", value = "#CCCC00") })

  # Reset colors to default
  observeEvent(input$resetColors, {
    preFilterColor("#DB3124")
    postFilterColor("#4B74B2")
    updateTextInput(session, "preFilterColorInput", value = "#DB3124")
    updateTextInput(session, "postFilterColorInput", value = "#4B74B2")
  })

  # QC filter title
  output$qcFilterTitle <- renderText({
    get_label("qc_filter_options", current_lang())
  })

  # QC mode UI
  output$qcModeUI <- renderUI({
    lang <- current_lang()
    shiny::radioButtons(
      "qcMode", get_label("qc_mode", lang),
      choices = stats::setNames(c("uniform", "individual"),
                        c(get_label("uniform_qc", lang), get_label("individual_qc", lang))),
      selected = "uniform"
    )
  })

  # Uniform QC controls
  output$uniformQCControls <- renderUI({
    lang <- current_lang()
    shiny::div(
      shiny::radioButtons(
        "filterType", get_label("filter_type", lang),
        choices = stats::setNames(c("threshold", "sd", "iqr"),
                          c(get_label("threshold_range", lang),
                            get_label("sd_multiplier", lang),
                            get_label("iqr_multiplier", lang)))
      ),
      shiny::conditionalPanel(
        condition = "input.filterType == 'threshold'",
        shiny::numericInput("minVal", get_label("min_threshold", lang), value = NA),
        shiny::numericInput("maxVal", get_label("max_threshold", lang), value = NA)
      ),
      shiny::conditionalPanel(
        condition = "input.filterType == 'sd'",
        shiny::numericInput("sdMultiplier", get_label("sd_multiplier", lang), value = 2, min = 0.1, step = 0.1)
      ),
      shiny::conditionalPanel(
        condition = "input.filterType == 'iqr'",
        shiny::numericInput("iqrMultiplier", get_label("iqr_multiplier", lang), value = 1.5, min = 0.1, step = 0.1)
      )
    )
  })

  # Individual QC title
  output$individualQCTitle <- renderText({
    get_label("configure_qc", current_lang())
  })

  # Action buttons UI
  output$actionButtonsUI <- renderUI({
    lang <- current_lang()
    shiny::div(
      shiny::actionButton("applyFilter", get_label("apply_filter", lang), class = "btn btn-primary"),
      shiny::actionButton("showDownloadModal", get_label("download_filtered", lang), class = "btn btn-success")
    )
  })

  # Tab titles
  output$dataPreviewTabTitle <- renderText({
    get_label("data_preview", current_lang())
  })

  output$qcResultsTabTitle <- renderText({
    get_label("qc_results", current_lang())
  })
  

  # Missing value modal text outputs
  output$missingValueModalTitle <- renderText({
    get_label("missing_value_modal_title", current_lang())
  })

  output$missingValueModalText <- renderText({
    get_label("missing_value_modal_text", current_lang())
  })

  output$missingValueFormatLabel <- renderText({
    get_label("missing_value_format_label", current_lang())
  })

  output$confirmDownloadText <- renderText({
    get_label("confirm_download_text", current_lang())
  })

  output$cancelDownloadText <- renderText({
    get_label("cancel_download_text", current_lang())
  })

  # QC Results content - only show when data is available
  output$qcResultsContent <- renderUI({
    lang <- current_lang()

    # Check if filtered data is available
    if (is.null(filteredData()) || is.null(selectedData())) {
      return(shiny::div(
        style = "text-align: center; padding: 50px; color: #666;",
        shiny::h4(get_label("no_data_comparison", lang))
      ))
    }

    shiny::div(
      shiny::div(
        get_label("removed_records", lang),
        style = "text-align:center; font-weight:bold; font-family:'Times New Roman', 'SimSun', serif; font-size:18px; margin-top:15px; margin-bottom:10px;"
      ),
      DT::dataTableOutput("filterStats"),
      shiny::br(),
      shiny::div(
        get_label("comparison_means", lang),
        style = "text-align:center; font-weight:bold; font-family:'Times New Roman', 'SimSun', serif; font-size:18px; margin-top:15px; margin-bottom:10px;"
      ),
      DT::DTOutput("qcSummaryTable"),
      shiny::plotOutput("comparisonPlots", height = "600px"),
      shiny::br(),
      shiny::downloadButton("downloadComparisonPlot", get_label("download_comparison", lang), class = "btn btn-success",
                            style = "float: right;")
    )
  })

  # Generate individual QC controls for each selected column
  output$individualQCControls <- renderUI({
    req(input$columns, input$qcMode == "individual")

    if (length(input$columns) == 0) {
      return(shiny::div("Please select columns first."))
    }

    controls <- list()

    for (i in seq_along(input$columns)) {
      col_name <- input$columns[i]

      controls[[i]] <- shiny::div(
        class = "well",
        style = "margin-bottom: 10px; padding: 10px;",
        shiny::h6(shiny::strong(paste("Trait:", col_name))),

        # Filter type selection for this trait
        shiny::radioButtons(
          inputId = paste0("filterType_", i),
          label = "Filter Type",
          choices = c(
            "Threshold Range" = "threshold",
            "Mean +/- Times Standard Deviation Multiplier" = "sd",
            "IQR Multiplier" = "iqr"
          ),
          selected = "sd"
        ),

        # Threshold inputs
        shiny::conditionalPanel(
          condition = paste0("input.filterType_", i, " == 'threshold'"),
          shiny::div(
            style = "display: inline-block; width: 48%; margin-right: 2%;",
            shiny::numericInput(
              inputId = paste0("minVal_", i),
              label = "Min Threshold",
              value = NA
            )
          ),
          shiny::div(
            style = "display: inline-block; width: 48%;",
            shiny::numericInput(
              inputId = paste0("maxVal_", i),
              label = "Max Threshold",
              value = NA
            )
          )
        ),

        # SD multiplier input
        shiny::conditionalPanel(
          condition = paste0("input.filterType_", i, " == 'sd'"),
          shiny::numericInput(
            inputId = paste0("sdMultiplier_", i),
            label = "SD Multiplier",
            value = 2,
            min = 0.1,
            step = 0.1
          )
        ),

        # IQR multiplier input
        shiny::conditionalPanel(
          condition = paste0("input.filterType_", i, " == 'iqr'"),
          shiny::numericInput(
            inputId = paste0("iqrMultiplier_", i),
            label = "IQR Multiplier",
            value = 1.5,
            min = 0.1,
            step = 0.1
          )
        )
      )
    }

    return(controls)
  })

  # Generate categorical filter controls
  output$categoricalFilters <- renderUI({
    req(input$categoricalColumns, data())

    lang <- current_lang()

    if (length(input$categoricalColumns) == 0) {
      return(shiny::div(get_label("no_categorical", lang)))
    }

    filters <- list()

    for (i in seq_along(input$categoricalColumns)) {
      col_name <- input$categoricalColumns[i]
      unique_values <- sort(unique(data()[[col_name]]))

      filters[[i]] <- shiny::div(
        class = "categorical-filter",
        shiny::h6(paste(get_label("filter_by", lang), col_name)),
        shiny::div(
          class = "checkbox-group",
          shiny::checkboxGroupInput(
            inputId = paste0("catFilter_", i),
            label = NULL,
            choices = unique_values,
            selected = unique_values,  # Default: select all
             inline = TRUE,  # Use horizontal layout to save space
            width = "100%"
          )
        )
      )
    }

    return(filters)
  })


  selectedData <- reactive({
    req(input$columns, data())

    df <- data()

    # Apply categorical filters if any are selected
    if (!is.null(input$categoricalColumns) && length(input$categoricalColumns) > 0) {
      for (i in seq_along(input$categoricalColumns)) {
        col_name <- input$categoricalColumns[i]
        selected_values <- input[[paste0("catFilter_", i)]]

        if (!is.null(selected_values) && length(selected_values) > 0) {
          df <- df[df[[col_name]] %in% selected_values, ]
        }
      }
    }

    # Select only the specified columns
    df %>% dplyr::select(dplyr::all_of(input$columns))
  })


  # Data summary information
  output$dataSummaryUI <- renderUI({
    req(selectedData(), data())

    lang <- current_lang()
    original_rows <- nrow(data())
    filtered_rows <- nrow(selectedData())

    summary_text <- paste0(
      "<div style='background-color: #E8F4FD; padding: 10px; border-radius: 5px; margin-bottom: 15px; font-family: Times New Roman, SimSun, serif;'>",
      "<strong>", get_label("data_summary", lang), "</strong><br>",
      get_label("original_dataset", lang), " ", original_rows, " ", get_label("rows", lang), "<br>",
      get_label("after_filtering", lang), " ", filtered_rows, " ", get_label("rows", lang), "<br>",
      get_label("filtered_out", lang), " ", original_rows - filtered_rows, " ", get_label("rows", lang), " (",
      round((original_rows - filtered_rows) / original_rows * 100, 1), "%)",
      "</div>"
    )

    shiny::HTML(summary_text)
  })

  output$previewTable <- DT::renderDataTable({
    req(selectedData())
    selectedData()
  }, options = list(pageLength = 10, dom = 't'))


  output$preDistPlot <- renderPlot({
    req(selectedData(), input$plotType)
    
    # Only select numeric columns for plotting
    numeric_cols <- selectedData() %>% 
      dplyr::select_if(is.numeric)
    
    if (ncol(numeric_cols) == 0) {
      lang <- current_lang()
      showNotification(get_label("no_data_plot", lang), type = "warning")
      return(NULL)
    }
    
    df_long <- numeric_cols %>%
      tidyr::pivot_longer(everything(), names_to = "Column", values_to = "Value") %>%
      dplyr::filter(!is.na(Value))
    plot_ready(TRUE)
    if (nrow(df_long) == 0) {
      lang <- current_lang()
      showNotification(get_label("no_data_plot", lang), type = "warning")
      return(NULL)

    }

    if (input$plotType == "histogram") {
      ggplot(df_long, aes(x = Value)) +
        geom_histogram(bins = input$bins, fill = preFilterColor(), alpha = 0.7) +
        facet_wrap(~ Column, scales = "free") +
        labs(title = "Pre-Filter Data Distribution", x = "Value", y = "Frequency") +
        theme_minimal(base_size = 14) +
        theme(plot.title = element_text(hjust = 0.5))
    } else {
      #ggplot(df_long, aes(y = Value, x = Column)) +
       # geom_boxplot(fill = preFilterColor(), alpha = 0.7) +
        #labs(title = "Pre-Filter Data Distribution (Boxplot)", x = "Column", y = "Value") +
        #theme_minimal(base_size = 14) +
        #theme(axis.text.x = element_text(angle = 45, hjust = 1),
         #     plot.title = element_text(hjust = 0.5))
      ggplot(df_long, aes(y = Value)) +
        geom_boxplot(fill = preFilterColor(), alpha = 0.7) +
        facet_wrap(~ Column, scales = "free") +
        labs(title = "Pre-Filter Data Distribution (Boxplot)", x = "Column", y = "Value") +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = ggplot2::element_text(hjust = 0.5),
          axis.text.x = ggplot2::element_blank(),  # \u6ca1\u5fc5\u8981\u663e\u793a x \u8f74\uff08\u56e0\u4e3a\u53ea\u6709\u4e00\u4e2a box\uff09
          axis.ticks.x = ggplot2::element_blank()
        )
    }
  })

  # \u5e94\u7528\u7b5b\u9009\u5e76\u7edf\u8ba1
  filteredData <- eventReactive(input$applyFilter, {
    req(input$columns, data())

    df <- data()

    # Apply categorical filters first
    if (!is.null(input$categoricalColumns) && length(input$categoricalColumns) > 0) {
      for (i in seq_along(input$categoricalColumns)) {
        col_name <- input$categoricalColumns[i]
        selected_values <- input[[paste0("catFilter_", i)]]

        if (!is.null(selected_values) && length(selected_values) > 0) {
          df <- df[df[[col_name]] %in% selected_values, ]
        }
      }
    }

    # Convert selected columns to numeric
    for (col in input$columns) {
      df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
    }

    filter_stats <- list()

    if (input$qcMode == "uniform") {
      # Original uniform filtering logic
      if (input$filterType == "threshold") {
        min_val <- input$minVal
        max_val <- input$maxVal
        criteria <- paste0("Threshold: [",
                            ifelse(is.na(min_val), "-Inf", min_val), ", ",
                            ifelse(is.na(max_val), "Inf", max_val), "]")
        for (col in input$columns) {
          original_count <- sum(!is.na(df[[col]]))
          keep <- rep(TRUE, nrow(df))
          if (!is.na(min_val)) keep <- keep & (df[[col]] >= min_val | is.na(df[[col]]))
          if (!is.na(max_val)) keep <- keep & (df[[col]] <= max_val | is.na(df[[col]]))

          df[[col]][!keep] <- NA
          filtered_count <- sum(!is.na(df[[col]]))
          filter_stats[[col]] <- list(
            removed = original_count - filtered_count,
            remaining = filtered_count,
            criteria = criteria
          )
        }

      } else if (input$filterType == "sd") {
        multiplier <- input$sdMultiplier
           criteria <- paste0("Mean +/- ", multiplier, " * SD")
        for (col in input$columns) {
          original_count <- sum(!is.na(df[[col]]))
          col_mean <- mean(df[[col]], na.rm = TRUE)
          col_sd <- sd(df[[col]], na.rm = TRUE)
          lower <- col_mean - multiplier * col_sd
          upper <- col_mean + multiplier * col_sd
          df[[col]][!(df[[col]] >= lower & df[[col]] <= upper | is.na(df[[col]]))] <- NA
          filtered_count <- sum(!is.na(df[[col]]))
          filter_stats[[col]] <- list(
            removed = original_count - filtered_count,
            remaining = filtered_count,
            criteria = criteria
          )
        }

      } else if (input$filterType == "iqr") {
        multiplier <- input$iqrMultiplier
           criteria <- paste0("IQR * ", multiplier)
        for (col in input$columns) {
          original_count <- sum(!is.na(df[[col]]))
          qs <- stats::quantile(df[[col]], probs = c(0.25, 0.75), na.rm = TRUE, names = FALSE)
          iqr <- qs[2] - qs[1]
          lower_bound <- qs[1] - multiplier * iqr
          upper_bound <- qs[2] + multiplier * iqr
          df[[col]][!(df[[col]] >= lower_bound & df[[col]] <= upper_bound | is.na(df[[col]]))] <- NA
          filtered_count <- sum(!is.na(df[[col]]))
          filter_stats[[col]] <- list(
            removed = original_count - filtered_count,
            remaining = filtered_count,
            criteria = criteria
          )
        }
      }
    } else {
      # Individual filtering logic
      for (i in seq_along(input$columns)) {
        col <- input$columns[i]
        filter_type <- input[[paste0("filterType_", i)]]

        original_count <- sum(!is.na(df[[col]]))
        criteria <- ""

        if (filter_type == "threshold") {
          min_val <- input[[paste0("minVal_", i)]]
          max_val <- input[[paste0("maxVal_", i)]]
          criteria <- paste0("Threshold: [",
                            ifelse(is.na(min_val), "-Inf", min_val), ", ",
                            ifelse(is.na(max_val), "Inf", max_val), "]")
          keep <- rep(TRUE, nrow(df))
          if (!is.na(min_val)) keep <- keep & (df[[col]] >= min_val | is.na(df[[col]]))
          if (!is.na(max_val)) keep <- keep & (df[[col]] <= max_val | is.na(df[[col]]))
          df[[col]][!keep] <- NA

        } else if (filter_type == "sd") {
          multiplier <- input[[paste0("sdMultiplier_", i)]]
          criteria <- paste0("Mean +/- ", multiplier, " * SD")
          col_mean <- mean(df[[col]], na.rm = TRUE)
          col_sd <- sd(df[[col]], na.rm = TRUE)
          lower <- col_mean - multiplier * col_sd
          upper <- col_mean + multiplier * col_sd
          df[[col]][!(df[[col]] >= lower & df[[col]] <= upper | is.na(df[[col]]))] <- NA

        } else if (filter_type == "iqr") {
          multiplier <- input[[paste0("iqrMultiplier_", i)]]
          criteria <- paste0("IQR * ", multiplier)
          qs <- stats::quantile(df[[col]], probs = c(0.25, 0.75), na.rm = TRUE, names = FALSE)
          iqr <- qs[2] - qs[1]
          lower_bound <- qs[1] - multiplier * iqr
          upper_bound <- qs[2] + multiplier * iqr
          df[[col]][!(df[[col]] >= lower_bound & df[[col]] <= upper_bound | is.na(df[[col]]))] <- NA
        }

        filtered_count <- sum(!is.na(df[[col]]))
        filter_stats[[col]] <- list(
          removed = original_count - filtered_count,
          remaining = filtered_count,
          criteria = criteria
        )
      }
    }

    list(data = df, stats = filter_stats)
  })


  output$filterStats <- DT::renderDataTable({
    req(filteredData())
    stats <- filteredData()$stats


    if (length(stats) == 0) {
      return(DT::datatable(
        data.frame(Column = character(0), Original = integer(0),
                   Removed = integer(0), Remaining = integer(0), RemovalRate = numeric(0), QC_Criteria = character(0)),
        options = list(dom = 't')
      ))
    }


    df <- data.frame(
      Column = names(stats),
      Original = sapply(stats, function(x) (x$removed %||% 0) + (x$remaining %||% 0)),
      Removed = sapply(stats, function(x) x$removed),
      Remaining = sapply(stats, function(x) x$remaining),
      QC_Criteria = sapply(stats, function(x) x$criteria %||% ""),
      stringsAsFactors = FALSE
    )
    df$RemovalRate <- ifelse(df$Original > 0, df$Removed / df$Original, NA_real_)


    total_row <- data.frame(
      Column = "Total",
      Original = sum(df$Original, na.rm = TRUE),
      Removed = sum(df$Removed, na.rm = TRUE),
      Remaining = sum(df$Remaining, na.rm = TRUE),
      QC_Criteria = "",
      RemovalRate = ifelse(sum(df$Original, na.rm = TRUE) > 0,
                           sum(df$Removed, na.rm = TRUE) / sum(df$Original, na.rm = TRUE),
                           NA_real_)
    )
    df <- rbind(df, total_row)

    # ---- Count complete individuals (all selected traits are non-NA) ----
    complete_cases <- filteredData()$data %>%
      dplyr::select(dplyr::all_of(input$columns)) %>%
      stats::complete.cases() %>%
      sum()

    complete_row <- data.frame(
      Column = "Complete_Cases",
      Original = NA_integer_,
      Removed = NA_integer_,
      Remaining = complete_cases,
      QC_Criteria = "",
      RemovalRate = NA_real_
    )

    # Add this row after df
    df <- rbind(df, complete_row)

    DT::datatable(
      df,
      rownames = FALSE,
      options = list(pageLength = 10, autoWidth = TRUE)
    ) |>
      DT::formatRound(c("Original", "Removed", "Remaining"), 0) |>
      DT::formatPercentage("RemovalRate", 2)
  })

  # ---- QC Pre/Post Mean/SD Summary ----
  qcSummary <- reactive({
    req(selectedData(), filteredData(), input$columns)

    pre_df <- selectedData()
    post_df <- filteredData()$data[, input$columns, drop = FALSE]

    # Convert to numeric
    for (col in input$columns) {
      pre_df[[col]]  <- suppressWarnings(as.numeric(pre_df[[col]]))
      post_df[[col]] <- suppressWarnings(as.numeric(post_df[[col]]))
    }

    # Calculate for each column
    res <- lapply(input$columns, function(col) {
      pre_vals  <- pre_df[[col]]
      post_vals <- post_df[[col]]

      pre_mean  <- mean(pre_vals,  na.rm = TRUE)
      pre_sd    <- stats::sd(pre_vals, na.rm = TRUE)
      post_mean <- mean(post_vals, na.rm = TRUE)
      post_sd   <- stats::sd(post_vals, na.rm = TRUE)

      data.frame(
        Column      = col,
        Pre_Mean    = pre_mean,
        Pre_SD      = pre_sd,
        Post_Mean   = post_mean,
        Post_SD     = post_sd,
        Delta_Mean  = post_mean - pre_mean,
        Delta_SD    = post_sd - pre_sd,
        stringsAsFactors = FALSE
      )
    })

    do.call(rbind, res)
  })

  output$qcSummaryTable <- DT::renderDataTable({
    req(qcSummary())
    DT::datatable(
      qcSummary(),
      rownames = FALSE,
      options = list(pageLength = 10, autoWidth = TRUE)
    ) |>
      DT::formatRound(c("Pre_Mean","Pre_SD","Post_Mean","Post_SD","Delta_Mean","Delta_SD"), 3)
  })

  output$comparisonPlots <- renderPlot({
    req(filteredData(), input$plotType)

    # Only use numeric columns for plotting
    pre_numeric <- selectedData() %>% 
      dplyr::select_if(is.numeric)
    
    post_numeric <- filteredData()$data %>% 
      dplyr::select_if(is.numeric)

    if (ncol(pre_numeric) == 0 || ncol(post_numeric) == 0) {
      lang <- current_lang()
      showNotification(get_label("no_data_comparison", lang), type = "warning")
      return(NULL)
    }

    pre_long <- pre_numeric %>%
      tidyr::pivot_longer(everything(), names_to = "Column", values_to = "Value") %>%
      dplyr::filter(!is.na(Value)) %>%
      dplyr::mutate(Type = "Pre-Filter")

    post_long <- post_numeric %>%
      tidyr::pivot_longer(everything(), names_to = "Column", values_to = "Value") %>%
      dplyr::filter(!is.na(Value)) %>%
      dplyr::mutate(Type = "Post-Filter")

    combined <- dplyr::bind_rows(pre_long, post_long)
    if (nrow(combined) == 0) {
      lang <- current_lang()
      showNotification(get_label("no_data_comparison", lang), type = "warning")
      return(NULL)
    }

    if (input$plotType == "histogram") {
      ggplot(combined, aes(x = Value, fill = Type)) +
        geom_histogram(bins = input$bins, alpha = 0.7, position = "dodge") +
        facet_wrap(~ Column, scales = "free") +
        scale_fill_manual(values = c("Pre-Filter" = preFilterColor(), "Post-Filter" = postFilterColor())) +
        labs(title = "Pre-Filter vs Post-Filter Data Distribution", x = "Value", y = "Frequency") +
        theme_minimal(base_size = 14) +
        theme(plot.title = element_text(hjust = 0.5))
    } else {
      ggplot(combined, aes(y = Value, x = Type, fill = Type)) +
        geom_boxplot(alpha = 0.7) +
        facet_wrap(~ Column, scales = "free") +
        scale_fill_manual(values = c("Pre-Filter" = preFilterColor(), "Post-Filter" = postFilterColor())) +
        labs(title = "Pre-Filter vs Post-Filter Data Distribution (Boxplot)", x = "Type", y = "Value") +
        theme_minimal(base_size = 14) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              plot.title = element_text(hjust = 0.5))
    }
  })


  # Show download modal when download button is clicked
  observeEvent(input$showDownloadModal, {
    req(filteredData())
    shiny::showModal(shiny::modalDialog(
      title = get_label("missing_value_modal_title", current_lang()),
      shiny::div(
        style = "text-align: center;",
        shiny::h5(get_label("missing_value_modal_text", current_lang())),
        shiny::br(),
        shiny::radioButtons(
          "missingValueFormat",
          label = get_label("missing_value_format_label", current_lang()),
          choices = list(
            "NA" = "na",
            "0" = "zero", 
            "-999" = "minus999"
          ),
          selected = "na",
          inline = TRUE
        ),
        shiny::br(),
        shiny::div(
          style = "text-align: center;",
          shiny::actionButton("confirmDownload", 
                             get_label("confirm_download_text", current_lang()), 
                             class = "btn btn-primary",
                             style = "margin-right: 10px;"),
          shiny::actionButton("cancelDownload", 
                             get_label("cancel_download_text", current_lang()), 
                             class = "btn btn-secondary")
        )
      ),
      size = "m",
      easyClose = FALSE,
      footer = NULL
    ))
  })

  # Handle cancel download
  observeEvent(input$cancelDownload, {
    shiny::removeModal()
  })

  # Store missing value format for download
  downloadFormat <- reactiveVal("na")
  
  # Handle confirm download
  observeEvent(input$confirmDownload, {
    shiny::removeModal()
    # Store the selected format
    downloadFormat(input$missingValueFormat)
    showNotification("Starting download...", type = "message")
    
    # Generate TXT content directly
    req(filteredData())
    data_to_download <- filteredData()$data
    
    # Convert missing values using efficient function
    missing_format <- input$missingValueFormat
    data_to_download <- convert_missing_values(data_to_download, missing_format)
    
    # Create TXT content with space delimiter
    txt_content <- readr::format_delim(data_to_download, delim = " ")
    
    # Create filename
    filename <- paste("phenotype_", Sys.Date(), ".txt", sep = "")
    
    # Create download link using JavaScript
    session$sendCustomMessage("downloadCSV", list(
      content = txt_content,
      filename = filename
    ))
  })

  # Hidden download link (kept for compatibility)
  output$hiddenDownloadLink <- renderUI({
    shiny::downloadLink("downloadData", "", style = "display: none;")
  })

  # Download handler (kept for compatibility)
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("phenotype_", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      req(filteredData())
      
      # Get the filtered data (only the data part, not the stats)
      data_to_download <- filteredData()$data
      
      # Convert missing values using efficient function
      missing_format <- downloadFormat()
      data_to_download <- convert_missing_values(data_to_download, missing_format)
      
      # Write TXT file with space delimiter
      readr::write_delim(data_to_download, file, delim = " ")
    }
  )
  observeEvent(input$applyFilter, {
    updateTabsetPanel(session, inputId = "mainTabs", selected = "qc_results")
  })
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste0("pre_filter_plot_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(selectedData(), input$plotType)

      # Only select numeric columns for plotting
      numeric_cols <- selectedData() %>% 
        dplyr::select_if(is.numeric)
      
      if (ncol(numeric_cols) == 0) {
        lang <- current_lang()
        showNotification(get_label("no_data_download", lang), type = "warning")
        return(NULL)
      }
      
      df_long <- numeric_cols %>%
        tidyr::pivot_longer(everything(), names_to = "Column", values_to = "Value") %>%
        dplyr::filter(!is.na(Value))

      if (nrow(df_long) == 0) {
        lang <- current_lang()
        showNotification(get_label("no_data_download", lang), type = "warning")
        return(NULL)
      }

      # \u4f7f\u7528 png \u8bbe\u5907\u4fdd\u5b58\u56fe\u50cf
      grDevices::png(file, width = 1200, height = 800, res = 120)

      if (input$plotType == "histogram") {
        p <- ggplot(df_long, aes(x = Value)) +
          geom_histogram(bins = input$bins, fill = preFilterColor(), alpha = 0.7) +
          facet_wrap(~ Column, scales = "free") +
          labs(title = "Pre-Filter Data Distribution", x = "Value", y = "Frequency") +
          theme_minimal(base_size = 14) +
          theme(plot.title = element_text(hjust = 0.5))
      } else {
        p <- ggplot(df_long, aes(y = Value)) +
          geom_boxplot(fill = preFilterColor(), alpha = 0.7) +
          facet_wrap(~ Column, scales = "free") +
          labs(title = "Pre-Filter Data Distribution (Boxplot)", x = "Column", y = "Value") +
          theme_minimal(base_size = 14) +
          theme(
            plot.title = element_text(hjust = 0.5),
            axis.text.x = ggplot2::element_blank(),
            axis.ticks.x = ggplot2::element_blank()
          )
      }

      print(p)
      grDevices::dev.off()
    }
  )
  output$plotDownloadUI <- renderUI({
    if (plot_ready()) {
      lang <- current_lang()
      downloadButton("downloadPlot", get_label("download_plot", lang), class = "btn btn-success")
    }
  })
  output$downloadComparisonPlot <- downloadHandler(
    filename = function() {
      paste0("comparison_plot_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(filteredData(), input$plotType)

      # Only use numeric columns for plotting
      pre_numeric <- selectedData() %>% 
        dplyr::select_if(is.numeric)
      
      post_numeric <- filteredData()$data %>% 
        dplyr::select_if(is.numeric)

      if (ncol(pre_numeric) == 0 || ncol(post_numeric) == 0) {
        lang <- current_lang()
        showNotification(get_label("no_data_comparison", lang), type = "warning")
        return(NULL)
      }

      # prepare pre-filter long data
      pre_long <- pre_numeric %>%
        tidyr::pivot_longer(everything(), names_to = "Column", values_to = "Value") %>%
        dplyr::filter(!is.na(Value)) %>%
        dplyr::mutate(Type = "Pre-Filter")

      # prepare post-filter long data
      post_long <- post_numeric %>%
        tidyr::pivot_longer(everything(), names_to = "Column", values_to = "Value") %>%
        dplyr::filter(!is.na(Value)) %>%
        dplyr::mutate(Type = "Post-Filter")

      combined <- dplyr::bind_rows(pre_long, post_long)

      if (nrow(combined) == 0) {
        lang <- current_lang()
        showNotification(get_label("no_data_download", lang), type = "warning")
        return(NULL)
      }

      grDevices::png(file, width = 1200, height = 800, res = 120)

      if (input$plotType == "histogram") {
        p <- ggplot(combined, aes(x = Value, fill = Type)) +
          geom_histogram(bins = input$bins, alpha = 0.7, position = "dodge") +
          facet_wrap(~ Column, scales = "free") +
          scale_fill_manual(values = c("Pre-Filter" = preFilterColor(), "Post-Filter" = postFilterColor())) +
          labs(title = "Pre vs Post Filter Data Distribution", x = "Value", y = "Frequency") +
          theme_minimal(base_size = 14) +
          theme(plot.title = element_text(hjust = 0.5))
      } else {
        p <- ggplot(combined, aes(y = Value, x = Type, fill = Type)) +
          geom_boxplot(alpha = 0.7) +
          facet_wrap(~ Column, scales = "free") +
          scale_fill_manual(values = c("Pre-Filter" = preFilterColor(), "Post-Filter" = postFilterColor())) +
          labs(title = "Pre vs Post Filter Data Distribution (Boxplot)", x = "Type", y = "Value") +
          theme_minimal(base_size = 14) +
          theme(
            plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 45, hjust = 1)
          )
      }

      print(p)
      grDevices::dev.off()
    }
  )
  output$uploadUI <- renderUI({
    lang <- input$language %||% "en"
    wellPanel(
      fileInput(
        "file",
        get_label("file_upload", lang),
        accept = c(".csv", ".txt", ".tsv", ".xlsx", ".xls", ".rds")
      ),
      HTML(paste0("<span style='color: #444;'>", get_label("supported_types", lang), "</span>"))
    )
  })
}
