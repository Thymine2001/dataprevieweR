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
app_server <- function(input, output, session) {

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
        showNotification(
          HTML(paste0("\u1f6ab <strong>Unsupported file type:</strong> <code>", ext, "</code>")),
          type = "error"
        )
        return(NULL)
      } else {
        return(res)
      }
    }, error = function(e) {
      showNotification(
        HTML("\u26a0\ufe0f <strong>Error reading file.</strong> Please check the file format."),
        type = "error"
      )
      return(NULL)
    })
  })


  observe({
    req(data())
    updateSelectInput(session, "columns", choices = names(data()))
  })


  selectedData <- reactive({
    req(input$columns, data())
    data() %>% dplyr::select(dplyr::all_of(input$columns))
  })


  output$previewTable <- DT::renderDataTable({
    req(selectedData())
    selectedData()
  }, options = list(pageLength = 10, dom = 't'))


  output$preDistPlot <- renderPlot({
    req(selectedData(), input$plotType)
    df_long <- selectedData() %>%
      tidyr::pivot_longer(everything(), names_to = "Column", values_to = "Value") %>%
      dplyr::mutate(Value = suppressWarnings(as.numeric(Value))) %>%
      dplyr::filter(!is.na(Value))
    plot_ready(TRUE)
    if (nrow(df_long) == 0) {
      showNotification("No numeric data available for plotting.", type = "warning")
      return(NULL)

    }

    if (input$plotType == "histogram") {
      ggplot(df_long, aes(x = Value)) +
        geom_histogram(bins = input$bins, fill = "#DB3124", alpha = 0.7) +
        facet_wrap(~ Column, scales = "free") +
        labs(title = "Pre-Filter Data Distribution", x = "Value", y = "Frequency") +
        theme_minimal(base_size = 14) +
        theme(plot.title = element_text(hjust = 0.5))
    } else {
      #ggplot(df_long, aes(y = Value, x = Column)) +
       # geom_boxplot(fill = "#DB3124", alpha = 0.7) +
        #labs(title = "Pre-Filter Data Distribution (Boxplot)", x = "Column", y = "Value") +
        #theme_minimal(base_size = 14) +
        #theme(axis.text.x = element_text(angle = 45, hjust = 1),
         #     plot.title = element_text(hjust = 0.5))
      ggplot(df_long, aes(y = Value)) +
        geom_boxplot(fill = "#DB3124", alpha = 0.7) +
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
    for (col in input$columns) {
      df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
    }

    filter_stats <- list()

    if (input$filterType == "threshold") {
      min_val <- input$minVal
      max_val <- input$maxVal
      for (col in input$columns) {
        original_count <- sum(!is.na(df[[col]]))
        keep <- rep(TRUE, nrow(df))
        if (!is.na(min_val)) keep <- keep & (df[[col]] >= min_val | is.na(df[[col]]))
        if (!is.na(max_val)) keep <- keep & (df[[col]] <= max_val | is.na(df[[col]]))

        df[[col]][!keep] <- NA
        filtered_count <- sum(!is.na(df[[col]]))
        filter_stats[[col]] <- list(
          removed = original_count - filtered_count,
          remaining = filtered_count
        )
      }

    } else if (input$filterType == "sd") {
      multiplier <- input$sdMultiplier
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
          remaining = filtered_count
        )
      }

    } else if (input$filterType == "iqr") {
      multiplier <- input$iqrMultiplier
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
          remaining = filtered_count
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
                   Removed = integer(0), Remaining = integer(0), RemovalRate = numeric(0)),
        options = list(dom = 't')
      ))
    }


    df <- data.frame(
      Column = names(stats),
      Original = sapply(stats, function(x) (x$removed %||% 0) + (x$remaining %||% 0)),
      Removed = sapply(stats, function(x) x$removed),
      Remaining = sapply(stats, function(x) x$remaining),
      stringsAsFactors = FALSE
    )
    df$RemovalRate <- ifelse(df$Original > 0, df$Removed / df$Original, NA_real_)


    total_row <- data.frame(
      Column = "Total",
      Original = sum(df$Original, na.rm = TRUE),
      Removed = sum(df$Removed, na.rm = TRUE),
      Remaining = sum(df$Remaining, na.rm = TRUE),
      RemovalRate = ifelse(sum(df$Original, na.rm = TRUE) > 0,
                           sum(df$Removed, na.rm = TRUE) / sum(df$Original, na.rm = TRUE),
                           NA_real_)
    )
    df <- rbind(df, total_row)

    # ---- 统计完整个体数（所有选定性状都非 NA）----
    complete_cases <- filteredData()$data %>%
      dplyr::select(dplyr::all_of(input$columns)) %>%
      stats::complete.cases() %>%
      sum()

    complete_row <- data.frame(
      Column = "Complete_Cases",
      Original = NA_integer_,
      Removed = NA_integer_,
      Remaining = complete_cases,
      RemovalRate = NA_real_
    )

    # 把这行加在 df 后面
    df <- rbind(df, complete_row)

    DT::datatable(
      df,
      rownames = FALSE,
      options = list(pageLength = 10, autoWidth = TRUE)
    ) |>
      DT::formatRound(c("Original", "Removed", "Remaining"), 0) |>
      DT::formatPercentage("RemovalRate", 2)
  })

  # ---- QC 前后均值/SD 汇总 ----
  qcSummary <- reactive({
    req(selectedData(), filteredData(), input$columns)

    pre_df <- selectedData()
    post_df <- filteredData()$data[, input$columns, drop = FALSE]

    # 转数值
    for (col in input$columns) {
      pre_df[[col]]  <- suppressWarnings(as.numeric(pre_df[[col]]))
      post_df[[col]] <- suppressWarnings(as.numeric(post_df[[col]]))
    }

    # 逐列计算
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

    pre_long <- selectedData() %>%
      tidyr::pivot_longer(everything(), names_to = "Column", values_to = "Value") %>%
      dplyr::mutate(Value = suppressWarnings(as.numeric(Value))) %>%
      dplyr::filter(!is.na(Value)) %>%
      dplyr::mutate(Type = "Pre-Filter")

    post_long <- filteredData()$data %>%
      dplyr::select(dplyr::all_of(input$columns)) %>%
      tidyr::pivot_longer(everything(), names_to = "Column", values_to = "Value") %>%
      dplyr::filter(!is.na(Value)) %>%
      dplyr::mutate(Type = "Post-Filter")

    combined <- dplyr::bind_rows(pre_long, post_long)
    if (nrow(combined) == 0) {
      showNotification("No data available for comparison plots.", type = "warning")
      return(NULL)
    }

    if (input$plotType == "histogram") {
      ggplot(combined, aes(x = Value, fill = Type)) +
        geom_histogram(bins = input$bins, alpha = 0.7, position = "dodge") +
        facet_wrap(~ Column, scales = "free") +
        scale_fill_manual(values = c("Pre-Filter" = "#DB3124", "Post-Filter" = "#4B74B2")) +
        labs(title = "Pre-Filter vs Post-Filter Data Distribution", x = "Value", y = "Frequency") +
        theme_minimal(base_size = 14) +
        theme(plot.title = element_text(hjust = 0.5))
    } else {
      ggplot(combined, aes(y = Value, x = Type, fill = Type)) +
        geom_boxplot(alpha = 0.7) +
        facet_wrap(~ Column, scales = "free") +
        scale_fill_manual(values = c("Pre-Filter" = "#DB3124", "Post-Filter" = "#4B74B2")) +
        labs(title = "Pre-Filter vs Post-Filter Data Distribution (Boxplot)", x = "Type", y = "Value") +
        theme_minimal(base_size = 14) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              plot.title = element_text(hjust = 0.5))
    }
  })


  output$downloadData <- downloadHandler(
    filename = function() {
      paste("filtered_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(filteredData())
      readr::write_csv(filteredData()$data, file)
    }
  )
  observeEvent(input$applyFilter, {
    updateTabsetPanel(session, inputId = "mainTabs", selected = "QC Results")
  }
  )
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste0("pre_filter_plot_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(selectedData(), input$plotType)

      df_long <- selectedData() %>%
        tidyr::pivot_longer(everything(), names_to = "Column", values_to = "Value") %>%
        dplyr::mutate(Value = suppressWarnings(as.numeric(Value))) %>%
        dplyr::filter(!is.na(Value))

      if (nrow(df_long) == 0) {
        showNotification("No data available to download.", type = "warning")
        return(NULL)
      }

      # \u4f7f\u7528 png \u8bbe\u5907\u4fdd\u5b58\u56fe\u50cf
      grDevices::png(file, width = 1200, height = 800, res = 120)

      if (input$plotType == "histogram") {
        p <- ggplot(df_long, aes(x = Value)) +
          geom_histogram(bins = input$bins, fill = "#DB3124", alpha = 0.7) +
          facet_wrap(~ Column, scales = "free") +
          labs(title = "Pre-Filter Data Distribution", x = "Value", y = "Frequency") +
          theme_minimal(base_size = 14) +
          theme(plot.title = element_text(hjust = 0.5))
      } else {
        p <- ggplot(df_long, aes(y = Value)) +
          geom_boxplot(fill = "#DB3124", alpha = 0.7) +
          facet_wrap(~ Column, scales = "free") +
          labs(title = "Pre-Filter Data Distribution (Boxplot)", x = "Column", y = "Value") +
          theme_minimal(base_size = 14) +
          theme(
            plot.title = element_text(hjust = 0.5),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank()
          )
      }

      print(p)
      grDevices::dev.off()
    }
  )
  output$plotDownloadUI <- renderUI({
    if (plot_ready()) {
      downloadButton("downloadPlot", "Download Plot (PNG)", class = "btn btn-success")
    }
  })
  output$downloadComparisonPlot <- downloadHandler(
    filename = function() {
      paste0("comparison_plot_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(filteredData(), input$plotType)

      # prepare pre-filter long data
      pre_long <- selectedData() %>%
        tidyr::pivot_longer(everything(), names_to = "Column", values_to = "Value") %>%
        dplyr::mutate(Value = suppressWarnings(as.numeric(Value))) %>%
        dplyr::filter(!is.na(Value)) %>%
        dplyr::mutate(Type = "Pre-Filter")

      # prepare post-filter long data
      post_long <- filteredData()$data %>%
        dplyr::select(dplyr::all_of(input$columns)) %>%
        tidyr::pivot_longer(everything(), names_to = "Column", values_to = "Value") %>%
        dplyr::filter(!is.na(Value)) %>%
        dplyr::mutate(Type = "Post-Filter")

      combined <- dplyr::bind_rows(pre_long, post_long)

      if (nrow(combined) == 0) {
        showNotification("No data available for plot.", type = "warning")
        return(NULL)
      }

      grDevices::png(file, width = 1200, height = 800, res = 120)

      if (input$plotType == "histogram") {
        p <- ggplot(combined, aes(x = Value, fill = Type)) +
          geom_histogram(bins = input$bins, alpha = 0.7, position = "dodge") +
          facet_wrap(~ Column, scales = "free") +
          scale_fill_manual(values = c("Pre-Filter" = "#DB3124", "Post-Filter" = "#4B74B2")) +
          labs(title = "Pre vs Post Filter Data Distribution", x = "Value", y = "Frequency") +
          theme_minimal(base_size = 14) +
          theme(plot.title = element_text(hjust = 0.5))
      } else {
        p <- ggplot(combined, aes(y = Value, x = Type, fill = Type)) +
          geom_boxplot(alpha = 0.7) +
          facet_wrap(~ Column, scales = "free") +
          scale_fill_manual(values = c("Pre-Filter" = "#DB3124", "Post-Filter" = "#4B74B2")) +
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
