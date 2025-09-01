#' Application server-side logic
#'
#' @param input,output,session Internal parameters for {shiny}.
#'   DO NOT REMOVE.
#' @noRd
app_server <- function(input, output, session) {
  # your server code here
}
app_server <- function(input, output, session) {

  data <- reactive({
    req(input$file)
    tryCatch({
      readr::read_csv(input$file$datapath, col_names = TRUE)
    }, error = function(e) {
      showNotification("Error reading CSV file. Please ensure it is a valid CSV with headers.", type = "error")
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
      ggplot(df_long, aes(y = Value, x = Column)) +
        geom_boxplot(fill = "#DB3124", alpha = 0.7) +
        labs(title = "Pre-Filter Data Distribution (Boxplot)", x = "Column", y = "Value") +
        theme_minimal(base_size = 14) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              plot.title = element_text(hjust = 0.5))
    }
  })

  # 应用筛选并统计
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


    DT::datatable(
      df,
      rownames = FALSE,
      options = list(pageLength = 10, autoWidth = TRUE)
    ) |>
      DT::formatRound(c("Original", "Removed", "Remaining"), 0) |>
      DT::formatPercentage("RemovalRate", 2)
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
}
