# -*- coding: UTF-8 -*-
#' Convert missing values efficiently
#'
#' @param data Data frame to convert
#' @param format Missing value format: "na", "zero", or "minus999"
#' @return Data frame with converted missing values
#' @export
convert_missing_values <- function(data, format = "na") {
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  
  if (!format %in% c("na", "zero", "minus999")) {
    stop("Format must be one of: 'na', 'zero', 'minus999'")
  }
  
  # If format is "na", return data as is
  if (format == "na") {
    return(data)
  }
  
  # Create a copy to avoid modifying original data
  result <- data
  
  # Efficient conversion using vectorized operations
  if (format == "zero") {
    # Replace NA with 0 for numeric columns only
    numeric_cols <- sapply(result, is.numeric)
    for (i in which(numeric_cols)) {
      result[[i]][is.na(result[[i]])] <- 0
    }
  } else if (format == "minus999") {
    # Replace NA with -999 for numeric columns only
    numeric_cols <- sapply(result, is.numeric)
    for (i in which(numeric_cols)) {
      result[[i]][is.na(result[[i]])] <- -999
    }
  }
  
  return(result)
}
