#' Internal imports
#'
#' These imports are used throughout the package.
#' @keywords internal
#' @name dataprevieweR-imports
#' @import shiny
#' @import golem
#' @import config
#' @importFrom magrittr %>%
#' @importFrom utils install.packages
#' @importFrom dplyr everything
#' @importFrom ggplot2 ggplot aes geom_histogram geom_boxplot facet_wrap labs theme_minimal theme element_text scale_fill_manual
#' @importFrom shinythemes shinytheme
#' @importFrom stats sd
#' @importFrom utils read.table
#' @importFrom shinythemes shinytheme
NULL
# R/imports.R
library(shiny)
library(shinythemes)
library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(DT)
library(ggplot2)
library(magrittr)
library(reticulate)
library(progress)
