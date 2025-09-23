# Load required libraries
library(shiny)
library(shinyjs)
library(readr)
library(dplyr)
library(DT)
library(ggplot2)
library(shinythemes)
library(tidyr)
library(readxl)
library(tools)
library(progress)
library(magrittr)

# Source the application files directly
source("R/app_ui.R")
source("R/app_server.R")
source("R/get_label.R")
source("R/convert_missing_values.R")

# Run the application
shinyApp(ui = app_ui, server = app_server)
