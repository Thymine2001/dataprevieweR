# Deploy script for shinyapps.io
library(rsconnect)

# Set up your shinyapps.io account
# You need to run this once to authenticate:
# rsconnect::setAccountInfo(name='your-username', 
#                          token='your-token', 
#                          secret='your-secret')

# Deploy the application
rsconnect::deployApp(
  appDir = ".",
  appFiles = c("app.R", "R/", "inst/", "man/", "DESCRIPTION", "NAMESPACE"),
  appName = "dataprevieweR",
  appTitle = "Data Preview and QC Tool",
  forceUpdate = TRUE
)
