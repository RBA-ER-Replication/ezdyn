# Dashboard deploy entrypoint ---------------------------------------------
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source("usa_dashboard.R", local = TRUE)
app <- usa_dashboard()
app
