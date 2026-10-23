# Runner script for EcoShield Shiny Dashboard
message("Starting EcoShield Shiny Dashboard on http://127.0.0.1:3838 ...")
shiny::runApp(
  appDir = ".",
  host = "127.0.0.1",
  port = 3838,
  launch.browser = FALSE
)
