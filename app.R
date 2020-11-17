library(shiny)
source("processx.R", local = TRUE)

ui <- fluidPage(
  column(
    width = 6,
    h2("Process 1"),
    helpText("Run an external shell script with no additional arguments passed"),
    processxUI("process1")
  ),
  column(
    width = 6,
    h2("Process 2"),
    helpText("Sleep system for x seconds and exit with error code 1. This demonstrates how to pass on additional arguments from shiny inputs outside of the module namespace. (This is not exactly how shiny modules are designed to be used, but it's the only way to do it without having individual modules for each process)"),
    sliderInput(
      inputId = NS("process2", "delay"),
      label = "Sleep time in seconds",
      min = 0,
      max = 10,
      value = 5,
      step = 1
    ),
    p(),
    processxUI("process2")
  )
)

server <- function(input, output, session) {
  processxServer("process1", 
                 command = "./script.sh")
  processxServer("process2",
                 command = "bash",
                 args = reactive({
                   delay <- as.integer(input[[NS("process2", "delay")]])
                   c(
                     "-c",
                     paste0("echo 'sleeping for ", delay, " seconds and exiting with error'; sleep ", delay, "; exit 1")
                   )})
                 )
}

shinyApp(ui, server)
