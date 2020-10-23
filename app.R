library(shiny)
source("processx.R", local = TRUE)

ui <- fluidPage(
  column(
    width = 6,
    h2("Process 1"),
    processxUI("process1")
  ),
  column(
    width = 6,
    h2("Process 2"),
    processxUI("process2")
  )
)

server <- function(input, output, session) {
  processxServer("process1", 
                 command = "script.sh")
  processxServer("process2",
                 command = "bash",
                 args = c("-c", "echo 'sleeping for 5 seconds and exiting with error'; sleep 5; exit 1"))
}

shinyApp(ui, server)
