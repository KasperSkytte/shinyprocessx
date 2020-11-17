#shiny module to start asynchronous processes using processx package
#by Kasper Skytte Andersen
#https://github.com/kasperskytte

#shiny must be version 1.4.0.9003 or later to use shiny modules, install from github
installGitHub <- function(...) {
  if(!require("remotes")) {
    install.packages("remotes")
  }
  remotes::install_github(...)
}

if(any(grepl("^shiny$", installed.packages()[,1]))) {
  if(packageVersion("shiny") < "1.4.0.9003") {
    installGitHub("rstudio/shiny")
  }
} else 
  installGitHub("rstudio/shiny")

require("shiny")
require("processx")

processxUI <- function(id, logHeight = "500px", ...) {
  #JavaScript code to automatically scroll the log file to the last line
  autoscroll_JS <- paste0("
$(document).ready(function(){
  var objDiv = document.getElementById('", NS(id, "processLog"), "');
  // create an observer instance
  var observer = new MutationObserver(function(mutations) {
    objDiv.scrollTop = objDiv.scrollHeight - objDiv.clientHeight;
  });
  // configuration of the observer
  var config = {childList: true};
  // observe objDiv
  observer.observe(objDiv, config);
})
")
  list(
    tags$head(
      tags$script(HTML(autoscroll_JS)),
      tags$style(paste0("#", NS(id, "processLog"), "{overflow-y:scroll; max-height: ", logHeight, ";}"))
    ),
    uiOutput(NS(id, "startStopBtn")),
    p(),
    uiOutput(NS(id, "processStatus")),
    h4("Process log"),
    verbatimTextOutput(NS(id, "processLog"), ...),
    downloadButton(NS(id, "downloadLogfile"), label = "Export log file")
  )
}

processxServer <- function(id, ...) {
  moduleServer(id, function(input, output, session) {
    #reactive to store processx R6 class object
    process <- reactiveVal()
    
    #reactive to store logfile created on start
    logfile <- reactiveVal(tempfile())
    
    #start/stop button
    output$startStopBtn <- renderUI({
      if(isFALSE(processAlive())) {
        actionButton(
          inputId = NS(id, "startStopProcess"),
          label = "Start process"
        )
      } else if(isTRUE(processAlive())) {
        actionButton(
          inputId = NS(id, "startStopProcess"),
          label = "Kill process"
        )
      }
    })
    
    #start a new process and logfile when actionbutton is pressed
    observeEvent(input$startStopProcess, {
      #start process if not already running, otherwise kill
      startProcess <- function(...) {
        #generate new log file for each new process
        logfile(tempfile())
        
        #generate list of additional arguments to be passed to process
        #from outside the module namespace. args must therefore be a reactive()
        dots <- list(...)
        if(!is.null(dots$args))
          dots$args <- as.character(dots$args())
        arg_list <- c(
          dots,
          stderr = "2>&1",
          stdout = logfile(),
          supervise = TRUE)
        
        #start process piping stderr+stdout to logfile
        process(
          do.call(processx::process$new, arg_list)
        )
      }
      if(is.null(process()$is_alive))
        startProcess(...)
      else if(!is.null(process()$is_alive))
        if(isTRUE(process()$is_alive()))
           process()$kill_tree()
        else if(isFALSE(process()$is_alive()))
          startProcess(...)
    })
    
    #read process status every 500 ms (alive or not)
    #(only for updating status message below, otherwise use 
    # process()$is_alive() to avoid refresh interval delay)
    processAlive <- reactivePoll(
      intervalMillis = 500,
      session = session,
      checkFunc = function() {
        if(!is.null(process()$is_alive))
          process()$is_alive()
        else
          FALSE
      },
      valueFunc = function() {
        if(!is.null(process()$is_alive))
          process()$is_alive()
        else
          FALSE
      }
    )

    #print status message of process and exit status if finished
    output$processStatus <- renderUI({
      if(isTRUE(processAlive())) {
        p("Process is running...")
      } else if(isFALSE(processAlive()) && !is.null(process()$get_exit_status)) {
        if(process()$get_exit_status() == 0)
          p("Process has finished succesfully")
        else if(process()$get_exit_status() == -9)
          p("Process was killed")
        else if(!process()$get_exit_status() %in% c(0, -9))
          p(paste0("Process has errored (exit status: ", process()$get_exit_status(), ")"))
      }
    })
    
    #read logfile every 500 ms
    readLogfile <- reactivePoll(
      intervalMillis = 500,
      session = session,
      checkFunc = function() {
        if(file.exists(logfile()))
          file.info(logfile())[["mtime"]][1]
        else
          return('No process has run yet')
      },
      valueFunc = function() {
        if(file.exists(logfile()))
          readLines(logfile())
        else
          return('No process has run yet')
      }
    )
    
    #print process logfile
    output$processLog <- renderText({
      readLogfile()
    },
    sep = "\n")
    
    #export process logfile
    output$downloadLogfile <- downloadHandler(
      filename = function() {
        #append module id and date to logfile filename
        paste0("logfile_", id, "_", format(Sys.time(), format = "%y%m%d_%H%M%S"), ".txt")
      },
      content = function(file) {
        file.copy(from = logfile(), to = file)
      },
      contentType = "text/plain"
    )
  })
}
