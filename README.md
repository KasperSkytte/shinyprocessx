# shinyprocessx
A simple shiny module to run and control any Linux process using the processx package.

Live demo here: https://kasperskytte.shinyapps.io/shinyprocessx

# Tips
To set a max height of the process log `verbatimTextOutput` for the particular process set `tags$head(tags$style("#name-processLog{overflow-y:scroll; max-height: 500px;}"))` in the app, where `name` is the ID.
