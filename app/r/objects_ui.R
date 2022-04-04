box::use(
  shiny[bootstrapPage, 
        moduleServer, 
        NS, 
        renderText, 
        tags, 
        textOutput, 
        selectInput, 
        tagList, 
        div, 
        wellPanel, 
        htmlOutput, 
        uiOutput,
        plotOutput
        ],
)

#' @export
form_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("form_out") )  
}

#' @export
dataset_ui <- function(id, inputLabel) {
  ns <- NS(id)
  tagList(
    selectInput(ns("sel_df_name"), label = inputLabel, c("iris","mtcars")),
  )
}

#' @export
summa_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("summa_out") )
}

#' @export
main_plot_ui <- function(id) {
  ns <- NS(id)
  
  plotOutput(ns("mainPlot") , height = 350)
  
}

#' @export
main_var_ui <- function(id, inputLabel, vars_dataset_init) {
  ns <- NS(id)
  tagList(
    selectInput(ns("sel_main_var"), label = inputLabel, c("Sepal.Length", "Sepal.Width","Petal.Length", "Petal.Width")
    ),
  )
}

#' @export
var_ui <- function(id, var_input_name, inputLabel, vars_dataset_init,idx) {
  
  ns <- NS(id)
  
  tagList(
    selectInput(ns(var_input_name), label = inputLabel , c("Sepal.Length", "Sepal.Width","Petal.Length", "Petal.Width")
    ),
  )
  
}

#' @export
table_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("table_out") )
}

