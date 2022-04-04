#
# form module
#

box::use(
  shiny[bootstrapPage, 
        moduleServer, 
        NS, 
        renderText, 
        tags, 
        reactive,
        textOutput, 
        tagList, 
        div, 
        fluidPage,
        selectInput,
        numericInput,
        actionButton,
        reactiveValues,
        renderUI,
        bindEvent,
        observe,
        uiOutput],
)


#' @description this module processes the objects of the app, i.e. selected 
#' data.frame and processes it to produce a form with its variables. If the 
#' button is clicked a new 'row' is produced
#' @param vars_unifier set of objects shared among modules
#' @return list (row) to rbind to data.frame
#' @export
ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("form_out") )  
}

#' @export
server <- function(id, vars_unifier) {
  moduleServer(id, function(input, output, session) {
    
    ns<-session$ns
    
    output$form_out <- renderUI({
      
      sel_df <- vars_unifier$sel_df()
      
      df_cl  <- sapply(sel_df[1,], class)
      
      tagList(
        div(class="form-columns",
            div(class="form-inputs",
                tagList(
                  lapply( names(df_cl[df_cl!="numeric"]),
                          function(x) {
                            selectInput(ns(x), x,
                                        unique(sel_df[,x])
                            )
                          }
                  ) 
                  ,lapply( names(df_cl[df_cl=="numeric"] ) ,
                           function(x) {
                             numericInput(ns(x), x,
                                          min(sel_df[,x])
                                          , max = max(sel_df[,x])*2
                                          , step= 0.1
                             )
                           }
                  )
                )
            )
            , div(class="form-button"
                  , actionButton(ns("update"), "Update data.frame")
            )
        )
      )
    })
    
    values <- reactiveValues()
    
    observe({
      
      c_names <- colnames(vars_unifier$sel_df() )
      vals    <- lapply(c_names,function(x) input[[x]] )
      names(vals) <- c_names
      values[["vals"]]  <- vals
      
    }) |> bindEvent(input$update)
    
    return(
      list(
        vals  = reactive({ values[["vals"]] })
      )
    )
    
  })}
