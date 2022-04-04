
#
# dataset module
#

#' @description module to allow choosing a data.frame name and its feedback from
#' same module different namespace
#' @param inputLabel label for selectInput
#' @param vars_other_input is the same output of this module but from the other
#' module namespace, see return, used for feedback
#' @return list with data.frame name (string) selected

dataset_ui <- function(id, inputLabel) {
  ns <- NS(id)
  tagList(
    selectInput(ns("sel_df_name"), label = inputLabel, c("iris","mtcars")),
  )
}

dataset_server <- function(id,vars_other_input) {
  moduleServer(id, function(input, output, session) {

    observe({
      updateSelectInput(
        session,
        'sel_df_name',
        selected = vars_other_input$sel_df_name()
      )
    }) %>% bindEvent(vars_other_input$sel_df_name())

    return(
      list(
        sel_df_name   = reactive({ input$sel_df_name })
      )
    )

})}

#
# summary table module
#

#' @param description   this module processes the variables and data.frame to
#' produce a summary data.frame for the selected variables in other modules
#' @param vars_unifier  set of objects shared among modules
#' @param vars_main_var input of categorical (color) main variables
#' @param vars_x_var    input of X-axis variables
#' @param vars_y_var    input of Y-axis variables
#' @param summary_init  default data.frame

summa_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("summa_out") )
}

summa_server  <- function(id,vars_unifier,vars_main_var
                          ,vars_x_var,vars_y_var,summary_init) {
  moduleServer(id, function(input, output, session) {

  values<-reactiveValues(sumdf=summary_init, oth_len=4)

  observeEvent(list(vars_main_var$main_var()
               ,vars_x_var$var()
               ,vars_y_var$var() )
          , ignoreInit=T, {

    df1     <- vars_unifier$sel_df()
    main_var<- vars_main_var$main_var()
    oth     <- vars_unifier$oth_vars()
    vx <- vars_x_var$var()
    vy <- vars_y_var$var()

    validate(need(try(df1[,main_var]),"recalculating"))
    validate(need(try(df1[,oth]),"recalculating"))

    sumdf <- aggregate(list(df1[,c(vx,vy)])
                       , by= list(as.character(df1[,main_var]))
                       , FUN = function(x) { my.mean = mean(x, na.rm = TRUE) }
    )

    colnames(sumdf)[1]<-main_var

    values[["sumdf"]]<-sumdf
    oth_len <- length(oth)
    values[["oth_len"]] <- oth_len

  })

  output$summa_out <- renderUI({
    div(style=paste0("max-height: 300px;")
        , DetailsList(items = values[["sumdf"]] )
    )
  })

  })}

#
# main var module
#

#' @param description this module offers a (set of) variable(s) (categ.) and
#' gets updated based on the other modules data.frame selection
#' @param inputLabel        label for selectInput
#' @param vars_dataset_init default value, name of main var.
#' @param vars_unifier      set of objects shared among modules
#' @return list with main variable selected

main_var_ui <- function(id, inputLabel, vars_dataset_init) {
  ns <- NS(id)
  tagList(
    selectInput(ns("sel_main_var"), label = inputLabel
                , vars_dataset_init$main_var
                ),
  )
}

main_var_server <- function(id,vars_unifier) {
  moduleServer(id, function(input, output, session) {
    values     <-reactiveValues()

    observeEvent(input$sel_main_var, {
      values[["sel_main_var"]] <- input$sel_main_var
    })

    observe({
      updateSelectInput(
        session,
          'sel_main_var'
        , choices = vars_unifier$main_var(),
      )
    }) %>% bindEvent(vars_unifier$main_var(), ignoreInit = T)

    return(
      list(
        main_var  = reactive({ values[["sel_main_var"]] })
      )
    )

  })}

#
# axes var module
#

#' @param description this module offers a set of variables to choose in the X
#' or Y axes, it gets updated according to the data.frame selected in other modules
#' @param var_input_name    name of input
#' @param inputLabel        label for selectInput
#' @param vars_dataset_init default values
#' @param idx               index defines X or Y var.
#' @param vars_unifier      set of objects shared among modules
#' @return list with name of X or Y variables to offer

var_ui <- function(id, var_input_name, inputLabel, vars_dataset_init,idx) {

  ns <- NS(id)

  tagList(
    selectInput(ns(var_input_name), label = inputLabel
                , vars_dataset_init$oth_vars
                , vars_dataset_init$oth_vars[idx]
    ),
  )

}

var_server <- function(id,var_input_name,vars_unifier,idx) {
  moduleServer(id, function(input, output, session) {

    observe({
      updateSelectInput(
        session,
        var_input_name
        , choices = vars_unifier$oth_vars(),
        selected = vars_unifier$oth_vars()[idx]
      )
    }) %>% bindEvent(vars_unifier$oth_vars())

    values     <- reactiveValues()

    observeEvent(input[[var_input_name]], {
      values[["var"]] <- input[[var_input_name]]
    })

    return(
      list(
        var  = reactive({ values[["var"]] })
      )
    )

  })}

#
# plot module
#

#' @description this module produces a plot based on the selected data.frame and
#' variables
#' @param vars_unifier      set of objects shared among modules
#' @param vars_main_var input of categorical (color) main variables
#' @param vars_x_var        X axis variable
#' @param vars_y_var        Y axis variable
#' @return plot

main_plot_ui <- function(id) {
  ns <- NS(id)

  plotOutput(ns("mainPlot") , height = 350)

}

main_plot_server <- function(id,vars_unifier,vars_main_var,vars_x_var,vars_y_var) {
  moduleServer(id, function(input, output, session) {

    values<-reactiveValues()

    colors1 <- c("#00AEBB", "#E5B800", "#FD4E07","#AD1451","#FA3243")
    shapes1 <- 15:19

    output$mainPlot <- renderPlot({
      req(!is.null(vars_x_var$var() ) )

      df2 <- vars_unifier$sel_df()
      main_var <- vars_main_var$main_var()

      validate(need(try(df2[,main_var]),"recalculating"))

      vx  <- vars_x_var$var()
      vy  <- vars_y_var$var()

      as_n    <- as.numeric( as.factor(df2[,main_var]) )
      colors  <- colors1[as_n]
      shapes  <- shapes1[as_n]
      order   <- unique(as_n)
      len_main_var <- length(order)

      par(pty="s", oma=rep(1.5,4) , mar=rep(0.5,4))

      fac<-my_range(df2[,vx]) / my_range(df2[,vy])

      plot(x = df2[,vx]
           , y = df2[,vy],
           xlab = "", ylab = "",
           col = colors, pch = shapes, asp=fac )

      legend("topright"
             , legend = as.character(unique(as.factor(df2[,main_var] ) ) )[order],
             col = colors1[1:len_main_var],
             pch = shapes1[1:len_main_var]
             )
    })

})}

#
# axes var module
#

#' @description this module produces a DetailsList table according to the selected
#' data.frame in other modules
#' @param vars_unifier      set of objects shared among modules
if(F){
table_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("table_out") )
}

table_server  <- function(id,vars_unifier) {
  moduleServer(id, function(input, output, session) {

    output$table_out <- renderUI({
      div(style="max-height: 290px; overflow:auto "
          , DetailsList(items = vars_unifier$sel_df())
      )
    })

  })}
}

#
# unifier module server only
#

#' @description this module watches both main dataset selectors to produce an
#' updated universal selection. In addition it observes the form (page) to
#' update the data.frame selected
#' @param vars_unifier      set of objects shared among modules
#' @return list with data.frame and categ. and numeric variables
if(F){
unifier <- function(vars_dataset
                    , vars_dataset_plot
                    , vars_form
                    , id = "pipe") {

  moduleServer( id, function(input, output, session) {

    values <- reactiveValues(sel_df_name="iris"
                             , sel_df=iris
                             , main_var="Species"
                             , oth_vars=setdiff(colnames(iris),"Species" )
                             )

    observe({
      values[["sel_df_name"]] <- vars_dataset$sel_df_name()
      values[["sel_df"]]  <- get(values[["sel_df_name"]] )
    }) %>% bindEvent(vars_dataset$sel_df_name() , ignoreInit = F)

    observe({
      values[["sel_df_name"]] <- vars_dataset_plot$sel_df_name()
      values[["sel_df"]]  <- get(values[["sel_df_name"]] )
    }) %>% bindEvent(vars_dataset_plot$sel_df_name() , ignoreInit = F)

    observe({

      sel_df <- values[["sel_df"]]

      sel_df <- rbind(sel_df, vars_form$vals())

      c_vars <- classify_vars(sel_df)

      values[["sel_df"]]   <- sel_df
      values[["main_var"]] <- c_vars$main_var
      values[["oth_vars"]] <- c_vars$oth_vars

    }) %>% bindEvent(vars_form$vals() , ignoreInit = T)

    observeEvent( values[["sel_df_name"]] , {

      sel_df <- get(values[["sel_df_name"]] )

      c_vars <- classify_vars(sel_df)

      values[["sel_df"]]   <- sel_df
      values[["main_var"]] <- c_vars$main_var
      values[["oth_vars"]] <- c_vars$oth_vars
    })

    return(
      list(
        sel_df   = reactive({ values[["sel_df"]]   }),
        main_var = reactive({ values[["main_var"]] }),
        oth_vars = reactive({ values[["oth_vars"]] })
      )
    )
})}
}
