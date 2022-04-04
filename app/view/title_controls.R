box::use(
  shiny[...]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$h3("string" ), # good
    uiOutput(ns("title2") ) # fails

    ,div(
      selectInput(ns("region_input"),"Region",c("place1"=1,"place2"=2)
      )
    )
    ,div(
      conditionalPanel(condition = "input.region_input == 2",
                       ns = ns,
                       uiOutput(ns("centerSelect2"))
      )
    )
    ,div(
      conditionalPanel(condition = "input.region_input == 1",
                       ns = ns,
                       uiOutput(ns("centerSelect1"))
      )
    )
  )

}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns<-session$ns

    output$title2 <- renderUI({
      tags$h3("string in uiOutput" )
    })

    output$centerSelect2 <- renderUI({
      div(
        selectInput(ns("center_input2")
                    , "Country"
                    , 20:30
        )
      )
    })

    output$centerSelect1 <- renderUI({
      div(
        selectInput(ns("center_input1")
                    , "State"
                    ,10:20
        )
      )
    })

  })}
