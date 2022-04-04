#
#   Map module
#
box::use(
  shiny[bootstrapPage,
        moduleServer, NS, renderText, tags, textOutput, tagList, div,
        fluidPage,
        observeEvent
        ],
  leaflet[
    leafletOutput,
    renderLeaflet
  ]
)

#
#   UI, leaflet map output
#
#' @export
ui <- function(id) {
  ns <- NS(id)
  leafletOutput(ns("map") )#, height=400 )
}

#
#   Server
#

#' @param consensusSel      dataset processed by mod. unifier
#' @param varsTable,        input from DT, to get row clicked. mod. Table
#' @param varsCountry     , get coordinates of selected country. mod. Country

#' @export
server <- function(id,dataset_init,idx,vars_unifier) {

  moduleServer(id,function(input, output, session) {

    output$map <- renderLeaflet({

      center_init<-"Cundinamarca"

      types_in_dataset <- sort(unique( dataset_init$service_type) )

      idx <- sapply(types_in_dataset
                    , function(x) grep(x
                                       ,ser_list[[1]]
                                       ,ignore.case=TRUE
                    ) )

      filteredService <- ser_list[[1]][idx]

      filterColor     <- colorList12[idx]

      pal <- colorFactor(palette = filterColor
                              ,filteredService
      )

      JSfunctions2 <- JSfunctions[idx]

      lng1 <- col_states_coord[which(col_states_coord$state %in% center_init),]$lng
      lat1 <- col_states_coord[which(col_states_coord$state %in% center_init),]$lat

      build_map(dataset_init,lng1,lat1,JSfunctions2,pal)

    }) # output map

    #####################################
    #
    #   MAP updater / observer of buttons
    #
    ####################################

    observeEvent(vars_unifier$dataset(),ignoreInit = T
                   # ,input$tag_button
                   # ,input$cash_subset_button
                   # ,input$health_subset_button
                   # ,input$trigram_subset_button

    ,
    {

    output$map <- renderLeaflet({

      validate(
        need(
          try(inherits(vars_unifier$dataset(),"data.frame" )) ,"" )
      )

      #

      dataset <- vars_unifier$dataset()

      types_in_dataset <- sort(unique( dataset$service_type ) )

      idx <- sapply(types_in_dataset
                    , function(x) grep(x
                                       ,ser_list[[as.numeric(vars_unifier$region() )]]
                                       ,ignore.case=TRUE
                    ) )

      filteredColors  <- colorList12[idx]

      filteredService <- ser_list[[as.numeric(vars_unifier$region() )]][idx]

      JSfunctions2 <- JSfunctions[idx]

      pal <-  colorFactor(palette = filteredColors
                                  ,filteredService)
      #

      build_map(dataset,vars_unifier$lng(),vars_unifier$lat(),JSfunctions2,pal)

      }) # map output
    }) # observeEvent

})}
