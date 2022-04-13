#
#   Map module
#
box::use(
  shiny[
    moduleServer, NS,

    observeEvent,
    validate,
    need
  ],
  shiny.react[JS],
  leaflet[
    leaflet,
    leafletOutput,
    renderLeaflet,
    colorFactor
  ]
)


box::use(
  app/objects/objects_NLP[...],
  app / logic / functions_NLP[...],
)


#
#   UI, leaflet map output
#
#' @export
ui <- function(id) {
  ns <- NS(id)
  leafletOutput(ns("map")) # , height=400 )
}

#
#   Server
#

#' @export
server <- function(id, dataset_init, idx,
                   vars_unify) {
  moduleServer(id, function(input, output, session) {
    output$map <- renderLeaflet({

      types_in_dataset <- sort(unique(dataset_init$service_type))

      initial_region <- 2

      idx <- sapply(
        types_in_dataset,
        function(x) {
          grep(x,
            ser_list[[initial_region]],
            ignore.case = TRUE
          )
        }
      )

      filtered_service <- ser_list[[initial_region]][idx]

      filter_color <- color_list12[idx]

      pal <- colorFactor(
        palette = filter_color,
        filtered_service
      )

      js_functions2 <- js_functions[idx]

      lat1 <- afr_locations_coord[which(afr_locations_coord$location_name %in% "Gihembe Camp"), ]$lat
      lng1 <- afr_locations_coord[which(afr_locations_coord$location_name %in% "Gihembe Camp"), ]$lng

      build_map(dataset_init, lng1, lat1, js_functions2, pal)
    }) # output map

    #####################################
    #
    #   MAP updater / observer of buttons
    #
    ####################################

      observeEvent(vars_unify$dataset(),
        ignoreInit = T, {
          output$map <- renderLeaflet({
            validate(
              need(
                try(inherits(vars_unify$dataset(), "data.frame")), ""
              )
            )

            dataset <- vars_unify$dataset()

            types_in_dataset <- sort(unique(dataset$service_type))

            idx <- sapply(
              types_in_dataset,
              function(x) {
                grep(x,
                  ser_list[[as.numeric(vars_unify$region())]],
                  ignore.case = TRUE
                )
              }
            )

            filtered_colors <- color_list12[idx]

            filtered_service <- ser_list[[as.numeric(vars_unify$region())]][idx]

            js_functions2 <- js_functions[idx]

            pal <- colorFactor(
              palette = filtered_colors,
              filtered_service
            )

            build_map(dataset, vars_unify$lng(), vars_unify$lat(), js_functions2, pal)
          }) # map output
        }
      ) # observeEvent

  })
}
