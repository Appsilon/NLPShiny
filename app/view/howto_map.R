box::use(
  shiny[
    moduleServer, NS, plotOutput,
    renderPlot,
  ],
  maps[map],
  graphics[par]
)
#
#   UI, leaflet map output
#

ui <- function(id) {
  ns <- NS(id)
  plotOutput(ns("minimap_plot"), height = 150)
}

#
#   Server
#

server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$minimap_plot <- renderPlot({
      par(mar = rep(0, 4))
      map("world", xlim = c(-20, 55), ylim = c(-34, 40))
    })
  })
}
