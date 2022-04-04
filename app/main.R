# R packages
box::use(
  shiny[bootstrapPage, moduleServer, NS, renderText, tags, textOutput, tagList, div,
        fluidPage,
        observeEvent],
)

box::use(
  shinyjs[useShinyjs]
)

box::use(
  shiny.router[...]
)

box::use(
  mongolite[...]
)


# Modules

# box::use(
#   r/objects_ui[...]
# )

box::use(
  app / layouts / sidebar[...],
  app / layouts / headers[...],
  app / layouts / footers[...],
  app / layouts / pages[...],
  app / mongo / mongo_fun[...],
  app / objects / global[...],
  app / objects / functions_NLP[...],
  app / objects / objects_NLP[...],
)

box::use(
  app / view / service_type,
  app / view / map,
  app / view / submit,
  app / view / table,
  app / view / tag,
  app / view / unify,
)

router <- purrr::lift(make_router)(pages_menu)

layout <- div(
  class = "grid-container",
  div(class = "header_left", header_left),
  div(class = "header_right", header_right),
  div(class = "sidenav", sidebar, id = "sidebar_id"),
  div(class = "main", router$ui),
  div(class = "footer", footer)
)

#' @export
ui <- fluidPage(
  useShinyjs(),
  # tags$head(
  #   tags$link(href = "style.css", rel = "stylesheet", type = "text/css"),
  # ),
  shiny::tags$body(
    # dir = "ltr",
    layout
  )
)

#
#   server
#
#' @export
server <- function(input, output, session) {
  router$server(input, output, session)

  #
  #   inputs
  #

  vars_submit <- submit$server("mod_submit", dataset_init)

  #
  # tag
  #

  vars_tag    <- tag$server("mod_tag", vars_unifier)

  #
  #   services
  #

  vars_cash   <- service_type$server("cash", vars_unifier, vars_submit)

  vars_health <- service_type$server("health", vars_unifier, vars_submit)

  #
  #  gather and unify
  #

  vars_unifier <- unify$server(
    "ns_unifier", dataset_init,
    vars_submit, vars_tag,
    vars_cash,
    vars_health
  )

  #
  #   map
  #

  map$server("nsMap", dataset_init, idx, vars_unifier)

  #
  # table
  #

  table$server("mod_table", dataset_init, vars_unifier)

  observeEvent(input$button, {
    shinyjs::toggle("sidebar_id")
  })
}

# shinyApp(ui, server)

