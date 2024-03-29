box::use(
  shiny[
    moduleServer, NS,
    tags, div,
    fluidPage,
    observe
  ],
  shinyjs[useShinyjs, onclick, toggle],
  shiny.router[make_router],
  shiny.semantic[update_tabset],
  utils[write.csv]
)

box::use(
  app / layouts / pages[...],
  app / layouts / sidebar[...],
  app / layouts / headers_footer[...],
  app / mongo / mongo_fun[...],
  app / logic / functions_NLP[...],
  app / objects / objects_NLP[...],
  app / objects / init[...],
)

box::use(
  app / view / filter,
  app / view / service_type,
  app / view / map,
  app / view / table,
  app / view / wordcloud,
  app / view / tag,
  app / view / unify,
  app / view / howto_map,
  app / view / form,
)

router <- purrr::lift(make_router)(pages_menu)

#
#   ui function
#
#' @export
ui <- function(id) {
  ns <- NS(id)
  # see pages.R for ns for shiny.router
  fluidPage(
    useShinyjs(),
    shiny::tags$body(
      dir = "ltr",
      div(
        class = "grid-container",
        div(class = "header_left", header_left),
        div(class = "header_right", header_right),
        div(class = "sidenav", sidebar, id = "sidebar_id"),
        div(class = "main", router$ui),
        div(class = "footer", footer)
      )
    )
  )
}

#
#   server
#
#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    router$server(input, output, session)

    #
    #   inputs
    #
    vars_filter <- filter$server("ns_filter")

    #
    # tag
    #
    vars_tag <- tag$server(
      "ns_tag",
      vars_unify
    )

    #
    #   services
    #
    vars_cash <- service_type$server(
      "ns_cash",
      "cash",
      vars_unify,
      vars_filter
    )

    vars_health <- service_type$server(
      "ns_health",
      "health",
      vars_unify,
      vars_filter
    )

    #
    #  gather vars and unify
    #
    vars_unify <- unify$server(
      "ns_unify",
      dataset_init,
      vars_filter,
      vars_tag,
      vars_cash,
      vars_health,
      vars_wordcloud
    )

    #
    #   map
    #
    map$server(
      "ns_map", dataset_init, idx,
      vars_unify
    )

    #
    # table
    #
    table$server(
      "ns_table", dataset_init,
      vars_unify
    )

    #
    # wordcloud
    #
    vars_wordcloud <- wordcloud$server("ns_wordcloud", vars_unify, dataset_init)

    #
    # form page
    #
    form$server("ns_form", vars_unify)

    #
    # howto page
    #
    howto_map$server("minimap")

    observe({
      onclick("button", {
          toggle("sidebar_id", asis = T)
        },
        asis = TRUE
      )

      onclick("app-ns_wordcloud-trigram_subset_button",
              update_tabset(session, "my_tabset", "table_tab"),
              asis = T)

    })
  })
}
