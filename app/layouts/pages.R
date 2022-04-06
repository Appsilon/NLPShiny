box::use(
  shiny[bootstrapPage, NS, renderText, tags,
        textOutput, tagList, div, wellPanel,
        br, h2, h4],
  shiny.fluent[IconButton.shinyInput, Text, CommandBar, Stack],
  shiny.semantic[semanticPage, tabset, segment, splitLayout],
  shiny.router[...]
)

box::use(
  app / view / filter,
  app / view / map,
  app / view / table,
  app / view / tag,
  app / view / service_type,
  app / view / form,
  app / layouts / cards[...],
)

#' @export
input_form_page <- function(id) {
  ns<-NS(id)
tagList(
  div(
    class = "grid-form",
      div(
        class = "parent",
        form$ui(ns("ns_form"))
      )
  )
)
}

#' @export
intro_page <- function() {
  tagList(
  div(style="margin-left:4px;",
      br(),
      h2("Introduction")
      , br(),
      div(style="max-width:33vw;",
        segment(style="padding:20px;",
                div(
                  Stack(
                    tokens = list(childrenGap = 15),
                    Text(
                      variant = "xLarge",
                      "Business problem",
                      block = F
                    ),
                    lapply(card7list, function(x) {
                      Text(x)
                    })
                  )
                )
        ),
        segment(style="padding:20px;",
                div(
                  Stack(
                    tokens = list(childrenGap = 15),
                    Text(
                      variant = "xLarge",
                      "Which model is used and how?",
                      block = F
                    ),
                    lapply(card8list, function(x) {
                      Text(x)
                    })
                  )
                )
        )
      ),
  )
)
}

howto_page <- function(map_ns) {
  tagList(
  div(style="margin-left:4px;",
      br(),
      h2("Pages functionality")
      , br()
  ),
  splitLayout(style = "background:#FFFFFF;",
    tagList(
  div(style="margin-left:5px;",
    h4("First page: Tag feedback")
  ),
  div(
    class = "grid-mini-1-up",
      segment(style="padding:20px;",
        card1
      ),
      segment(style="padding:20px 10px 0 10px;",
              #
              #     ns call
              #
        card2(map_ns)
      )
    ),
  div(
    class = "grid-mini-1-down",
    segment(style="padding:20px;",
            card3
    ),
    segment(style="padding:20px;",
            card4
    )
  )
  ),
  tagList(
    div(style="margin-left:5px;",
      h4("2nd page: Database")
    ),
    div(
      class = "grid-mini-2",
      div(class="seg5",
      segment(style="padding:20px;min-height:240px;",
              card5
      )
      ),
      div(class="seg6",
      segment(style="padding:20px;",
              card6
      )
    )
   )
  )
 )
)
}

#' @export
main_page <- function(id) {
  ns<-NS(id)
  tagList(
  div(class="grid-inside-up",
      div(class="filter-container",
          segment(style="min-height:400px;",
                  filter$ui(ns("ns_filter"))
          )
      ),
      div(class="map-container",
          map$ui(ns("ns_map"))
      )
  )
  , div(class="grid-inside-down",
        div(class="downleft"
            , tag$ui(ns("ns_tag"))
            , service_type$ui(ns("ns_cash"), "cash")
            , service_type$ui(ns("ns_health"), "health")
        )
        , div(class="downright",
              semanticPage(
                tabset(tabs =
                         list(
                           list(menu = "Feedback and metadata"
                                , content =
                                  tagList(
                                    div(
                                      table$ui(ns("ns_table"))
                                    )
                                  )
                           )
                           , list(menu = "Details"
                                  , content =
                                    tagList(
                                      div(style="padding:20px;",
                                          card1
                                      )
                                    )
                           )
                         )
                )
              )
        )
  )
)
}

#' @export
pages_menu <- c(list(
  route("/", main_page("app")),
  route("inputform", input_form_page("app")),
  route("howto", howto_page("app")),
  route("intro", intro_page())
))
