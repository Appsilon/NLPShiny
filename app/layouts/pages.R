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
  app / view / wordcloud,
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
      segment(style="margin: 20px 5px 0; padding:12px 20px 0 20px;",
        card1
      ),
      segment(style="margin: 20px 5px 0; padding:12px 20px 0 20px;",
              #
              #     ns call
              #
        card2(map_ns)
      )
    ),
  div(
    class = "grid-mini-1-down",
    segment(style="margin: 20px 5px 0; padding:12px 20px 0 20px;",
            card3
    ),
    div(style="height:5vh;margin: 0px 5px 0; padding:20px 10px 0 10px;",
      semanticPage(
        tabset(id = "mini_tabset",
               tabs = list(
                 list(menu = "Feedback table",
                      content =
                        div(style ="height: 12vh;min-height: 190px;padding-top:5px;",
                        card4
                        )
                 ),
                 list(menu = "Word Cloud",
                      content =
                        div(style ="height: 12vh;min-height: 190px;padding-top:5px;",
                            card_wordcloud
                        )
                 )
               )
        )
      )
     )
    )
  ),
  tagList(
    div(style="margin-left:5px;",
      h4("Second page: Database")
    ),
    div(
      div(class="grid-mini-2-up",
      segment(style="margin: 20px 5px 0; padding:12px 20px 0 20px;",
              card5
      )
      ),
      div(class="grid-mini-2-down",
      segment(style="margin: 20px 5px 0; padding:12px 20px 0 20px;",
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
                tabset(id = ns("my_tabset"),
                  tabs = list(
                           list(menu = "Feedback and metadata"
                                , content =
                                  tagList(
                                    div(
                                      table$ui(ns("ns_table"))
                                    )
                                  )
                                , id = "table_tab"
                           )
                           , list(menu = "Word Cloud"
                                  , content =
                                    tagList(
                                      div(style="padding:20px;",
                                          wordcloud$ui(ns("ns_wordcloud"))
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
