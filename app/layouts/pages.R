box::use(
  shiny[bootstrapPage, NS, renderText, tags, textOutput, tagList, div, wellPanel],
)

box::use(
  shiny.fluent[IconButton.shinyInput, Text, CommandBar, Stack],
)

box::use(
  shiny.semantic[semanticPage, tabset, segment],
)

box::use(
  shiny.router[...]
)

box::use(
  app / view / service_type,
  app / view / map,
  app / view / submit,
  app / view / table,
  app / view / tag,
  app / view / unify,
  app / view / form
)

#' @export
inputFormPage <- tagList(
  div(class="grid-form",
      wellPanel(
        div(class="parent",
            form$ui("mod_form")
        )
      )
  )
)

#' @export
card1 <- div(
  Stack(
    tokens = list(childrenGap = 15),
    Text(
      variant = "xLarge",
      "Welcome, this example was built with view, shiny.fluent, shiny.router and shiny.semantic !",
      block = F
    ),
    Text(
      "shiny.fluent is a package that allows you to build Shiny apps using Microsoft's Fluent ui."
    ),
    Text(
      "shiny.router allows the functioning of the different pages at the left."
    ),
    Text(
      "shiny.semantic allows beautiful tabs in this example."
    )
  )
)

#' @export
aboutPage <- tagList(
  wellPanel(style="max-width:1000px",#class="grid-form",
            div(style="padding:20px;",
                card1
            )
  )
)

#' @export

mainPage <- tagList(
  div(class="grid-inside-up"
      , segment(
        submit$ui("mod_submit")
      )
      , div(class="map-container",
        map$ui("nsMap")
      )
  )
  , div(class="grid-inside-down",
        div(class="downleft"
            , tag$ui("mod_tag")
            , service_type$ui("cash")
            , service_type$ui("health")
        )
        , div(class="downright",# style="max-height:35vh;width:63vw;",
              semanticPage(
                tabset(tabs =
                         list(
                           list(menu = "Table"
                                , content =
                                  tagList(
                                    div(#style="max-height:300px",
                                      table$ui("mod_table")
                                    )
                                  )
                           )
                           , list(menu = "Details"
                                  , content =
                                    tagList(
                                      div(style="padding:20px;",
                                          card1
                                          # verbatimTextOutput("irisText"
                                          #                    , height = 640
                                      )
                                    )
                           )
                         ),
                       active = "second_tab",
                       id = "exampletabset"
                )
              )
        )
  )
)


#' @export
pages_menu  <- c(list(route("/", mainPage)
                 ,route("inputform",inputFormPage)
                 ,route("about",aboutPage)
)
)

