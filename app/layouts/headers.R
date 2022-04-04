box::use(
  shiny[bootstrapPage, moduleServer, NS, renderText, tags, textOutput, tagList, div],
)

box::use(
  shiny[img],
)

box::use(
  shiny.fluent[IconButton.shinyInput, Text, CommandBar],
)

#' @export
header_left <- tagList(img(src = "static/images/appsilon-logo.png", class = "logo"))

#' @export
header_right <- tagList(
  IconButton.shinyInput(
    "button", iconProps = list("iconName" = "CollapseMenu")), 
  div(Text(variant = "xLarge", "shiny.semantic, shiny.router and shiny.fluent example")
      , class = "title"
      , style= "align: center; white-space:nowrap;"
  ), CommandBar(
    farItems = list(
      list(
        key = "info",
        text = "Repo",
        ariaLabel = "Info",
        iconOnly = F,
        href = "https://github.com/Appsilon/possible_example",
        iconProps = list(iconName = "Info"#shiny::icon("github")
        )
      )
    ),
    style = list(width = "100%"))
)