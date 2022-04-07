box::use(
  shiny.fluent[Text,  Stack],
  shiny[tagList, div, img],
  shiny.fluent[IconButton.shinyInput, Text, CommandBar],
)

logo <- img(src = "static/logoNLP.png", class = "logo")

far_items <- list(
  list(
    key = "info",
    text = "About",
    ariaLabel = "Info",
    iconOnly = F,
    href = "https://github.com/Appsilon/NLPShiny",
    iconProps = list(
      iconName = "Info"
    )
  )
)

command_bar <- CommandBar(
  farItems = far_items,
  style = list(width = "80%")
)

title <- div(Text(variant = "xLarge", "Community feedback tagging using NLP"),
  class = "title",
  style = "align: center; white-space:nowrap;"
)

#' @export
header_right <- tagList(
  title, command_bar
)

#' @export
header_left <- tagList(logo,
                       div(style="margin-left:auto;",
                       IconButton.shinyInput("button",
                                             iconProps =
                                               list("iconName" = "CollapseMenu")
                       )
                       )
                       )

#' @export
footer <- Stack(
  horizontal = T,
  tokens = list(childrenGap = 150),
  Text(variant = "large", "Built by Fernando Roa & Patrick Alverga", block = TRUE),
  Text(variant = "medium", nowrap = FALSE,
       "Backbone based on https://github.com/Appsilon/shiny.fluent")
)
