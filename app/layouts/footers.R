box::use(
  shiny.fluent[Text,  Stack],
)

#' @export
footer <- Stack(horizontal=T,
                tokens = list(childrenGap = 150),
                Text(variant="large", "Built with ❤ by Fernando Roa & Patrick Alverga"
                     , block=TRUE)
)
