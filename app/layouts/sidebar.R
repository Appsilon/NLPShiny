# R packages
box::use(
  shiny[bootstrapPage, moduleServer, NS, renderText, tags, textOutput, tagList, div],
)

box::use(
  shiny[img],
)

box::use(
  shiny.fluent[IconButton.shinyInput, Text, CommandBar, Nav],
)

#' @export
sidebar_style <- list(
  root = list(
    height = "100%",
    boxSizing = "border-box",
    border = "1px solid #eee",
    overflowY = "auto"
  )
)

#' @export
list_link_pages <- list(
  list(
    links = list(
      list(
        name = "Tag feedback",
        url = "#!/",
        key = "home",
        isExpanded = FALSE,
        icon = "home"
      ),
      list(
        name = "Database",
        url = paste0("#!/", "inputform"),
        key = "input_form",
        isExpanded = FALSE,
        icon = "archive"
      ),
      list(
        name = "How to",
        url = paste0("#!/", "howto"),
        key = "howto",
        isExpanded = FALSE,
        icon = "StatusCircleQuestionMark"
      ),
      list(
        name = "Introduction",
        url = paste0("#!/", "intro"),
        key = "intro",
        isExpanded = FALSE,
        icon = "DocumentSet"
      )
    )
  )
)

#' @export
sidebar <- tagList(
  Nav(
    groups = list_link_pages,
    initialSelectedKey = "home",
    styles = sidebar_style
  )
)
