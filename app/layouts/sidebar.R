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
sidebarStyle <- list(
  root = list(
    height = '100%',
    boxSizing = 'border-box',
    border = '1px solid #eee',
    overflowY = 'auto'
  )
)

#' @export
listLinkPages<-list(list(links = list(
  list(
    name = 'Main',
    url = '#!/',
    key = 'home',
    isExpanded = FALSE
  ),
  list(
    name = 'InputForm',
    url = paste0('#!/', "inputform"),
    key = 'inputForm',
    isExpanded = FALSE
  ),
  list(
    name = 'About',
    url = paste0('#!/', "about"),
    key = 'about',
    isExpanded = FALSE
  )
)))

#' @export
sidebar <- tagList(
  Nav(
    groups = listLinkPages,
    initialSelectedKey = 'home',
    styles = sidebarStyle
  )
)