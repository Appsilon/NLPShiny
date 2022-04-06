box::use(
  shiny[
    moduleServer,
    NS,
    tags,
    tagList,
    div,
    renderUI,
    reactive,
    br,
    h4,
    reactiveValues,
    observeEvent,
    uiOutput
  ],
  shiny.semantic[
    icon,
    update_dropdown_input,
    dropdown_input,
    action_button
  ],
  shinyjs[disabled],
  utils[write.csv]
)

box::use(
  app/objects/objects_NLP[...]
)

#' @export
ui <- function(id, service) {
  ns <- NS(id)
  tagList(
    div(
      uiOutput(
        ns(paste0(service, "_select_UI"))
      )
    )
  )
}
#' @export
server <- function(id, service,
                   vars_unify,
                   vars_filter) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    rv <- reactiveValues()
    rv$alist <- list()

    caps_id <- gsub("(^[[:alpha:]])", "\\U\\1", service, perl = TRUE)

    output[[paste0(service, "_select_UI")]] <- renderUI({
      tagList(
        br(),
        h4(paste0(caps_id, " tags:")),
        div(
          id = paste0(service, "_select_UI_id2"),
          class = "drop-container",
          dropdown_input(
            ns(paste0(service, "_input")),
            choices = NULL,
            default_text = "Use the tag button"
          )
        ),
        br(),
        disabled(
          div(
            id = paste0(service, "_select_UI_id1"),
            action_button(ns(paste0(service, "_subset_button")),
              paste(
                "Subset",
                long_type_names[service]
              ),
              icon("table"),
              style = "width:265px; color: #fff; background-color: #337ab7; border-color: #2e6da4"
            )
          )
        )
      )
    })

    observeEvent(vars_unify$active_tag(), ignoreInit = TRUE, {
      update_dropdown_input(
        session,
        paste0(service, "_input"),
        choices = vars_unify[[paste0(service, "_cat")]](),
        value   = vars_unify[[paste0(service, "_cat")]]()[1]
      )

      rv$alist$ds_copy <- vars_unify$dataset_whole()
    })

    observeEvent(
      vars_filter$submit(),
      ignoreInit = T, {
        rv$alist$ds_copy <- vars_unify$dataset_whole()

        update_dropdown_input(
          session,
          paste0(service, "_input"),
          choices = "Use the tag button",
        )
      }
    )


    observeEvent(input[[paste0(service, "_subset_button")]], ignoreInit = T, {
      rv$alist$dataset_cash_health <- rv$alist$ds_copy[which(
        rv$alist$ds_copy$nlp_tag %in%
          input[[paste0(service, "_input")]] &
          rv$alist$ds_copy$service_type %in%
            long_type_names[service]
      ), ]

      if (file.exists("app/outfiles/selection.csv")) {
        file.remove("app/outfiles/selection.csv")
      }

      rv$dataset_subset <- rv$alist$dataset_cash_health

      write.csv(tolower(rv$dataset[, "feedback"]), "app/outfiles/selection.csv", row.names = T)

      rv$alist$ds_copy <- vars_unify$dataset_whole()
    })

    return(
      list(
        dataset_subset = reactive({
          rv[["dataset_subset"]]
        }),
        dataset_not_subset = reactive({
          rv$alist$ds_copy
        })
      )
    )
  })
}
