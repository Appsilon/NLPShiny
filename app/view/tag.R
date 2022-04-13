#
# tag module
#
box::use(
  shiny[
    moduleServer,
    NS,
    tags,
    tagList,
    div,
    observeEvent,
    reactiveValues,
    reactive,
    helpText
  ],
  shiny.semantic[
    actionButton,
    icon
  ],
  shinyjs[enable, disable],
  utils[write.csv]
)

#' @description module to allow choosing a data.frame name and its feedback from
#' same module different namespace
#' @param
#' @param
#' module namespace, see return, used for feedback
#' @return list with data.frame name (string) selected
#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      style = "padding:25px 0 0 0;max-width:266px;", class = "tag-container",
      actionButton(ns("tag_button"), "NLP in action, tag the feedback!", icon("tag"),
        style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
      ),
      div(
        style = "text-align:center;color:#A8A8A8;",
        helpText("add tag column to table")
      )
    )
  )
}

cash_spanish <- "app/pkl/model_SVC.pkl app/pkl/tfidf_cash.pkl"
cash_english <- "app/pkl/model_SVCE.pkl app/pkl/tfidf_cash_english.pkl"
cash_strings <- c(cash_spanish, cash_english)

health_spanish <- "app/pkl/model_SVH.pkl app/pkl/tfidf_health.pkl"
health_english <- "app/pkl/model_SVHE.pkl app/pkl/tfidf_health_english.pkl"
health_strings <- c(health_spanish, health_english)

#' @export
server <- function(id,
                   vars_unify) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(tag_active = 0)

    observeEvent(input$tag_button, ignoreInit = T, {

      if (file.exists("app/outfiles/cash_df.csv")) {
        file.remove("app/outfiles/cash_df.csv")
      }

      if (file.exists("app/outfiles/health_df.csv")) {
        file.remove("app/outfiles/health_df.csv")
      }

      if (file.exists("app/outfiles/cash_out.txt")) {
        file.remove("app/outfiles/cash_out.txt")
      }

      if (file.exists("app/outfiles/health_out.txt")) {
        file.remove("app/outfiles/health_out.txt")
      }

      disable("tag_button")

      dataset <- vars_unify$dataset_whole()

      #
      #   make dfs for tagging
      #

      health_df <- as.data.frame(dataset[which(dataset$service_type == "Healthcare"), "feedback"])

      cash_df <- as.data.frame(dataset[which(dataset$service_type == "Cash Transfer"), "feedback"])

      if (nrow(cash_df) > 0) {
        write.csv(cash_df, "app/outfiles/cash_df.csv", row.names = T)

        system(paste("python3 app/py/load_model.py",
                     cash_strings[vars_unify$region()],
                     "app/outfiles/cash_df.csv app/outfiles/cash_out.txt"))

        cash_tags <- readLines("app/outfiles/cash_out.txt")

        if (!"nlp_tag" %in% colnames(dataset)) {
          dataset$nlp_tag <- as.character(NA)
        }

        dataset[which(dataset$service_type == "Cash Transfer"), ]$nlp_tag <- cash_tags
        rv$cash_cat <- unique(cash_tags)
        enable("cash_select_UI_id1", asis = T)
        enable("cash_select_UI_id2", asis = T)
      } else {
        rv$cash_cat <- "nothing to show"
      }

      if (nrow(health_df) > 0) {
        write.csv(health_df, "app/outfiles/health_df.csv", row.names = T)

        system(paste("python3 app/py/load_model.py",
                     health_strings[vars_unify$region()],
                     "app/outfiles/health_df.csv app/outfiles/health_out.txt"))

        health_tags <- readLines("app/outfiles/health_out.txt")

        if (!"nlp_tag" %in% colnames(dataset)) {
          dataset$nlp_tag <- as.character(NA)
        }

        dataset[which(dataset$service_type == "Healthcare"), ]$nlp_tag <- health_tags
        rv$health_cat <- unique(health_tags)
        enable("health_select_UI_id1", asis = T)
        enable("health_select_UI_id2", asis = T)
      } else {
        rv$health_cat <- "nothing to show"
      }

      rv$dataset_tag <- dataset
      rv$tag_active <- rv$tag_active+1
    })

    return(
      list(
        tag_active = reactive({
          rv$tag_active
        }),
        dataset_tag = reactive({
          rv$dataset_tag
        }),
        cash_cat = reactive({
          rv$cash_cat
        }),
        health_cat = reactive({
          rv$health_cat
        })
      )
    )
  })
}
