#
# tag module
#
box::use(
  shiny[bootstrapPage,
        moduleServer,
        NS,
        renderText,
        tags,
        textOutput,
        tagList,
        div,
        fluidPage,
        actionButton,
        observeEvent,
        reactiveValues,
        reactive
        ],
  shiny.semantic[
    icon
  ]
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
    div(style="padding:25px 0px 0 0",class="tag-container",
      actionButton(ns("tag_button"), "NLP in action, tag the feedback!", icon("tag"),
               style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
               )
    )
  )
}
#' @export
server <- function(id, vars_unifier) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    rv <- reactiveValues()

      observeEvent(input$tag_button, ignoreInit = T
             , {
                 if(file.exists("app/outfiles/cash_df.csv")){
                   file.remove("app/outfiles/cash_df.csv")
                 }

                 if(file.exists("app/outfiles/health_df.csv")){
                   file.remove("app/outfiles/health_df.csv")
                 }

               shinyjs::disable("tag_button")

               rv$alist$ds_copy <- NULL

               rv$alist$dataset_cash_health <- NULL

               dataset <- vars_unifier$dataset()

               ###
               #
               #   initial datasets
               #
               ###

               if(file.exists("app/outfiles/selection.csv")){
                 file.remove("app/outfiles/selection.csv")
               }

               write.csv(tolower(dataset[,"idea"]), "app/outfiles/selection.csv", row.names = T)

               #
               #   make dfs for tagging
               #

               health_df <- as.data.frame(dataset[which(dataset$service_type=="Healthcare"),"idea"])

               cash_df   <- as.data.frame(dataset[which(dataset$service_type=="Cash Transfer"),"idea"])

               if(nrow(cash_df)>0) {
                 write.csv(cash_df, "app/outfiles/cash_df.csv", row.names = T)
                 system("python3 py/load_model.py pkl/model_SVC.pkl pkl/tfidf_cash.pkl app/outfiles/cash_df.csv outfiles/cash_out.txt")
                 cash_tags <- readLines("app/outfiles/cash_out.txt")
                 dataset$NLP_tag<-as.character(NA)
                 dataset[which(dataset$service_type=="Cash Transfer"),]$NLP_tag <- cash_tags
                 rv$cash_cat <- unique(cash_tags)
                 shinyjs::show("cash_select_UI_id", asis = T)
               }

               if(nrow(health_df)>0) {
                 write.csv(health_df, "app/outfiles/health_df.csv", row.names = T)
                 system("python3 py/load_model.py pkl/model_SVH.pkl pkl/tfidf_health.pkl app/outfiles/health_df.csv outfiles/health_out.txt")
                 health_tags <- readLines("app/outfiles/health_out.txt")

                 #  avoid destroying column

                 if(!"NLP_tag" %in% colnames(dataset) ){
                   dataset$NLP_tag<-as.character(NA)
                 }

                 dataset[which(dataset$service_type=="Healthcare"),]$NLP_tag <- health_tags
                 rv$health_cat <- unique(health_tags)
                 shinyjs::show("health_select_UI_id", asis = T)
               }

             rv$dataset <- dataset

             }) # tagButton

return(
  list(
    dataset  = reactive( rv$dataset ),
    cash_cat = reactive( rv$cash_cat ),
    health_cat = reactive( rv$health_cat )
  )
)

})}
