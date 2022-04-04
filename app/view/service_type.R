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
        selectInput,
        renderUI,
        reactive,
        br,
        actionButton,
        reactiveValues,
        observeEvent,
        uiOutput],
  shiny.semantic[
    icon
  ]
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(id=paste0(id,"_select_UI_id"),
                      uiOutput(
                        ns(paste0(id,"_select_UI"))
                      )
            )
    )
}
#' @export
server <- function(id, vars_unifier, vars_other_dataset) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns
    rv <- reactiveValues(click_count=0)
    rv$alist <- list()

    observeEvent(vars_unifier[[paste0(id,"_cat")]](), ignoreInit = TRUE
             , {
               capsId<-gsub("(^[[:alpha:]])", "\\U\\1", id, perl = TRUE)

               output[[paste0(id,"_select_UI")]] <- renderUI({
                 tagList(
                   br()
                   ,div(class="drop-container",
                      selectInput(ns(paste0(id,"_input") )
                                  ,paste0(capsId
                                          , " tags:"
                                          )
                                  ,vars_unifier[[paste0(id,"_cat") ]]()
                                  )
                  )
                  ,br()
                  , actionButton(ns(paste0(id,"_subset_button") )
                               , paste("Subset"
                                       , longTypeNames[id]
                                       )
                               , icon("table")
                               , style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
                  )
                )
              })

        observeEvent(input[[paste0(id,"_subset_button")]],
                     {
                       rv$click_count <- rv$click_count + runif(1,1,2)
                       session$sendCustomMessage(type = 'testmessage',
                                                 message = rv$click_count)
                     },ignoreInit = T)

        observeEvent(input[[paste0(id,"_subset_button")]], ignoreInit = T
                           , {

                              # rv$wchide<-0
                              # rv$trigramhide<-0

                              #
                              #    creation alist dscopy
                              #

                      if(!is.null(vars_unifier$dataset_not_subset() ) ) {
                        rv$alist$ds_copy <- vars_unifier$dataset_not_subset()
                      } else {
                        rv$alist$ds_copy <- vars_unifier$dataset()
                      }

                              # ceration dataset_cash_health

                      rv$alist$dataset_cash_health <- rv$alist$ds_copy[which(rv$alist$ds_copy$NLP_tag %in%
                                                                                       input[[paste0(id,"_input")]]
                                                                                     ),]

                        if(file.exists("app/outfiles/selection.csv")){
                                file.remove("app/outfiles/selection.csv")
                         }

                         rv$dataset<-rv$alist$dataset_cash_health

                         write.csv(tolower(rv$dataset[,"idea"] ), "app/outfiles/selection.csv", row.names = T)
                         })

})

    return(
      list(
        dataset            = reactive({ rv[["dataset"]]   }),
        dataset_not_subset = reactive({ rv$alist$ds_copy   }),
        click_count        = reactive({ rv[["click_count"]] })
      )
    )

})}
