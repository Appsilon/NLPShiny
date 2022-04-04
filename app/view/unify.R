#
# unifier module server only
#
box::use(
  shiny[bootstrapPage, moduleServer, NS, renderText, tags, textOutput, tagList, div,
        fluidPage,
        reactiveValues,
        reactive,
        bindEvent,
        observe
        ],
)

#' @description this module watches both main dataset selectors to produce an
#' updated universal selection. In addition it observes the form (page) to
#' update the data.frame selected
#' @param vars_unifier      set of objects shared among modules
#' @return list with data.frame and categ. and numeric variables

server <- function(id,dataset_init,vars_submit
                    , vars_tag, vars_cash, vars_health
                    ) {

  moduleServer( id, function(input, output, session) {
    ns <- session$ns

    values <- reactiveValues(dataset=dataset_init, cash_cat=F)

    observe({
      values[["dataset"]] <- vars_submit$dataset()
      values[["region"]] <- vars_submit$region()
      values[["lat"]] <- vars_submit$lat()
      values[["lng"]] <- vars_submit$lng()


      shinyjs::hide("cash_select_UI_id", asis = T)
      shinyjs::hide("health_select_UI_id", asis = T)
      shinyjs::enable("mod_tag-tag_button", asis=T)
      # shinyjs::hide("cash_button_UI_id", asis = T)
      # shinyjs::hide("health_select_UI_id", asis = T)
      # shinyjs::hide("health_button_UI_id", asis = T)
      values[["dataset_not_subset"]]    <- NULL

    }) |> bindEvent(vars_submit$dataset() , ignoreInit = T)

    observe({
      values[["dataset"]]    <- vars_tag$dataset()
      values[["cash_cat"]]   <- vars_tag$cash_cat()
      values[["health_cat"]] <- vars_tag$health_cat()

    }) |> bindEvent(vars_tag$dataset() , ignoreInit = T)

    observe({
      values[["dataset"]]    <- vars_cash$dataset()
      values[["dataset_not_subset"]]    <- vars_cash$dataset_not_subset()


    }) |> bindEvent(vars_cash$click_count() , ignoreInit = T)

    observe({
      values[["dataset"]]    <- vars_health$dataset()
      values[["dataset_not_subset"]]    <- vars_health$dataset_not_subset()

    }) |> bindEvent(vars_health$click_count() , ignoreInit = T)

    return(
      list(
        dataset             = reactive({ values$dataset }),
        dataset_not_subset  = reactive({ values$dataset_not_subset }),
        cash_cat            = reactive({ values$cash_cat }),
        health_cat          = reactive({ values$health_cat }),
        region              = reactive({ values$region }),
        lat              = reactive({ values$lat }),
        lng              = reactive({ values$lng })
      )
    )

  })}
