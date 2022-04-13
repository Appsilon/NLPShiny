#
# unifier module server only
#
box::use(
  shiny[
    moduleServer, NS,
    reactiveValues,
    reactive,
    bindEvent,
    observe
  ],
  shinyjs[enable, disable]
)

box::use(
  app/objects/objects_NLP[...]
)

#' @description this module watches both main dataset selectors to produce an
#' updated universal selection. In addition it observes the form (page) to
#' update the data.frame selected
#' @param vars_unify      set of objects shared among modules
#' @return list with data.frame and categ. and numeric variables

server <- function(id, dataset_init, vars_filter,
                   vars_tag, vars_cash, vars_health,
                   vars_wordcloud) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    lat1 <- afr_locations_coord[which(afr_locations_coord$location_name %in% "Gihembe Camp"), ]$lat
    lng1 <- afr_locations_coord[which(afr_locations_coord$location_name %in% "Gihembe Camp"), ]$lng

    values <- reactiveValues(
      dataset = dataset_init,
      dataset_whole = dataset_init,
      cash_cat = F,
      region = 1,
      lat = lat1,
      lng = lng1,
      active_tag = 0,
      subset = 0
    )

    observe({
      values[["dataset_whole"]] <- values[["dataset"]] <- vars_filter$dataset()
      values[["region"]] <- vars_filter$region()
      values[["lat"]] <- vars_filter$lat()
      values[["lng"]] <- vars_filter$lng()
      values[["dataset_not_subset"]] <- NULL
      values[["allow_sub"]] <- vars_filter$allow_sub()

      enable("app-ns_tag-tag_button", asis = T)
    }) |> bindEvent(vars_filter$submit(), ignoreInit = T)

    observe({
      values[["dataset_whole"]] <- values[["dataset"]] <- vars_tag$dataset_tag()
      values[["cash_cat"]] <- vars_tag$cash_cat()
      values[["health_cat"]] <- vars_tag$health_cat()
      values[["active_tag"]] <- values$active_tag + 1
    }) |> bindEvent(vars_tag$tag_active(), ignoreInit = T)

    observe({
      values[["dataset"]] <- vars_cash$dataset_subset()
      values[["subset"]] <- values[["subset"]]+1
      disable("app-ns_wordcloud-trigram_subset_button", asis = T)
    }) |> bindEvent(vars_cash$subset_action(), ignoreInit = T)

    observe({
      values[["dataset"]] <- vars_health$dataset_subset()
      values[["subset"]] <- values[["subset"]]+1
      disable("app-ns_wordcloud-trigram_subset_button", asis = T)
    }) |> bindEvent(vars_health$subset_action(), ignoreInit = T)

    observe({
      values[["dataset"]] <- vars_wordcloud$dataset_trigram()
    }) |> bindEvent(vars_wordcloud$dataset_trigram(), ignoreInit = T)

    return(
      list(
        active_tag = reactive({
          values$active_tag
        }),
        dataset = reactive({
          values$dataset
        }),
        dataset_whole = reactive({
          values$dataset_whole
        }),
        cash_cat = reactive({
          values$cash_cat
        }),
        health_cat = reactive({
          values$health_cat
        }),
        subset = reactive({
          values$subset
        }),
        region = reactive({
          as.numeric(values$region)
        }),
        lat = reactive({
          values$lat
        }),
        lng = reactive({
          values$lng
        }),
        allow_sub = reactive(values$allow_sub)
      )
    )
  })
}
