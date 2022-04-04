#
# submit module
#
box::use(
  shiny[
    bootstrapPage,
    moduleServer,
    NS,
    renderText,
    tags,
    textOutput,
    tagList,
    div,
    fluidPage,
    uiOutput,
    selectInput,
    conditionalPanel,
    strong,
    numericInput,
    HTML,
    reactive,
    renderUI,
    br,
    observeEvent,
    actionButton,
    reactiveValues
    ],
  shiny.semantic[
    split_layout,
    multiple_checkbox,
    icon
  ],
  shiny.fluent[
    DatePicker.shinyInput,
    ChoiceGroup.shinyInput,
  ]
)

box::use(
  app / mongo / mongo_fun[...],
  app / objects / global[...],
  app / objects / objects_NLP[...],
  app / objects / functions_NLP[...],
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
  split_layout(style = "background:#FFFFFF;",
  cell_widths = c("30%","5%","30%","5%","30%"),
    tagList(

  div(style=styleTop,
    selectInput(ns("region_input"),"Region",c("Colombia"=1,"Africa"=2)
                )
  )
  ,div(
       conditionalPanel(condition = "input.region_input == 2",
                        ns = ns,
                        uiOutput(ns("africaCountrySelect"))
       )
  )
  ,div(
       conditionalPanel(condition = "input.region_input == 1",
                         ns = ns,
                         uiOutput(ns("centerSelect"))
       )
  )
  ,div(
       conditionalPanel(condition = "input.region_input == 2",
                         ns = ns,
                         uiOutput(ns("centerSelect2"))
       )
  )
  , div(style=styleTop
        , strong("Date interval")
  )
  ,div(class="calendar1",
       tagList(
         tags$div(style="max-width:200px;", tags$div(strong("From")),
                  DatePicker.shinyInput(ns("dateInput_from")
                                        , formatDate=formatDate
                                        , value = "2016-01-01")
         )
         ,HTML("&nbsp&nbsp&nbsp&nbsp")
         ,tags$div(style="max-width:200px;", tags$div(strong("To")),
                   DatePicker.shinyInput(ns("dateInput_to")
                                         , formatDate=formatDate
                                         , value = Sys.Date())

         )
       )
  )
  , div(style=styleTop,
        numericInput(ns("max_input"),"Entries to load",1500,min=1000,max=250000 )
  )
)
,br()
,tagList(
  div(style=styleTopBottom,
      strong("Service type")
  )
  ,div(style=styleBottom
    ,shiny.semantic::checkbox_input(input_id=ns("allNoneInput")
                         , label = "All/None"
                         , is_marked = TRUE
                         , style = NULL
    )
  )
  ,div(
       conditionalPanel(condition = "input.region_input == 1",
                         ns = ns,
                         uiOutput(ns("servicetypeUI1"))
       )
       # ,uiOutput("africaCountrySelect")
  )
  ,div(
       conditionalPanel(condition = "input.region_input == 2",
                         ns = ns,
                         uiOutput(ns("servicetypeUI2"))
       )
  )
)
,br()
,div(class="third-column",
     tagList(
       div(class="third-container"

           , div(style=styleTopBottom,
      strong("Satified:")
  )
      ,multiple_checkbox(
        input_id = ns("satisfiedInput"),
        label = "",
        choices = c("Yes","No"),
        choices_value = c("TRUE","FALSE"),
        selected = c("TRUE","FALSE")
      )
  , div(style=styleTop
    , strong("Sort:")
  )
    ,ChoiceGroup.shinyInput(ns("old_new_input")
                           , value = -1,
                             options = list(
                               list(key = -1, text="newest"),
                               list(key = 1 , text="oldest")
                             )
                           )

  , div(style="position:absolute; bottom:15px;"

      ,actionButton(ns("submit")
                    , "1. Show Selected Community Feedback", icon("map")
                    ,style="color: #fff;
                     background-color: #337ab7;
                     border-color: #2e6da4;
                     border-radius:10px;"
            )
    )
  )
)
)
)

}

#
# server
#
#' @export
server <- function(id,dataset) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv<-reactiveValues(notReady=TRUE, subtaghide=0)

    output$africaCountrySelect <- renderUI({
      div(style=styleTop
        , selectInput(ns("africa_country_input")
                      , "Country"
                      , afr_countries
        )
      )
    })

    output$centerSelect <- renderUI({
      div(style=styleTop,
        selectInput(ns("center_input"),"State"
                      ,col_states
                      ,selected = "Cundinamarca"
        )
      )
    })

    output$servicetypeUI1  <- renderUI({
      tagList(
           multiple_checkbox(
             input_id = ns("type1"),
             label = "",
             choices = ser_list[[1]],
             choices_value = as.character(seq_along(ser_list[[1]])),
             selected      = as.character(seq_along(ser_list[[1]]) )
           )
      )
    })

    output$servicetypeUI2  <- renderUI({
      validate(
        need(
          try(input$region_input),""
        )
      )
      tagList(
          multiple_checkbox(ns("type2"),
                        label = "",
                               choices = ser_list[[2]]
                               ,choices_value = as.character(seq_along(ser_list[[2]]))
                               ,selected      = as.character(seq_along(ser_list[[2]]) )
      )
      )
    })

    observeEvent(input$allNoneInput,{
      validate(
        need(
          try(input$region_input)
          ,"")
      )
      if(input$allNoneInput) {
        update_multiple_checkbox(session
                                 , input_id=inTypeNa[as.numeric(input$region_input)]
                                 , selected = as.character(seq_along(
                                   ser_list[[as.numeric(input$region_input)]]) )
        )
      } else {
        update_multiple_checkbox(session
                                 , input_id=inTypeNa[as.numeric(input$region_input)]
                                 , selected = list())
      }
    })

    output$centerSelect2 <- renderUI({
      validate(
        need(
          try(input$region_input),""
        )
      )
      div(style=styleTop,
        selectInput(ns("center_input_2"),"Location"
                    ,unique(afr_locations[
                      which(
                        afr_locations$country_name ==
                               input$africa_country_input),]$location_name)
        )
      )
    })

    observeEvent(input$submit
                 , ignoreInit = T
                 ,
                 {


                   rv$subtaghide<-1
                   rv$wchide<-0
                   rv$trigramhide<-0

                 if(input$region_input==1) {
          collectionName <- "colombia_big"
          databaseName   <- "kujakuja"

          db_col258<<- db_col <- connectdb(collectionName,databaseName,mongo_fun$options_mongodb)

          satis_str   <- ifelse(
            length(
              tolower(
                as.character(input$satisfiedInput))
              ) %in% c(0,2)
            , '{ "$in" : [ true, false ] }'
            , tolower(as.character(input$satisfiedInput) )
            )

          service_str <- paste0("\""
                                ,paste0(ser_list[[as.numeric(input$region_input)]][as.numeric(input$type1)]
                                        ,collapse= "\",\""),"\"")

          find_string <- paste0('{'
                                ,'"service_type":{"$in" : [',service_str,'] }'
                                ,',"state":"',input$center_input,'"'
                                ,paste0(',"created_at_tz_posix":{"$gt":{"$date":"'
                                        ,input$dateInput_from,'T00:00:00Z"},"$lt":{"$date":"'
                                        ,input$dateInput_to,'T23:59:59Z"}}'
                                )
                                ,',"satisfied":',satis_str,'}'
          )

          dataset <- readData(db_col,find_string,input$max_input,
                            paste0('{\"$natural\":',input$old_new_input[[1]],'}')
          )

          if(nrow(dataset)==0) {
            find_string<- paste0('{'
                                 ,'"state":"',input$center_input,'"'
                                 ,paste0(',"created_at_tz_posix":{"$gt":{"$date":"'
                                         ,input$dateInput_from,'T00:00:00Z"},"$lt":{"$date":"'
                                         ,input$dateInput_to,'T23:59:59Z"}}'
                                 )
                                 ,',"satisfied":',satis_str,'}'
            )

            dataset<-readData(db_col,find_string,input$max_input,
                              paste0('{\"$natural\":',input$old_new_input,'}')
            )
          } # nrow 0

          #
          # add coordinates for a new mongo search
          #

          rv$dataset <- colombia_coord_date(dataset)

    rv$lat <- col_states_coord[which(col_states_coord$state %in% input$center_input),]$lat

    rv$lng <- col_states_coord[which(col_states_coord$state %in% input$center_input),]$lng

           } else {

                   ##
                   collectionName <-"africa_big"
                   databaseName   <-"kujakuja"

                   db_afr <- connectdb(collectionName,databaseName)

                   satis_str <- ifelse(length(
                     input$satisfiedInput
                     ) %in% c(0,2),
                     '{ "$in" : [ true, false ] }'
                     ,tolower(as.character(input$satisfiedInput) ) )

                   service_str<-paste0("\""
                                       ,paste0(ser_list[[as.numeric(input$region_input)]][as.numeric(input$type2)]
                                               ,collapse= "\",\""),"\""
                   )

                   find_string <- paste0('{'
                                         ,'"service_type":{"$in" : [',service_str,'] }'
                                         ,',"country_name":"',input$africa_country_input,'"'
                                         ,',"location_name":"',input$center_input_2,'"'
                                         ,paste0(',"created_at_tz_posix":{"$gt":{"$date":"'
                                                 ,input$dateInput_from,'T00:00:00Z"},"$lt":{"$date":"'
                                                 ,input$dateInput_to,'T23:59:59Z"} } '
                                         )
                                         ,',"satisfied":',satis_str,'}'
                   )

                   dataset <-readData(db_afr,find_string,input$max_input
                                      ,paste0('{\"$natural\":',input$old_new_input,'}')
                   )

                   if(nrow(dataset)==0) {
                     find_string <- paste0('{'
                                           ,'"country_name":"',input$africa_country_input,'"'
                                           ,',"location_name":"',input$center_input_2,'"'
                                           ,paste0(',"created_at_tz_posix":{"$gt":{"$date":"'
                                                   ,input$dateInput_from,'T00:00:00Z"},"$lt":{"$date":"'
                                                   ,input$dateInput_to,'T23:59:59Z"} } '
                                           )
                                           ,',"satisfied":',satis_str,'}'
                     )

                     # row number max.
                     # show last or first

                     dataset<-readData(db_afr,find_string,input$max_input,
                                       paste0('{\"$natural\":',input$old_new_input,'}')
                     )
                   }

                   rv$dataset<-africa_coord_date(dataset)

                   rv$lat<-afr_locations_coord[which(afr_locations_coord$location_name %in% input$center_input_2),]$lat
                   rv$lng<-afr_locations_coord[which(afr_locations_coord$location_name %in% input$center_input_2),]$lng

           }
                   ##
            filter_cols <- setdiff(colnames(rv$dataset), unwanted_columns)

            no_idea <- setdiff(filter_cols,"idea")

            rv$dataset <-rv$dataset[,c("idea",no_idea)]


    ###
    if(file.exists("app/outfiles/selection.csv")) {
      file.remove("app/outfiles/selection.csv")
    }

    if(input$region_input==1){
      rv$height=350
    } else {
      rv$height=390
    }

    write.csv(tolower(dataset[,"idea"]), "app/outfiles/selection.csv", row.names = T)


    }) #observeEvent submit

    rv$notReady <- FALSE

    ###

    return(
      list(
        submit   = reactive({ input$submit }),
        dataset  = reactive({ rv$dataset }),
        region   = reactive({ input$region_input }),

        # mycolors = reactive({ rv$mycolors  }),
        # filteredColors = reactive({ rv$filteredColors }),
        # filteredService= reactive({ rv$filteredService }),
        # height   = reactive({ rv$height  }),
        lat      = reactive({ rv$lat  }),
        lng      = reactive({ rv$lng  }) #,

        # JSfunctions_reac = reactive({ rv$JSfunctions_reac }) #,
        # notReady = reactive({ rv$notReady }),
        # subtaghide = reactive({ rv$subtaghide }),
        # wchide     = reactive({ rv$wchide }),
        # trigramhide= reactive({ rv$trigramhide })
      )
    )
})}
