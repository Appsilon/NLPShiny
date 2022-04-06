#
# form module
#

box::use(
  shiny[
    moduleServer,
    NS,
    renderText,
    tagList,
    div,
    reactiveValues,
    renderUI,
    uiOutput,
    observeEvent,
    h3,
    verbatimTextOutput,
    br
  ],
  shiny.semantic[
    segment,
    form,
    actionButton,
    icon,
    checkbox_input,
    splitLayout
  ]
)

box::use(
  app / logic / functions_NLP[...],
)

#' @description this module processes the objects of the app, i.e. selected
#' data.frame and processes it to produce a form with its variables. If the
#' button is clicked a new 'row' is produced
#' @param vars_unify set of objects shared among modules
#' @return list (row) to rbind to data.frame
#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("input1Box")),
    uiOutput(ns("input2Box"))
  )
}

#' @export
server <- function(id, vars_unify) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(termButtonVal = 0, deleteButton = 0)

    output$input1Box <- renderUI({
      div(
        style = "width:500px;",
        segment(
          form(
            div(
              style = "text-align:center;",
              h3("Add an entry to the database")
            ),
            br(),
            render_field_text_in(ns, "Country", "country_name", "Colombia", 300),
            render_field_text_in(ns, "Service", "service_type", "Healthcare", 300),
            render_field_text_in(ns, "Response type", "response_type", "Survey", 300),
            render_field_text_in(ns, "User ID", "user_id", "6685654", 300),
            render_field_text_in(ns, "City", "city", "Soacha", 300),
            render_field_text_in(ns, "Departamento", "state", "Cundinamarca", 300),
            render_field_text_in(ns, "Local", "location", "Soacha", 300),
            render_field_text_in(ns, "Service Point", "service_point_name", "On-site", 300),
            render_field_text_in(ns, "Neighbourhood", "neighbourhood", "SOACHA", 300),
            splitLayout(
              style = "padding-top:15px;padding-bottom:15px;",
              checkbox_input(ns("satisfied"), "Is the user satisfied?", TRUE),
              checkbox_input(ns("is_starred"), "female", TRUE)
            ),
            div(
              style = "padding-top:15px;padding-bottom:15px;",
              render_field_text_in(
                ns, "Feedback:", "idea",
                "Todos los servicios muy completos", 400
              )
            )
          )
        )
      )
    })

    output$input2Box <- renderUI({
      div(
        style = "width:500px;",
        segment(
          splitLayout(
            style = "background:#FFFFFF;",
            tagList(
              div(
                style = "padding:10px 0px 20px 10px",
                actionButton(ns("upload_mongo"),
                  "Upload entry to mongo Colombian dataset",
                  icon("cloud upload"),
                  style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                )
              ),
              div(
                style = "padding:10px 0px 10px 5px",
                verbatimTextOutput(ns("uploadText"))
              )
            ),
            tagList(
              div(
                style = "padding:10px 0px 20px 10px",
                actionButton(ns("removelast"),
                  "Remove last entry in Colombian dataset",
                  icon("eraser"),
                  style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                )
              ),
              div(
                style = "padding:10px 0px 20px 15px",
                verbatimTextOutput(ns("deleteText"))
              )
            )
          )
        )
      )
    })

    observeEvent(vars_unify$dataset(), ignoreInit = T, {
      rv$termButtonVal <- 1

      rv$deleteButton <- 1
    })

    observeEvent(input$upload_mongo, ignoreInit = T, {
      rv$termButtonVal <- 0

      rv$deleteButton <- 1

      # satisfied_num<- as.integer(input$satisfied)
      satisfied_num <- as.integer(input$satisfied)

      created_at_tz <- paste0(
        sub(
          "([0-9]{4}-[0-9]{2}-[0-9]{2}).*", "\\1",
          Sys.time()
        ),
        "T12:00:00Z"
      )

      randomi <- c(8, 4, 4, 4, 13)

      set1 <- c(0:9, tolower(LETTERS[1:6]))

      unique_id <- paste0(sapply(randomi, function(x) paste0(sample(set1, x), collapse = "")), collapse = "-")
      my_entry <- data.frame(
        country_name = input$country_name,
        service_type = input$service_type,
        satisfied = input$satisfied,
        response_type = input$response_type,
        idea = input$idea,
        user_id = input$user_id,
        is_starred = input$is_starred,
        city = input$city,
        state = input$state,
        location = input$location,
        service_point_name = input$service_point_name,
        neighbourhood = input$neighbourhood,
        unique_id = unique_id,
        satisfied_num = satisfied_num,
        created_at_tz = created_at_tz
      )

      my_entry$created_at_tz_posix <- as.POSIXct(my_entry$created_at_tz, "GMT")

      saved <- tryCatch(save_data(my_entry, "colombia_big", "kujakuja"),
        error = function(e) {
          print(paste("no internet"))
          "no internet"
        }
      )

      if (class(saved) != "character") {
        rv$messageUpload <- "Successfully uploaded"
      } else {
        rv$messageUpload <- "not uploaded, check internet"
      }
    })

    ############################################################
    #
    #   Observer remove last entry added to mongo
    #
    ############################################################

    observeEvent(input$removelast, {
      rv$deleteButton <- 0
      rv$termButtonVal <- 1

      collectionName <- "colombia_big"
      databaseName <- "kujakuja"

      db_col <- connectdb(collectionName, databaseName)

      iter <- db_col$iterate(sort = paste0('{\"$natural\":', -1, "}"))
      json <- iter$json(1)
      # json
      db_col$remove(json)
      rv$removeText <- "deleted 1 item"
    })

    output$uploadText <- renderText({
      if (rv[["termButtonVal"]] == 0) {
        rv$messageUpload
      } else {
        return("")
      }
    })

    output$deleteText <- renderText({
      if (rv[["deleteButton"]] == 0) {
        rv$removeText
      } else {
        return("")
      }
    })
  })
}
