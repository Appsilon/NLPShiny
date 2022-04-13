box::use(
  shiny[
    moduleServer, NS, uiOutput, reactive, reactiveValues,
    renderPlot, tagList, imageOutput, renderImage, renderUI,
    observeEvent, br, div, outputOptions, reactiveFileReader
  ],
  shiny.semantic[
    actionButton, icon, segment, selectInput, updateSelectInput, split_layout
  ],
  utils[
    write.csv, head
  ],
  data.table[
    setDF, fread,
  ],
  shinyjs[
    disabled, enable
  ],
  jpeg[
    readJPEG
  ]
)

script_string <- "python3 app/py/wc.py app/outfiles/selection.csv app/outfiles/word_freq.csv app/outfiles/word_cloud.jpg"
language_vec <- c("spanish", "english")

#
#  UI, leaflet map output
#

ui <- function(id) {
  ns <- NS(id)
  split_layout(cell_widths= "350px",
               style = "background:#FFFFFF;",
               tagList(
                 div(
                   actionButton(ns("wc_button"), "Generate", icon("cloud"),
                                style="color: #fff; background-color: #337ab7; border-color: #2e6da4; width:265px;"
                   )
                 ),
                 br(),
                 div(class="drop-container",
                     uiOutput(ns("wordcloud_select_ui"))
                 ),
                 br(),
                 div(
                   actionButton(ns("trigram_subset_button"), "Filter by trigram"
                                , icon("table"),
                                style="color: #fff; background-color: #337ab7; border-color: #2e6da4; width:265px;"
                   )
                 )
               ),
               div(style="max-width:400px;",
                   segment(
                     imageOutput(ns("my_image"), height="300px")
                   )
               )
  )
}

#
#  Server
#

server <- function(id, vars_unify, dataset_init) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(dataset=dataset_init)

    #
    # create word cloud jpg
    #
    jpeg_file <- reactiveFileReader(1000, session, "app/outfiles/word_cloud.jpg", readJPEG)

    initial_region <- 2

    if (file.exists("app/outfiles/selection.csv")) {
      system(paste(script_string, language_vec[initial_region]))
    }

    output$my_image <- renderImage({
      jpeg_file()
      list(src = "app/outfiles/word_cloud.jpg",
           contentType = "image/jpeg",
           width = 360,
           height = 280,
           alt = "wc")
    }, deleteFile = F)

    word_freq <- setDF(fread("app/outfiles/word_freq.csv"))

    output$wordcloud_select_ui<- renderUI({
      disabled(
        div(style="max-width:300px;",
            selectInput(ns("trigram_input"),
                        "Choose trigram",
                        choices = head(word_freq$trigram, 25)
            )
        )
      )
    })

    ##########################################
    #
    #  Observer calling python script    #
    #
    ##########################################

    observeEvent(c(#vars_unify$dataset_whole(),
                   vars_unify$subset()
    ), {
      updateSelectInput(session,
                        "trigram_input",
                        "Choose trigram",
                        choices = "Press Generate"
      )
    })

    observeEvent(c(input$wc_button,
                   vars_unify$dataset_whole()
                   ),
                 ignoreInit = T, {

      enable("trigram_input")

      rv$dataset <- vars_unify$dataset()

      if (file.exists("app/outfiles/selection.csv")) {
        file.remove("app/outfiles/selection.csv")
      }

      write.csv(tolower(rv$dataset[, "feedback"]), "app/outfiles/selection.csv", row.names = T)

      if (file.exists("app/outfiles/selection.csv")) {
        system(paste(script_string, language_vec[vars_unify$region()]))
      }

      rv$word_freq <- setDF(fread("app/outfiles/word_freq.csv"))

      updateSelectInput(session,
                        "trigram_input",
                        "Choose trigram",
                        choices = head(rv$word_freq$trigram, 25)
      )

      enable("trigram_subset_button")

    }) # wc_b

    observeEvent(input$trigram_subset_button #, ignoreNULL=FALSE
                 , {
                   rv$dataset_trigram <- rv$dataset[which(rv$dataset$feedback %in%
                                                            grep(paste0("\\b",
                                                                        unlist(strsplit(input$trigram_input,
                                                                                        " ")),
                                                                        "\\b",
                                                                        collapse=".*")
                                                                 , rv$dataset$feedback
                                                                 , ignore.case=TRUE
                                                                 , value=T)), ]

                 })

    return(list(
      dataset_trigram = reactive(rv$dataset_trigram)
    ))

  })}
