box::use(
  mongolite[...]
)

box::use(
  app / mongo / mongo_secret[...],
  app / objects / functions_NLP[...],
  app / objects / objects_NLP[...]
)

##################################
#
#   mongo connection is enabled by default
#
###################################

# options_mongodb = list(
#   "host" = "cyto.s17ph.mongodb.net",
#   "username" = "ds4a_user",
#   "password" = "passwordds4a"
# )

#' @export
connectdb <- function(collectionName,databaseName
                      # ,options_mongodb1=options_mongodb
                      ) {
  # print(options()$mongodb$username)

  db <- tryCatch(
    mongo(collection = collectionName,
          url = sprintf(
            "mongodb+srv://%s:%s@%s/%s%s",
            options.mongodb$username,
            options.mongodb$password,
            options.mongodb$host,
            databaseName
            ,"?sockettimeoutms=1200000") )
    , error = function(e) {print(paste("42 no internet"))
      ; "no internet" } )
  return(db)
}

#' @export
readData <- function(connection,find_string, mylimit,mysort) {
  data <-
    # tryCatch(
    connection$find(find_string, limit=mylimit, sort=mysort)
  # , error = function(e) {print(paste("44 no internet")) ; "no internet" } )
  return(data)
}

#' @export
basic_colombia <- function() {

  ######################################################################
  #
  #  connecting mongodb !!!
  #
  ######################################################################

  collectionName <- "colombia_big"
  databaseName   <- "kujakuja"

  db58<<-db_col <- connectdb(collectionName,databaseName)

  center_init<-"Cundinamarca"
  find_string<- paste0('{"state":"',center_init,'"}')

  dataset <- readData(db_col,find_string, 1500,
                      paste0('{\"$natural\":',-1,'}')
  )

  dataset <- colombia_coord_date(dataset)

  filter_cols <- setdiff(colnames(dataset),unwanted_columns)

  no_idea <- setdiff(filter_cols,"idea")

  dataset<-dataset[,c("idea",no_idea)]

}
# #' @export
#' options(mongodb = list(
#'   "host" = "cyto.s17ph.mongodb.net",
#'   "username" = "ds4a_user",
#'   "password" = "passwordds4a"
#' ))



#' @export
basic_africa <- function(){

  collectionName <- "africa_big"
  databaseName   <- "kujakuja"

  if(!exists("db_afr")){
    db_afr <- connectdb(collectionName,databaseName)
  }

  find_string<- paste0('{"country_name":"',input$africa_country_input,'"'
                       ,',"location_name":"',input$center_input,'"}'
  )
  dataset <- readData(db_afr,find_string,input$max_input,
                      paste0('{\"$natural\":',input$old_new_input,'}')
  )
  dataset <- africa_coord_date(dataset)
}

######################################################################
#
# used 1 time for uploading data to mongodb !!!
#
######################################################################

#' @export
# saveData <- function(data,collectionName,databaseName) {
#   db <- tryCatch(mongo(collection = collectionName,
#                        url = sprintf(
#                          "mongodb+srv://%s:%s@%s/%s",
#                          options()$mongodb$username,
#                          options()$mongodb$password,
#                          options()$mongodb$host,
#                          databaseName)), error = function(e) {print(paste("25 no internet"))
#                            ; "no internet" } )
#   # data <- as.data.frame(data)
#   db$insert(data)
# }
