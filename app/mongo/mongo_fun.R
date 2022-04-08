box::use(
  mongolite[...]
)

box::use(
  # app / mongo / mongo_secret[...],
  app / logic / functions_NLP[...],
  app / objects / objects_NLP[...]
)

#' @export
select_columns <- function(dataset, unwanted_columns) {
  filter_cols <- setdiff(colnames(dataset), unwanted_columns)

  no_idea <- setdiff(filter_cols, "idea")

  colnames(dataset)[which(names(dataset) == "idea")] <- "feedback"

  dataset <- dataset[, c("feedback", no_idea)]

  return(dataset)
}

#' @export
basic_colombia <- function() {
  collection_name <- "colombia_big"
  database_name <- "kujakuja"

  db_col <- connectdb(collection_name, database_name)

  center_init <- "Cundinamarca"

  find_string <- paste(
    "{\"service_type\":{\"$in\" : [\"Healthcare\",\"Cash Transfer\"] },",
    "\"state\":\"Cundinamarca\",",
    "\"created_at_tz_posix\":{\"$gt\":{\"$date\":\"2016-01-01T00:00:00Z\"}, \"$lt\":{\"$date\":\"2022-03-26T23:59:59Z\"}},",
    "\"satisfied\":false}"
  )

  dataset <- read_data(
    db_col, find_string, 1500,
    paste0('{\"$natural\":', -1, "}")
  )

  dataset <- colombia_coord_date(dataset)

  dataset <- select_columns(dataset, unwanted_columns)
}

#' @export
basic_africa <- function() {
  collection_name <- "africa_big"
  database_name <- "kujakuja"

  if (!exists("db_afr")) {
    db_afr <- connectdb(collection_name, database_name)
  }

  find_string <- paste0(
    '{"country_name":"', input$africa_country_input, '"',
    ', "location_name":"', input$center_input, '"}'
  )
  dataset <- read_data(
    db_afr, find_string, input$max_input,
    paste0('{\"$natural\":', input$old_new_input, "}")
  )
  dataset <- africa_coord_date(dataset)

  dataset <- select_columns(dataset, unwanted_columns)
}

#' @export
connectdb <- function(collection_name, database_name) {
  db <- tryCatch(
    mongo(
      collection = collection_name,
      url = sprintf(
        "mongodb+srv://%s:%s@%s/%s%s",
        Sys.getenv("MONGO_USERNAME"),
        # options.mongodb$username,
        Sys.getenv("MONGO_PASSWORD"),
        # options.mongodb$password,
        # options.mongodb$host,
        Sys.getenv("MONGO_HOST"),
        database_name,
        "?sockettimeoutms=1200000"
      )
    ),
    error = function(e) {
      print(paste("42 no internet"))
      "no internet"
    }
  )
  return(db)
}

#' @export
read_data <- function(connection, find_string, mylimit, mysort) {
  data <-
    connection$find(find_string, limit = mylimit, sort = mysort)
  return(data)
}

######################################################################
#
# used 1 time for uploading data to mongodb !!!
#
######################################################################

#' @export
save_data <- function(data, collection_name, database_name) {
  db <- tryCatch(mongo(
    collection = collection_name,
    url = sprintf(
      "mongodb+srv://%s:%s@%s/%s",
      options.mongodb$username,
      options.mongodb$password,
      options.mongodb$host,
      database_name
    )
  ), error = function(e) {
    print(paste("25 no internet"))
    "no internet"
  })
  db$insert(data)
}
