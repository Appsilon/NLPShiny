box::use(
  shiny[reactiveValues],
  utils[write.csv]
)

box::use(
  app / mongo / mongo_fun[...]
)

#' @export
dataset_init <- basic_colombia()

# box::use(
#   mongo / mongo_secret,
#   mongo / mongo_fun
# )

#' @export
localMode<-FALSE

#' @export
styleTop      <- "padding-top:10px"

#' @export
styleBottom   <- "padding-bottom:0.5em;"

#' @export
styleTopBottom<- "padding-top:10px;padding-bottom:5px"

#' @export
styleCalendar <- "margin-top:10px;max-width:150px;"

#' @export
inTypeNa<-c("type1","type2")

#' @export
longTypeNames <- c("cash"="Cash Transfer","health"="Healthcare")

#' @export
rv<-reactiveValues(height=365)

###
#
#   initial datasets
#
###

if(file.exists("app/outfiles/selection.csv")){
  file.remove("app/outfiles/selection.csv")
}

write.csv(tolower(dataset_init[,"idea"]), "app/outfiles/selection.csv", row.names = T)

#
#   make dfs for tagging
#

health_df <- as.data.frame(dataset_init[which(dataset_init$service_type=="Healthcare"),"idea"])

cash_df   <- as.data.frame(dataset_init[which(dataset_init$service_type=="Cash Transfer"),"idea"])

if(file.exists("app/outfiles/cash_df.csv")){
  file.remove("app/outfiles/cash_df.csv")
}

if(file.exists("app/outfiles/health_df.csv")){
  file.remove("app/outfiles/health_df.csv")
}

if(nrow(health_df)>0) {
  write.csv(health_df, "app/outfiles/health_df.csv", row.names = T)
}

if(nrow(cash_df)>0) {
  write.csv(cash_df, "app/outfiles/cash_df.csv", row.names = T)
}
