box::use(
  utils[write.csv]
)

box::use(
  app / mongo / mongo_fun[...],
)

#' @export
dataset_init <- basic_africa()

if (!dir.exists("app/outfiles")) {
  dir.create("app/outfiles")
}

if (file.exists("app/outfiles/selection.csv")) {
  file.remove("app/outfiles/selection.csv")
}

write.csv(tolower(dataset_init[, "feedback"]),
          "app/outfiles/selection.csv",
          row.names = T
)
