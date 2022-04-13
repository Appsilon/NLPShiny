box::use(
  data.table[setDF, fread],
)

#####################################
#
#   COMPLEMENTARY OBJECTS, SMALL TABLES AND LISTS
#
####################################

#' @export
today_date <- Sys.Date() + 1

#' @export
ser_list <- readRDS("app/rds/ser_list.rds")

#####################################
#
#   places, locations
#
####################################

#' @export
col_states <- c(
  "Antioquia", "Arauca", "Atlántico", "Bolivar", "Cundinamarca",
  "La Guajira", "Magdalena", "Nariño", "Norte de Santander", "Santander",
  "Valle del Cauca"
)

#' @export
col_states_coord <- setDF(fread("app/datasets/col_states_coord.csv", encoding = "UTF-8"))

#' @export
colombia_neigh <- setDF(fread("app/datasets/withNeigh_coords.csv", encoding = "UTF-8"))

#' @export
colombia_locations <- setDF(fread("app/datasets/colombia_locations.csv", encoding = "UTF-8"))

# names country vector
#' @export
afr_countries <- readRDS("app/rds/afr_countries.rds")

# country_name  location_name   lat      lng
#' @export
afr_locations_coord <- setDF(fread("app/datasets/afr_locations_coord.csv", encoding = "UTF-8"))

# country_name  location_name
#' @export
afr_locations <- readRDS("app/rds/afr_locations.rds")

# country_name location_name  service_point_name       lat      lng
#' @export
africa_locations <- setDF(fread("app/datasets/africa_locations.csv", encoding = "UTF-8"))

#####################################
#
#   MAP colors
#
####################################

#' @export
color_list12 <- c(
  "forestgreen", # 228b22 34 139 34
  "#ee0000", # red2 238 0 0
  "orange", #     255 165 0
  "cornflowerblue", # 6495ed  100 149 237
  "magenta", # ff00ff  255 0 255
  "#6e8b3d", # darkolivegreen4', #6e8b3d 110 139 61
  "indianred1", # ff6a6a 255 106 106
  "tan4", # 8b5a2b 139 90 43
  "darkblue", # 00008b  0 0 139
  "#8b7e66", # wheat4 139 126 102
  "#8b1a1a" # 139 26 26
  , "#99ff99"
) # 153 255 153
names(color_list12) <- ser_list[[2]]

#' @export
dec_color_list12 <- c(
  "34, 139, 34",
  "238, 0, 0",
  "255, 165, 0",
  "100, 149, 237",
  "255, 0, 255",
  "110, 139, 61",
  "255, 106, 106",
  "139, 90, 43",
  "0, 0, 139",
  "139, 126, 102",
  "139,26, 26",
  "153, 255, 153"
)
names(dec_color_list12) <- ser_list[[2]]

#####################################
#
#   columns
#
####################################

#' @export
dontwrap_cols <- c(
  "feedback", "date", "nlp_tag", "location", "created_at_tz", "location_name",
  "city"
  # , "service_point_name"
  , "Service Point"
)

#' @export
unwanted_columns <- c(
  "country_name", "satisfied_num",
  "response_type",
  "created_at_tz",
  "unique_id",
  "user_id",
  "state",
  "created_at_tz_posix",
  "is_starred",
  "city"
)

#' @export
first_cols <- c(
  "nlp_tag", "feedback", "date", "satisfied", "service_type",
  "service_point_name"
  # , "Service Point"
)

#' @export
columns_col <- c(
  "date", "country_name", "satisfied", "response_type",
  "satisfied_num", "created_at_tz", "unique_id",
  "user_id", "is_starred", "city", "state",
  "location", "service_point_name", "neighbourhood"
)
columns_col <- sort(columns_col)

#' @export
columns_afr <- c(
  "date", "country_name", "location_name", "service_point_name",
  "satisfied", "satisfied_num", "created_at_tz",
  "unique_id", "user_id"
)
columns_afr <- sort(columns_afr)

#' @export
style_top <- "padding-top:10px;"

#' @export
style_bottom <- "padding-bottom:0.5em;"

#' @export
style_top_bottom <- "padding-top:10px;padding-bottom:5px;"

#' @export
style_calendar <- "margin-top:10px;max-width:150px;"

#' @export
i_type_na <- c("type1", "type2")

#' @export
long_type_names <- c("cash" = "Cash Transfer", "health" = "Healthcare")
