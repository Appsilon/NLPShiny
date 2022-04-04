box::use(
  data.table[setDF,fread]
)
# localMode <- TRUE  # uses csv

#####################################
#
#   COMPLEMENTARY OBJECTS, SMALL TABLES AND LISTS
#
####################################

#' @export
today_date <- Sys.Date() + 1

#' @export
ser_list            <- readRDS("app/rds/ser_list.rds")

############################
#
#   places, locations
#
############################

#' @export
col_states<-c("Antioquia", "Arauca", "Atlántico", "Bolivar", "Cundinamarca",
              "La Guajira", "Magdalena", "Nariño", "Norte de Santander", "Santander",
              "Valle del Cauca")

#' @export
col_states_coord    <- setDF(fread("app/datasets/col_states_coord.csv",encoding = 'UTF-8'))
#' @export
colombia_neigh      <- setDF(fread("app/datasets/withNeigh_coords.csv", encoding = 'UTF-8') )
#' @export
colombia_locations  <- setDF(fread("app/datasets/colombia_locations.csv", encoding = 'UTF-8') )

# names country vector
#' @export
afr_countries       <- readRDS("app/rds/afr_countries.rds")

# country_name  location_name   lat      lng
#' @export
afr_locations_coord <- setDF(fread("app/datasets/afr_locations_coord.csv",encoding = 'UTF-8')  )

# country_name  location_name
#' @export
afr_locations       <- readRDS("app/rds/afr_locations.rds")

# country_name location_name  service_point_name       lat      lng
#' @export
africa_locations    <- setDF(fread("app/datasets/africa_locations.csv" , encoding = 'UTF-8') )

#####################################
#
#   MAP colors
#
####################################
#' @export
colorList12  <- c('forestgreen',  #228b22 34 139 34
                  '#ee0000',      # red2 238 0 0
                  'orange',       #     255 165 0
                  'cornflowerblue', #6495ed  100 149 237
                  'magenta',      # ff00ff  255 0 255
                  '#6e8b3d',      #darkolivegreen4', #6e8b3d 110 139 61
                  'indianred1',   #ff6a6a 255 106 106
                  'tan4',         #8b5a2b 139 90 43
                  'darkblue',     # 00008b  0 0 139
                  '#8b7e66',      # wheat4 139 126 102
                  '#8b1a1a'       #139 26 26
                  ,'#99ff99')     # 153 255 153
names(colorList12) <- ser_list[[2]]

#' @export
decColorList12  <- c("34, 139, 34",
                     '238, 0, 0',
                     '255, 165, 0',
                     '100, 149, 237',
                     '255, 0, 255',
                     '110, 139, 61',
                     '255, 106, 106',
                     '139, 90, 43',
                     '0, 0, 139',
                     '139, 126, 102',
                     '139,26, 26',
                     '153, 255, 153')
names(decColorList12) <-  ser_list[[2]]

#####################################
#
#   columns
#
####################################

#' @export
dontwrap_cols<- c("idea","date","NLP_tag","location","created_at_tz","location_name"
                  ,"city"
                  # ,"service_point_name"
                  ,"Service Point"
)

#' @export
unwanted_columns<-c("country_name","satisfied_num"
                    ,"response_type"
                    ,"created_at_tz"
                    ,"unique_id"
                    ,"user_id"
                    ,"state"
                    ,"created_at_tz_posix"
                    ,"is_starred"
                    ,"city")

#' @export
first_cols<-c("NLP_tag","idea","date","satisfied","service_type"
              ,"service_point_name"
              # ,"Service Point"
)

# remove idea, its presence is mandatory, service_type, lat, lng

#' @export
mandatory<-c("idea","service_type","service_point_name","unique_id","lat","lng")

#' @export
columns_col<-c("date", "country_name",  "satisfied"     ,     "response_type"
               ,    "satisfied_num"  ,                  "created_at_tz"   ,   "unique_id"
               ,   "user_id"   ,         "is_starred"  ,       "city"           ,    "state"
               , "location"  ,         "service_point_name" ,"neighbourhood"
)

#' @export
columns_col<-sort(columns_col)

#' @export
columns_afr<-c("date","country_name"   , "location_name"  ,    "service_point_name"
               ,"satisfied"     ,     "satisfied_num"  ,         "created_at_tz"
               ,"unique_id"     ,     "user_id"
)

#' @export
columns_afr<-sort(columns_afr)
