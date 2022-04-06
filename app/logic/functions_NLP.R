box::use(
  shiny.react[JS],
  leaflet[setView,leaflet,
          addTiles,providerTileOptions,leafletOptions,
          markerClusterOptions,
          addLegend,
          addCircleMarkers],
  shiny[tags],
  shiny.semantic[field,text_input],
  DT[datatable,formatStyle,formatRound]
)

box::use(
  app / objects / objects_NLP[...]
)

#######################
#
#  DT table function
#
######################

#' @export
make_table <- function(dataset_init, filtered_cols,
                       loc_u, dontwrap_cols_filt,
                       filtered_cols_sp, new_names) {
  mydt<-datatable(dataset_init[, filtered_cols],
                      colnames = new_names,
                      rownames = F,
                      extensions = c("Scroller", "ColReorder"), # c('Responsive', 'ColReorder'),
                      options = list(
                        "pageLength" = 5,
                        dom = "ftipR",
                        scrollX = TRUE,
                        scrollY = TRUE,
                        columnDefs = list(list(
                          targets = loc_u,
                          render = JS("function(data, type, row, meta) {",
                                      "return type === 'display' && data.length > 15 ?",
                                      "'<span title=\"' + data + '\">' + data.substr(0, 8) + '...</span>' : data;",
                                      "}")
                        ))
                      )
  ) |>
    formatStyle(dontwrap_cols_filt, "white-space" = "pre") |>
    formatStyle(columns = filtered_cols_sp, `font-size` = "12px") |>
    formatRound(columns = c("lat", "lng"), digits = 2)
  mydt
}

#' @export
format_date <- JS("
  function format_date(date) {
    if (!date) return '';
    const month = (date.getMonth() + 1).toString().padStart(2, '0');
    const day = date.getDate().toString().padStart(2, '0');
    const year = date.getFullYear().toString().substr(-2);
    return `${day}/${month}/${year}`;
  }
")

#' @export
js_functions <- lapply(dec_color_list12, function(color) {
  JS(paste0("function (cluster) {
                          var childCount = cluster.getChildCount();
                          if (childCount < 100) {
                            c = 'rgba(", color, ", 0.4);'
                          } else if (childCount < 500) {
                            c = 'rgba(", color, ", 0.55);'
                          } else {
                            c = 'rgba(", color, ", 0.6);'
                          }
                          return new L.DivIcon({ html: '<div style=\"background-color:'+c+'\"><span>' + childCount + '</span></div>',
                          className: 'marker-cluster'
                          });
                        }"))
})

#' @export
africa_coord_date <- function(dataset) {
  dataset <- dplyr::left_join(dataset,
    africa_locations,
    by = c(
      "country_name", "location_name",
      "service_point_name"
    ),
    all.x = TRUE,
    sort = FALSE
  )
  dataset$date <- as.Date(sub(" UTC", "", dataset$created_at_tz))
  dataset$idea <- gsub("(.{1,80})(\\s|$)", "\\1\n", dataset$idea)
  return(dataset)
}

#' @export
colombia_coord_date <- function(dataset) {
  dataset <- dplyr::left_join(dataset,
    colombia_neigh,
    by = c("state", "city", "neighbourhood"),
    all.x = TRUE,
    sort = FALSE
  )
  idx_no_lat <- which(is.na(dataset$lat))
  cols_no_coord <- setdiff(colnames(dataset), c("lat", "lng"))
  dataset[idx_no_lat, ] <- dplyr::left_join(dataset[idx_no_lat, cols_no_coord],
    colombia_locations,
    by = c(
      "city", "location", "country_name",
      "state", "service_point_name"
    ),
    all.x = TRUE,
    sort = FALSE
  )[, colnames(dataset)]
  dataset$date <- as.Date(sub(" UTC", "", dataset$created_at_tz))
  dataset$idea <- gsub("(.{1,80})(\\s|$)", "\\1\n", dataset$idea)
  return(dataset)
}

#' @export
build_map <- function(dataset_init, lng1, lat1, js_functions2, pal) {
  themap <- leaflet(
    data = dataset_init,
    options = leafletOptions(preferCanvas = TRUE)
  ) |>
    addTiles(options = providerTileOptions(
      updateWhenZooming = FALSE,
      updateWhenIdle = FALSE
    )) |>
    setView(
      lng = lng1,
      lat = lat1,
      zoom = 11
    )
  i <- 0
  for (service in sort(unique(dataset_init$service_type))) {
    i <- i + 1
    themap <- addCircleMarkers(
      map = themap,
      data = dataset_init[which(dataset_init$service_type %in% service), ],
      ~lng,
      ~lat,
      clusterOptions = markerClusterOptions(
        iconCreateFunction = js_functions2[[i]],
        spiderfyOnMaxZoom = TRUE,
        showCoverageOnHover = FALSE
      ),
      fillColor = ~ pal(service_type),
      stroke = FALSE,
      fillOpacity = 0.7
    ) # aCM
  } # for
  themap <- addLegend(
    map = themap,
    "bottomleft",
    pal = pal,
    values = ~service_type,
    title = "Services:",
    opacity = 1
  )
  themap
}

#' @export
render_field_text_in<- function(ns, label1, input1, value, width) {
field(class="inline field",
  tags$label(label1),
  text_input(ns(input1),
             value = value,
             type = "text",
             attribs=list(style=paste0("width:", width, "px;"))
  )
)
}
