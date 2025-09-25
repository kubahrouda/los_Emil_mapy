install.packages(c("sf", "leaflet", "dplyr"))

library(sf)
library(leaflet)
library(dplyr)

# ---- 1. Načtení CSV ----
data <- read.csv("emil_presuny_eu_datum.csv", stringsAsFactors = FALSE, header = TRUE)

# Automatické rozpoznání obou formátů data
# 'ymd' pro americký formát (2023-05-01)
# 'dmy' pro evropský formát (1.5.2023)
data$datum <- parse_date_time(data$datum, orders = c("d.m.y H:M", "dmy", "ymd"))

# Ujistíme se, že data jsou správně seřazena
data <- data %>% arrange(datum)

# ---- 2. Body jako sf ----
points_sf <- st_as_sf(data, coords = c("lon", "lat"), crs = 4326)

# ---- 3. Linie mezi body ----
lines_list <- lapply(1:(nrow(data)-1), function(i) {
  st_linestring(matrix(c(data$lon[i], data$lat[i],
                         data$lon[i+1], data$lat[i+1]),
                       ncol = 2, byrow = TRUE))
})
segments_sf <- st_sfc(lines_list, crs = 4326) %>%
  st_sf(typ = data$typ[-1], geometry = .)

# ---- 4. Interaktivní mapa ----
leaflet() %>%
  addProviderTiles(providers$OpenStreetMap) %>%  # OSM podklad
  # segmenty podle typu
  addPolylines(data = segments_sf,
               color = ~ifelse(typ == "přirozeně", "green", "red"),
               weight = 3,
               opacity = 0.8,
               label = ~typ) %>%
  # body s popupem datum
  addCircleMarkers(data = points_sf,
                   radius = 5,
                   color = "blue",
                   fill = TRUE,
                   fillOpacity = 0.7,
                   popup = ~paste0(as.character(datum), "<br>",
                                   poznamka)) %>%
  addLegend(position = "topright",
            colors = c("green", "red"),
            labels = c("přirozeně", "umělý převoz"),
            title = "Způsob přesunu")

