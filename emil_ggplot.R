# install.packages(c("sf", "ggplot2", "rosm", "ggspatial", "dplyr", "lubridate"))

library(sf)
library(ggplot2)
library(rosm)
library(ggspatial)
library(dplyr)
library(lubridate)

# ---- 1. Načtení dat ----
data <- read.csv("emil_presuny_eu_datum.csv", stringsAsFactors = FALSE, header = TRUE)

# Automatické rozpoznání obou formátů data
# 'ymd' pro americký formát (2023-05-01)
# 'dmy' pro evropský formát (1.5.2023)
data$datum <- parse_date_time(data$datum, orders = c("d.m.y H:M", "dmy", "ymd"))

# Ujistíme se, že data jsou správně seřazena
data <- data %>% arrange(datum)

# ---- 2. Body ----
points_sf <- st_as_sf(data, coords = c("lon", "lat"), crs = 4326)

# ---- 3. Segmenty jako sf ----
# body pro každý úsek
lines_list <- lapply(1:(nrow(data)-1), function(i) {
  st_linestring(matrix(c(data$lon[i], data$lat[i],
                         data$lon[i+1], data$lat[i+1]),
                       ncol = 2, byrow = TRUE))
})

# sf objekt
segments_sf <- st_sfc(lines_list, crs = 4326) %>%
  st_sf(typ = data$typ[-1], geometry = .) %>%   # typ podle cílového bodu
  st_transform(3857)  # převod do stejné projekce jako mapa

# ---- 4. Převod do Web Mercator (EPSG:3857) – nutné pro rosm ----
points_3857 <- st_transform(points_sf, 3857)
bbox <- st_bbox(points_3857)


# buffer kolem všech bodů
buffer <- st_buffer(st_union(points_3857), dist = 20000) # 20000 metrů

# nový bbox z bufferu
bbox_buffered <- st_bbox(buffer)




# Původní kód
#...

# ---- Vylepšení pro čtvercovou mapu ----

# Vypočítání rozsahu pro X a Y osy
rozsah_x <- bbox_buffered["xmax"] - bbox_buffered["xmin"]
rozsah_y <- bbox_buffered["ymax"] - bbox_buffered["ymin"]

# Vypočítání středu mapy
stred_x <- mean(c(bbox_buffered["xmin"], bbox_buffered["xmax"]))
stred_y <- mean(c(bbox_buffered["ymin"], bbox_buffered["ymax"]))

# Použití většího rozsahu pro obě osy, aby mapa byla čtvercová
max_rozsah <- max(rozsah_x, rozsah_y) / 2

# Nové, čtvercové hranice
novy_xmin <- stred_x - max_rozsah
novy_xmax <- stred_x + max_rozsah
novy_ymin <- stred_y - max_rozsah
novy_ymax <- stred_y + max_rozsah

# Vlastní vizualizace
ggplot() +
  annotation_map_tile(type = "osm", zoom = 10) +
  
  # segmenty pohybu
  geom_sf(data = segments_sf, aes(linetype = typ),
          color = "coral", linewidth = 1) +
  
  # body pozorování
  geom_sf(data = points_3857, color = "tomato", size = 3) +
  
  # popisky s datem a poznámkou
  geom_sf_text(data = points_3857, 
               aes(label = paste(datum, poznamka, sep = "\n")),
               size = 2.5,
               nudge_y = 50,
               fontface = "bold", # nový parametr pro tučný text
               lineheight = 0.8) + # nový parametr pro menší mezery
  
  scale_linetype_manual(
    name = "Způsob přesunu",
    values = c("přirozeně" = "solid", "umělý převoz" = "dashed")
  ) +
  
  coord_sf(xlim = c(novy_xmin, novy_xmax),
           ylim = c(novy_ymin, novy_ymax),
           expand = FALSE) +
  
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "tl", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  theme_minimal() +
  
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )


ggsave("mapa_los.jpg", width = 8, height = 6, dpi = 300)
