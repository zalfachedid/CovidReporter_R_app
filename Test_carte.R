library(leaflet)
library(sf)
library(dplyr)
library(stringr)
library(readr)

# Construction du fond de la carte interactive
nc <- st_read("../data/MapData/departements-20170102.shp")
nc
deps_wgs84 <- nc %>% mutate(nom = str_conv(nom, "utf-8"))
 # Arrangement par code_insee = code des départements
deps <- deps_wgs84 %>% arrange(code_insee)
 # Correction des code_insee
deps$code_insee[70] <- "69"
deps$code_insee[71] <- "71"


# Lecture covid
covid_data <- read_csv2("departement2020.csv")
head(covid_data,34)

# Jointure à gauche
deps <- deps %>% 
  left_join(covid_data, by = "code_insee")
deps

pal <- colorBin(
  palette = "viridis",
  domain = deps$nb_rea,
  reverse = TRUE
)

map_dep <- leaflet() %>%
  addTiles() %>%
  # polygone des regions
  addPolygons(
    data = deps, 
    label = ~nom,
    # popup = ~paste0("Densité : ", round(density), " hab/km2"), 
    fill = TRUE, 
    # Application de la fonction palette
    fillColor = ~pal(nb_rea),
    fillOpacity = 0.8,
    highlightOptions = highlightOptions(color = "white", weight = 2)) %>%
  addLegend(
    title = "Nombre d'hopitalisation en milier",
    pal = pal, values = deps$nb_rea)
map_dep