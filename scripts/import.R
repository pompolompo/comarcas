library(sf)
library(readxl)

### importar datos ###

# shapefile
shp = read_sf("datos/shape_data/13_01_TerminoMunicipal.shp") %>%
  .[which(.[["provincia"]] == "Málaga"), ]

# flujos municipales
flux = read_xlsx("datos/malaga.xlsx")
