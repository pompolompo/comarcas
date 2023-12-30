library(terra)
library(dplyr)
library()

malaga <- vect("data/shape_data/13_01_TerminoMunicipal.shp") %>%
  .[which(.[["provincia"]] == "Málaga"), ] # shapefile municipios málaga

flujos <- read.csv("data/flujos.csv") # flujos municipales


# selección cabeceras

## parámetros
k = 7 # número de cabeceras
d = 10000 # distancia máxima a considerar
c = 2 # grado de colindancia máxima a considerar

# filtrar flujos
aux = flujo %>% filter(dist <= d, colind <= c)

# vull diferenciar la suma i mitjana segons el grau de colindancia,
# de manera que es crein noves columnes com suma_1, suma_2, ..., suma_c y media_1, media_2, ..., media_c
# ara mateix el resultat és una fila per cada combinació destí grau colindancia
# puc intentar group by different o passar el resultat from long to wide

# mesidas resumen
resum0 = aux %>% group_by(destino) %>%
  summarise(
    n = n(),
    suma = sum(flux),
    media = mean(flux)
  ) %>% arrange(desc(suma))

# mesidas resumen
resum1 = aux %>% group_by(destino, colind) %>%
  summarise(
    n = n(),
    suma = sum(flux),
    media = mean(flux)
  ) %>% arrange(desc(suma))
