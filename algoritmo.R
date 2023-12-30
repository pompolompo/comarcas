library(terra)
library(dplyr)

malaga <- vect("data/shape_data/13_01_TerminoMunicipal.shp") %>%
  .[which(.[["provincia"]] == "Málaga"), ] # shapefile municipios málaga

flujos <- read.csv("data/flujos.csv") # flujos municipales

w = matrix(c(.1, .25, .25, .15, .1, .1 , .05))
flujo = flujos %>% select(origen, destino, colind, dist) %>%
  bind_cols(flux = as.matrix(flujos[,3:9]) %*% w)

# selección cabeceras

## parámetros
k = 7 # número de cabeceras
d = 10000 # distancia máxima a considerar
c = 2 # grado de colindancia máxima a considerar
i = c(-Inf, 1, 10, 20, Inf)*1000 # cortes niveles distancia

# filtrar flujos
aux = flujo %>% filter(dist <= d, colind <= c)

# nivells distància
aux$dist_l = cut(aux$dist, breaks = i, include.lowest = TRUE)
        
# cal pensar com descontem estar més lluny (en nivells de ditància o nivells de colindància)
# així trobem una mesura única per ordenar els candidats a cabecera

# després cal considerar les incompatibilitats (i.e dos candidats enganxats)


# mesidas resumen
resum0 = aux %>% group_by(destino) %>%
  summarise(
    n = n(),
    suma = sum(flux),
    media = mean(flux),
    .groups = "keep"
  ) %>% arrange(desc(suma))

resum1 = aux %>% group_by(destino, colind) %>%
  summarise(
    n = n(),
    suma = sum(flux),
    media = mean(flux),
    .groups = "keep"
  ) %>% arrange(desc(suma))


# 