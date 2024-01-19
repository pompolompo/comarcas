library(dplyr)
library(sf)
library(stringr)
source("funcs/codigo_nombre.R")
source("scripts/import.R")
### preparación de los flujos ###

# ponderación criterios
w = matrix(c(.1, .25, .25, .15, .1, .1 , .05))
flux0 = select(flux, -ORIGEN, -DESTINO) %>%
  as.matrix(.) %*% w %>%
  bind_cols(ORIGEN = flux$ORIGEN, 
            DESTINO = flux$DESTINO, 
            FLUJO = .)

# se tocan? qué distancia los separa?

# tocan = st_touches(shp, sparse = FALSE)
# dist = st_distance(shp)
# as.vector(dist) %>% .[. != 0] %>% quantile(probs = c(.1, .9))
load("datos/relaciones.RData")

a = sapply(X = as.character(flux0$ORIGEN), 
           FUN = str_which,
           string = shp$cod_mun)

b = sapply(X = as.character(flux0$DESTINO), 
           FUN = str_which,
           string = shp$cod_mun)

flux0$SE_TOCAN = tocan[a, b] %>% diag()
flux0$DISTANCIA = dist[a, b] %>% diag() #%>% 
#  cut(breaks = c(-Inf, 1000, 5000, 25000, 500000, Inf), 
#      labels = c("MUY CERCA", "CERCA", "MEDIA", "LEJOS", "MUY LEJOS"))

# distancia y colindancia sin ponderar
flux00 = flux
flux00$SE_TOCAN = tocan[a, b] %>% diag()
flux00$DISTANCIA = dist[a, b] %>% diag()
