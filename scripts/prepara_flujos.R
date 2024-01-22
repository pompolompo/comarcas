library(dplyr)
library(sf)
library(stringr)
source("funcs/codigo_nombre.R")
source("scripts/import.R")
### preparación de los flujos ###

# municipios con con almenos 5 dependientes de mayor flujo entrante
sumas = flux %>%
  group_by(DESTINO) %>%
  summarise(
    n = n(),
    across('COMERCIO':'OTROS SERV', sum),
  ) %>% mutate(TOTAL = rowSums(.[,-1])) %>%
  arrange(desc(TOTAL)) %>%
  select(DESTINO, n, TOTAL, everything()) %>%
  filter(n >= 5) %>%
  codigo_nombre(tbl = .)

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

# normalización de los flujos ponderados
d = 10000
units(d) = "m"

normal = flux0 %>%
  #  filter(DISTANCIA < d) %>%
  codigo_nombre() %>%
  group_by(DESTINO) %>%
  summarise( # ponderación según distancia
    n = n(),
    media_F = mean(FLUJO),
    media_FD = mean(
      ifelse(SE_TOCAN, FLUJO, FLUJO/DISTANCIA)),
    suma_F = sum(FLUJO),
    suma_FD = sum(
      ifelse(SE_TOCAN, FLUJO, FLUJO/DISTANCIA))
  ) %>% mutate( # normalización nadir 1
    media_F = (media_F - min(media_F))/(max(media_F)-min(media_F)),
    media_FD = (media_FD - min(media_FD))/(max(media_FD)-min(media_FD)),
    suma_F = (suma_F - min(suma_F))/(max(suma_F)-min(suma_F)),
    suma_FD = (suma_FD - min(suma_FD))/(max(suma_FD)-min(suma_FD)),
    .keep = "unused"
  ) %>% mutate( # media aritmética estadísticos
    ENTRANTE = (media_F + media_FD + suma_F + suma_FD)/4,
    .keep = "unused"
  ) %>% mutate( # normalización nadir 2
    ENTRANTE = (ENTRANTE - min(ENTRANTE))/(max(ENTRANTE)-min(ENTRANTE)),
    .keep = "unused"
  ) %>%
  bind_rows( # añadir 0 para municipios sin flujos entrantes
    .,
    tibble(
      DESTINO = shp$nombre[!(shp$nombre %in% .[["DESTINO"]])],
      n = 0,
      ENTRANTE = 0
    )
  ) %>% arrange(desc(ENTRANTE))


# municipios dependientes de una o más cabeceras

# cabeceras
nb = c("Ronda", "Antequera", "Marbella", "Vélez-Málaga", "Álora")
cd = codigo_nombre(nb, nb_a_id = T, num = T)

# relaciones dependencia
dep0 = filter(flux0, DESTINO %in% cd) %>% 
  select(ORIGEN, DESTINO) %>% 
  group_by(ORIGEN) %>% 
  summarise(d = paste(DESTINO, collapse = ", ")) %>% 
  arrange(desc(str_length(d)))

# relaciones dependencia con nombre y separadas
dep = dep0[10:nrow(dep0),] %>% mutate(
  d1 = d,
  d2 = NA,
  .keep = "unused"
) %>% bind_rows(
  dep0[1:9,] %>% mutate( 
    d1 = strsplit(d, split = ", ") %>% unlist() %>% .[seq(1, 18, 2)],
    d2 = strsplit(d, split = ", ") %>% unlist() %>% .[seq(2, 18, 2)],
    .keep = "unused")
) %>%
  mutate(
    ORIGEN = codigo_nombre(ORIGEN),
    CABECERA = codigo_nombre(d1),
    DESTINO_2 = codigo_nombre(d2),
    .keep = "none"
  ) %>% bind_rows(
    data.frame(
      ORIGEN = shp$nombre[!(shp$nombre %in% .[["ORIGEN"]])],
      CABECERA = NA,
      DESTINO_2 = NA
    )
  )
