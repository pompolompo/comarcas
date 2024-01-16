library(dplyr)
library(mapsf)
source("scripts/prepara_flujos.R")
### selección de cabeceras

### normalizando medias
# con distancias menores a d
d = 10000
units(d) = "m"

normal = flux0 %>%
  filter(DISTANCIA < d) %>%
  codigo_nombre() %>%
  group_by(DESTINO) %>%
  summarise(
    n = n(),
    media_F = mean(FLUJO),
    media_FD = mean(
      ifelse(SE_TOCAN, FLUJO, FLUJO/DISTANCIA)
    ),
    suma_F = sum(FLUJO),
    suma_FD = sum(
      ifelse(SE_TOCAN, FLUJO, FLUJO/DISTANCIA)
    )
  ) %>%
  filter(n > 4) %>%
  mutate(media_F = media_F/(max(media_F)-min(media_F)),
         media_FD = media_FD/(max(media_FD)-min(media_FD)),
         suma_F = suma_F/(max(suma_F)-min(suma_F)),
         suma_FD = suma_FD/(max(suma_FD)-min(suma_FD)),
         .keep = "unused"
         ) %>% 
  mutate(media_N = (media_F + media_FD)/2,
         suma_N = (suma_F + suma_FD)/2) %>%
  arrange(desc(media_N))

# tabla y gráfico (1)
n = 8
head(normal, n)

shp$cabecera = ifelse(shp$nombre %in% normal$DESTINO[1:n],
                      "Cabecera", "Adherible")

mf_map(x = shp, var = "cabecera", type = "typo",
       leg_pos = "topleft", leg_frame = TRUE, leg_title = "Tipología")

mf_label(x = shp[shp$cabecera == "Cabecera", ], var = "nombre",
  cex = 0.9, halo = TRUE, r = 0.15)

mf_layout(title = "Candidatos a cabecera", 
          credits = "",
          arrow = FALSE)

mf_arrow("topright")

#
shp$cabecera = ifelse(shp$nombre %in% normal$DESTINO[1:10] &
                        !(shp$nombre %in% c("Málaga",
                                            "Álora",
                                            "Marbella")),
                      "Cabecera", "Adherible")

