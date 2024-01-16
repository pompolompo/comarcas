library(dplyr)
library(mapsf)
source("scripts/prepara_flujos.R")
source("funcs/codigo_nombre.R")
### selección de cabeceras

d = 10000
units(d) = "m"

normal = flux0 %>%
#  filter(DISTANCIA < d) %>%
  codigo_nombre() %>%
  group_by(DESTINO) %>%
  summarise(
    n = n(),
    media_F = mean(FLUJO),
    media_FD = mean(
      ifelse(SE_TOCAN, FLUJO, FLUJO/DISTANCIA)),
    suma_F = sum(FLUJO),
    suma_FD = sum(
      ifelse(SE_TOCAN, FLUJO, FLUJO/DISTANCIA))
  ) %>% mutate(
    media_F = media_F/(max(media_F)-min(media_F)),
    media_FD = media_FD/(max(media_FD)-min(media_FD)),
    suma_F = suma_F/(max(suma_F)-min(suma_F)),
    suma_FD = suma_FD/(max(suma_FD)-min(suma_FD)),
    .keep = "unused"
  ) %>% mutate(
    ENTRANTE = (media_F + media_FD + suma_F + suma_FD)/4,
    .keep = "unused"
    ) %>%
  bind_rows(
    .,
    tibble(
      DESTINO = shp$nombre[!(shp$nombre %in% .[["DESTINO"]])],
      n = 0,
      ENTRANTE = 0
    )
  ) %>% arrange(desc(ENTRANTE))

# dependencia normalizada
n = 10
shp$depende = setNames(normal$ENTRANTE, normal$DESTINO)[shp$nombre]
shp$cabecera = ifelse(shp$nombre %in% normal$DESTINO[1:n],
                      "Cabecera", "Adherible")

# tabla y gráfico (1)
mf_theme("candy")

mf_map(x = shp, var = "cabecera", type = "typo",
       leg_pos = "topleft", leg_frame = TRUE, leg_title = "Tipología")

mf_label(x = shp[shp$cabecera == "Cabecera", ], var = "nombre",
  cex = 0.9, halo = TRUE, r = 0.15)

mf_layout(title = "Candidatos a cabecera", 
          credits = "",
          arrow = FALSE)

mf_arrow("topright")

mf_inset_on(fig = c(.6, .95, .1, .4))

bks = c(0, .2, .3, .4, .5, 1)
fg = getOption("mapsf.fg")

hist(shp$depende[which(shp$nombre %in% normal$DESTINO[1:n])][-which(shp$nombre == "Málaga")],
     breaks = bks, border = fg,
     col = hcl.colors(n = 5, palette = "Dark Mint", rev = TRUE),
     axes = FALSE, labels = "", xlab = "", ylab = "", main = "")

text(label = "Proporción de \nflujos entrantes\n en comparación \ncon la capital" , 
     x = .8, y = 2, cex = 1, col = fg)

axis(side = 1, at = bks, las = 1, 
     tick = FALSE, line = -.9,  cex.axis = .7)

mf_inset_off()
