library(dplyr)
library(mapsf)
source("scripts/import.R")
source("funcs/codigo_nombre.R")
### tablas resumen ###

# 8 municipios con con almenos 5 dependientes de mayor flujo entrante
sumas = flux %>%
  group_by(DESTINO) %>%
  summarise(
    n = n(),
    across('COMERCIO':'OTROS SERV', sum),
  ) %>% mutate(TOTAL = rowSums(.[,-1])) %>%
  arrange(desc(TOTAL)) %>%
  select(DESTINO, n, TOTAL, everything()) %>%
  filter(n >= 5) %>%
  head(8) %>%
  codigo_nombre(tbl = .)

# gráfica con los municipios dependientes de los 8 anteriores
dev.new()
par(mfrow = c(4, 2))
mf_theme("candy")

for(m in sumas$DESTINO){
  # códigos municipios dependientes
  d = flux %>% filter(
    DESTINO == codigo_nombre(m, nb_a_id = TRUE, num = TRUE)
  ) %>% pull(ORIGEN)
  
  # flujos hacia el municipio destino 
  f = flux %>% filter(
    DESTINO == codigo_nombre(m, nb_a_id = TRUE, num = TRUE)
  ) %>% select(-DESTINO) %>%
    mutate(FLUJO = rowSums(across('COMERCIO':'OTROS SERV')), .keep = "unused")
  
  # flujos hacia cualquier municipio
  t = flux %>% 
    filter(ORIGEN %in% d) %>% 
    select(-DESTINO) %>%
    group_by(ORIGEN) %>%
    summarise(
      TOTAL = sum(across(everything()))
    )
  
  # shapefile con la información anterior
  s = left_join(shp, f, join_by(cod_mun == ORIGEN)) %>%
    left_join(t, join_by(cod_mun == ORIGEN))
  
  # municipios (no) dependientes y destino
  s$municipio = NA
  s$municipio[which(s$cod_mun %in% d)] = 0
  s$municipio[which(s$nombre == m)] = 1
  
  # gráfico con dos variables
  mf_map(x = s, var = "municipio", type = "typo", leg_pos = NA, 
         pal = "Pastel 1", alpha = .7, col_na = "white")
  
  mf_map(s, var = c("FLUJO", "TOTAL") , type = "prop_choro", 
         breaks = "geom", nbreaks = 5, pal = "Greens",
         leg_val_rnd = c(0, -2), leg_pos = "topleft",
         leg_frame = TRUE, inches = .1, add = TRUE)
  
  mf_layout(
    title = paste("Municipios dependientes de", m),
    credits = paste0(
      "Fuente: Universidad de Sevilla\n",
      "mapsf ",
      packageVersion("mapsf")
    ),
    arrow = FALSE, frame = TRUE
  )
  mf_arrow(pos = "topright")
  
}