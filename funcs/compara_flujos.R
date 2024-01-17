library(tidyr)
library(mapsf)
source("funcs/codigo_nombre.R")
source("scripts/prepara_flujos.R")
### función compara gráficamente los flujos hacia 2 municipios ###

compara_flujos = function(nb, # nombre de los municipios a comparar
                          shp, # shapefile general
                          flux0, # flujos ponderados
                          f = c(.7, .975, .05, .3), # coordenadas mapa inset
                          l = "topright" # posición de la leyenda
                          ){
  
  # origen, diferencia y destino mayoritario
  x = flux0 %>% codigo_nombre() %>%
    select(-SE_TOCAN, -DISTANCIA) %>%
    filter(DESTINO %in% nb) %>% 
    pivot_wider(
      names_from = DESTINO,
      values_from = FLUJO,
      values_fill = 0
    ) %>% mutate(
      DIFERENCIA = abs(pick(nb[1]) - pick(nb[2]))[,1],
      'DIRECCIÓN' = ifelse(pick(nb[1]) > pick(nb[2]),
                           nb[1], nb[2])[,1]) %>%
    select(ORIGEN, DIFERENCIA, DIRECCIÓN)
  
  # partes del mapa a usar
  comparados = filter(shp, nombre %in% nb)
  participan = left_join(shp, x, by = join_by(nombre == ORIGEN)) %>%
    filter(is.na(DIRECCIÓN) == FALSE)
  cercanos = st_join(shp, participan, by = st_touches(shp, participan), left = FALSE)
  
  ### mapa ###
  mf_theme("candy")
  
  # comparación de flujos
  mf_map(cercanos)
  mf_map(comparados, var = "nombre", type = "typo", add = TRUE, leg_pos = NA)
  mf_map(participan, var = c("DIFERENCIA", "DIRECCIÓN"), type = "prop_typo", 
         leg_pos = l, leg_frame = TRUE)
  mf_layout(title = paste("Comparación de flujos entre", nb[1], "y", nb[2]),
            credits = paste0(
              "Fuente: Universidad de Sevilla\n",
              "mapsf ",
              packageVersion("mapsf")
            ))

  # posición en el mapa general
  mf_inset_on(fig = f)
  mf_map(shp)
  mf_map(filter(shp, nombre %in% c(comparados$nombre, participan$nombre)),
         var = "provincia", type = "typo", leg_pos = NA, add = TRUE, pal = "black")
  mf_scale(pos = "bottomleft")
  mf_inset_off()
  return(invisible())
}
