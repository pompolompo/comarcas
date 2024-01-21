library(tidyr)
library(mapsf)
source("funcs/codigo_nombre.R")
source("scripts/prepara_flujos.R")
### función compara gráficamente los flujos hacia 2 municipios ###

compara_flujos = function(nb, # nombre de los municipios a comparar
                          shp, # shapefile general
                          flux0, # flujos ponderados
                          expa = c(.25, 0, 0, .35), # expansión marco (abajo, izq, arriba, derecha)
                          fig = c(.65, .975, .05, .4) # marco mapa interior
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
                           nb[1], nb[2])[,1],
      AMBOS = ifelse(pick(nb[1]) == 0 | pick(nb[2]) == 0, 
                     "Uno", "Ambos")) %>%
    select(ORIGEN, DIFERENCIA, DIRECCIÓN, AMBOS)
  
  # partes del mapa a usar
  comparados = filter(shp, nombre %in% nb)
  participan = left_join(shp, x, by = join_by(nombre == ORIGEN)) %>%
    filter(is.na(DIRECCIÓN) == FALSE)
  cercanos = st_join(shp, participan, by = st_touches(shp, participan), left = FALSE)
  
  ### mapa ###
  mf_theme("candy")
  
  pal = c("#00A890", "grey")
  if(length(unique(x$AMBOS)) == 1) pal = "grey"
  # comparación de flujos
  mf_map(participan, var = "AMBOS", type = "typo", expandBB = expa,
         pal = pal, leg_frame = TRUE, leg_pos = "interactive")
  mf_map(comparados, var = "nombre", type = "typo", add = TRUE,
         leg_pos = NA , leg_frame = TRUE, pal = c("#422C70", "#00598B"))
  mf_map(participan, var = c("DIFERENCIA", "DIRECCIÓN"), type = "prop_typo", add = TRUE,
         leg_pos = "topright", leg_frame = TRUE, pal = c("#422C70", "#00598B"))
  
  mf_layout(title = paste("Comparación de flujos entre", nb[1], "y", nb[2]),
            credits = paste0(
              "Fuente: Universidad de Sevilla\n",
              "mapsf ",
              packageVersion("mapsf")
            ))

  # posición en el mapa general
  mf_inset_on(fig = fig)
  mf_map(shp)
  mf_map(filter(shp, nombre %in% c(participan$nombre, comparados$nombre)),
         var = "provincia", type = "typo", leg_pos = NA, 
         add = TRUE, pal = "black")
  mf_scale(pos = "bottomleft")
  mf_inset_off()
  return(invisible())
}
