library(ggplot2)
source("scripts/import.R")
source("scripts/prepara_flujos.R")
### AGRUPACIÓN ###

## descriptiva ##

# tipología de los desplazamientos totales ---> descr_1
flux[,-c(1:2)] %>% colSums() %>%
  data.frame(Tipo = names(.),
             Flujo = .) %>%
  ggplot(aes(x = Tipo, y = Flujo, fill = Tipo)) +
  geom_col() +
  theme_bw() + theme(
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.title = element_blank()
  ) + ggtitle("Tipología de los desplazamientos totales")

# km de desplazamiento según tipología --> descr_11
mutate(flux00, d = ifelse(SE_TOCAN, 1, DISTANCIA)) %>%
  mutate(across('COMERCIO':'OTROS SERV', 
                ~.x*d*.001), .keep = "unused") %>%
  .[,-c(1,2,10,11)] %>% colSums() %>%
  data.frame(Tipo = names(.),
             Flujo = .) %>%
  ggplot(aes(x = Tipo, y = Flujo, fill = Tipo)) +
  geom_col() +
  theme_bw() + theme(
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.title =  NULL
  ) + ggtitle("Tipología de los km totales")

# 7 destinos con mayor flujo entrante --- descr_2
aux0 = mutate(shp, top7 = nombre %in% sumas$DESTINO[1:7])
aux1 = filter(aux0, top7) %>%
  .[order(.[["nombre"]]), ] %>%
  mutate(flujo = sumas[order(sumas$DESTINO[1:7]), "TOTAL"] %>% pull()) %>%
  .[order(.[["flujo"]], decreasing = TRUE),] %>%
  mutate(pinta = paste0(1:7, ". ", nombre))

mf_export(x = aux0, filename = "grafs/descr_2.png", width = 1000)
mf_theme("candy")

mf_map(aux0, var = "top7", type ="typo", leg_pos = "topright", leg_frame = TRUE)
mf_label(x = aux1, var = "pinta",
         cex = 0.75, halo = TRUE, r = 0.15)
mf_layout(title = "Municipios con mayor flujo entrante", 
          credits = paste0(
            "Fuente: Universidad de Sevilla\n",
            "mapsf ",
            packageVersion("mapsf")))
dev.off()

# flujos normalizados según #mun dep
ggplot(normal, aes(x = ENTRANTE, fill = cut(n, breaks = c(-Inf, 3, 10, 25, Inf)))) +
  geom_histogram(binwidth = .25) +
  labs(title = "Histograma de flujos normalizados según cantidad de municipios dependientes") +
  theme_bw() + theme(legend.title = element_blank())
ggsave(filename = "grafs/normalflux1.png")


# proporción de flujos en los 7 municipios más importantes --> descr_3
data.frame(
  con_capital = rep(c("Con Málaga", "Sin Málaga"), each = 7),
  prop = c(cumsum(sumas$TOTAL[1:7])/sum(sumas$TOTAL),
           cumsum((sumas$TOTAL[2:8])/sum(sumas$TOTAL[-1]))),
  n = as.factor(rep(1:7, 2))
) %>% mutate(rest = 1-prop, over75 = ifelse(prop > .75, ">75%", "<75%")) %>%
  ggplot(aes(x = n, y = prop, fill = over75)) +
  geom_col() +
  facet_wrap(vars(con_capital)) +
  ylab(NULL) + theme_bw() + theme(legend.position = "none")
ggsave("grafs/descr_3.png")

### SELECCIÓN CABECERAS ###

# localización 10 municipios más importantes (filtrando) # selec_1

m = arrange(normal, desc(ENTRANTE)) %>%
  filter(n > 3, n < 30) %>% 
  select(DESTINO, ENTRANTE) %>% head(10)

shp$cabecera = ifelse(shp$nombre %in% m$DESTINO, 
                      "Cabecaera", "Adherible")
s0 = mutate(shp, top10 = nombre %in% m$DESTINO)
s1 = filter(s0, top10) %>%
  .[order(.[["nombre"]]), ] %>%
  mutate(flujo = m[order(m$DESTINO[1:10]), "ENTRANTE"] %>% pull()) %>%
  .[order(.[["flujo"]], decreasing = TRUE),] %>%
  mutate(pinta = paste0(1:10, ". ", nombre))

mf_export(x = s0, filename = "grafs/selec_1.png", width = 1000)

mf_map(shp, var = "cabecera", type ="typo", 
       leg_pos = "topright", leg_frame = TRUE, leg_title = "Municipio")
mf_label(x = s1, var = "pinta",
         cex = 0.75, halo = TRUE, r = 0.15)
mf_layout(title = "Municipios con mayor flujo entrante normalizado", 
          credits = paste0(
            "Fuente: Universidad de Sevilla\n",
            "mapsf ",
            packageVersion("mapsf")))
dev.off()

# gráfica con los municipios dependientes de los 7 anteriores
dev.new()
n = 7
par(mfrow = c(4, 2))
mf_theme("candy")

for(m in sumas$DESTINO[1:n]){
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

### cabecaeras seleccionadas y 
## cantidad de cabeceras dependientes para cada municipio de adhesión

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

s = left_join(shp, dep, by = join_by(nombre == ORIGEN)) %>%
  mutate(DESTINOS = ifelse(
           is.na(CABECERA), 0,
           ifelse(
             is.na(DESTINO_2), 1, 2
           )
         ))

# color cabecera 1
mf_map(arrange(s, CABECERA), var = "CABECERA", type = "typo", 
       leg_pos = "topright", leg_frame = TRUE)
# cabeceras
mf_map(filter(s, nombre %in% nb), lwd = 3,
       var = "nombre", type = "typo", add = TRUE, leg_pos = NA)
mf_map(filter(s, nombre == "Málaga"), lwd = 3,
       var = "nombre", type = "typo", add = TRUE, leg_pos = NA, pal = "grey22")
# municipios entre 2 cabeceras
mf_map(filter(s, DESTINOS == 2), var = "DESTINOS", type = "prop",
       leg_pos = NA, inches = .1)

mf_layout(title = "Agregación comarcal 1",
          credits = paste0(
            "Fuente: Universidad de Sevilla\n",
            "mapsf ",
            packageVersion("mapsf")
          ))


### links ### (s es el mismo mapa que el de arriba)

nombre = "Alfarnate"
k = codigo_nombre(nombre, nb_a_id = T, num= T)

x = shp
y = filter(flux0, ORIGEN == k)
l = mf_get_links(x = shp[,-1], df = filter(flux0, ORIGEN == k))

bks = mf_get_breaks(l$FLUJO[,1], nbreaks = 3, "jenks")

# color cabecera 1
mf_map(arrange(s, CABECERA), var = "CABECERA", type = "typo", 
       leg_pos = "topright", leg_frame = TRUE)
# cabeceras
mf_map(filter(s, nombre %in% nb), lwd = 3,
       var = "nombre", type = "typo", add = TRUE, leg_pos = NA)
mf_map(filter(s, nombre == "Málaga"), lwd = 3,
       var = "nombre", type = "typo", add = TRUE, leg_pos = NA, pal = "grey22")
# links
mf_map(l, var = "FLUJO", type = "grad", breaks = bks, lwd = (1:(length(bks)-1))*2, 
       col = "red4", add = TRUE, leg_pos = "interactive", leg_frame = TRUE)

mf_layout(title = paste("Flujos procedentes de", nombre),
          credits = paste0(
            "Fuente: Universidad de Sevilla\n",
            "mapsf ",
            packageVersion("mapsf")
          ))

## agrupación final

x = filter(dep, !is.na(DESTINO_2) | is.na(CABECERA)) %>%
  filter(!(ORIGEN %in% c("Ronda", "Antequera", "Marbella", "Vélez-Málaga", "Álora", "Málaga"))) %>%
  mutate(CABECERA = c(
    "Antequera",    # Alfarnate
    "Antequera",    # Alfarnatejo
    "Antequera",    # Almargen
    "Álora",        # Ardales
    "Antequera",    # Cañete la Real
    "Álora",        # Carratraca
    "Marbella",     # Jubrique
    "Antequera",    # Riogordo
    "Antequera",    # Teba
    "Ronda",        # Montecorto
    "Antequera",    # Villanueva de la Concepción
    "Vélez-Málaga", # Macharaviaya
    "Ronda",        # Cartajima
    "Marbella",     # Benalmádena
    "Vélez-Málaga", # Torrox
    "Antequera",    # Villanueva del Trabuco
    "Ronda",        # Serrato
    "Vélez-Málaga", # Totalán
    "Álora",        # Alhaurín de la Torre
    "Álora",        # Coín
    "Marbella",     # Genalguacil
    "Ronda",        # Júzcar
    "Ronda",        # Parauta
    "Marbella"      # Torremolinos
  )) %>% select(-DESTINO_2) %>%
  right_join(., dep, by = "ORIGEN") %>% 
  select(-DESTINO_2) %>%
  mutate(
    ORIGEN = ORIGEN,
    CABECERA = c(CABECERA.x[1:24], CABECERA.y[25:97], ORIGEN[98:103]),
    .keep = "unused"
  )

x$CABECERA[x$ORIGEN == "Almáchar"] = "Vélez-Málaga"
sxp = left_join(shp, x, join_by(nombre == ORIGEN))

mf_map(sxp, var = "CABECERA", type = "typo", 
       leg_pos = "topright", leg_frame = TRUE)
mf_map(filter(sxp, nombre %in% c("Ronda", "Antequera", "Marbella", 
                                 "Vélez-Málaga", "Álora", "Málaga")),
       var = "CABECERA", type = "typo", lwd = 3, add = TRUE, leg_pos = NA)
mf_label(filter(sxp, nombre %in% c("Ronda", "Antequera", "Marbella", 
                                   "Vélez-Málaga", "Álora", "Málaga")), 
         var = "nombre", cex = 1, halo = TRUE, r = 0.15)
mf_layout(title = "Agrupación comarcal definitiva",
          credits = paste0(
            "Fuente: Universidad de Sevilla\n",
            "mapsf ",
            packageVersion("mapsf")
          ))
