source("scripts/import.R")
source("scripts/prepara_flujos.R")
source("scripts/descriptiva.R") # -> sumas
### AGRUPACIÓN ###

## descriptiva ##

# tipología de desplazamientos
desplaza = flux[,-c(1:2)] %>% colSums() %>%
  data.frame(p = ./sum(.),
             d = round(.)) %>%
  arrange(desc(p)) %>%
  mutate('Proporción' = round(p*100, 2) %>%
           paste("%"), 
         'Desplazamientos' = round(d),
         .keep = "none") %>%
  kable(booktabs = TRUE, format = "latex",
        caption = "Personas desplazadas según motivo") %>%
  kable_styling(latex_options = c("striped", "hold_position")) %>%
  footnote(general = "Desplazamientos redondeados a números enteros",
           general_title = "Nota:", footnote_as_chunk = TRUE)

# tipología de desplazamientos, según distancia
desplaza_km = mutate(flux00, d = ifelse(SE_TOCAN, 1, DISTANCIA)) %>%
  mutate(across('COMERCIO':'OTROS SERV', 
                ~.x*d*.001), .keep = "unused") %>%
  .[,-c(1,2,10,11)] %>% colSums() %>%
  data.frame(
    km = round(.),
    p = ./sum(.)
  ) %>% arrange(desc(p)) %>%
    mutate(
    'Proporción' = round(p*100, 2) %>%
      paste("%"),
    km = round(km),
    .keep = "none"
  )  %>%
  kable(booktabs = TRUE, format = "latex",
        caption = "Personas desplazadas según motivo") %>%
  kable_styling(latex_options = c("striped", "hold_position")) %>%
  footnote(general = "Kilómetros redondeados a números enteros",
           general_title = "Nota:", footnote_as_chunk = TRUE)

# top7 destinos
top7 = sumas %>% rename(
  Destino = 'DESTINO',
  tot = 'TOTAL',
  com = 'COMERCIO',
  educa = 'ENSEÑANZA',
  salud = 'SANIDAD',
  admin = 'ADMINISTRACIÓN',
  ocio = 'OCIO Y CULTURA',
  trans = 'TRANSPORTES',
  otro = 'OTROS SERV'
) %>% head(7) %>%
  mutate(across('tot':'otro',
                function(x) round(x/1000, 2))) %>% 
  kable(booktabs = TRUE, format = "latex", 
        caption = "Municipios con mayores flujos entrantes") %>% 
  kable_styling(latex_options = c("striped", "hold_position")) %>%
  footnote(general_title = "La tipología de los flujos es, de izquierda a derecha:",
           general = "Total; Comercio; Educación; Sanidad; Administración; Ocio y Cultura; Transportes; Otros ",
           footnote_as_chunk = FALSE, title_format = "underline") %>%
  footnote(general_title = "Unidad de medida",
           general = "Flujos en miles de pesonas",
           footnote_as_chunk = TRUE, title_format = "underline")
  
## ponderación ##

row.names(w) = names(flux)[-c(1,2)]
colnames(w) = "Peso"
pondera = kable(w, booktabs = TRUE, format = "latex",
                caption = "Ponderación para la agregación de tipologías de desplazamiento") %>%
  kable_styling(latex_options = c("striped", "hold_position"))
  