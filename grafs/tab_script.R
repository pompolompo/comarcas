source("scripts/import.R")
source("scripts/prepara_flujos.R")
### AGRUPACIÓN ###

## descriptiva ##

# tipología de desplazamientos, en número y según distancia
desplaza = mutate(flux00, d = ifelse(SE_TOCAN, 1, DISTANCIA)) %>%
  mutate(across('COMERCIO':'OTROS SERV', 
                ~.x*d*.001), .keep = "unused") %>%
  .[,-c(1,2,10,11)] %>% colSums() %>%
  data.frame(
    km = round(.),
    p = ./sum(.)
  ) %>% arrange(desc(p)) %>%
    mutate(
    'Proporción ' = round(p*100, 2) %>%
      paste("%"),
    km = round(km),
    .keep = "none"
  )  %>%  select('Proporción ', 'km') %>%
  bind_cols(., 
            flux[,-c(1:2)] %>% colSums() %>%
              data.frame(p = ./sum(.),
                         d = round(.)) %>%
              arrange(desc(p)) %>%
              mutate(' Proporción' = round(p*100, 2) %>%
                       paste("%"), 
                     'Desplazamientos' = round(d),
                     .keep = "none") %>% select(' Proporción', 'Desplazamientos')) %>%
  kable(booktabs = TRUE, format = "latex", align = "c",
        caption = "Personas desplazadas y km según motivo") %>%
  kable_styling(latex_options = c("striped", "hold_position"),
                full_width = FALSE) %>%
  add_header_above(c("", "Kilómetros" = 2, "Desplazamientos" = 2)) %>%
  footnote(general = "Desplazamientos y kilómetros redondeados a números enteros",
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
  kable(booktabs = TRUE, format = "latex", align = "c",
        caption = "Municipios con mayores flujos entrantes") %>% 
  kable_styling(latex_options = c("striped", "hold_position"),
                full_width = FALSE) %>%
  footnote(general_title = "La tipología de los flujos es, de izquierda a derecha:",
           general = "Total; Comercio; Educación; Sanidad; Administración; Ocio y Cultura; Transportes; Otros ",
           footnote_as_chunk = FALSE, title_format = "underline") %>%
  footnote(general_title = "Unidad de medida",
           general = "Flujos en miles de pesonas",
           footnote_as_chunk = TRUE, title_format = "underline")
  
## ponderación ##

row.names(w) = names(flux)[-c(1,2)]
colnames(w) = "Peso"
pondera = kable(w, booktabs = TRUE, format = "latex", align = "c",
                caption = "Ponderación para la agregación de tipologías de desplazamiento") %>%
  kable_styling(latex_options = c("striped", "hold_position"))

## cabecera ##

norma0 = arrange(normal, desc(ENTRANTE)) %>%
  head(10) %>% select(n ,everything()) %>%
  kable(booktabs = TRUE, format = "latex", align = "c",
        caption = "Flujos entrantes normalizados",
        full_width = FALSE) %>%
  kable_styling(latex_options = c("striped", "hold_position")) %>%
  footnote(general_title = "Nota:",
           general = "Se muestran municipios con los 10 mayores flujos entrantes",
           footnote_as_chunk = TRUE, title_format = "underline")
  
norma1 = arrange(normal, desc(ENTRANTE)) %>%
  filter(n > 3, n < 30) %>%
  head(10) %>% select(n, everything()) %>%
  kable(booktabs = TRUE, format = "latex", align = "c",
        caption = "Flujos entrantes normalizados") %>%
  kable_styling(latex_options = c("striped", "hold_position"),
                full_width = FALSE) %>%
  footnote(general_title = "Nota:",
           general = "Se muestran municipios con los 10 mayores flujos entrantes",
           footnote_as_chunk = TRUE, title_format = "underline")

### adhesión ###

# relaciones dependencia
dep0 = filter(flux0, DESTINO %in% cd) %>% 
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
    .keep = "unused"
  ) %>% bind_rows(
    data.frame(
      ORIGEN = shp$nombre[!(shp$nombre %in% .[["ORIGEN"]])],
      CABECERA = NA,
      DESTINO_2 = NA
    )
  )

# tabla
depende2 = filter(dep, !is.na(DESTINO_2)) %>% 
  arrange(CABECERA, DESTINO_2) %>%
  rename('DESTINO 1' = CABECERA, 
         'DESTINO 2' = DESTINO_2) %>%
  kable(booktabs = TRUE, format = "latex", align = "c",
        caption = "Municipios que dependen de dos cabeceras") %>%
  kable_styling(latex_options = c("striped", "hold_position"),
                full_width = FALSE)