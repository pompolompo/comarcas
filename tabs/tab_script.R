source("scripts/import.R")
source("scripts/descriptiva.R") # -> sumas
### METODOLOGÍA ###

# 
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
  
