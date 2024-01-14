# dada una tabla con códigos municipales con nombre "ORIGEN" y "DESTINO"
# cámbialos por el nombre de los numinicipios
# di nb_a_id = TRUE realiza el procedimiento inverso

codigo_nombre = function(tbl, # lo que queremos cambiar
                         pares = NULL, # las parejas id/nombre
                         nb_a_id = FALSE, # en qué dirección vamos
                         num = FALSE # formato del output
                         ){
  # si no se especifican los pares, impórtalos
  if(is.null(pares)) pares = read.csv("datos/pares.csv")
  
  # queremos trabajar con una tabla cuyos id sean string
  vec = FALSE
  if(is.vector(tbl)) vec = TRUE
  if(vec) tbl = data.frame(DESTINO = as.character(tbl))
  pares$id = as.character(pares$id)
  
  # cambiamos el destino, el origen o ambos?
  destino = ifelse("DESTINO" %in% names(tbl), TRUE, FALSE)
  origen = ifelse("ORIGEN" %in% names(tbl), TRUE, FALSE)
  
  # pasamos de nombre a id o de id a nombre?
  if(nb_a_id){
    if(destino){
      tbl[["DESTINO"]] = as.character(tbl[["DESTINO"]])
      tbl = left_join(tbl, pares, by = join_by(DESTINO == nb)) %>%
        select(-DESTINO) %>%
        rename(DESTINO = id) %>%
        select(DESTINO, everything())
    }
    if(origen){
      tbl[["ORIGEN"]] = as.character(tbl[["ORIGEN"]])
      tbl = left_join(tbl, pares, by = join_by(ORIGEN == nb)) %>%
        select(-ORIGEN) %>%
        rename(ORIGEN = id)
    }
  }else{
    if(destino){
      tbl[["DESTINO"]] = as.character(tbl[["DESTINO"]])
      tbl = left_join(tbl, pares, by = join_by(DESTINO == id)) %>%
        select(-DESTINO) %>%
        rename(DESTINO = nb) %>%
        select(DESTINO, everything())
    }
    if(origen){
      tbl[["ORIGEN"]] = as.character(tbl[["ORIGEN"]])
      tbl = left_join(tbl, pares, by = join_by(ORIGEN == id)) %>%
        select(-ORIGEN) %>%
        rename(ORIGEN = nb)
    }
  }
  
  # convierte a numérico
  if(num){
    if(destino) tbl[["DESTINO"]] = as.numeric(tbl[["DESTINO"]])
    if(origen) tbl[["ORIGEN"]] = as.numeric(tbl[["ORIGEN"]])
    }
  

  # devuelve un objeto de la clase que entra
  if(vec){
    return(pull(tbl))
  }else{
    return(tbl)
  } 
}
