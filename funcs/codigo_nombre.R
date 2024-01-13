# dada una tabla con códigos municipales con nombre "ORIGEN" y "DESTINO"
# cámbialos por el nombre de los numinicipios
# di nb_a_id = TRUE realiza el procedimiento inverso

codigo_nombre = function(pares = read.csv("datos/pares.csv"), tbl, nb_a_id = FALSE){
  pares$id = as.character(pares$id)
  
  vec = FALSE
  if(is.vector(tbl)) vec = TRUE
    
  if(vec) tbl = data.frame(DESTINO = as.character(tbl))
  
  if(nb_a_id){
    if(c("DESTINO" %in% names(tbl))){
      tbl[["DESTINO"]] = as.character(tbl[["DESTINO"]])
      tbl = left_join(tbl, pares, by = join_by(DESTINO == nb)) %>%
        select(-DESTINO) %>%
        rename(DESTINO = id) %>%
        select(DESTINO, everything())
    }
    if("ORIGEN" %in% names(tbl)){
      tbl[["ORIGEN"]] = as.character(tbl[["ORIGEN"]])
      tbl = left_join(tbl, pares, by = join_by(ORIGEN == nb)) %>%
        select(-ORIGEN) %>%
        rename(ORIGEN = id)
    }
  }else{
    if("DESTINO" %in% names(tbl)){
      tbl[["DESTINO"]] = as.character(tbl[["DESTINO"]])
      tbl = left_join(tbl, pares, by = join_by(DESTINO == id)) %>%
        select(-DESTINO) %>%
        rename(DESTINO = nb) %>%
        select(DESTINO, everything())
    }
    if("ORIGEN" %in% names(tbl)){
      tbl[["ORIGEN"]] = as.character(tbl[["ORIGEN"]])
      tbl = left_join(tbl, pares, by = join_by(ORIGEN == id)) %>%
        select(-ORIGEN) %>%
        rename(ORIGEN = nb)
    }
  }
  
  if(vec) return(pull(tbl))
  return(tbl)
}
