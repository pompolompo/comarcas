library(dplyr)
library(mapsf)
source("scripts/import.R")
source("funcs/codigo_nombre.R")
### tablas resumen ###

# municipios con con almenos 5 dependientes de mayor flujo entrante
sumas = flux %>%
  group_by(DESTINO) %>%
  summarise(
    n = n(),
    across('COMERCIO':'OTROS SERV', sum),
  ) %>% mutate(TOTAL = rowSums(.[,-1])) %>%
  arrange(desc(TOTAL)) %>%
  select(DESTINO, n, TOTAL, everything()) %>%
  filter(n >= 5) %>%
  codigo_nombre(tbl = .)
