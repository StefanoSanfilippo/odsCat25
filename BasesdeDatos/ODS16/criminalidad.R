library(dplyr)
library(stringr)
library(tidyr)
library(readxl)


# Portal criminalidad: https://estadisticasdecriminalidad.ses.mir.es/publico/portalestadistico/balances.html

criminalidad <- read_xlsx("BasesdeDatos/ODS16/1409012.xlsx",
                          skip = 6)

criminalidad <- criminalidad %>% 
    filter(grepl("^08|^17|^25|^43", ...1)) %>% 
    mutate(ine= str_sub(...1,1,5),
           Municipio= str_sub(...1,7,100)) %>% 
    select(3,4,2)


writexl::write_xlsx(criminalidad, "BasesdeDatos/ODS16/criminalidad.xlsx")
