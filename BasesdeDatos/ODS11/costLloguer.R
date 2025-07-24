#====================================================================
# DADES DEL COST DEL LLOGUER ALS MUNICIPIS - Departament de Territori
#====================================================================

library(dplyr)
library(stringr)
#library(httr)
library(ODataQuery)

# URL DESCARREGA DADES FORMAT ODATA
url <- 'https://analisi.transparenciacatalunya.cat/api/odata/v4/qww9-bvhh'





llog <- retrieve_all(url) # Obtenir data mitjançant ODataQuery

llog2022 <- llog %>% 
    filter(any==2022 & !is.na(renda)) %>% 
    group_by(codi_territorial) %>% 
    summarise(n_habitatges= sum(habitatges), 
              rendaHabM= weighted.mean(renda, habitatges)) %>% 
    rename(ine= codi_territorial)


llogtot <- llog %>% 
  filter(any!=2025 & !is.na(renda)) %>% 
  group_by(codi_territorial, any) %>% 
  summarise(n_habitatges= sum(habitatges), 
            rendaHabM= weighted.mean(renda, habitatges)) %>% 
  rename(ine= codi_territorial)

writexl::write_xlsx(llog2022, "BasesdeDatos/ODS11/costlloguer.xlsx")

writexl::write_xlsx(llogtot, "BasesdeDatos/ODS11/costlloguer_07-24.xlsx")
