#================================================
# ZONES VERDES PER HABITANT - MUNICIPIS CATALUNYA
# https://analisi.transparenciacatalunya.cat/Urbanisme-infraestructures/Dades-del-mapa-urban-stic-de-Catalunya/epsm-zskb/about_data
#================================================

# Superfície (en m2) de zona verda per habitant en sòl urbà consolidat

#=====================================================================

library(dplyr)
library(stringr)
library(ODataQuery)

# URL DESCARREGA DADES FORMAT ODATA
url <- 'https://analisi.transparenciacatalunya.cat/api/odata/v4/epsm-zskb'

zv <- retrieve_all(url) # Obtenir data mitjançant ODataQuery
zvHab <- zv %>% 
    select(1,2,3,5,11,69) %>% # Se selecciona la columna amb la dada de superf. verda/hab.
    filter(any=='2024')

writexl::write_xlsx(zvHab,"BasesdeDatos/ODS11/zonesverdes.xlsx")



