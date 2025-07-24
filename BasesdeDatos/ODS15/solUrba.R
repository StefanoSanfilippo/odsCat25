#=========================================
# DADES MAPA URBANÍSTIC CATALUNYA
# https://analisi.transparenciacatalunya.cat/Urbanisme-infraestructures/Dades-del-mapa-urban-stic-de-Catalunya/epsm-zskb/about_data
#=========================================
#=========================================
# Veure definició de SNU: Sòl No Urbanitzable:
# https://territori.gencat.cat/ca/06_territori_i_urbanisme/sol_no_urbanitzable_i_paisatge/sol_no_urbanitzable/
#=========================================

library(dplyr)
library(stringr)
library(httr)
library(XML)
library(ODataQuery)

# URL DESCARREGA DADES FORMAT ODATA
url <- 'https://analisi.transparenciacatalunya.cat/api/odata/v4/epsm-zskb'

urb <- retrieve_all(url) # Obtenir data mitjançant ODataQuery


# DADES SOL NO URBANITZABLE
snu <- urb %>% 
    filter(any== 2024) %>% 
    select(2,3,5,11,24,25) %>% 
    rename(ine= codi_ine_5_txt,
           Municipi= nommun) %>% 
    select(2,3,1,4,5,6) %>% 
    filter(ine!= "17104") # Medinyà: ja no existeix



writexl::write_xlsx(snu,"BasesdeDatos/ODS15/snu.xlsx")
