#=======================================
# DADES RECOLLIDA SELECTIVA CATALUNYA
#=======================================

#=======================================
# FONT:Recollida selectiva per habitant
#=======================================

# https://analisi.transparenciacatalunya.cat/Medi-Ambient/Estad-stiques-de-residus-municipals/69zu-w48s/about_data

library(dplyr)
library(stringr)
library(httr)
library(XML)
library(ODataQuery)

# URL DESCARREGA DADES FORMAT ODATA
url <- 'https://analisi.transparenciacatalunya.cat/api/odata/v4/69zu-w48s'

res <- retrieve_all(url) # Obtenir data mitjançant ODataQuery

residus23 <- res %>% filter(any== "2023")

writexl::write_xlsx(residus23,"BasesdeDatos/ODS12/residus23.xlsx")
