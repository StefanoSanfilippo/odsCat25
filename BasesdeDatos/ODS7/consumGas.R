# ============================================
# DADES CONSUM NATURAL CANALITZAT A CATALUNYA
# ============================================

# =========================================
# Font de les dades: Dades Obertes Catalunya
# https://analisi.transparenciacatalunya.cat/Energia/Consum-de-gas-natural-canalitzat-per-municipis-i-s/qvqg-zag8/about_data
# =========================================

library(dplyr)
library(stringr)

gas <- read.csv(
    "BasesdeDatos/ODS7/Consum_de_gas_natural_canalitzat_per_municipis_i_sectors_de_Catalunya_20250707.csv",
                 colClasses = c("numeric","character","character",
                                "character","character","character",
                                "numeric","character"))

gasDom <- gas %>% 
    filter(SECTOR== "DOMÈSTIC") %>% 
    select(4,5,1,2,3,6,7)

writexl::write_xlsx(gasDom, "BasesdeDatos/ODS7/consumGas.xlsx")


