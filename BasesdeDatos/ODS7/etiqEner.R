#=================================
# DADES ETIQUETES ENERGÈTIQUES
# https://analisi.transparenciacatalunya.cat/Energia/Certificats-d-efici-ncia-energ-tica-d-edificis/j6ii-t3w2/about_data
#=================================

#================================

library(dplyr)
library(stringr)
#================================

a <- read.csv("BasesdeDatos/ODS7/Certificats_d_efici_ncia_energ_tica_d_edificis_20250723.csv")

#--------------------------------------------------------
# Esta base de datos no cabe en github.com. Hay que descargarla directamente en:
# https://analisi.transparenciacatalunya.cat/Energia/Certificats-d-efici-ncia-energ-tica-d-edificis/j6ii-t3w2/about_data
#--------------------------------------------------------

certificats <- a %>% 
    mutate(CODI_POBLACIO= as.character(CODI_POBLACIO),
           CODI_POBLACIO= str_sub(CODI_POBLACIO, 1,5),
           CODI_POBLACIO= str_pad(CODI_POBLACIO, width= 5, side= 'left', pad= 0),
           CODI_POBLACIO= str_replace_all(CODI_POBLACIO,"^8", "08"),
           CODI_POBLACIO= str_sub(CODI_POBLACIO, 1,5),
           CODI_POBLACIO= case_when(CODI_POBLACIO== "08510"~ "08183",
                                    .default = CODI_POBLACIO)) %>% # Error Roda de Ter
    group_by(Qualificació.de.consum.d.energia.primaria.no.renovable, CODI_POBLACIO) %>% 
    count() %>% 
    rename(ine= CODI_POBLACIO,
           qualificacio= Qualificació.de.consum.d.energia.primaria.no.renovable,
           certificats= n) %>%
    select(2,1,3) %>% 
    ungroup() %>% 
    group_by(ine) %>% 
    mutate(tot.ine= sum(certificats)) %>% # el total dels certificats per municipi
    arrange(ine,qualificacio) %>% 
    tidyr::pivot_wider(names_from = qualificacio, values_from = certificats, values_fill = 0)

writexl::write_xlsx(certificats,"BasesdeDatos/ODS7/etiqEner.xlsx")  


