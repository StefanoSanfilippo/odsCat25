
#=================================
# DADES MORTALDAT CATALUNYA - 2022
#=================================

library(dplyr)

#-----------------------------------------------------------

# Dades carregades en la secció de microdatos de l'INE:
# https://www.ine.es/dyngs/INEbase/operacion.htm?c=Estadistica_C&cid=1254736177008&menu=resultados&idp=1254735573002#

load(
    "BasesdeDatos/ODS3/datos_2023/R/MNPdefun_2023.RData")

#----------------------------------------------------------

mort <- Microdatos %>% 
    filter(CPRORE %in% c("08","17", "25", "43") & !is.na(CMUNRE)) %>% 
    mutate(ine = paste0(CPRORE,CMUNRE)) %>% 
    rename(Edad = ANIOSC) %>% 
    select(39,5,20)

writexl::write_xlsx(mort, "BasesdeDatos/ODS3/mort.xlsx")
