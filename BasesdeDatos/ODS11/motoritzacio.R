#=================================================================
# DADES DE MOTORITZACIÓ - DIRECCION GENERAL DE TRÁFICO - DGT EN CIFRAS 
# https://www.dgt.es/menusecundario/dgt-en-cifras/dgt-en-cifras-resultados/dgt-en-cifras-detalle/Datos-municipales-informacion-general-2024/
#=================================================================

library(dplyr)


# INDEX DE MOTORITZACIÓ: TURISMES + MOTOS / 1000 HABITANTS

motoritzacio2024 <- readxl::read_xlsx(
    "BasesdeDatos/ODS11/DatosMunicipalesGeneral_2024.xlsx") %>% 
    rename(ine= `Código INE`) %>% 
    select(1,11,12,13) %>% 
    rowwise() %>% 
    mutate(PTotal = sum(c_across(2:4))) 
    
    

writexl::write_xlsx(motoritzacio2024,"BasesdeDatos/ODS11/motoritzacio.xlsx")



