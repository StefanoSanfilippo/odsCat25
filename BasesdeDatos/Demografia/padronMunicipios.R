# =====================
# PADRÓN MUNICIPAL
# =====================
# =====================


library(dplyr)
library(stringr)

# ===========================================================================
# Obtener base de datos de padrón municipal del INE
# Datos: https://www.ine.es/dynt3/inebase/index.htm?padre=10607&capsel=10607
# https://www.ine.es/jaxiT3/Tabla.htm?t=68065&L=0
# ===========================================================================



padron_21_24 <- readxl::read_xlsx("BasesdeDatos/Demografia/68065.xlsx",
                                 skip = 7) %>% 
    mutate(ine= str_sub(...1, 1, 5)) %>% 
    filter(grepl("^08|^17|^25|^43", ine)) |> 
    mutate(...1= str_sub(...1, 7,100)) %>% 
    select(14,1:5) %>% 
    #rename(Municipio= ...1) |> 
    mutate_at(.vars = c(3:6), as.integer) %>% 
    arrange(ine)

names(padron_21_24) <- c("ine", "Municipio", "2024", "2023", "2022", "2021")

writexl::write_xlsx(padron_21_24,"BasesdeDatos/Demografia/padronMunicipios.xlsx")
