#=========================================
# POBLACIÓ ESTRANGERA RESIDENT A CATALUNYA
#=========================================

library(dplyr)

estrangers <- readxl::read_xlsx("BasesdeDatos/Demografia/b6r228ct0a0y2023lesd0.xlsx",
                 skip = 4) %>%
    select(1,2,3) %>% 
    rename(Municipi=Categoria,
           Poblacio= `(1) Población`,
           estrangers=`Población extranjera. Total`) %>% 
    slice(1:906)


writexl::write_xlsx(estrangers,"BasesdeDatos/Demografia/estrangers.xlsx")
