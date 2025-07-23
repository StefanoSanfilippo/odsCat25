# =========================================
# DADES AUTOCONSUM FOTOVOLTAIC A CATALUNYA
# =========================================

# =========================================
# Font de les dades: Dades Obertes Catalunya
# https://analisi.transparenciacatalunya.cat/Energia/Instal-lacions-d-autoconsum-el-ctric/2b4s-skfm/about_data
# =========================================

library(dplyr)
library(stringr)

inst <- read.csv("BasesdeDatos/ODS7/Instal_lacions_d_autoconsum_el_ctric_20250708.csv",
                 colClasses = c("character","character","character",
                                "character","character","character",
                                "numeric","character","character",
                                "character","character","character"))

autocFotoV <- inst %>% 
    mutate(Codi.IDESCAT.municipi= str_sub(Codi.IDESCAT.municipi,1,5)) %>% 
    mutate(Tecnologia= tolower(Tecnologia), # Depuració de noms
           Tecnologia= stringi::stri_trans_general(Tecnologia, "Latin-ASCII")) %>% 
    filter(Tecnologia== "fotovoltaica") %>% 
    select(9,10,1,7) %>%
    group_by(Codi.IDESCAT.municipi) %>% 
    summarise(n.Inst= n(), potencia= sum(Potència, na.rm = TRUE)) %>% 
    arrange(Codi.IDESCAT.municipi) %>% 
    filter(Codi.IDESCAT.municipi != "") %>% 
    rename(ine= Codi.IDESCAT.municipi)

writexl::write_xlsx(autocFotoV, "BasesdeDatos/ODS7/autoconsumFoto.xlsx")


