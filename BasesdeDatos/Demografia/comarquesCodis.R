#===================================
# CODIFICACIÓ COMARQUES DE CATALUNYA
#===================================

library(dplyr)
library(stringr)
library(tidyr)

com <- read.csv("BasesdeDatos/Demografia/comarques.csv",
         sep = ";",
         skip = 3,
         colClasses = c("character","character","character","character"))
 
com <- com %>% select(1:3) %>% 
    pivot_wider(names_from = Nivell, values_from = Codi) %>% 
    fill(Comarca) %>% 
    rename(codCom= Comarca) %>% 
    mutate(Comarca= ifelse(is.na(Municipi),Nom, NA)) %>% 
    fill(Comarca) %>% 
    filter(!is.na(Municipi)) %>% 
    rename(Municipi=Nom,
           ine= Municipi) %>% 
    mutate(ine=str_sub(ine, 1,5)) %>% 
    select(3,1,2,5)

writexl::write_xlsx(com,"BasesdeDatos/Demografia/comarquesCodis.xlsx")
