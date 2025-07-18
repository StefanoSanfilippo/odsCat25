#==================================================
# POBLACIÓ ANY A ANY MUNICIPIS DE CATALUNYA - 2021
#==================================================

#==================================================
library(dplyr)
library(stringr)
library(tidyr)
library(httr)
#==================================================

url <- 'https://www.idescat.cat/pub/?id=pmh&n=1180&by=sec&t=2021&f=zip&fi=ssv'

GET(url, write_disk(tf <- tempfile(fileext = ".zip")))

pobAny <- read.csv(unz(tf,"pmh1180sec2021.csv"),
                sep = ";")

pobAnyany <- pobAny %>% 
    filter(sexe== "total",
           edat!= "total") %>% 
    select(2,3,7) %>% 
    mutate(ine= str_sub(seccions.censals, 1,5),
           edat= as.integer(str_extract(edat, "^[0-9]+"))) %>% 
    group_by(ine,edat) %>% 
    summarise(n= sum(valor))


writexl::write_xlsx(pobAnyany, "BasesdeDatos/Demografia/Padron_Anyo-Anyo/pobAnyany21.xlsx")
