#==================================================
# POBLACIÓ ANY A ANY MUNICIPIS DE CATALUNYA - 2022
#==================================================

#==================================================
library(dplyr)
library(stringr)
library(tidyr)
library(httr)
#==================================================

url <- 'https://www.idescat.cat/pub/?id=pmh&n=1180&by=sec&t=2022&f=zip&fi=ssv'

GET(url, write_disk(tf <- tempfile(fileext = ".zip")))

pobAny <- read.csv(unz(tf,"pmh1180sec2022.csv"),
                sep = ";",
                colClasses = c("character","character","character",
                "character","character","character",
                "character","integer"))
pobAnyany <- pobAny %>% 
    filter(sexe== "total",
           edat!= "total") %>% 
    select(3,4,8) %>% 
    mutate(ine= str_sub(seccions.censals, 1,5),
           edat= as.integer(str_extract(edat, "^[0-9]+"))) %>% 
    group_by(ine,edat) %>% 
    summarise(n= sum(valor)) %>% 
    filter(grepl("[0-9]+", ine))


writexl::write_xlsx(pobAnyany, "BasesdeDatos/Demografia/Padron_Anyo-Anyo/pobAnyany22.xlsx")
