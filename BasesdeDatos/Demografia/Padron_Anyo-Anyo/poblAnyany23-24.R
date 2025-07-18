#======================================================
# POBLACIÓ ANY A ANY MUNICIPIS DE CATALUNYA - 2022-2024
#======================================================

#======================================================
library(dplyr)
library(stringr)
library(tidyr)
library(httr)
#======================================================

# url <- 'https://www.idescat.cat/pub/?id=pmh&n=1180&by=sec&t=2022&f=zip&fi=ssv'
url2 <- 'https://www.idescat.cat/pub/?id=censph&n=10&by=mun&f=zip&fi=ssv'
GET(url2, write_disk(tf <- tempfile(fileext = ".zip")))

pobAny <- read.csv(unz(tf,"censph10mun-1.csv"),
                sep = ";")
pobAnyany <- pobAny %>% 
    filter(sexe == "total",
           edat != "total",
           any == "2024" | any == "2023") %>%  
    select(1:3,7) %>% 
    mutate(edat= as.integer(str_extract(edat, "^[0-9]+"))) %>% 
    rename(Municipio= municipi)

mun_clean <- readxl::read_xlsx("BasesdeDatos/Demografia/padronMunicipios.xlsx") %>% 
    select(1,2) %>% 
    mutate(Municipio= tolower(Municipio),
           Municipio= stringi::stri_trans_general(Municipio, "Latin-ASCII"),
           Municipio= sub("^l'(.*)", "\\1 l'", Municipio),
           Municipio= gsub("'", "", Municipio),
           Municipio= gsub("`", "", Municipio),
           Municipio= gsub("´","", Municipio),
           Municipio= gsub(",", "", Municipio),
           Municipio= gsub("\\(", "", Municipio),
           Municipio= gsub(")", "", Municipio),
           Municipio= trimws(Municipio, "both"),
           Municipio= gsub("\\s+"," ", Municipio))

pobAnyany_clean <- pobAnyany %>%  
    mutate(Municipio= tolower(Municipio),
           Municipio= stringi::stri_trans_general(Municipio, "Latin-ASCII"),
           Municipio= sub("^l'(.*)", "\\1 l'", Municipio),
           Municipio= gsub("'", "", Municipio),
           Municipio= gsub("`", "", Municipio),
           Municipio= gsub("´","", Municipio),
           Municipio= gsub(",", "", Municipio),
           Municipio= gsub("\\(", "", Municipio),
           Municipio= gsub(")", "", Municipio),
           Municipio= trimws(Municipio, "both"),
           Municipio= gsub("\\s+"," ", Municipio),
           Municipio= case_match(Municipio,
                                 "bisbal de montsant la"~
                                     "bisbal de falset la",
                                 "masarac i vilarnadal"~ "masarac",
                                 .default = as.character(Municipio)))


join <- pobAnyany_clean %>% inner_join(mun_clean, by= "Municipio")

mun_anyoanyo_2324 <- join %>% 
    mutate(valor = as.numeric(valor)) %>% 
    select(1,5,3,4) %>% 
    rename(n = valor)

mun_anyoanyo_23 <- mun_anyoanyo_2324 %>% 
    filter(any== 2023) %>% 
    select(2:4) %>% 
    arrange(ine)

writexl::write_xlsx(mun_anyoanyo_23, "BasesdeDatos/Demografia/Padron_Anyo-Anyo/pobAnyany23.xlsx")


mun_anyoanyo_24 <- mun_anyoanyo_2324 %>% 
    filter(any== 2024) %>% 
    select(2:4) %>% 
    arrange(ine)

writexl::write_xlsx(mun_anyoanyo_24, "BasesdeDatos/Demografia/Padron_Anyo-Anyo/pobAnyany24.xlsx")

