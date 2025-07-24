#==================================
# PARTICIPACIÓ ELECCIONS MUNICIPALS
# https://analisi.transparenciacatalunya.cat/Sector-P-blic/Eleccions-municipals-Participaci-/ques-gd6z/about_data
#==================================


library(dplyr)
library(stringr)
library(httr)


#=================================================
# WEB DE INFO ELECTORAL DEL MINISTERIO DE INTERIOR
# https://infoelectoral.interior.gob.es/es/elecciones-celebradas/area-de-descargas/
#=================================================

# URL de les eleccions municipals de 2023
urlmun23 <- 'https://infoelectoral.interior.gob.es/estaticos/docxl/04_202305_1.zip'

zip <- 'https://infoelectoral.interior.gob.es/estaticos/docxl/04_202305_1.zip'
filename <- '04_202305_1.xlsx'


archive::archive_extract(archive = zip, 
                         dir = tempdir(), 
                         files = filename)
dat <- readxl::read_xlsx(file.path(tempdir(), filename))

names(dat) <- dat[3,]
names(dat) <- make.names(names(dat))

dat <- dat[-c(1:3),]

datpart <- dat %>% 
    select(2:13) %>% 
    mutate(Código.de.Provincia = str_pad(Código.de.Provincia, 
                                         width = 2,
                                         side = "left",
                                         pad = "0"),
           Código.de.Municipio = str_pad(Código.de.Municipio,
                                         width = 3,
                                         side = "left",
                                         pad = "0"),
           ine= paste0(Código.de.Provincia,Código.de.Municipio)) %>% 
    select(13,5:12)


participacio <- datpart %>% 
    mutate(votants_percent = 
               round(as.numeric(Total.votantes) / 
                                    as.numeric(Total.censo.electoral) * 100, 2),
           any = '2023') %>% 
    rename(Municipio= Nombre.de.Municipio) %>% 
    select(1,2,10,11) %>% 
    filter(grepl("^08|^17|^25|^43", ine))

writexl::write_xlsx(participacio, "BasesdeDatos/ODS16/participacio.xlsx")

