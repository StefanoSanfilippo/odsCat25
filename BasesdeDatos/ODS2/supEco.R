

library(dplyr)
library(stringr)
library(httr)



# Datos extraidos del sitio web de datos abiertos de CCPAE
# https://www.ccpae.org/index.php?option=com_content&task=view&id=400&Itemid=232&lang=ca_ES

# 2023
urlccpae <- 'https://www.ccpae.org/docs/estadistiques/2023/03_2023_superficie_total-comarcal-municipal.xlsx'

GET(urlccpae, write_disk(tf1 <- tempfile(fileext = ".xlsx")))
dfEco <- readxl::read_xlsx(tf1)
supEco <- dfEco |> 
    filter(!CULTIU %in% c("C. PASTURES PERMANENTS", "BOSC")) %>% # no es tenen en compte
    select(3,6) %>% 
    rename(Municipio= MUNICIPI) %>% 
    mutate(Municipio= tolower(Municipio),
           Municipio= stringi::stri_trans_general(Municipio, "Latin-ASCII"),
           Municipio= sub("^l'(.*)", "\\1 l'", Municipio),
           Municipio= gsub("'", "", Municipio),
           Municipio= gsub("`", "", Municipio),
           Municipio= gsub("´","", Municipio),
           Municipio= gsub(",", "", Municipio),
           Municipio= gsub("\\(", "", Municipio),
           Municipio= gsub(")", "", Municipio),
           Municipio= trimws(Municipio, "both")) %>% 
    group_by(Municipio) %>% 
    summarise(supEco= sum(`SUP (Ha)`))

writexl::write_xlsx(supEco, "BasesdeDatos/ODS2/supEco23.xlsx")


    
