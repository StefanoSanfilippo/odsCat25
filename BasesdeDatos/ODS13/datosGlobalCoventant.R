library(dplyr)
library(stringr)
library(tidyr)
library(httr)


# Bases de datos de proyectos y firmantes de la UE
# https://data.jrc.ec.europa.eu/dataset/b425918f-53a1-495c-8619-cd370c302eb0

url <- 'https://jeodpp.jrc.ec.europa.eu/ftp/public/JRC-OpenData/GCOM-MyCovenant/2023_Fourth_release/df1_Signatories_4th%20Release%20-%20March%202023.xlsx'

GET(url, write_disk(tf <- tempfile(fileext = ".xlsx")))

dfGC <- readxl::read_xlsx(tf, sheet = 3) %>% 
    filter(coordinator_name %in% c("Province of Barcelona", 
                                   "Province of Girona", 
                                   "Diputació de Lleida",
                                   "Diputació de Tarragona")) %>%  
    select(1,2,3,5,7,8)

writexl::write_xlsx(dfGC, "BasesdeDatos/ODS13/datosGlobalCoventant.xlsx")
