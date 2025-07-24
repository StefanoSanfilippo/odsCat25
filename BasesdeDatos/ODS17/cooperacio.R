#=================================
# ANÁLISI COOP. AL DESENVOLUPAMENT
# https://www.fonscatala.org/ca/observatori/fonts-de-dades
# https://www.antropotic.cat/Observatori/InversioCooperacionsCatalanes.zip
#=================================

library(dplyr)
library(stringr)
library(tidyr)


a <- read.csv("BasesdeDatos/ODS17/RawData.csv",
               header = TRUE,
               sep = "\t")

# INVERSIÓ EN COOPERACIÓ AL DESENVOLUPAMENT L'ANY 2018

coop2018 <- a %>% 
    filter(Any== 2018 & Institucio %in% c("FCCD", "Ens Locals", "AjBarcelona")) %>% 
    filter(!grepl("^Diputaci|^Consell|^Consorci|Metropolitana", InstitucioII)) %>% 
    mutate(Inversió= ifelse(Inversió == "", "0", Inversió),
           Inversió= gsub(",", ".", Inversió),
           Inversió= as.numeric(Inversió)) %>% 
    group_by(InstitucioII) %>% 
    summarise(sum(Inversió))

writexl::write_xlsx(coop2018, "BasesdeDatos/ODS17/cooperacion2018.xlsx")
    


