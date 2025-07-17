#===================================

# DADES MUNICIPALS POBRESA 2015-2021
# https://www.ine.es/dynt3/inebase/index.htm?padre=7132

#===================================
library(readxl)
library(dplyr)
library(stringr)
library(tidyr)

PobAnyB <- read_xlsx("BasesdeDatos/ODS1/30901_Pob15-21_BCN.xlsx",
                     skip = 8,
                     col_types = c(
                         "text",
                         "numeric","numeric","numeric","numeric","numeric","numeric","numeric",
                         "numeric","numeric","numeric","numeric","numeric","numeric","numeric",
                         "numeric","numeric","numeric","numeric","numeric","numeric","numeric"))

PobAnyG <- read_xlsx("BasesdeDatos/ODS1/31021_Pob15-21_GI.xlsx",
          skip = 8,
          col_types = c(
              "text",
              "numeric","numeric","numeric","numeric","numeric","numeric","numeric",
              "numeric","numeric","numeric","numeric","numeric","numeric","numeric",
              "numeric","numeric","numeric","numeric","numeric","numeric","numeric"))


PobAnyL <- read_xlsx("BasesdeDatos/ODS1/31084_Pob-15-21_LL.xlsx",
                     skip = 8,
                     col_types = c(
                         "text",
                         "numeric","numeric","numeric","numeric","numeric","numeric","numeric",
                         "numeric","numeric","numeric","numeric","numeric","numeric","numeric",
                         "numeric","numeric","numeric","numeric","numeric","numeric","numeric"))


PobAnyT <- read_xlsx("BasesdeDatos/ODS1/31228_Pob15-21_TR.xlsx",
                     skip = 8,
                     col_types = c(
                         "text",
                         "numeric","numeric","numeric","numeric","numeric","numeric","numeric",
                         "numeric","numeric","numeric","numeric","numeric","numeric","numeric",
                         "numeric","numeric","numeric","numeric","numeric","numeric","numeric"))


dfPob <- rbind(PobAnyB,PobAnyG,PobAnyL,PobAnyT) %>%
    filter(grepl("^08|^17|^25|^43", ...1))


pobAnys <- dfPob %>% select(1:8) %>% 
    `colnames<-`(c("Municipi", "2021", "2020", "2019", "2018","2017","2016","2015")) %>% 
    mutate(ine= str_sub(Municipi, 1,5),
           Municipi= str_sub(Municipi,7,100)) %>% 
    select(9,1,8:2) %>% 
    inner_join(info_mun[,c(2,3)], by= "ine") %>% 
    filter(!is.na(`2015`)) %>% 
    select(1,2,10,3:9) %>% 
    pivot_longer(4:10) %>% 
    mutate(name= lubridate::year(as.Date(name, format= "%Y")))

writexl::write_xlsx(pobAnys,"BasesdeDatos/ODS1/Pob15-21.xlsx")


