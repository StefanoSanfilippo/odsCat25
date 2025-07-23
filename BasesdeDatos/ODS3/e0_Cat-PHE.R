
#=================================
# DADES MORTALDAT CATALUNYA - 2023
#=================================

library(dplyr)
library(readxl)
library(stringr)
library(PHEindicatormethods)
# source("BasesdeDatos/ODS3/exp_vida.R")
#-----------------------------------------------------------

# Dades carregades en la secció de microdatos de l'INE:
# https://www.ine.es/dyngs/INEbase/operacion.htm?c=Estadistica_C&cid=1254736177008&menu=resultados&idp=1254735573002#

load(
    "BasesdeDatos/ODS3/datos_2023/R/MNPdefun_2023.RData")

#----------------------------------------------------------

mort <- Microdatos %>% 
    filter(CPRORE %in% c("08","17", "25", "43") & CMUNRE != "   ") %>% 
    mutate(ine = paste0(CPRORE,CMUNRE)) %>% 
    rename(Edat = ANIOSC) %>% 
    select(39,5,20)

countMort <- mort %>% 
    group_by(ine, Edat) %>% 
    count()





#===============================================================
# DADES DE POBLACIÓ ANY EN ANY ALS MUNICIPIS DE CATALUNYA - 2023
#===============================================================

popCat23 <- read_xlsx("BasesdeDatos/Demografia/Padron_Anyo-Anyo/PobAnyany23.xlsx")

names(popCat23) <- c("ine", "Edat", "Pop")

#======================================================
# TABLAS DE MORTALDAT ALS MUNICIPIS DE CATALUNYA - 2023
#======================================================


tab <- popCat23 %>% 
    left_join(countMort, by= c("ine", "Edat")) %>% 
    mutate(n= ifelse(is.na(n), 0, n)) %>% 
    group_by(ine) %>% 
    mutate(ntot= sum(n)) %>% 
    filter(ntot != 0)




tabMort <- tab %>% mutate(startage = case_match(Edat,
                                                  0~ "0",
                                                  c(1:4)~ "1 - 4",
                                                  c(5:9)~ "5 - 9",
                                                  c(10:14)~ "10 - 14",
                                                  c(15:19)~ "15 - 19",
                                                  c(20:24)~ "20 - 24",
                                                  c(25:29)~ "25 - 29",
                                                  c(30:34)~ "30 - 34",
                                                  c(35:39)~ "35 - 39",
                                                  c(40:44)~ "40 - 44",
                                                  c(45:49)~ "45 - 49",
                                                  c(50:54)~ "50 - 54",
                                                  c(55:59)~ "55 - 59",
                                                  c(60:64)~ "60 - 64",
                                                  c(65:69)~ "65 - 69",
                                                  c(70:74)~ "70 - 74",
                                                  c(75:79)~ "75 - 79",
                                                  c(80:84)~ "80 - 84",
                                                  c(85:89)~ "85 - 89",
                                                  .default = "90 +")) %>% 
    group_by(ine,startage) %>%
    summarise(deaths= sum(n), pops= sum(Pop))


# ===============================================================
# ESPERANÇA DE VIDA PER EDATS - MUNICIPIS DE CATALUNYA - ANY 2023
# ===============================================================

pheLe <- phe_life_expectancy(tabMort, deaths, pops, startage,
                        age_contents = c("0", "1 - 4", "5 - 9",
                                         "10 - 14", "15 - 19",
                                         "20 - 24", "25 - 29",
                                         "30 - 34", "35 - 39",
                                         "40 - 44", "45 - 49",
                                         "50 - 54", "55 - 59",
                                         "60 - 64", "65 - 69",
                                         "70 - 74", "75 - 79",
                                         "80 - 84", "85 - 89",
                                         "90 +"))



# ===============================================================
# ESPERANÇA DE VIDA AL NÉIXER - MUNICIPIS DE CATALUNYA - ANY 2023
# ===============================================================

pheLE_0 <- pheLe %>% 
    filter(startage == "0")


writexl::write_xlsx(pheLE_0, "BasesdeDatos/ODS3/e0_Cat.xlsx")





