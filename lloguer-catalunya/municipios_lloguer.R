
#=========================================
# DADES PER A APP SOBRE PREUS DE LLOGUERS ALS MUNICIPIS DE CATALUNYA
#=========================================
#install.packages("fst")
library(dplyr)
library(readxl)
library(stringr)
library(tidyr)
library(ggplot2)
#library(fst)
library(readr)


municipios <- readxl::read_xlsx(
    "BasesdeDatos/Demografia/padronMunicipios.xlsx") %>% 
    filter(ine != "17104")
comarques <-   readxl::read_xlsx(
    "BasesdeDatos/Demografia/comarquesCodis.xlsx")
cllog <- readxl::read_xlsx(
    "BasesdeDatos/ODS11/costlloguer_07-24.xlsx") %>% 
    mutate(rendaHabM= round(rendaHabM,0))

# Dades Renta familiar disponible bruta. Per habitant (€): Municipi / Any
#rdfb <- readxl::read_xlsx("BasesdeDatos/ODS1/rfdbc-13301-14148-mun.xlsx",
#                          sheet = 2,
#                          skip=1,
#                          col_names = TRUE,
#                          col_types = c("text",
#                                        "numeric","numeric","numeric",
#                                        "numeric","numeric","numeric",
#                                        "numeric","numeric","numeric",
#                                        "numeric","numeric","numeric")) %>% 
#    mutate(ine= stringr::str_sub(...1, 2,6),
#           Municipio= stringr::str_sub(...1,9,100)) %>% 
#    select(14,15,2:13)

# Renda bruta/habitant: any 2021
#rdfb21 <- rdfb %>% select(1,2,14) %>% 
#    rename(rdfb.hab.21= `2021`) %>% 
#    mutate(quintilRenda21= cut(rdfb.hab.21,
#                             breaks = qu <- quantile(rdfb.hab.21, probs = seq(0,1, by=0.2), 
#                                                     na.rm = T),
#                             labels = names(qu)[-1], include.lowest=TRUE))


municipios_ll <- municipios %>% 
    select(1,2,3) %>% 
    mutate(Provincia= case_match(stringr::str_sub(ine,1,2),
                                 "08"~ "Barcelona",
                                 "17"~ "Girona",
                                 "25"~ "Lleida",
                                 "43"~ "Tarragona",
                                 .default = as.character(ine))) %>% 
    inner_join(comarques[,c(1,4)], by= 'ine') %>% 
    inner_join(cllog, by= 'ine' ) %>% 
    rename(Població= `2024`) %>% 
    mutate(cat_pobl= ifelse(Població < 1000, "< 1.000",
                            ifelse(Població < 5000 & Població >= 1000, 
                                   "1.000-5.000",
                                   ifelse(Població < 10000 & Població >= 5000, 
                                          "5.000-10.000",
                                          ifelse(Població < 50000 & Població >= 10000, 
                                                 "10.000-50.000",
                                                 ifelse(Població < 100000 & Població >= 50000, 
                                                        "50.000-100.000",
                                                 "> 100.000"))))),
           cat_pobl= factor(cat_pobl,
                            levels= c("< 1.000","1.000-5.000","5.000-10.000",
                                      "10.000-50.000", "50.000-100.000","> 100.000"))) # %>% 
#    left_join(rdfb21[,c(1,3,4)], by= 'ine')

# Dades Històriques (201-2021) Renda Familiar Neta Mitjana per Llar

# https://www.ine.es/dynt3/inebase/index.htm?padre=12385&capsel=12384

a <- read_xlsx("BasesdeDatos/ODS11/30896.xlsx",
               skip = 7,
               col_names = TRUE,
               col_types = c("text",
                             "numeric", "numeric", "numeric",
                             "numeric", "numeric", "numeric",
                             "numeric","numeric")) %>% 
    filter(grepl("^[0-9]",...1))

b<- read_xlsx("BasesdeDatos/ODS11/31016.xlsx",
              skip = 7,
              col_names = TRUE,
              col_types = c("text",
                            "numeric", "numeric", "numeric",
                            "numeric", "numeric", "numeric",
                            "numeric","numeric")) %>% 
    filter(grepl("^[0-9]",...1))
c<- read_xlsx("BasesdeDatos/ODS11/31079.xlsx",
              skip = 7,
              col_names = TRUE,
              col_types = c("text",
                            "numeric", "numeric", "numeric",
                            "numeric", "numeric", "numeric",
                            "numeric","numeric")) %>% 
    filter(grepl("^[0-9]",...1))
d<- read_xlsx("BasesdeDatos/ODS11/31223.xlsx",
              skip = 7,
              col_names = TRUE,
              col_types = c("text",
                            "numeric", "numeric", "numeric",
                            "numeric", "numeric", "numeric",
                            "numeric","numeric")) %>% 
    filter(grepl("^[0-9]",...1))

rFam <- bind_rows(a,b,c,d) %>% 
    mutate(ine= str_sub(...1, 1,5),
           Municipi= str_sub(...1, 7,100)) %>% 
    select(10:11,2:9) %>% 
    pivot_longer(cols = 3:10, names_to = "any", values_to = "renda_famNeta") %>% 
    mutate(any= as.integer(any))

municipios_ll <- municipios_ll %>%
    left_join(rFam[, c(1,3,4)], by= c('ine','any')) %>% 
    mutate(pes_llog= round(rendaHabM*12/renda_famNeta*100,2))
    
    


write.csv(municipios_ll, "lloguer-catalunya/municipios_lloguer.csv",
          row.names = FALSE)

#write.fst(municipios_ll, "lloguer-catalunya/municipios_lloguer.fst")

#ggplot(municipios_ll[municipios_ll$any %in% c(2015:2021), ] ,
#       aes(x=any,y=pes_llog, group = Municipio, colour = Municipio)) +
#    geom_line()+
#    theme(legend.position = 'none')

write_rds(municipios_ll, "lloguer-catalunya/municipios_lloguer.rds")


