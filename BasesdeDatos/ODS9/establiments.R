#=========================
# ESTABLIMENTS INDUSTRIALS
#=========================

library(dplyr)
library(stringr)

establiments <- readxl::read_xlsx("BasesdeDatos/ODS9/4721.xlsx",
                  skip = 8,
                  col_names = FALSE,
                  col_types = c("text","numeric","numeric")) %>% 
    filter(grepl("^08|^17|^25|^43",...1)) %>% 
    mutate(ine= str_sub(...1, 1,5),
           Municipio= str_sub(...1, 7,100)) %>% 
    select(4,5,2,3) %>% 
    rename(totEmpr= ...2,
           emprInd= ...3)
    
writexl::write_xlsx(establiments, "BasesdeDatos/ODS9/establiments.xlsx")
