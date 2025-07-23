# ==============================
# Package PHEindicatormethods 
# Function phe_life_expectancy

# Funció per a calcular la Taxa de Esperança de Vida
# =============================

# =============================
# NOTA IMPORTANT
# The function is for an abridged life table using 5 year age intervals with a final 
# age interval of 90+.

# Life expectancy cannot be calculated if the person-years in any given age interval 
# is zero. It will also not be calculated if the total person-years is less than 5,000 
# as this is considered to be the minimum size for robust calculation of life expectancy.(5)
# Zero death counts are not a problem, except for the final age interval - there must be 
# at least one death in the 90+ interval for the calculations to be possible.

# The methodology used in this function, along with discussion of alternative options 
# for life expectancy calculation for small areas, were described Eayres and Williams.(6)


# (5) Toson B, Baker A. Life expectancy at birth: methodological options for small 
# populations. National Statistics Methodological Series No 33. HMSO 2003.

# (6) Eayres DP, Williams ES. Evaluation of methodologies for small area life expectancy
# estimation. J Epidemiol Community Health 2004;58:243-249

# =============================

# EXEMPLES

#install.packages("PHEindicatormethods")
library(PHEindicatormethods)
library(dplyr)
library(stringr)


df <- data.frame(startage = c(0L, 1L, 5L, 10L, 15L, 20L, 25L, 30L, 35L, 40L, 45L, 50L, 55L,
                              60L, 65L, 70L, 75L, 80L, 85L, 90L),
                 pops = c(7060L, 35059L, 46974L, 48489L, 43219L, 38561L, 46009L, 57208L,
                          61435L, 55601L, 50209L, 56416L, 46411L, 39820L, 37978L,
                          37039L, 33288L, 23306L, 11936L, 11936L),
                 deaths = c(17L, 9L, 4L, 8L, 20L, 15L, 24L, 33L, 50L, 71L, 100L, 163L,
                            263L, 304L, 536L, 872L, 1390L, 1605L, 1936L, 1937L))

phe_life_expectancy(df, deaths, pops, startage)

## o  con mútiples intervals de confiança
phe_life_expectancy(df, deaths, pops, startage, confidence = c(95, 99.8))

## Bandes d'edat desordenades
df1 <- data.frame(startage = c("0", "1-4", "5-9", "10 - 14", "15 - 19", "20 - 24", "25 - 29",
                              "30 - 34", "35 - 39", "40 - 44", "45 - 49", "50 - 54",
                              "55 - 59", "60 - 64", "65 - 69", "75 - 79", "80 - 84",
                              "85 - 89", "90 +", "70 - 74"),
                 pops = c(7060L, 35059L, 46974L, 48489L, 43219L, 38561L, 46009L, 57208L,
                          61435L, 55601L, 50209L, 56416L, 46411L, 39820L, 37039L,
                          23306L, 11936L, 11936L, 37978L, 33288L),
                 deaths = c(17L, 9L, 4L, 8L, 20L, 15L, 24L, 33L, 50L, 71L, 100L, 163L,
                            263L, 304L, 872L, 1605L, 1936L, 1937L, 536L, 1390L))
phe_life_expectancy(df1, deaths, pops, startage,
                    age_contents = c("0", "1-4", "5-9",
                                     "10 - 14", "15 - 19",
                                     "20 - 24", "25 - 29",
                                     "30 - 34", "35 - 39",
                                     "40 - 44", "45 - 49",
                                     "50 - 54", "55 - 59",
                                     "60 - 64", "65 - 69",
                                     "70 - 74", "75 - 79",
                                     "80 - 84", "85 - 89",
                                     "90 +"))


# per grups
df2 <- data.frame(area = c(rep("Area 1", 20), rep("Area 2", 20)),
                 startage = rep(c(0L, 1L, 5L, 10L, 15L, 20L, 25L, 30L, 35L, 40L, 45L, 50L, 55L,
                                  60L, 65L, 70L, 75L, 80L, 85L, 90L), 2),
                 pops = rep(c(7060L, 35059L, 46974L, 48489L, 43219L, 38561L, 46009L, 57208L,
                              61435L, 55601L, 50209L, 56416L, 46411L, 39820L, 37978L,
                              37039L, 33288L, 23306L, 11936L, 11936L), 2),
                 deaths = rep(c(17L, 9L, 4L, 8L, 20L, 15L, 24L, 33L, 50L, 71L, 100L, 163L,
                                263L, 304L, 536L, 872L, 1390L, 1605L, 1936L, 1937L), 2))


df2 %>%
    group_by(area) %>%
    phe_life_expectancy(deaths, pops, startage) %>% 
    slice(1) # Primera fila de l'Area 1 i de l'Area 2





# ==============================
# Exemple ajuntament de Barcelona
# ==============================

bcnedat21 <- readxl::read_xlsx("BasesdeDatos/ODS3/pmh-1180-8078-mun.xlsx",
                  sheet = 2,
                  skip = 1) %>% 
    filter(grepl("^[0-9]", ...1)) %>% 
    rename(Edat= ...1,
           Persones= Valor) %>% 
    mutate(Edat= as.integer(str_extract(Edat, "^[0-9]+")))

bcnmort21 <- read.csv("BasesdeDatos/ODS3/t269mun_080193202100c3.csv",
                    sep = ";",
                    skip = 8,
                    header = TRUE) %>% 
    filter(grepl("^[0-9]", X)) %>% 
    rename(Edat= X) %>% 
    mutate(Edat= as.integer(str_extract(Edat, "^[0-9]+"))) %>% 
    select(1,3) %>% 
    rename(Defuncions= X2021)

tabMort <- bcnmort21 %>% inner_join(bcnedat21, by= "Edat")

tabMort <- tabMort %>% mutate(catEdat= case_match(Edat,
                                       0~ "0",
                                       c(1:4)~ "01-04",
                                       c(5:9)~ "05-09",
                                       c(10:14)~ "10-14",
                                       c(15:19)~ "15-19",
                                       c(20:24)~ "20-24",
                                       c(25:29)~ "25-29",
                                       c(30:34)~ "30-34",
                                       c(35:39)~ "35-39",
                                       c(40:44)~ "40-44",
                                       c(45:49)~ "45-49",
                                       c(50:54)~ "50-54",
                                       c(55:59)~ "55-59",
                                       c(60:64)~ "60-64",
                                       c(65:69)~ "65-69",
                                       c(70:74)~ "70-74",
                                       c(75:79)~ "75-79",
                                       c(80:84)~ "80-84",
                                       c(85:89)~ "85-89",
                                       .default = "90 +")) %>% 
    group_by(catEdat) %>% 
    summarise(Defuncions= sum(Defuncions), Persones= sum(Persones))


phe_life_expectancy(data = tabMort, 
                    deaths = Defuncions, 
                    population = Persones, 
                    startage = catEdat,
                    age_contents = tabMort$catEdat,
                    le_age = "0") # Esperança de vida al néixer











