

pacman::p_load(
    raster,        # raster data
    terra,         # raster data
    exactextractr, # Extracció de raster data agregant-les per pol. sf
    sf,
    tmap,
    mapview,
    leafsync,
    tidyverse,
    paletteer,
    ggthemes,
    classInt)

#terraOptions(progress=0) # Per a no veure la barra de progrés

#=======================================
# EXTRACCIÓN DE DATOS
#=======================================

#===========================
# 1. MUNICIPIOS SF
#===========================


# Mapa municipis catalunya
mun_cat <- st_read(
    "./Mapas/divisions-administratives/divisions-administratives-v2r1-municipis-1000000-20240118.shp")
mun_cat <- sf::st_as_sf(mun_cat)
mun_cat <- st_transform(mun_cat, crs = 4326)



#=================================================================
# EXTRACCIÓ DE LA DADES D'INUNDABILITAT PER PROVÍNCIA DE CATALUNYA
#=================================================================

#https://www.miteco.gob.es/es/cartografia-y-sig/ide/descargas/agua/zi-lamina.html

# Adreça de descàrrega: 
# https://www.mapama.gob.es/app/descargas/descargafichero.aspx?f=laminasPB-q500.zip

#============================================
# L'arxiu shp no és a github.com: pesa massa
#============================================


inunEsp <- st_read("./Mapas//laminaspb-q500/Q500_2Ciclo_PB_20240912_ETRS89.shp")
inunEsp <- sf::st_as_sf(inunEsp)
inunEsp <- st_transform(inunEsp, crs = 4326)

# Filtrado de los polígonos que coinciden con los de la prov. de Barcelona
cod.prov <- c('08','17','25','43')

sf_use_s2(FALSE)
list_inun <-lapply(cod.prov, function(x) {
    
    a <- st_intersects(inunEsp, mun_cat[mun_cat$CODIPROV== x, ])
    b <- inunEsp[lengths(a) > 0,]
})
sf_use_s2(TRUE)

list_inun_simpl <- lapply(list_inun, function(x) {
    rmapshaper::ms_simplify(x,keep = 0.1,keep_shapes = TRUE)
})


#----------------------------------------
# LECTURA DADES DESCARREGADES DEL SIOSE 
#----------------------------------------

# Adreça de descàrrega: 
# https://centrodedescargas.cnig.es/CentroDescargas/catalogo.do?Serie=SIOSE
# Seleccionar: Cataluña - GeoPackage - 2014

#=============================================
# L'arxiu gpkg no és en github.com: pesa massa
#=============================================


cob_cat <- st_read(
    "./Mapas/SIOSE_Catalunya_2014_H31_GPKG/SIOSE_Catalunya_2014_H31.gpkg"
)

# FILTRADO DE LAS ÁREAS RESIDENCIALES (VER MANUAL)
zona_residencial <- cob_cat %>% filter(CODIIGE %in% c(111:113))

# AJUSTE PROYECCION
zona_residencial <- st_transform(zona_residencial, crs = 4326)

#-----------------------------------------------
# Subset poligons segons províncies de Catalunya
#-----------------------------------------------

sf_use_s2(FALSE)
list_resid <-lapply(cod.prov, function(x) {
    
    a <- st_intersects(zona_residencial, mun_cat[mun_cat$CODIPROV== x, ])
    b <- zona_residencial[lengths(a) > 0,]
})
sf_use_s2(TRUE)



residencial_list <- lapply(list_resid, function(x) {
    x %>% 
        mutate(Cobertura= case_match(CODIIGE,
                                     111~ "Casc urbà",
                                     112~ "Eixample",
                                     113~ "Discontinu"),
               Cobertura= factor(Cobertura,
                                 levels= c(
                                     "Casc urbà",
                                     "Eixample",
                                     "Discontinu")
               ))
})


#===================================================================
# CÀLCUL EXTENSIÓ/MUNICIPI DE LES ZONES RESIDENCIALS
#===================================================================
residencial_cat <- bind_rows(residencial_list) # Llista => dataframe


#--------------------------------------------------------------------
# Interseccio del df de les zones residencials amb la el df dels polígons dels municipis

sf_use_s2(FALSE)
resid_mun <- st_intersection(mun_cat,
                             st_geometry(residencial_cat)) %>% 
    st_as_sf() %>% 
    mutate(
        area= units::set_units(st_area(.), "ha")) %>% 
    st_drop_geometry() %>% 
    group_by(CODIMUNI, NOMMUNI) %>% 
    summarise(area_resid = sum(area))
sf_use_s2(TRUE)

#===================================================================
# CÀLCUL EXTENSIÓ/MUNICIPI DE LES ZONES RESIDENCIALS INUNDABLES
#===================================================================

#--------------------------------------------------------------------
# Interseccio del df de les zones residencials amb la llista de zones inundables => zones residencials inundables
sf_use_s2(FALSE)
list_zones_res_inun <- lapply(list_inun_simpl, function(x) {
    st_intersection(st_geometry(x), st_geometry(residencial_cat)) %>% 
        st_as_sf()
})
sf_use_s2(TRUE)

zones_res_inun <- bind_rows(list_zones_res_inun) # Llista => dataframe

#-------------------------------------------------------------
# Interseccio zones residencials inundables amb municipis =>
# => zones residencials inundables per municipi
sf_use_s2(FALSE)
mun_inund <- st_intersection((mun_cat), 
                             st_geometry(zones_res_inun)) %>% 
    st_as_sf() %>% 
    mutate(
        area= units::set_units(st_area(.), "ha")) %>% 
    st_drop_geometry()
sf_use_s2(TRUE)
#-------------------------------------------------------------

# S'agreguen els valors de zones inundables per municipi
area_inund <- mun_inund %>% 
    #mutate(CODIMUNI= str_sub(CODIMUNI,1,5)) %>% 
    group_by(CODIMUNI, NOMMUNI) %>%
    summarise(area_inund= sum(area))

mun_cat_inund <- mun_cat %>% 
    left_join(resid_mun[,c(1,3)], by= "CODIMUNI") %>%
    left_join(area_inund[,c(1,3)], by= "CODIMUNI") %>% 
    mutate(area_inund= ifelse(area_inund > area_resid,area_resid,
                              area_inund),
           pct_inun= round(area_inund / area_resid*100,2),
           pct_inun= ifelse(is.na(pct_inun),0,pct_inun))

mun_cat_inund

writexl::write_xlsx(mun_cat_inund, "BasesdeDatos/ODS11/inundacions.xlsx")



