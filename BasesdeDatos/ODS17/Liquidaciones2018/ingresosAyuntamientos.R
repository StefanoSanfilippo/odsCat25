library(dplyr)
library(stringr)
library(tidyr)

# Lectura de la tabla "tb_economica_cons.txt" extraída de la bbbdd Acces Liquidaciones 2018
tb_economica_cons <-read.delim("BasesdeDatos/ODS17/Liquidaciones2018/tb_economica_cons.txt", sep = ";", dec = ",", header = FALSE)
tb_inventario <-read.delim("BasesdeDatos/ODS17/Liquidaciones2018/tb_inventario.txt", sep = ";", dec = ",", header = FALSE)


# Esta tabla contiene los importes liquidados del presupuesto por id de entidad, I/G y codigo cuenta.

# Explicacion de las columnas de tb_economica_cons:

#V1= Id: Identificador único del Ente dependiente.
#V2= tipReig: Tipo de cuenta (ingresos o gastos)
#V3= Cdcta: Código de la cuenta económica
#V4= Imported: Previsión o créditos definitivos del ejercicio corriente.
#V5= Importer: Derechos u obligaciones reconocidas del ejercicio corriente.
#V6= Importel: Recaudación o pagos líquidos realizados del ejercicio corriente.
#V7= Importec: Recaudación o pagos líquidos de ejercicios cerrados.




tb_economica_cons <- tb_economica_cons |> 
    mutate(V3=trimws(V3)) |> # Se eliminan espacios en blanco de la columna V3 (tipo de gasto)
    filter(V2== "I") |>  # Se filtran las partidas de Ingresos
    rename_with( ~ c('Id','tipReig','Cdcta','Imported','Importer','Importel','Importec'),
                 all_of(c('V1','V2','V3','V4','V5','V6','V7')))


# Explicacion de las columnas de tb_inventarios:

# V1= Id: identificativo ente
# V2= Codbdgel: codigo desprictivo entidad => Ayuntamiento = AA
# V3= Nombreppal: nombre
# V4= Idente
# V5= Codente
# V6= Mombrente
# V7= Nsec
# V8= Poblacion
# V9= Estado



tb_inventario <- tb_inventario |> 
    rename_with(~ c('Id','Codbdgel','Nombreppal','Idente','Codente','Mombrente','Nsec','Poblacion','Estado'),
                all_of(c('V1','V2','V3','V4','V5','V6','V7','V8','V9'))) |> 
    mutate(TipoAdm= str_sub(Codbdgel, 6,7)) |>  # Extrae los codigo que classifican las administraciones Ayuntamiento=AA
    mutate(ine = str_sub(Codbdgel, 1,5))

ingresos2018Todos <- tb_economica_cons |> inner_join(tb_inventario, by= c('Id'='Idente'))

ingresos2018Cat <- ingresos2018Todos %>% 
    filter(grepl("^08|^17|^25|^43", ine))

ingrPropios2018 <- ingresos2018Cat[, c(1,2,3,5,12,16,17)] %>% 
    #inner_join(municipios[,c(1,2)], by= "ine") |> 
    filter(TipoAdm== "AA") %>% 
    filter(Cdcta %in% c("1","2","3","5","300","345","38")) |> # Subcuentas 300,345 y 38, según criterios Fons Català Cooperació
    group_by(ine,Cdcta) %>% 
    summarise(ingrCta_18= sum(Importer)) %>% 
    pivot_wider(names_from = Cdcta, values_from = ingrCta_18, values_fill = 0) %>% 
    mutate(ingPropios18= `1`+`2`+`3`+`5`-`300`-`345`-`38`) %>% 
    select(1,9,2:8) %>% 
    arrange(ine)



writexl::write_xlsx(ingrPropios2018, 'BasesdeDatos/ODS17/Liquidaciones2018/ingresosPropio2018.xlsx')



