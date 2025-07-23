# GENERAR UN FICHERO DE DATOS DEL GASTO SOCIAL DEL GRUP 32 EN ESPAÑA - 2022

library(dplyr)
library(stringr)

# Lectura de la tabla "tb_economica_cons.txt" extraída de la bbbdd Acces Liquidaciones 2022
tb_funcional_cons <-read.delim("./BasesdeDatos/ODS1/Liquidaciones2022/tb_funcional_cons.txt",
    sep = ";", dec = ",", header = FALSE)
tb_inventario <-read.delim("./BasesdeDatos/ODS1/Liquidaciones2022/tb_inventario.txt",
    sep = ";", dec = ",", header = FALSE)


# Esta tabla contiene los importes liquidados del presupuesto por id de entidad, I/G y codigo cuenta.

# Explicacion de las columnas de tb_economica_cons:

#V1= Id: Identificador único del Ente dependiente.
#V2=Cdcta: Código de la cuenta económica.
#V3=Cdfgr: Código de la cuenta funcional o por programas.
#V4=Importe: Importe del dato económico.



tb_funcional_cons <- tb_funcional_cons |> 
    mutate(V3=trimws(V3),
           V2=trimws(V2)) |> # Se eliminan espacios en blanco de la columna V3 (tipo de gasto)
    filter(V3== 32) |>  # Se filtra la partida 32: Educación
    rename_with( ~ c('Id','Cdcta','Cdfgr','Importe'),
                all_of(c('V1','V2','V3','V4')))


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

desp32 <- tb_funcional_cons |> 
    inner_join(tb_inventario, by= c('Id'='Idente')) |> 
    mutate(anyo= '2022')

writexl::write_xlsx(desp32, 'BasesdeDatos/ODS4/desp32-2022.xlsx')


