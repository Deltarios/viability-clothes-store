########################  PAQUETES REQUERIDOS  ########################

library(openxlsx)
library(ggplot2)
library(sf)
library("stringr") 
library(dplyr)
suppressMessages(suppressWarnings(library(dplyr)))

########################  CARGAMOS DATOS REQUERIDOS  ########################

setwd("") # Establecemos directorio de trabajo
# DATOS: Porcentaje de Obesidad por Municipio y Estado 2018-2019
obesidad.Excel <- "obesidad ENSANUT2018.xlsx" 
obesidad <- read.xlsx(obesidad.Excel, sheet = 1, skipEmptyRows = FALSE)
# DATOS: PIB de las actividades económicas por entidad federativa/ Actividades terciarias/ Comercio al por menor 2019
PIB.Excel <- "PIBE_27 DATA.xlsx" 
PIB <- read.xlsx(PIB.Excel, sheet = 1, skipEmptyRows = FALSE)
# DATOS: Índice de Complejidad Económica (ECI) 2020
ECI.Excel <- "ECI.xlsx" 
ECI <- read.xlsx(ECI.Excel, sheet = 1, skipEmptyRows = FALSE)

########################  LIMPIAMOS LOS DATOS REQUERIDOS  ######################## 

obesidad <- rename(obesidad, UUID.Municipio = Identificador.único.del.municipio, Clave.Estado = Clave.de.entidad.federativa)
obesidad <- rename(obesidad, Clave.Municipio = Clave.de.municipio.o.delegación, Nombre.Municipio = Municipio.o.delegación)
obesidad <- rename(obesidad, Porcentaje.obesidad = Porcentaje.de.población.de.20.años.y.más.con.obesidad.)
obesidad <- mutate(obesidad, Porcentaje.obesidad = round(as.numeric(Porcentaje.obesidad), 4)) # Transformamos a entero y redondeamos a 4 posiciones
obesidad <- select(obesidad, UUID.Municipio:Porcentaje.obesidad) #Seleccionamos las columnas requeridas
obesidad <- filter(obesidad, Estimador == "Valor") #Filtramos solo las filas con valor
# Datos por estado
obesidad.estado <- filter(obesidad, Nombre.Municipio == "Total") #Filtramos solo las filas con valores correspondientes a los Total por estado
obesidad.estado <- filter(obesidad.estado, UUID.Municipio != "00000") #Quitamos el Total correspondiente al País
# Datos por Municipio
obesidad <- filter(obesidad, Nombre.Municipio != "Total") #Quitamos las filas de Total
# Datos PIB
PIB <- PIB %>% slice(44:75) #Seleccionamos solo las filas del PIB de Participación Porcentual por estado
PIB <- rename(PIB, Entidad.federativa = "Instituto.Nacional.de.Estadística.y.Geografía.(INEGI).", "PIB.2019" = X18)
PIB <- select(PIB, Entidad.federativa, PIB.2019)  # seleccionamos solo las columnas de Entidad Federativa y PIB.2019
PIB <- mutate(PIB, PIB.2019 = round(as.numeric(PIB.2019), 4)) # Transformamos a entero y redondeamos a 4 posiciones
PIB$Entidad.federativa <- str_trim(PIB$Entidad.federativa) #Eliminamos caracteres blancos al principio para hacer el merge
obesidad.estado <- merge(obesidad.estado, PIB, by = "Entidad.federativa") # Unimos los DataFrames PIB y obesidad por Entidad Federativa
# Datos ECI
ECI <- rename(ECI, Entidad.federativa = "State")
ECI <- rename(ECI, ECI.valor = "Number.of.Employees.Midpoint.ECI")
ECI <- mutate(ECI, ECI.valor = round(as.numeric(ECI.valor), 4)) # Transformamos a entero y redondeamos a 4 posiciones
ECI <- select(ECI, Entidad.federativa, ECI.valor)  # seleccionamos solo las columnas de Entidad Federativa y PIB.2019
obesidad.estado <- merge(obesidad.estado, ECI, by = "Entidad.federativa") # Unimos el ECI al DataFrame de obesidad.estado

########################  GRAFICAMOS RESULTADOS  ######################## 

# GRÁFICA: TOP 15 Municipios con mayor obesidad en México 2018-2019
obesidad <- obesidad[with(obesidad, order(-obesidad$Porcentaje.obesidad)), ] # Orden inverso
ggplot(head(obesidad,15), aes(x=Porcentaje.obesidad , y=reorder(paste(Nombre.Municipio,Entidad.federativa,sep=','), Porcentaje.obesidad))) +
  geom_bar(stat='identity', aes(fill=Porcentaje.obesidad), position='dodge') +
  ggtitle('Municipios con Mayor Obesidad en México 2018-2019') +
  xlab('Proporción estimada de personas con obesidad respecto a la población de 20 o más años') +
  ylab('Municipio y Entidad Federativa') +
  geom_text(aes(label = Porcentaje.obesidad), size = 3, hjust = -0.2, vjust = 0.5, position = "stack") 

# GRÁFICA: TOP 15 Entidades Federativas con mayor obesidad en México 2018-2019
obesidad.estado <- obesidad.estado[with(obesidad.estado, order(-obesidad.estado$Porcentaje.obesidad)), ] # Orden inverso
ggplot(head(obesidad.estado,15), aes(x=Porcentaje.obesidad , y=reorder(paste(Entidad.federativa,sep=','), Porcentaje.obesidad))) +
  geom_bar(stat='identity', aes(fill=Porcentaje.obesidad), position='dodge') +
  ggtitle('Entidad Federativa con Mayor Indice de Obesidad en México 2018-2019') +
  xlab('Proporción estimada de personas con obesidad respecto a la población de 20 o más años') +
  ylab('Entidad Federativa') +
  geom_text(aes(label = Porcentaje.obesidad), size = 3, hjust = -0.2, vjust = 0.5, position = "stack") 

# GRÁFICA: TOP 15 Entidades Federativas con mayor PIB en México 2019
obesidad.estado <- obesidad.estado[with(obesidad.estado, order(-obesidad.estado$PIB.2019)), ] # Orden inverso
ggplot(head(obesidad.estado,15), aes(x=PIB.2019 , y=reorder(paste(Entidad.federativa,sep=','), PIB.2019))) +
  geom_bar(stat='identity', aes(fill=PIB.2019), position='dodge') +
  ggtitle('Entidad Federativa con Mayor PIB en México / Actividades terciarias/ Comercio al por menor 2019') +
  xlab('Producto Interno Bruto (PIB)') +
  ylab('Entidad Federativa') +
  geom_text(aes(label = PIB.2019), size = 3, hjust = -0.2, vjust = 0.5, position = "stack") 

# GRÁFICA: TOP 15 Índice de Complejidad Económica (ECI) 2020
obesidad.estado <- obesidad.estado[with(obesidad.estado, order(-obesidad.estado$ECI.valor)), ] # Orden inverso
ggplot(head(obesidad.estado,15), aes(x=ECI.valor , y=reorder(paste(Entidad.federativa,sep=','), ECI.valor))) +
  geom_bar(stat='identity', aes(fill=ECI.valor), position='dodge') +
  ggtitle('Entidad Federativa con Mayor ECI en México 2020') +
  xlab('Índice de Complejidad Económica (ECI)') +
  ylab('Entidad Federativa') +
  geom_text(aes(label = ECI.valor), size = 3, hjust = -0.2, vjust = 0.5, position = "stack") 

########################  ELECCIÓN DE ESTADO ######################## 
#Ordenamos importancia de parámetros
obesidad.estado <- obesidad.estado %>%
  arrange(Porcentaje.obesidad, PIB.2019, ECI.valor)

