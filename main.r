#instalaremos paquetes necesarios para el analisis de nicho ecologico
install.packages("shiny")
install.packages("tmap")
install.packages("terra") 
install.packages("rgbif")
install.packages("TeachingDemos")
install.packages("dismo")
install.packages("raster")
install.packages("geodata")
#instalaremos paquetes necesarios para el analisis de la data
#manipulacion y limpieza de datos
install.packages("tidyverse")
# sirve para crear y manipular objetos espaciales de entidades simples como shapefile
install.packages("sf") 
# sirve para hacer mapas
install.packages("tmap")
#otras mas para analisar rasters
install.packages("rgdal")
install.packages("gtools")
install.packages("rgeos")
install.packages("base")

#otraspara correlacion
install.packages("corrplot")
install.packages("velox")
install.packages("usdm")
install.packages("Hmisc")
install.packages("rJava")

#Leer drive
install.packages("googledrive")

install.packages("dplyr")
install.packages("sf")

#Cargando cada libreria
library(rgbif)
library(TeachingDemos)
library(dismo)
library(raster)
library(tidyverse)
library(sf)
library(tmap)
library(geodata)
library(rgdal)
library(gtools)
library(rgeos)
library(corrplot)
library(velox)
library(usdm)
library(Hmisc)
library(googledrive)
library(rJava)
library(dplyr)


#descargando data de Gbif
cacao<- gbif("Theobroma", "cacao", geo= FALSE)   #Descargamos la data directamente del Gbif pues hemos cargado con anterioridad el paquete y la libreria respectivos
setwd("C:/Users/marlo/Desktop/PRACTICA ECO/2MIERCOLESRSTUDIO2") #Se establece una carpeta general de trabajo
write.csv(cacao ,file="caca_o.csv") #Se guarda los cambios obtenidos de la busqueda de un archivo csv

#Leer archivo csv
cacao<- read.csv("C:/Users/marlo/Desktop/PRACTICA ECO/2MIERCOLESRSTUDIO2/caca_o.csv", sep=",") #Lee el archivo ubicado en la ruta, con un parametro que es separdo por coma
class(cacao) #determina la clase del objeto
str(cacao) #determina la estructura de datos del objeto
head(cacao) #nos muestra una vista preliminar de los datos
dim(cacao) #nos muestra las dimensiones del conjunto de datos
names(cacao) #obtiene los nombres de las variables
sumary(cacao) #resume los datos 

#eliminar datos sin coordenadas
cacao<-subset(cacao,!is.na(lon) & !is.na(lat)) #se crea un nuevo objeto llamado "cacao" que es un subconjunto de datos de "cacao" original.
dim(cacao) #se utiliza la función "dim" para obtener las dimensiones del objeto "cacao" actualizadas, es decir, el número de filas y columnas.
cacao<-subset(cacao,!is.na(locality)) #A continuación, se realiza una operación similar para la columna "localidad". Se elimina cualquier fila que tenga un valor faltante en la columna "locality" utilizando la función "subset".
dim(cacao) #El resultado actualizado se almacena nuevamente en el objeto "cacao", y las dimensiones del objeto se imprimen utilizando la función "dim".
cacao<-subset(cacao,!is.na(year)) #Finalmente, se realiza una operación similar para la columna "año". Se eliminan las filas con valores faltantes en la columna "year" utilizando la función "subset
dim(cacao) #El resultado se guarda en el objeto "cacao" una vez más, y las dimensiones se imprimen utilizando la función "dim".
names(cacao) #Después de ejecutar estas operaciones de manipulación de datos, se imprime en la consola los nombres de las columnas presentes en el objeto "cacao" utilizando la función "names

#filtro de observaciones
unique(cacao$country) #La línea "unique(cacao$country)" devuelve los valores únicos en la columna "country" del conjunto de datos "cacao
cacao <-cacao %>%
  filter(country =="Peru")  #filtra el conjunto de datos "cacao" para incluir solo las filas donde el país sea "Perú".
dim(cacao) #devuelve las dimensiones del conjunto de datos "cacao".
names(cacao) #muestra los nombres de las columnas en el conjunto de datos "cacao" después de aplicar el filtro.

#SELECCION DE VARIABLES
cacao_final<-cacao %>% 
  select(species,lon,lat)  #El objetivo es generar un nuevo conjunto de datos llamado "cacao_final" que contenga únicamente las columnas de "species" (especie), "lon" (longitud) y "lat" (latitud). se utiliza la función `%>%` de RStudio, que permite encadenar operaciones de manera más legible
dim(cacao_final)  ##devuelve las dimensiones del conjunto de datos
head(cacao_final) #muestra los nombres de las columnas del conjunto de datos

cacao_ok<- distinct(cacao_final)  #crea un nuevo objeto llamado "cacao_ok"
write.csv(cacao_ok,file="C:/Users/marlo/Desktop/PRACTICA ECO/2MIERCOLESRSTUDIO2/cacao_Peru.csv")
dim(cacao_ok) #obtener las dimensiones del objeto "cacao_ok".
names(cacao_ok) #para obtener los nombres de las columnas del objeto "cacao_ok"

#cargar los shapes
geometria <- st_read("C:/Users/marlo/Desktop/PRACTICA ECO/2MIERCOLESRSTUDIO2/limites_departamentales.shp") #En primer lugar, se utiliza la función `st_read()` para leer un archivo shapefile llamado "limites_departamentales.shp"
plot(geometria) #mostrar los límites de los departamentos en un gráfico
unique(geometria$NOMBDEP) #obtener los nombres únicos de los departamentos presentes en el conjunto de datos. Esto proporciona una lista de los nombres de todos los departamentos
geometria <- geometria %>%
  filter(NOMBDEP == "LORETO") #solo se incluyan las filas donde el nombre del departamento (`NOMBDEP`) sea igual a "LORETO".
unique(geometria$NOMBDEP) #  para obtener los nombres únicos de los departamentos en el conjunto de datos filtrados

st_write(geometria, "C:/Users/marlo/Desktop/PRACTICA ECO/2MIERCOLESRSTUDIO2/LORETO.shp") #Para exportar los datos de la variable "geometria" a un archivo de forma (shapefile) en RStudio, utilizamos la función "st_write".
geometria <- shapefile("C:/Users/marlo/Desktop/PRACTICA ECO/2MIERCOLESRSTUDIO2/LORETO.shp")
plot(geometria) #Para visualizar los datos geográficos contenidos en "geometria",
class(geometria) #Esto nos permite confirmar que los datos cargados son del tipo esperado, en este caso, datos geográficos.

#Leer stack histórico
stack_recortado <- stack("C:/Users/marlo/Desktop/PRACTICA ECO/2MIERCOLESRSTUDIO2/stack_recortado.tif") #Esta función carga un conjunto de capas o imágenes en un objeto denominado "stack_recortado".
plot(stack_recortado) #El comando `plot(stack_recortado)` produce el gráfico correspondiente.

#cortar stack histórico en R con máscara
stack_geometria_hist <- raster::crop(stack_recortado, geometria) %>%  
  raster::mask(geometria)  #Se crea una nueva variable llamada "stack_geometria_hist". Se utiliza la función "crop" de la librería "raster" para recortar un raster llamado "stack_recortado" utilizando una geometría específica. A continuación, se aplica la función "mask" también de la librería "raster" a "stack_geometria_hist" para enmascarar los valores que están fuera de la geometría definida anteriormente
plot(stack_geometria_hist) #se muestra un gráfico del raster recortado
head(stack_geometria_hist) #mostrar las primeras filas del raster recortado
names(stack_geometria_hist) #btener los nombres de las capas presentes 

writeRaster(stack_geometria_hist, filename = "C:/Users/marlo/Desktop/PRACTICA ECO/2MIERCOLESRSTUDIO2/stack_recortado_hist_loreto.tif", format = "GTiff") #La ruta de salida para el archivo GeoTIFF es especificada en el parámetro

#Seleccionar cacao SOLO GEOMETRIA
geometria <- st_as_sf (geometria) #ahora lo leeremos como un vector
names(cacao_ok) #se imprime en la consola los nombres de las columnas del objeto
cacao_geometria <- st_as_sf(cacao_ok, coords = c("lon", "lat"), crs = st_crs(geometria))  #se convierte el objeto "cacao_ok" en un objeto espacial utilizando la función "st_as_sf". Se especifican las coordenadas de longitud y latitud mediante el argumento "coords = c("lon", "lat")". Además, se define el sistema de coordenadas de referencia (CRS) utilizando "crs = st_crs(geometria)". Esto garantiza que ambos objetos, "cacao_geometria" y "geometria", tengan el mismo formato espacial.
plot(cacao_geometria) #permite visualizar los datos espaciales
cacao_geometria <- st_intersection(cacao_geometria, geometria) #Esta operación identifica la superposición entre los polígonos de ambos objetos y crea un nuevo objeto espacial resultante llamado "cacao_geometria".
st_write(cacao_geometria, "C:/Users/marlo/Desktop/PRACTICA ECO/2MIERCOLESRSTUDIO2/cacao_loreto.shp")
head(cacao_geometria) #se muestra una vista previa de las primeras filas del objeto
dim(cacao_geometria) #Se obtiene la dimensión del objeto
plot(cacao_geometria) #Esto permite visualizar los cambios realizados después de la conexión espacial.

##convertir shp en formato punto a csv para poder extraer los datos de cada uno de los stack
cacao_geometria_punto <- as.data.frame(mutate(cacao_geometria, long=st_coordinates(cacao_geometria)[, "X"])%>%
                                             mutate(cacao_geometria, lat=st_coordinates(cacao_geometria)[, "Y"])) #Se está convirtiendo el objeto "cacao_geometria" en un marco de datos llamado "cacao_geometria_punto". Esto se logra utilizando la función "as.data.frame" y la función "mutate" para agregar dos nuevas columnas al marco de datos. La primera columna, llamada "long", se obtiene extrayendo las coordenadas X de la geometría del cacao utilizando la función "st_coordinates". La segunda columna, llamada "lat", se obtiene extrayendo las coordenadas Y de la geometría del cacao utilizando la misma función.
cacao_geometria_punto <- select(cacao_geometria_punto,species,long,lat) #A continuación, se seleccionarán las columnas "species", "long" y "lat" del data frame "cacao_geometria_punto" utilizando la función "select".
dim(cacao_geometria_punto) 
names(cacao_geometria_punto)
head(cacao_geometria_punto)
plot(cacao_geometria_punto) #se utiliza la función "plot" para generar un gráfico

write.csv(cacao_geometria_punto, file = "C:/Users/marlo/Desktop/PRACTICA ECO/2MIERCOLESRSTUDIO2/cacao_geometria_punto.csv", row.names = FALSE)
cacao_loreto_csv <- read.csv("C:/Users/marlo/Desktop/PRACTICA ECO/2MIERCOLESRSTUDIO2/cacao_geometria_punto.csv")
names(cacao_loreto_csv) #obtener los nombres de las columnas
head(cacao_loreto_csv) #mostrar las primeras filas del objeto de datos

#Probar si esta bien georeferenciado
png(filename = './cacao_geometria_100.png', width = 9, height = 6, units = 'in', res = 300)
plot(geometria[1])
points(cacao_loreto_csv$long)
dev.off() 

####Extraer valores de las variables, donde cada punto debe tener una columna por cada variable bioclimática
cacao_stack_ok <- raster::extract(stack_geometria_hist, cacao_geometria) %>%   #En la primera línea, utilizamos la función "extract" del paquete "raster" para extraer datos específicos del conjunto de datos "stack_geometria_hist" basados en una geometría dada llamada "cacao_geometria". Esto nos proporciona un nuevo conjunto de datos llamado "cacao_stack_ok".
  cbind(cacao_geometria, .) %>%   #Luego, utilizamos la función "cbind" para combinar el conjunto de datos original "cacao_geometria" con el conjunto de datos recién creado "cacao_stack_ok". Esto nos da un nuevo conjunto de datos con todas las columnas de "cacao_geometria" más las columnas extraídas de "stack_geometria_hist".
  as.data.frame()  #Después, utilizamos la función "as.data.frame()" para convertir el resultado en un marco de datos (data frame) para facilitar su manejo y análisis.
cacao_geom_stack <- select(cacao_stack_ok, -geometry) #A continuación, creamos un nuevo conjunto de datos llamado "cacao_geom_stack" mediante la función "select". Esta función nos permite seleccionar columnas específicas del conjunto de datos "cacao_stack_ok".

str(cacao_geom_stack)
dim(cacao_geom_stack)
names(cacao_geom_stack)
head(cacao_geom_stack) #La función "str" muestra la estructura del conjunto de datos, "dim" devuelve el número de filas y columnas, "names" muestra los nombres de las columnas y "head" muestra las primeras filas del conjunto de datos.

#Generar matrix 
mtx_geo <- cacao_geom_stack[,6 :ncol(cacao_geom_stack)] #se crea una variable llamada "mtx_geo" utilizando el operador de lectura "<-" en RStudio. La variable "mtx_geo" se obtiene seleccionando todas las filas y las columnas que van desde la columna 6 hasta la última columna de la variable "cacao_geom_stack".
names(mtx_geo) #La segunda línea utiliza la función "names" para obtener los nombres de las columnas de la variable "mtx_geo". Esto permite conocer los nombres de las variables o características presentes en la matriz "mtx_geo".
head(mtx_geo) #mostrar las primeras filas de la matriz "mtx_geo".

#Analisis de correlacion
m_geo <- cor(mtx_geo) #se utiliza la función 'cor' para realizar este cálculo y se guarda el resultado en una nueva matriz llamada 'm_geo'.
head(m_geo) #para mostrar las primeras filas de la matriz

#Plotear correlación
corrplot(m_geo, method = 'circle') #se utiliza para calcular la matriz de correlación utilizando los datos almacenados en 'm_geo'. Posteriormente, genera un gráfico que representa visualmente las relaciones entre las variables en el conjunto de datos.

#Detectar variables menos correlacionadas -Analisis VIF
#Revisar información sobre regresion lineal -xjemplo: Montgomery - Peck- Vinning
vif.res <- vif(x = cacao_geom_stack[,6:ncol(cacao_geom_stack)])   #El VIF se utiliza para evaluar la multicolinealidad entre las variables predictoras. En este caso, se calcula el VIF para las variables que van desde la columna 6 hasta la última columna del dataframe.               #a partir de la columna 6 se presentan los datos
vif.step <- vifstep(x = cacao_geom_stack[,6:ncol(cacao_geom_stack)], th = 10)  #El objetivo es eliminar las variables que están altamente correlacionadas con otras y que pueden afectar negativamente la calidad del modelo. Aquí, se utiliza un umbral de 10 para decidir qué variables eliminar
vrs <- vif.step@results$Variables %>%                                                  #se obtienen los nombres de las variables seleccionadas después de aplicar el paso anterior.
  as.character()        
names(cacao_geom_stack)

# 3,4,7,10,15,18
#Seleccionar Variables
names(cacao_geom_stack) #muestra los nombres de las columnas en el objeto
head(cacao_geom_stack) #muestra las primeras filas

cacao_geom_stack_final <- cacao_stack_ok %>% #crea un nuevo objeto llamado "cacao_geom_stack_final".
  as_tibble%>%   #Aquí se realiza una serie de transformaciones en el objeto "cacao_stack_ok". Primero, se convierte el objeto "cacao_stack_ok" en un tibble utilizando la función "as_tibble()".
  dplyr::select(species,geometry,vrs)  #Luego, se seleccionan solo las columnas "species", "geometry" y "vrs" del tibble utilizando la función "dplyr::select()". Esto crea un nuevo tibble con solo estas tres columnas.
names(cacao_geom_stack_final) #muestra los nombres de las columnas en el objeto "

#Extraemos capas vrs
num_capas <- nlayers(stack_geometria_hist) #se utiliza la función `nlayers()` para determinar el número de capas en el objeto `stack_geometria_hist` y se guarda el resultado en la variable `num_capas
names(num_capas) #para obtener los nombres de las capas #A continuación, se ejecuta un bucle `for` que itera desde 1 hasta el número total de capas (`num_capas`).

for (i in 1:num_capas) {
  capa <- stack_geometria_hist[[i]]
  nombre_archivo <- paste0("capa", i , ".asc")
  ruta_archivo <- file.path("C:/Users/marlo/Desktop/PRACTICA ECO/2MIERCOLESRSTUDIO2/asc", nombre_archivo)
  writeRaster(capa, filename = ruta_archivo, format = "ascii")
}


