#Preparar paquetes para trabajar con los datos

install.packages("readr")
install.packages("tidyverse")
install.packages("gdata")
install.packages("stargazer")

library(readr)
library(tidyverse)
library(gdata)
library(stargazer)

###EJERCICIO GEIH
#Descargar los archivos de la GEIH correspondientes para los meses de enero, 
#febrero y marzo.
#Cambiar los nombres de las carpetas por números, así Enero -> 01 , 
#Febrero -> 02 y Marzo -> 03
#Posteriormente conservar únicamente los archivos de la carpeta CSV con los
#nombres de características generales,ocupados y no ocupados.
#Renombrar los archivos siguiendo la siguiente nomenclatura
#cargen <- Características Generales
#nocup <- No Ocupados
#ocup <- Ocupados
#Los vamos a cargar como objetos siguiendo unos prefijos
#cg <- Características Generales
#no <- No Ocupados
#o <- Ocupados
#'3 primeras letras del mes´'Prefijos´.csv


#Fijar directorio de trabajo
rm(list = ls())
setwd("C:/Users/Lenovo/Desktop/GEIH")

###Importar datos GEIH

#Mes de Enero

Enecg <- read.csv("01/cargen.csv", header=TRUE, sep=";")
Eneno <- read.csv("01/nocup.csv", header=TRUE, sep=";")
Eneo <- read.csv("01/ocup.csv", header=TRUE, sep=";")

#Mes de Febrero

Febcg <- read.csv("02/cargen.csv", header=TRUE, sep=";")
Febno <- read.csv("02/nocup.csv", header=TRUE, sep=";")
Febo <- read.csv("02/ocup.csv", header=TRUE, sep=";")

#Mes de Marzo

Marcg <- read.csv("03/cargen.csv", header=TRUE, sep=";")
Marno <- read.csv("03/nocup.csv", header=TRUE, sep=";")
Maro <- read.csv("03/ocup.csv", header=TRUE, sep=";")

#PEGAR MÓDULOS

Cargen <- rbind(Enecg,Febcg)
Cargen <- rbind(Cargen,Marcg)

#Nos sale error porque no tenemos el mismo número de columnas, vamos a quitarle
#columnas a los objetos Enecg y Febcg

Enecg <- subset(Enecg, select = -c(LGB_Numerica,LGB_sectores,LGBT_Numerica,Trans_numerica,Discapacidad,Dificultad,Campesina))
Febcg <- subset(Febcg, select = -c(LGB_Numerica,LGB_sectores,LGBT_Numerica,Trans_numerica,Discapacidad,Dificultad,Campesina))

#Ahora sí, pegamos los meses

Cargen <- rbind(Enecg,Febcg)
Cargen <- rbind(Cargen,Marcg)

Nocup <- rbind(Eneno,Febno)
Nocup <- rbind(Nocup,Marno)

Ocup <- rbind(Eneo,Febo)
Ocup <- rbind(Ocup,Maro)

#Ahora para poder unir las bases necesitamos hacer algunos ajustes
#Cambiar los nombres de las ID's

Cargen <- rename.vars(Cargen,
                      from = c('DIRECTORIO', 'ORDEN', 'SECUENCIA_P'),
                      to = c('ID.Vivienda', 'ID.Persona', 'ID.Hogar'))

Nocup <- rename.vars(Nocup,
                      from = c('DIRECTORIO', 'ORDEN', 'SECUENCIA_P'),
                      to = c('ID.Vivienda', 'ID.Persona', 'ID.Hogar'))

Ocup <- rename.vars(Ocup,
                      from = c('DIRECTORIO', 'ORDEN', 'SECUENCIA_P'),
                      to = c('ID.Vivienda', 'ID.Persona', 'ID.Hogar'))

#Crear un único ID mezclando las 3 columnas

Cargen <-  Cargen %>% 
  add_column(ID = paste(as.character(Cargen$ID.Vivienda), 
                        as.character(Cargen$ID.Hogar),
                        as.character(Cargen$ID.Persona), 
                        sep = ''),
             .before = 1)

Nocup <-  Nocup %>% 
  add_column(ID = paste(as.character(Nocup$ID.Vivienda), 
                        as.character(Nocup$ID.Hogar),
                        as.character(Nocup$ID.Persona), 
                        sep = ''),
             .before = 1)

Ocup <-  Ocup %>% 
  add_column(ID = paste(as.character(Ocup$ID.Vivienda), 
                        as.character(Ocup$ID.Hogar),
                        as.character(Ocup$ID.Persona), 
                        sep = ''),
             .before = 1)

#Comprobamos que los ID sean únicos para cada observación del dataframe
#El resultado de estas líneas debería ser TRUE
#Si fuese FALSE, significa que los ID's se repiten entre observaciones
#Y significaría que cometimos algún error en los pasos anteriores

length(Cargen$ID) == length(unique(Cargen$ID))
length(Nocup$ID) == length(unique(Nocup$ID))
length(Ocup$ID) == length(unique(Ocup$ID))

#Antes de unir las bases, Se revisa los nombres que son idénticos en ambas bases
#de datos y se deja, solamente, los de 'Cargen', pues es el
#documento más extenso, y el que posee a todos los individuos

intersect(names(Cargen), names(Nocup))
intersect(names(Cargen), names(Ocup))

Nocup <-  select(Nocup, -c("PERIODO",
                       "MES","PER",
                       "ID.Vivienda", 
                       "ID.Hogar", 
                       "ID.Persona", 
                       "HOGAR", "REGIS", 
                       "AREA", "MES", 
                       "DPTO", 
                       "FEX_C18"))

Ocup <-  select(Ocup, -c("PERIODO",
                       "MES","PER",
                       "ID.Vivienda", 
                       "ID.Hogar", 
                       "ID.Persona", 
                       "HOGAR", "REGIS", 
                       "AREA", "MES", 
                       "DPTO", 
                       "FEX_C18"))

#Unimos ahora las bases

geih <- left_join(Cargen,Nocup, by = c("ID"))
geih <- left_join(geih,Ocup, by= c("ID"))

#Separamos el set de datos correspondiente a Santander

antioquia <- subset(geih, DPTO == 05)
rm(antioquia)
santander <- subset(geih, DPTO == 68)
head(santander)

#Como tenemos datos acerca de Ocupados y No ocupados, vamos a tener que hacer 
#una división en la base para revisar algunas estadísticas comparadas
#Pero primero vamos a analizar algunas características generales para saber
#cómo están todas las personas en Santander
#Convertir las variables de ID en factores

santander$ID.Vivienda<-factor(santander$ID.Vivienda)
santander$ID.Hogar<-factor(santander$ID.Hogar)
santander$ID.Persona<-factor(santander$ID.Persona)

#Podríamos hacer algunas medidas posicionales, pero este sería el resultado

summary(santander)

#Arroja mucha información sobre variables que probablemente no vayamos a utilizar
#Lo mejor sería reducir las variables a utilizar a algunas de interés
#A continuación se sugieren variables y los datos que representan
#     -        ID: Identificador único
#     -       MES: Mes de la encuesta
#     -        PT: Identificador de módulo personas
#     -       OCI: Identificador de módulo ocupados
#     -       DSI: Identificador de módulo no ocupados
#     -     P3271: Sexo
#     -     P6040: Edad
#     -     P6080: Reconocimiento de alguna etnia
#     -     P6070: Estado civil.
#     -     P6090: Afiliación a EPS
#     -     P6100: Régimen de seguridad social
#     -     P6160: Alfabetización
#     -     P6170: Asiste a alguna IE
#     -     P3041: Régimen de la IE
#     -     P3042: Mayor nivel educativo
#     -   P3042S1: Mayor nivel educativo (año o grado)
#     -     P3043: Título o diploma de mayor nivel
#     -     P3038: Atracción sexual o romántica
#     -     P3039: Reconocimiento (género)
#     - RAMA4D_R4: Rama de actividad CIIU 4, 4 dígitos
#     - OFICIO_C8: Oficios CIUO - 08 - A.C.
#     -     P6440: Tipo de contrato
#     -     P6400: Empresa donde trabaja es la misma que lo contrata
#     -     P6410: Empresa que contrata es de tipo
#     -   P6420S2: Rama de actividad empresa que lo contrato a 4 digitos
#     -     P6430: Tipo de trabajo que desempeña
#     -     P6800: Horas que trabaja a la semana
#     -     P6850: Horas trabajadas la semana pasada.
#     -     P7045: Horas trabajadas en una labor secundaria a la semana.
#     -     P7180: Pertenece a gremio o sindicato
#     -   P7170S1: Satisfacción con el trabajo
#     -   inglabo: Ingresos laborales mensuales.
#     -OFICIO1_C8: ¿En qué ocupación, oficio o labor ha buscado trabajo?  
#    -RAMA4D_D_R4: Actividad que desempeñaba la unidad donde trabajó por última vez
#     -     P7250: Semanas buscando trabajo
#     -     P7280: Ha buscado trabajo como
#     -     P1806: Sueldo mínimo que aceptaría para trabajar
#     -   P7440S1: Tiempo desde la última vez que trabajo (meses)
#     -     P7450: Motivo para dejar el trabajo
#     -     P7350: En este último trabajo era
#     -     P9460: Recibe subsidio de desempleo?
# Si quieren sacar todas estas variables, la línea de código de abajo lo hace
#Quitar los #
#santander_data <- select(santander, 
             #c('ID','MES','PT','OCI','DSI','P3271','P6040','P6080','P6070',
             #'P6090','P6100','P6160','P6170','P3041','P3042','P3042S1',
             #'P3043','P3038','P3039','RAMA4D_R4','OFICIO_C8','P6440','P6400',
             #'P6410','P6420S2','P6430','P6800','P6850','P7045','P7180',
             #'P7170S1','INGLABO','OFICIO1_C8','RAMA4D_D_R4','P7250','P7280',
             #'P1806','P7440S1','P7450','P7350','P9460'))
#Para este ejemplo utilizaremos estas variables

santander_data <- select(santander, 
                   c('ID','MES','PT','OCI','DSI','P3271','P6040','P6080','P6090',
                     'P6070','P6160', 'P6170','P3041','P3042','P3042S1','P3043',
                     'P3038','P3039','P6800','P6850','P7045','INGLABO',
                     'OFICIO1_C8','RAMA4D_D_R4','P7250','P7280','P1806',
                     'P7440S1','P7450','P7350','P9460'))

#Empecemos por arreglar las variables, empezando por nombres y tipo de datos

datos <-  select(santander_data, c('MES', 'P6040', 'P3271', 'P6080', 'P6070',
                                 'P6090', 'P3043', 'P6800','INGLABO', 
                                 'RAMA4D_D_R4'))
colnames(datos) <- c('Mes', 'Edad', 'Sexo', 'Grupo etnico', 'Estado Civil',
                     'EPS', 'Educacion','Trabajo semanal', 'Ingreso mensual', 
                     'CIUU 4rev')

#Mirar el encabezado de los datos, para ver cómo quedan las columnas
head(datos)

#Y ahora sí, descriptivas, una primera revisión puede hacerse usando la
#función summary, que me devuelve algunas medidas posicionales

summary(datos)

#Punto importante, no todos los datos son continuos (¿qué significa eso?)
#Variable continua = se puede medir
#Variable discreta = se puede contar

#Un ejemplo de variable discreta es el sexo, miremos cómo arreglar esto
#%>% es un símbolo conocido como pipeline, nos permite escribir una secuencia
#de operaciones de izquierda a derecha
#La función mutate nos permite transformar variables en el dataframe, así
#como anidar varias operaciones a la vez

datos <-  datos %>% mutate(Sexo = case_when(Sexo == 1 ~ 'Hombre', 
                                            Sexo == 2 ~ 'Mujer'),
                         Sexo = factor(Sexo, levels = c('Hombre', 'Mujer')))

#Veamos como cambia el resultado al aplicar summary

summary(datos$Sexo) #Nos da sus frecuencias absolutas

#Ejercicio rápido, la variable EPS nos dice si la persona está afiliada o no.
#Cuál sería la forma en la que recodificaríamos la variable?

datos <-  datos %>% mutate(EPS = case_when(EPS == 1 ~ 'Afiliado', 
                                           EPS == 2 ~ 'No afiliado'),
                           EPS = factor(EPS, levels = c('Afiliado', 
                                                        'No afiliado')))

#Repetir proceso para Estado civil, Grupo étnico y CIUU

#La educación también es factor, pero tiene un orden (de menor a mayor)


