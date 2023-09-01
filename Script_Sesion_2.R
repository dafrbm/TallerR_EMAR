#Preparar paquetes para trabajar con los datos

install.packages("tidyverse")
library(tidyverse)

### Repetimos el ejercicio de creación del data frame de la anterior sesión
### Creamos 3 vectores

nombres <- c("Luisa", "Andrea", "Cristian", "David") #Datos tipo character
nombres

edad <- c(20, 20, 22, 24) #Datos tipo numeric
edad

car.econ <- c(TRUE, TRUE, FALSE, TRUE) #Datos tipo logical
car.econ

###DATAFRAME
#Unimos los vectores en un dataframe

talleristas <- data.frame(nombres,edad,car.econ)
talleristas

#Tenemos una nueva variable que quisiéramos añadir
asignaturafavorita <- c("Macroeconomía", "Econometría", "Econometría", "Historia Económica")

#Esto sería equivalente a agregar columnas

#Método 1: Usar la función cbind
talleristas <- cbind(nombres, edad, car.econ)
talleristas

#Apunte: cbind nos crea un objeto de tipo matriz, y eso nos puede traer problemas al hacer operaciones más adelante
#Por eso es preferible el método 2, usando la función data.frame

rm(talleristas) #Borramos el objeto que creamos en el método 1

talleristas <- data.frame(nombres, edad, car.econ) #Volvemos a crearlo con las 3 primeras columnas
talleristas

talleristas <- data.frame(talleristas,asignaturafavorita) #Añadimos la nueva columna

#Pegando Bases con identificadores
#Muchas veces las bases se dividen por temas de volumen o facilidad de consulta de la información
#Para poder "pegarse" suelen tener una o varias columnas que las identifican
#Estas columnas se suelen llamar Primary Keys o PK
#Si pegáramos los objetos usando data.frame duplicaríamos la columna de identificación
#Left_Join nos permite hacer estas fusiones sin repetir las columnas de ID
#Creamos un dataframe con la columna nombres que va a servirnos de ID

talleristas2 <- data.frame(nombres, asignaturafavorita) 
talleristas2

#Y unimos los 2 data.frames sin duplicar la columna

talleristas3 <- left_join(talleristas,talleristas2, by=c("nombres"))
talleristas3

#A veces queremos agregar observaciones, lo cual es equivalente a agregar Filas
#Agreguemos una nueva persona a nuestro dataframe

talleristaNueva <- c("Laura", 21, T, "Microeconomía")
talleristaNueva

#añadimos la nueva fila con la función rbind

talleristas3 <- rbind(talleristas3, talleristaNueva)
talleristas3

###EJERCICIO GEIH

#Fijar directorio de trabajo
setwd("C:/Users/Lenovo/Desktop/GEIH")

###Importar datos GEIH

Enecg <- read.csv("Enecg.csv", header=TRUE, sep=";")
Eneno <- read.csv("Eneno.csv", header=TRUE, sep=";")
Eneo <- read.csv("Eneo.csv", header=TRUE, sep=";")

Febcg <- read.csv("Febcg.csv", header=TRUE, sep=";")
Febno <- read.csv("Febno.csv", header=TRUE, sep=";")
Febo <- read.csv("Febo.csv", header=TRUE, sep=";")

Marcg <- read.csv("Marcg.csv", header=TRUE, sep=";")
Marno <- read.csv("Marno.csv", header=TRUE, sep=";")
Maro <- read.csv("Maro.csv", header=TRUE, sep=";")

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

#Y finalmente unimos los módulos usando un left_join

geih <- left_join(Cargen,Nocup, by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN", "HOGAR"))
geih <- left_join(geih,Ocup, by= c("DIRECTORIO", "SECUENCIA_P", "ORDEN", "HOGAR"))

