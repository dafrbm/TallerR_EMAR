###SESIÓN 1: TALLER DE PROCESAMIENTO Y ANÁLISIS DE DATOS CON R

print("R Studio es más fachero que R")

###TIPOS DE DATOS

### 3 tipos: Numeric, Character y Logical.
###VECTORES

nombres <- c("Luisa", "Andrea", "Cristian", "David") #Datos tipo character
nombres

edad <- c(20, 20, 22, 24) #Datos tipo numeric
edad

car.econ <- c(TRUE, TRUE, FALSE, TRUE) #Datos tipo logical
car.econ

#Revisar el tipo de objeto

class(nombres)
class(edad)
class(car.econ)

is.numeric(edad)
is.numeric(nombres)
is.numeric(car.econ)

#Traten de repetir con las funciones de character y logical
#Borren el # y repitan el proceso con los 3 objetos como se hace en las líneas 25-27

#is.character

#is.logical

#Borrado y corrección de vectores

rm(edad)

edad <- c(20, 20, 22, 24)

###FACTORES

carrera <- c("Economia", "Matemáticas", "Ing. Eléctrica")

carrera <- as.factor(c("Economía", "Matemáticas", "Ing. Eléctrica"))

###ARRAYS Y MATRICES
###Las matrices son un tipo de array con largo y ancho (filas y columnas)

1:12

matriz <- matrix(1:12) #Sin especificar filas y columnas
matriz

matriz <- matrix(1:12, nrow = 3, ncol = 4)
matriz

matriz <- matrix(1:12, nrow=3, ncol=4, byrow = T)
matriz

vector1 <- c(1:2)
vector2 <- c(1:4)
vector3 <- c(1:8)

matriz <- rbind(vector1,vector2,vector3)
matriz

matriz <- cbind(vector1, vector2, vector3)
matriz

###LISTAS
###Vectores "Mixtos"

lista <- list("David", c(1:3), TRUE)
lista

###DATAFRAME
#Permite guardar conjuntos de vectores de la misma longitud

talleristas <- data.frame(nombres,edad,car.econ)
talleristas
