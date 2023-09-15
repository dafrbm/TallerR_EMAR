#Preparar paquetes para trabajar con los datos

rm(list = ls())
install.packages("readr")
install.packages("tidyverse")
install.packages("gdata")
install.packages("GGally")
install.packages("ggplot2")

library(readr)
library(tidyverse)
library(gdata)
library(GGally)
library(ggplot2)

#Fijar directorio de trabajo

setwd("C:/Users/Lenovo/Desktop/GEIH")

#Cargar base de datos

datostaller <- read.csv("santander.csv", header = TRUE, sep = ",")

summary(datostaller)

datostaller$Estado.Civil <- as.factor(datostaller$Estado.Civil)
datostaller$Grupo.etnico <- as.factor(datostaller$Grupo.etnico)
datostaller$EPS <- as.factor(datostaller$EPS)
datostaller$Sexo <- as.factor(datostaller$Sexo)
datostaller$Educacion <- as.factor(datostaller$Educacion)

#Restringimos la muestra a personas de 15 años en adelante

datostaller <- subset(datostaller, Edad>14)

#Eliminamos todas las observaciones con información incompleta (NA)

datostaller <- na.omit(datostaller)

###GRÁFICAS 4K FULL HD EN R

#Gráfico de Columnas (Educación)

ggplot(datostaller, aes(x= Educacion, fill=Educacion))+
       geom_bar()+
       ggtitle("Nivel Educativo")+
       theme_bw()+
       theme(legend.position="none")+
       labs(y="Frecuencia", x="")+
       theme(axis.text = element_text( size = 12),
             axis.title = element_text( size = 12),
             plot.title = element_text(hjust = 0.5),
             axis.text.x = element_text(angle=45,hjust = 1))

#Gráfico de Columnas (Estado Civil)

ggplot(datostaller, aes(x= Estado.Civil, fill=Estado.Civil))+
       geom_bar()+
       ggtitle("Estado Civil")+
       theme_bw()+
       theme(legend.position="none")+
       labs(y="Frecuencia", x="")+
       theme(axis.text = element_text( size = 12),
             axis.title = element_text( size = 12),
             plot.title = element_text(hjust = 0.5),
             axis.text.x = element_text(angle=45,hjust = 1))

#Gráfico de cajas y bigotes

#Ingreso por Sexo

ggplot(datostaller, aes(x = Sexo, y = log(Ingreso.mensual), fill = Sexo))+
      geom_boxplot()+
      ggtitle("Ingreso por Sexo")+
      theme_bw()+
      theme(legend.position="none")+
      labs(y="Logaritmo del Ingreso mensual", x="Sexo")+
      theme(axis.text = element_text( size = 12),
            axis.title = element_text( size = 12),
            plot.title = element_text(hjust = 0.5))
  
#Ingreso por nivel educativo

ggplot(datostaller, aes(x = Educacion, y = log(Ingreso.mensual), fill = Educacion))+
  geom_boxplot()+
  ggtitle("Ingreso por Nivel Educativo")+
  theme_bw()+
  theme(legend.position="none")+
  labs(y="Logaritmo del Ingreso mensual", x="")+
  theme(axis.text = element_text( size = 12),
        axis.title = element_text( size = 12),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle=45,hjust = 1))

#Gráficos de dispersión

ggplot(datostaller, aes(x=Edad, y = log (Ingreso.mensual)))+
       geom_point()+
       labs(y="Log del Ingreso mensual", x="Edad")+
       ggtitle("Edad vs Salario")+
       theme_bw()+
       theme(axis.text = element_text( size = 12),
             axis.title = element_text( size = 12),
             plot.title = element_text(hjust = 0.5),
             axis.text.x = element_text(angle=45,hjust = 1))

#Gráficos de correlación

df2 <- subset(datostaller, select = c("Edad", "Ingreso.mensual", "Trabajo.semanal"))

ggcorr(df2)



