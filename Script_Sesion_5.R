#Preparación

rm(list = ls())

install.packages("readr")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("tibble")
install.packages("ggplot2")
install.packages("GGally")
install.packages("stargazer")

library(readr)
library(tidyverse)
library(dplyr)
library(tibble)
library(ggplot2)
library(GGally)
library(stargazer)

#Fijar directorio de trabajo

setwd("C:/Users/Lenovo/Desktop/GEIH")

#Cargar bases de Datos

load("TallerR-5.RData")
load("DatosGeih.RData")

wages <- datos
santander <- datostaller

rm(datos, datostaller)

view(wages)

#Análisis exploratorio
#Creamos un dataset más pequeño, para solo explorar estas variables

df <- subset(wages, select=c("wage", "educ", "exper","age"))

#Gráficos cruzados de dispersión

pairs(df)

#Matriz de correlaciones

cor(df)

#Gráfico de correlación

ggcorr(df)

#Gráfico de dispersión

ggplot(data = wages, aes(x = educ, y = wage)) +
  geom_point() +
  theme_bw()
    
#Regresión Lineal
#Regresión Lineal Simple (1 explicada vs 1 explicativa)

Modelo0 <- lm (wage ~ educ, data = wages)
summary(Modelo0)

#Graficar la regresión

ggplot(data = wages, aes(x = educ, y = wage)) +
  geom_point() +
  theme_bw()+
  geom_smooth(method = "lm", se = FALSE)

summary(wages$educ)

#Regresión Lineal Múltiple (1 explicada vs n explicativas)

Modelo1 <- lm (wage ~ educ + age , data = wages)
summary(Modelo1)

Modelo2 <- lm (wage ~ educ + exper , data = wages)
summary(Modelo2)

ggplot(data = wages, aes(x = exper, y = wage)) +
  geom_point() +
  theme_bw()+
  geom_smooth(method = "lm", se = FALSE)

#Transformaciones del salario
#Crear salario por hora

wages <- mutate(wages, 'hourlywage' = (wages$wage*12)/(wages$hours*52))

#Transformarlo a logaritmo

wages <- mutate(wages, 'lhrlywg' = log(wages$hourlywage))

#Vamos a probar con nuevas variables explicadas o dependientes

Modelo3 <- lm(hourlywage ~ educ, data = wages)
summary(Modelo3)

Modelo4 <- lm(lwage ~ educ, data = wages)
summary(Modelo4)

Modelo5 <- lm(lhrlywg ~ educ, data = wages)
summary(Modelo5)

#Regresiones múltiples con log(salario por hora)

Modelo6 <- lm(lhrlywg ~ educ + black , data = wages)
summary(Modelo6)

#Gráficos con categóricas
#Barras

ggplot(wages, aes(x=black, y=hourlywage, fill = black))+
  geom_bar(stat = "identity")+
  ggtitle("Salario por raza")+
  theme_bw()+
  theme(legend.position="none")+
  labs(y="Salario por Hora", x="")+
  theme(axis.text = element_text( size = 12),
        axis.title = element_text( size = 12),
        plot.title = element_text(hjust = 0.5))

#Cajas

ggplot(wages, aes(x = black, y = lhrlywg, group = black, fill = black))+
  geom_boxplot()+
  ggtitle("Ingreso por raza")+
  theme_bw()+
  theme(legend.position="none")+
  labs(y="Logaritmo del Ingreso por hora", x="")+
  theme(axis.text = element_text( size = 12),
        axis.title = element_text( size = 12),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle=45,hjust = 1))

