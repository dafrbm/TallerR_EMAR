install.packages("haven")
install.packages("extrafont")

library(extrafont)
library(GGally)
library(lmtest)
library(haven)
library(dplyr)
library(ggplot2)
loadfonts()

setwd("C:/Users/Usuario/Documents/Sexto/Econometría II/Datos_MRL")
df<-read_dta('mroz.dta')

df <- df %>% mutate(annualwage=wage*hours,
                    lwage=log(annualwage))
head(df)

summary(df)

colnames(df)

#Horas del esposo, salario de la mujer, educación,

df$inlf<-as.factor(df$inlf)
df$city<-as.factor(df$city)

levels(df$inlf)<-c("Desempleada","Empleada")

df1<-df %>% select(inlf,hours,kidslt6,kidsge6,age,educ,huseduc,huswage,exper,lwage,wage,faminc,annualwage)


df_employ<-df1[df1$inlf=="Empleada",]

###Boxplot###
ggplot(df_employ,aes(inlf,annualwage))+
  geom_boxplot()

ggplot(df_employ,aes(inlf,lwage))+
  geom_boxplot()


ggplot(df_employ,aes(inlf,hours))+
  geom_boxplot(fill="#FAD366")+
  theme_bw()+
  labs(x="",y="Horas")+
  theme(text=element_text(family="LM Roman 10"),
        plot.title=element_text(hjust=0.5),
        title=element_text(size=12,face="bold"))+
  ggtitle("Diagrama de Cajas")


ggplot(df_employ,aes(annualwage))+
  geom_histogram(fill='#FAD366',color="black",bins=20)+
  theme_bw()+
  labs(x="Salario",y="Frecuencia")+
  theme(text=element_text(family="LM Roman 10"),plot.title=element_text(hjust=0.5))+
  ggtitle("Histograma")


ggplot(df_employ,aes(huswage))+
  geom_histogram(fill='#FAD366',color="black",bins=20)+
  theme_bw()+
  labs(x="Salario",y="Frecuencia")+
  theme(text=element_text(family="LM Roman 10"),plot.title=element_text(hjust=0.5))+
  ggtitle("Histograma")


#Hacer intento con Calibri, Times New Roman

ggplot(df_employ,aes(faminc,lwage))+
  geom_point()+       
  theme_bw()+
  labs(x="Salario Anual",y="Horas")+
  theme(text=element_text(family="LM Roman 10"),
        plot.title=element_text(hjust=0.5),
        title=element_text(size=12,face="bold"))+
  ggtitle("Diagrama de Dispersión")

ggplot(df_employ,aes(annualwage,hours))+
  geom_point()+
  theme_bw()+
  labs(x="Ingreso familiar",y="Horas")+
  theme(text=element_text(family="LM Roman 10"),
        plot.title=element_text(hjust=0.5),
        title=element_text(size=12,face="bold"))+
  ggtitle("Diagrama de Dispersión")

ggplot(df_employ,aes(huswage,wage))+
  geom_point()+
  geom_smooth(method = "lm", se= F)
theme_bw()+
  labs(x="Salario del esposo por hora",y="Salario F")+
  theme(text=element_text(family=""),
        plot.title=element_text(hjust=0.5),
        title=element_text(size=12,face="bold"))+
  ggtitle("Diagrama de Dispersión")

cor(df_employ$wage, df_employ$annualwage,method = c("pearson"))

ggcorr(df1)+theme_bw()+
        theme(text=element_text(family="LM Roman 10"),
              plot.title=element_text(hjust=0.5),
              title=element_text(size=12,face="bold"))+
        ggtitle("Correlación")

reg<-lm(annualwage~educ+huswage+kidslt6+age+faminc+huseduc+hours,df_employ)
summary(reg)
regh<-lm(hours~educ+huswage+kidslt6+age+faminc+huseduc+annualwage,df_employ)
summary(regh)   
reg2<-lm(lwage~educ+huswage+kidslt6+age+faminc+huseduc+hours,df_employ)
summary(reg2)


summary(reg)
summary(regh)       
summary(reg2)