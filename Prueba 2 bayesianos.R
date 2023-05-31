
library(rio)
library(tidyverse)
library(ggplot2)

# Pregunta 1----

## b----

#Estadístico 

z<--3.6167
valor_p<- pnorm(z, 0,1)


# Pregunta 3----
## a----
datos <- import("C:\\Users\\Dayanna\\Downloads\\arboles.csv")
attach(datos)
names(datos)
n<-length(table(circunferencia))

# Gráfico de dispersión 

datos|>
  ggplot(aes(x=circunferencia,y=altura))+
  geom_point(alpha = 1.2) +
  labs(title="Altura árboles vs circunferencia tronco",y="Altura ",x= "Circunferencia ")+
  theme_bw()


# Ajuste modelo y recta min.cuadrados

modelo <- lm(altura ~ circunferencia -1, datos)
summary(modelo)

datos|>
  ggplot(aes(x=circunferencia,y=altura))+
  geom_point(alpha = 1.2) +
  labs(title="Altura árboles vs circunferencia tronco",y="Altura ",x= "Circunferencia ")+
  geom_smooth(method = lm, se = F,color="magenta",size=0.5)+
  theme_bw()


# Test-t 
summary(modelo)
coef(modelo)

# Obtenemos :  R^2=0.9977, valor t= 78.17 y grados de libertad=14
# Tomamos una significancia de 0.05

qt(0.95,14)  #1.761

# H_0: circunferencia no significativa 

# Como valor t > qt, entonces rechazamos H_0, es decir la covariable circunferencia es significativa e influye en la altura de los árboles.

## b----

suma_xi2<- sum(circunferencia^2)
suma_xiyi<- sum(circunferencia*altura)



