rm(list=ls())
load("Aplicacao3.RData")
library(ggplot2)
library(gridExtra)
library(gamlss)
dados<- dados1
dados$pH<- as.numeric(dados$pH)
dados$Forested<- as.factor(dados$Forested)
dados$FieldpH<- as.numeric(dados$FieldpH)
dados$Temperature<- as.numeric(dados$Temperature)

v1<- ggplot(dados, aes(pH))+
  geom_histogram(bins=11,aes(y=..density..),fill="steelblue")+
  theme(axis.text.x = element_text(size = 16, face = "plain"),
        axis.text.y = element_text(size = 16, face = "plain"),  
        axis.title.x = element_text(size = 20, face = "plain"),
        axis.title.y = element_text(size = 20, face = "plain"))+
  stat_function(fun=dNO,
                args = list(mu=mean(dados$pH), 
                            sigma=sd(dados$pH)),
                col="black", size=1)+xlim(c(3,11))
v1

v2<- ggplot(dados, aes(Forested,pH))+
  geom_boxplot(fill="steelblue")+
  theme(axis.text.x = element_text(size = 16, face = "plain"),
        axis.text.y = element_text(size = 16, face = "plain"),  
        axis.title.x = element_text(size = 20, face = "plain"),
        axis.title.y = element_text(size = 20, face = "plain"))

v3<- ggplot(dados, aes(FieldpH,pH))+
  geom_point(size=3)+
  geom_smooth(se=FALSE,col="red", size=2)+
  theme(axis.text.x = element_text(size = 16, face = "plain"),
        axis.text.y = element_text(size = 16, face = "plain"),  
        axis.title.x = element_text(size = 20, face = "plain"),
        axis.title.y = element_text(size = 20, face = "plain"))

v4<- ggplot(dados, aes(Temperature,pH))+
  geom_point(size=3)+
  geom_smooth(se = FALSE,col="red", size=2)+
  theme(axis.text.x = element_text(size = 16, face = "plain"),
        axis.text.y = element_text(size = 16, face = "plain"),  
        axis.title.x = element_text(size = 20, face = "plain"),
        axis.title.y = element_text(size = 20, face = "plain"))

v5<- ggplot(dados, aes(Date2,pH))+
  geom_point(size=3)+
  geom_smooth(se = FALSE,col="red", size=2, method = "lm")+
  theme(axis.text.x = element_text(size = 16, face = "plain"),
        axis.text.y = element_text(size = 16, face = "plain"),  
        axis.title.x = element_text(size = 20, face = "plain"),
        axis.title.y = element_text(size = 20, face = "plain"))+
  xlab("Days")
grid.arrange(v2,v3,v5,v4)
