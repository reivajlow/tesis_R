# rm(list=ls())
# library(readxl)
# library(gamlss)#chamando o pacote gamlss
#setwd("C:/Users/Julio Cezar/Desktop/codigos_do_artigo_MLPGS/Codigos_anteriores/Aplicacao_dados_Novos")
dados1 <- datos
attach(dados1)

# y_<-as.numeric(pH)
y_ <- log_iga
# eliminar na
hist(y_)#sim?trica com valores at?picos na calda esquerda
summary(y_)#? sim?trica, pois mediana pr?ximo da m?dia

cat_vacuna <- as.factor(cat_vacuna)
table(cat_vacuna)

levels(cat_vacuna) <- c("pfizer","coronavac")

x1 <- imc
x2 <- as.factor(cat_vacuna)
x3 <- log_neutra
x4 <- edad
x5 <- as.factor(sexo)

plot(x1,y_)
plot(x2,y_)
plot(x3,y_)
plot(x4,y_)
plot(x5,y_)

summary(y_)
require(e1071)
sd(y_)
skewness(y_)
kurtosis(y_)
length(y_)

#___Fun??o de liga??o identidade na posi??o (mu)
library(gamlss)

#______________Normal_________________________
Normal_identidade<- gamlss(y_~x2+x5+cs(x1+x3+x4),
                           family=NO(mu.link = identity,sigma.link = log),
                           data=dados1,n.cyc=1000, trace=TRUE)

#_____________t_Student_______________________
tStudent_identidade<- gamlss(y_~x1+x2+x3+x5+cs(x4),
                             family=TF(mu.link = identity,sigma.link = log),
                             data=dados1,n.cyc=1000, trace=TRUE)


#______________Exponencial pot?ncia___________
Exponencial_potencia_identidade<- gamlss(y_~x1+x2+x3+x5+cs(x4),
                                         family=PE(mu.link = identity,sigma.link = log),
                                         sigma.start=1,data=dados1,n.cyc=10000, trace=TRUE)


#____________________________________________
#___Fun??o de liga??o inversa na posi??o (mu)

#____________Normal__________________________
Normal_inversa<- gamlss(y_~x1+x2+x3+x5+cs(x4),
                        family=NO(mu.link = inverse,sigma.link = log),
                        data=dados1,n.cyc=1000, trace=TRUE)

#___________t-Student________________________
tStudent_inversa<- gamlss(y_~x1+x2+x3+x5+cs(x4),
                          family=TF(mu.link = inverse,sigma.link = log),
                          data=dados1,n.cyc=1000, trace=TRUE)

summary(tStudent_inversa)
paste("lambda=",round(getSmo(tStudent_inversa)$lambda1,4))
paste("df=",round(sum(getSmo(tStudent_inversa)$lev),4))#df ? s?o os graus de liberdade efetivos, ou seja, o grau do polin?mio de cada parti??o da curva

#___________Exponencial pot?ncia_____________
Exponencial_potencia_inversa<- gamlss(y_~x1+x2+x3+x5+cs(x4),
                                      family=PE(mu.link = inverse,sigma.link = log),
                                      sigma.start=1,data=dados1,n.cyc=1000,trace=TRUE)

#____________________________________________
#___Fun??o de liga??o log na posi??o (mu)

#____________Normal__________________________
Normal_log<- gamlss(y_~x1+x2+x3+x5+cs(x4),
                    family=NO(mu.link = log,sigma.link = log),
                    data=dados1,n.cyc=1000, trace=TRUE)


#___________t-Student________________________
tStudent_log<- gamlss(y_~x1+x2+x3+x5+cs(x4),
                      family=TF(mu.link = log,sigma.link = log),
                      data=dados1,n.cyc=1000, trace=TRUE)


#___________Exponencial pot?ncia_____________
Exponencial_potencia_log<- gamlss(y_~x1+x2+x3+x5+cs(x4),
                                  family=PE(mu.link = log,sigma.link = log),
                                  data=dados1,n.cyc=1000,trace=TRUE)

#____________________________________________
#___Fun??o de liga??o squt na posi??o (mu)

#____________Normal__________________________
Normal_sqrt<- gamlss(y_~x1+x2+x3+x5+cs(x4),
                     family=NO(mu.link = "sqrt",sigma.link = log),
                     data=dados1,n.cyc=1000, trace=TRUE)

#___________t-Student________________________
tStudent_sqrt<- gamlss(y_~x1+x2+x3+x5+cs(x4),
                       family=TF(mu.link = "sqrt",sigma.link = log),
                       data=dados1,n.cyc=1000, trace=TRUE)


#___________Exponencial pot?ncia_____________
Exponencial_potencia_sqrt<- gamlss(y_~x1+x2+x3+x5+cs(x4),
                                   family=PE(mu.link ="sqrt",sigma.link = log),
                                   sigma.start=1,data=dados1,n.cyc=1000,trace=TRUE)


GAIC(Normal_identidade,
     tStudent_identidade,
     Exponencial_potencia_identidade,
     Normal_inversa,
     tStudent_inversa,
     Exponencial_potencia_inversa,
     Normal_log,
     tStudent_log,
     Exponencial_potencia_log,
     Normal_sqrt,
     tStudent_sqrt,
     Exponencial_potencia_sqrt)

wp(tStudent_inversa,ylim.all = 3)

summary(tStudent_log)

exp(tStudent_inversa$nu.coefficients)

#x11()
term.plot(tStudent_inversa, 
          what = c("mu"),
          terms = 4,main="",
          col.shaded="royalblue",
          col.term = "gray83",
          xlabs="Edad",col.se="black",
          ylabs="Partial for cs(edad)",
          lwd.term = 4,
          lwd.se=2,
          lty.se = 1)
#____________________________________________________________________
#Tendencias y vs x's 

#y vs x2
#x11()
plot(x4,y_,pch=19,main="",col="royalblue4",
     xlab="Measured height",
     ylab="Measured weight")
lines(smooth.spline(x4,fitted(tStudent_inversa)),
     type="l",col="red",lwd=4)


#____________________________________________________________
#           Qualidade de ajuste da dist. t-Student,
#justificativa para dizer que a dist. t-Student n?o 
#ajustou-se bem aos dados

#_______Gr?fico dos res?duos vs indices 
Res.q2 <- tStudent_inversa$residuals
Res.qo2 <- sort(abs(Res.q2))

index <- 1:length(dados1$log_iga)

#x11()
plot(Res.q2,pch=1,col="red2",lwd=4,
     ylab="Quantile residuals",xlab="Index",
     main ="",ylim = c(-5,5),cex.lab=1.2)
abline(h=-3,lwd=4,lty=2,col="1")
abline(h=0,lwd=2,lty=1,col="4")
abline(h=3,lwd=4,lty=2,col="1")
#identify(index,Res.q2)

#__________Envelope simulado 
B <- 500  # Número de simulaciones
n <- length(y_)
j <- 1
iter <- 0
Res.q <- tStudent_inversa$residuals
Res.qo <- sort(Res.q)

# Simulaciones
set.seed(123)
mrq <- matrix(0, B, n)

while (j < B + 1) {
  simula <- rTF(n, fitted(tStudent_inversa))
  
  # Intenta ajustar el modelo, captura cualquier error
  m1s <- try(gamlss(simula ~ x1 + x2 + x3 + cs(x4),
                    family = TF(mu.link = inverse, sigma.link = log),
                    data = dados1, n.cyc = 1000, trace = TRUE), silent = TRUE)
  
  # Verifica si la asignación fue exitosa y si hay un objeto válido
  if (!inherits(m1s, "try-error")) {
    Res.qs <- m1s$residuals
    mrq[j, ] <- Res.qs
    j <- j + 1
  }
  cat("iteration =", iter <- iter + 1, ", j =", j, "\n")
}

mrq2 <- t(apply(mrq, 1, sort))
Z <- qnorm((1:n - 3/8) / (n + 1/4))
rqm <- apply(mrq2, 2, mean)
rqmin <- apply(mrq2, 2, min)
rqmax <- apply(mrq2, 2, max)
mrq3 <- cbind(Z, Res.qo, rqmin, rqm, rqmax)

(res.out <- sum(c(sum(Res.qo>=rqmax),sum(Res.qo<=rqmin))))
(per.out <- round(res.out/n*100,2))


##x11()
qqnorm(Res.qo,pch=19,col="red2",
       xlim=c(-3,3),
       ylim=c(-4,4),
       ylab="Quantile residuals",
       xlab="N(0,1) quantiles",main = "",cex.lab=1.2)
lines(Z,rqmax,col="1",lwd=3)
lines(Z,rqm , lty = 2,col="4",lwd=3)
lines(Z,rqmin,col="1",lwd=3)
legend("topleft", c(paste("Total points:", n),
       paste("Points out of envelope:",
       res.out , "(", per.out, "%)")),
       bty="n", cex=1.2)


save.image("Aplicacion_thomas_javier_log_iga.RData")
