dados1 <- datos
attach(dados1)
library(ggplot2)
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


#hist log iga
ggplot(dados1, aes(log_iga)) +
  geom_histogram(binwidth = 1, fill = "royalblue", col = "black") +
  theme(
    axis.text.x = element_text(size = 16, face = "plain"),
    axis.text.y = element_text(size = 16, face = "plain"),  
    axis.title.x = element_text(size = 20, face = "plain"),
    axis.title.y = element_text(size = 20, face = "plain")
  ) +
  xlab("Log IGA")


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
tStudent_identidade_x1 <- gamlss(y_~x2+x5+cs(x1),
                             family=TF(mu.link = identity,sigma.link = log),
                             data=dados1,n.cyc=1000, trace=TRUE)


#_____________t_Student_______________________
tStudent_identidade_x3 <- gamlss(y_~x2+x5+cs(x3),
                             family=TF(mu.link = identity,sigma.link = log),
                             data=dados1,n.cyc=1000, trace=TRUE)


#_____________t_Student_______________________
tStudent_identidade_x4 <- gamlss(y_~x2+x5+cs(x4),
                             family=TF(mu.link = identity,sigma.link = log),
                             data=dados1,n.cyc=1000, trace=TRUE)



#_____________t_Student_______________________
tStudent_identidade_x134 <- gamlss(y_~x2+x5+x4+x1+cs(x3),
                                 family=TF(mu.link = identity,sigma.link = log),
                                 data=dados1,n.cyc=1000, trace=TRUE)

#_____________t_Student_______________________
tStudent_identidade_xcs134 <- gamlss(y_~x2+x5+cs(x4)+cs(x1)+cs(x3),
                                   family=TF(mu.link = identity,sigma.link = log),
                                   data=dados1,n.cyc=1000, trace=TRUE)


summary(tStudent_identidade_x1)
summary(tStudent_identidade_x3)
summary(tStudent_identidade_x4)
summary(tStudent_identidade_x134)
summary(tStudent_identidade_xcs134)

#x3 significativa
#x1 e x4 n?o s?o significativas


#plot with cs(x3)

wp(tStudent_identidade_x3,ylim.all = 2)

term.plot(tStudent_identidade_x3,what = "mu",term = 3,
          col.shaded="royalblue",
          col.term = "black",
          xlabs="Log_neutra",col.se="red",ylabs="partial for cs(log_neutra)",
          lwd.term = 2,lwd.shaded = 2)


#plot with sexo
term.plot(tStudent_identidade_x3,what = "mu",term = 2,
          col.shaded="royalblue",
          col.term = "black",
          xlabs="Genero",col.se="red",ylabs="partial for sexo",
          lwd.term = 2,lwd.shaded = 2)


#plot with cs(x4)
term.plot(tStudent_identidade_x3,what = "mu",term = 1,
          col.shaded="royalblue",
          col.term = "black",
          xlabs="Vacuna",col.se="red",ylabs="partial for vacuna",
          lwd.term = 2,lwd.shaded = 2)
          
# 
v4 <- ggplot(dados1, aes(x3, y_)) +
  geom_point(size = 3) +
  geom_smooth(se = FALSE, method = "loess", col = "red", size = 2) + 
  theme(
    axis.text.x = element_text(size = 16, face = "plain"),
    axis.text.y = element_text(size = 16, face = "plain"),  
    axis.title.x = element_text(size = 20, face = "plain"),
    axis.title.y = element_text(size = 20, face = "plain")
  ) +
  xlab("Log Neutra")

v4

# plot fitted term for x3
N <- 69
plot(smooth.spline(x3,fitted(tStudent_identidade_x3)),
     type="l",col="firebrick",lwd=4,
     xlab="t3",
     ylab="Fitted term for t3",main="", cex.lab=1.2)
for(i in 2:N){
  lines(smooth.spline(x3,fitted(tStudent_identidade_x3,term=i)),
                                type="l",col="firebrick",lwd=4)
                      }
curve((sin(x3))^2+5,col="gray83",lwd=4)
legend("topleft",lty=c(1,1),lwd=c(3,3),
       c("True","Normal"),col=c("gray83","firebrick"),
       bty="n",cex=1.2)
     
          
          
Res.q2 <- tStudent_identidade_x3$residuals
Res.qo2 <- sort(abs(Res.q2))
          
          
# plot ajuste cs(x3)
plot(x3,y_,pch=19,main="",col="royalblue4",
     xlab="Measured height",
     ylab="Measured weight")
lines(smooth.spline(x3,fitted(tStudent_identidade_x3)),
      type="l",col="red",lwd=4)


#grafico de res?duos

Res.q2 <- tStudent_identidade_x3$residuals
Res.qo2 <- sort(abs(Res.q2))

index <- 1:length(dados1$log_iga)

#x11()
plot(Res.q2,pch=1,col="red2",lwd=4,
     ylab="Quantile residuals",xlab="Index",
     main ="",ylim = c(-5,5),cex.lab=1.2)
abline(h=-3,lwd=4,lty=2,col="1")
abline(h=0,lwd=2,lty=1,col="4")
abline(h=3,lwd=4,lty=2,col="1")

#grafico de quantis
B <- 500  # Número de simulaciones
n <- length(y_)
j <- 1
iter <- 0
Res.q <- tStudent_identidade_x3$residuals
Res.qo <- sort(Res.q)

set.seed(123)
mrq <- matrix(0, B, n)


mrq2 <- t(apply(mrq, 1, sort))
Z <- qnorm((1:n - 3/8) / (n + 1/4))
rqm <- apply(mrq2, 2, mean)
rqmin <- apply(mrq2, 2, min)
rqmax <- apply(mrq2, 2, max)
mrq3 <- cbind(Z, Res.qo, rqmin, rqm, rqmax)

(res.out <- sum(c(sum(Res.qo>=rqmax),sum(Res.qo<=rqmin))))
(per.out <- round(res.out/n*100,2))



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

