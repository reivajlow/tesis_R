rm(list=ls())
load("Aplicacion_thomas_javier_log_iga.RData")
library(ggplot2)
library(gridExtra)
library(gamlss)
dados <- datos
y <- y_
dados$Forested<- as.factor(dados$Forested)
dados$FieldpH<- as.numeric(dados$FieldpH)
dados$Temperature<- as.numeric(dados$Temperature)

v1 <- ggplot(dados, aes(x = y_)) +  # Agrega aes() alrededor de y_
  geom_histogram(bins = 11, aes(y = ..density..), fill = "steelblue") +
  theme(
    axis.text.x = element_text(size = 16, face = "plain"),
    axis.text.y = element_text(size = 16, face = "plain"),
    axis.title.x = element_text(size = 20, face = "plain"),
    axis.title.y = element_text(size = 20, face = "plain")
  ) +
  stat_function(
    fun = dNO,
    args = list(mu = mean(y_), sigma = sd(y_)),  # Utiliza y_ en lugar de y
    col = "black",
    size = 1
  ) +
  xlim(c(3, 11))

v1

v2 <- ggplot(dados, aes(x2, y_)) +
  geom_boxplot(fill = "steelblue") +
  theme(
    axis.text.x = element_text(size = 16, face = "plain"),
    axis.text.y = element_text(size = 16, face = "plain"),  
    axis.title.x = element_text(size = 20, face = "plain"),
    axis.title.y = element_text(size = 20, face = "plain")
  )

v3 <- ggplot(dados, aes(x2, y_)) +
  geom_point(size = 3) +
  geom_smooth(se = FALSE, aes(group = 1), col = "red", size = 2) +  # Agrega aes(group = 1)
  theme(
    axis.text.x = element_text(size = 16, face = "plain"),
    axis.text.y = element_text(size = 16, face = "plain"),  
    axis.title.x = element_text(size = 20, face = "plain"),
    axis.title.y = element_text(size = 20, face = "plain")
  )

v4 <- ggplot(dados, aes(x3, y_)) +
  geom_point(size = 3) +
  geom_smooth(se = FALSE, aes(group = 1), col = "red", size = 2) +  # Agrega aes(group = 1)
  theme(
    axis.text.x = element_text(size = 16, face = "plain"),
    axis.text.y = element_text(size = 16, face = "plain"),  
    axis.title.x = element_text(size = 20, face = "plain"),
    axis.title.y = element_text(size = 20, face = "plain")
  )

v5 <- ggplot(dados, aes(x4, y_)) +
  geom_point(size = 3) +
  geom_smooth(se = FALSE, col = "red", size = 2, method = "lm") +
  theme(
    axis.text.x = element_text(size = 16, face = "plain"),
    axis.text.y = element_text(size = 16, face = "plain"),  
    axis.title.x = element_text(size = 20, face = "plain"),
    axis.title.y = element_text(size = 20, face = "plain")
  ) +
  xlab("Days")

grid.arrange(v2, v3, v5, v4)

