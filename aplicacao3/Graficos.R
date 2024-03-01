rm(list=ls())
load("Aplicacion_thomas_javier_log_iga.RData")
library(ggplot2)
library(gridExtra)
library(gamlss)
dados <- datos
y <- y_


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
  xlim(c(3, 11)) +
  xlab("Log IGG")


v1

v2 <- ggplot(dados, aes(x2, y_)) +
  geom_boxplot(fill = "steelblue") +
  theme(
    axis.text.x = element_text(size = 16, face = "plain"),
    axis.text.y = element_text(size = 16, face = "plain"),  
    axis.title.x = element_text(size = 20, face = "plain"),
    axis.title.y = element_text(size = 20, face = "plain")
  )+
  xlab("Categoria Vacuna")

v3 <- ggplot(dados, aes(x1, y_)) +
  geom_point(size = 3) +
  geom_smooth(se = FALSE, aes(group = 1), col = "red", size = 2) +  # Agrega aes(group = 1)
  theme(
    axis.text.x = element_text(size = 16, face = "plain"),
    axis.text.y = element_text(size = 16, face = "plain"),  
    axis.title.x = element_text(size = 20, face = "plain"),
    axis.title.y = element_text(size = 20, face = "plain")
  ) +
  xlab("IMC")

v4 <- ggplot(dados, aes(x3, y_)) +
  geom_point(size = 3) +
  geom_smooth(se = FALSE, aes(group = 1), col = "red", size = 2) +  # Agrega aes(group = 1)
  theme(
    axis.text.x = element_text(size = 16, face = "plain"),
    axis.text.y = element_text(size = 16, face = "plain"),  
    axis.title.x = element_text(size = 20, face = "plain"),
    axis.title.y = element_text(size = 20, face = "plain")
  ) +
  xlab("Log Neutra")

v5 <- ggplot(dados, aes(x4, y_)) +
  geom_point(size = 3) +
  geom_smooth(se = FALSE, col = "red", size = 2, method = "lm") +
  theme(
    axis.text.x = element_text(size = 16, face = "plain"),
    axis.text.y = element_text(size = 16, face = "plain"),  
    axis.title.x = element_text(size = 20, face = "plain"),
    axis.title.y = element_text(size = 20, face = "plain")
  ) +
  xlab("Edad")

grid.arrange(v2, v3, v5, v4)
