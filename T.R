# load data
library(readxl)
datos_from_py <- read_excel("datos_from_py.xlsx")

#revision datos
datos <- datos_from_py

#revision NA
summary(datos)
str(datos)

# convertir a factor
datos$sexo <- as.factor(datos$sexo)
datos$hta <- as.factor(datos$hta)
datos$grp_et <- as.factor(datos$grp_et)

# delete NA
datos <- na.omit(datos)
