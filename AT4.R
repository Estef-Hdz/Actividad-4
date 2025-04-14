library(readr)
salarios_mujeres <- read_csv("Datasets/A2/salarios_mujeres.csv")
View(salarios_mujeres)


archivo_csv <- "salarios_mujeres.csv"
salarios_mujeres <- read.csv(archivo_csv)

#Visualización de primeras filas "head()"
print("Primeras 6 filas del dataset:")
head(salarios_mujeres)

#Revisión de estructura "str()"
print("Información general del dataset:")
str(salarios_mujeres)

print("Dimensiones del dataset (filas x columnas):")
dim(salarios_mujeres)

print("Nombres de las columnas:")
names(salarios_mujeres)

print("Resumen estadístico de las variables numéricas:")
summary(salarios_mujeres)


install.packages("dplyr")
library(dplyr)

#Aplicación de filtro por categoría 'categoria' es igual a "Edad"
datos_filtrados <- salarios_mujeres %>%
  filter(Edad == "35")

print(datos_filtrados)

datos_filtrados <- salarios_mujeres %>%
  filter(Salario == "4090")

print(datos_filtrados)


#Presencia de valores faltantes "NA"

# Ver cuántos valores faltantes hay por columna
print("Número de valores NA por columna:")
colSums(is.na(salarios_mujeres))

# Mostrar las filas con valores faltantes
print("Filas con valores NA:")
datos_filtrados[!complete.cases(salarios_mujeres), ]

#Identificar los NA y reemplazarlos en 

salarios_mujeres$Edad[is.na(salarios_mujeres$Edad)] <- "Desconocido"
salarios_mujeres

salarios_mujeres$Salario[is.na(salarios_mujeres$Salario)] <- "Desconocido"
salarios_mujeres

salarios_mujeres$Genero[is.na(salarios_mujeres$Genero)] <- "Desconocido"
salarios_mujeres

#Registros repetidos 
duplicados <- salarios_mujeres[duplicated(salarios_mujeres),]
print(duplicados)

library(readr)
salarios_mujeres <- read_csv("Datasets/A2/salarios_mujeres.csv")
View(salarios_mujeres)

# Calcular la media
media <- mean(salarios_mujeres$Edad)
print(paste("Media: ", media))

# Calcular la mediana
mediana <- median(salarios_mujeres$Edad)
print(paste("Mediana: ", mediana))

# Función para calcular la moda
moda <- function(x) {
  uniq_x <- unique(x) 
  tab <- table(x)     
  mode <- uniq_x[which.max(tab)]  
  return(mode)
}

# Calcular la moda
moda_resultado <- moda(salarios_mujeres$Edad)
print(paste("Moda: ", moda_resultado))

# Verificar el tipo actual de la columna
str(salarios_mujeres$Edad)

salarios_mujeres$Edad <- as.numeric(as.character(salarios_mujeres$Edad))

str(salarios_mujeres$Edad)

#Histograma
#ver la distribución 

hist(salarios_mujeres$Edad,
     main = "Histograma de Salario por Edad",
     xlab = "Edad",
     ylab = "Salario",
     col= "purple", las =2)

install.packages("ggplot2")
library(ggplot2)
library(readr)
colnames(salarios_mujeres)

ggplot(salarios_mujeres, aes(x = Salario)) +
  geom_histogram(binwidth = 3500, fill = "lightpink", color = "lightgreen", alpha = 0.5) +
  labs(title = "Frecuencia de los salarios",
       x = "Salario",
       y = "Frecuencia") +
  theme_gray()

ggplot(salarios_mujeres, aes(x = Edad)) +
  geom_histogram(fill = "lightpink", color = "lightblue", alpha = 0.5) +
  labs(title = "Frecuencia de edad",
       x = "Edad",
       y = "Frecuencia") +
  theme_gray()
