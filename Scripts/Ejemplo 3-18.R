# Ejemplo de RLM con varios tipos de preguntas

# Se tienen los resultados de cuatro pruebas para la selección de 
# personal (X1; X2; X3; X4) que fueron aplicadas a un grupo de 20 
# aspirantes a cargos administrativos en una agencia gubernamental. 
# Para el propósito del estudio, se aceptaron a todos los aspirantes 
# para 20 cargos independiente de los resultados en las pruebas. 
# Después de 2 años desde la contratación estos empleados fueron 
# evaluados mediante un puntaje de la aptitud exhibida
# para el trabajo (Y)

# Para cargar las funciones del curso
source("https://raw.githubusercontent.com/fhernanb/Repo_curso_estadistica_2/main/Funciones/funciones.R")

# Para cargar los datos
aptitud <- read.csv("https://raw.githubusercontent.com/fhernanb/Repo_curso_estadistica_2/main/Datos/Ex01Ch03.csv")
aptitud
attach(aptitud)

# Creación de matriz de gráficas de dispersión entre todas las variables
# del objeto "aptitud" que incluye las correlaciones .
pairs(aptitud, upper.panel=panel.smooth, lower.panel=myPanel.cor)

# Gráfica de dispersión 3D para la Respuesta vs. Prueba 1 y Prueba 2
library(car)
scatter3d(punt.Aptitud ~ Prueba1 + Prueba2, fov=60, revolutions=1)
# Gráfica de dispersión 3D para la Respuesta vs. Prueba 3 y Prueba 4
scatter3d(punt.Aptitud ~ Prueba3 + Prueba4, fov=60, revolutions=1)

# Matriz de correlaciones para las variables del ejemplo de aptitud laboral
cor(aptitud)

# Ajuste del modelo
modelo <- lm(punt.Aptitud ~ Prueba1 + Prueba2 + Prueba3 + Prueba4 , 
             data=aptitud )

summary(modelo)

# Para obtener todas las regresiones posibles
library(leaps)
myAllRegTable(modelo)

# Para realizar seleccion de variables hacia adelante
myStepwise(full.model=modelo, alpha.to.enter=0.05, alpha.to.leave=1)

