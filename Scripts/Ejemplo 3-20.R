# Ejemplo del almacen para RL con variables cualitativas

# Para cargar las funciones del curso
source("https://raw.githubusercontent.com/fhernanb/Repo_curso_estadistica_2/main/Funciones/funciones.R")

# Para cargar los datos
almacen <- read.csv("https://raw.githubusercontent.com/fhernanb/Repo_curso_estadistica_2/main/Datos/Ex02Ch03.csv")
almacen

# Gráfica de dispersión Publicidad vs. Ventas según Sección
library(ggplot2)
ggplot(almacen, aes(x=Publicidad, y=Ventas, color=Seccion)) +
  geom_point(cex=2)

# Modelo general
# rectas que difieren tanto en pendiente como en intercepto
modelo1 <- lm(Ventas ~ Publicidad * Seccion, data=almacen)
summary(modelo1)

# Modelo general
# rectas que difieren tanto en pendiente como en intercepto
almacen$Seccion <- with (almacen, relevel(as.factor(Seccion), ref="C"))
modelo2 <- lm(Ventas ~ Publicidad * Seccion, data=almacen)
summary(modelo2)

# Análisis de residuales para el modelo 2
par(mfrow=c(2, 2))

# Gráfica de residuales estudentizados vs. Valores de Publicidad observados
with(almacen, plot(Publicidad, Ventas, 
                   ylab="Ventas(miles de dólares)",
                   xlab="Publicidad(cientos de dólares)", 
                   col=as.factor(Seccion), lwd=2,
                   pch=as.integer(factor(Seccion)), 
                   cex=2, cex.lab=1.5, las=1))
legend("topleft", legend=c("A", "B", "C"), pch=1:3, col=1:3)

# Gráfica de residuales estudentizados vs. Valores de Sección
with (almacen, plot(as.integer(Seccion), rstudent(modelo2), 
                    pch=as.integer(Seccion), 
                    xlab=" Sección", 
                    ylab=" Studentized Residuals", cex=2, 
                    col=Seccion, xaxt="n", cex.lab=1.5, 
                    las=1, lwd=2))
abline (h=0)
axis(1, at=c(1, 2, 3), c("A", "B", "C"), cex=1.5)

# Gráfica de residuales estudentizados vs. Valores de Publicidad ajustados
with(almacen, plot(fitted(modelo2), rstudent(modelo2), 
                   pch=as.integer(Seccion), 
                   xlab="Adjusted Values ", 
                   ylab="Studentized Residuals", 
                   cex=2, col=Seccion, cex.lab=1.5, las=1, lwd=2))
abline (h=0)

# Gráfica de cuantiles normales con resultados de la prueba de normalidad
with(almacen, 
     myQQnorm(modelo2, student=T, cex=2, cex.lab=1.5, 
              pch=as.integer(Seccion), col=Seccion, lwd=2))


# Prueba para determinar si la recta de ventas vs. publicidad
# es diferente para cada sección
car::linearHypothesis(model=modelo2, 
                      hypo=c("SeccionA = 0", 
                             "SeccionB = 0",
                             "Publicidad:SeccionA = 0",
                             "Publicidad:SeccionB = 0"))

# Para crear la matriz X del modelo 2
matrizXmodelo2 <- model.matrix(object=Ventas ~ Publicidad * Seccion, data=almacen)
matrizXmodelo2

modelo2a <- lm(Ventas ~ Publicidad + SeccionA + Publicidad:SeccionB , 
               data = data.frame(Ventas=almacen$Ventas, matrizXmodelo2))
summary(modelo2a)


