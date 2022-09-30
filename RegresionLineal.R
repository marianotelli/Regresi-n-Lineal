####################
setwd("~/DS_Course/Proyecto_Personal")
library(readxl)
library(readr)
library(palmerpenguins) # supuestos
x <- read_csv("Datos_Agencia.csv")
library(dplyr)
library(psych)
library(ggplot2)
library(ggfortify)
library(dplyr)
library(scales) # para evitar la notación cientifica
library(MASS)    # Evaluar Normalidad 
library(caTools)
library(GGally)
library(car)
library(lmtest) #Breusch-Pagan Test Para la Homoscedasticidad
library(lawstat)

## Queremos hacer una función de regresión que calcule los Profits (Ganancias)
# que tiene una empresa al invertir en diferentes tipos de publicidad que son:
# "Print Media Expenses" (gastos en media impresa), "Social Media Expenses" (gastos en redes sociales)
# y "Outdoor Ad Expenses" (gastos en publicidad al aire libre)


# Correlaciones entre variables
ggpairs(x)
cor(x)
# Correlación alta entre la variable "Print Media Expenses" (gastos en medios impresos)
# y los Profits (Ganancias). Asimismo, tenemos alta en "Outdoor Ad Expenses" (gastos en anuncios al aire libre) y las ganancias.
# Gastos al aire libre también tiene una correlación alta con Medios impresos.
# La correlación entre "Social Media Expenses" (gastos en redes sociales) y las ganancias es casi nula a

## Resumen de los datos
summary(x)



## Datos atípicos
boxplot(x$Profit, horizontal = TRUE)
boxplot(x$`Print Media Expenses`, horizontal = TRUE)
boxplot(x$`Outdoor Ad Expenses`, horizontal = TRUE)
boxplot(x$`Social Media Expenses`, horizontal = TRUE)

stripchart(x$Profit, method = "jitter", pch = 19, add = TRUE, col = "blue")



#HISTOGRRMA####################
#######################HISTOGRAMAS
# Calculando el número de barras como la función hist()
nbreaks <- pretty(range(x$Profit), n = nclass.Sturges(x$Profit),
                  min.n = 1)

### Histograma con Distribución empírico
ggplot(data.frame(x$Profit), aes(x = x$Profit)) + # Convertimos a DF nuestra variable Ingresos
  geom_histogram(aes(y = ..density..), breaks = nbreaks,
                 color = "firebrick", fill = "#FF9999") +
  geom_density(fill = "lightblue", alpha = 0.2) +
  labs(title = 'Nivel de Ingresos',
     x = 'Ingresos',
     y = 'Conteos',
     subtitle = 'Distribución normal y empírica',
     caption = 'Por: @Marianotelli') +
  stat_function(fun = dnorm, 
                args = list(mean = mean(x$Profit), 
                            sd = sd(x$Profit))) +
  scale_x_continuous(labels = comma) + 
  scale_y_continuous(labels = comma) # función que nos quita la notación científica en Y


#######PROBANDO MODELOS##################


modelo1 <- lm(Profit ~ `Print Media Expenses` + `Social Media Expenses`+
                `Outdoor Ad Expenses`, data = x)
summary(modelo1)

## En este modelo que calcula las ganancias, los únicos coeficientes o betas significativos
# son los gastos en medios impresos y el intercepto, es decir, la ordenada al origen.
# Tenemos un R^2 muy grande de 0.95, lo cual nos dice que el modelo explica un 95% a la variable real.
# La interpretación de estos coeficientes es que, cuando no invertimos en ningún tipo de publicidad, nuestras
# ganancias son de $50,120. Por otro lado, Gastos de medios impresos tienen una pendiente de 0.8057.
# Por su parte, los gastos en redes sociales, llevan una relación inversa, debido a que si todo se mantiene constante, 
# por cada unidad que se agregue en este rubro, disminuyen en 0.02682 las ganancias.
# Y por cada unidad invertida en gastos en anuncios al aire libre, nuestro Beneficio (Profit) aumenta en 0.02723

## Para intentar un modelo múltiple, corrimos una vez más el modelo con la variable de gastos en medios impresos
# e incluimos los Gastos de publicidad al aire libre debido a su pvalue de 0.1 para ver si puede ajustarse mejor
modelo2 <- lm(Profit ~ `Print Media Expenses` +
                `Outdoor Ad Expenses`, data = x)
summary(modelo2)
## El resultado de este segundo modelo nos arroja un pvalue más pequeño para Gastos de publicidad al aire libre
# sin embargo, sigue sin ser estadísticamente significativo.Su R^2 de 0.95 es similar al primer modelo

## Por último, se hará el modelo con solo una variable si es el más óptimo. La variable
# es Gastos en medios impresos.
modelo3 <- lm(Profit ~ `Print Media Expenses`
              , data = x)
summary(modelo3)

## Tenemos que con una variable significativa explicamos el 94% de nuestra variable independiente.
# Igualmente, apelando al principio de parsimonia, podemos adoptar este modelo



#########SUPUESTOS DE UNA REGRESIÓN##########
# Linealidad
# Normalidad
# Homoedasticidad
# Independencia


## linealidad: queremos residuos cercanos a 0
mean(modelo3$residuals)

plot(modelo3,1) #mientras más cerca de 0 esté la linea roja, más lineal será el modelo
autoplot(modelo3,1) # Residuales vs Valores Ajustes

## normalidad
res_st <- studres(modelo3) # saca la dist. de los residuos studentizada
shapiro.test(res_st) # no encontramos diferencias significativas con la distr. Norm. y por lo tanto se cumple nuestro supuesto.
hist(res_st)
# Si el valor p es menor que α = .05, hay evidencia suficiente para decir que 
# la muestra no proviene de una población con distribución normal.

# Gráficas de normalidad
plot(modelo2,2) # vemos que el dato No. 50 está muy fuera de la línea del mismo modo que el 46 y 15
hist(res_st, freq = F,
     main = "Distribución Studentizada de los Residuos")
xfit <- seq(min(res_st),max(res_st), length = 60)
yfit <- dnorm(xfit)
lines(xfit, yfit)
## Podemos notar una asimetría a la izquierda de la curva normal.
# Este supuesto no se cumple, motivo por el cual nuestros predictores pueden estar sesgados.

## Homocedasticidad
# Usaremos el el test de la puntuación de la varianza NO constante 
# (Non-constant Variance Score Test)
ncvTest(modelo3)
# p value = 0.019 <0.05. No hay homocedasticidad.
# El test de Breusch-Pagan para la Homoscedasticidad
bptest(modelo3)
# gráfica
plot(modelo2,3) # los datos no presentan ningún patrón
autoplot(modelo2,3)


# independencia
lawstat::runs.test(modelo3$residuals) # p value > 0.05 señala independencia

plot(modelo3$residuals) # esperamos NO ver un patrón en los residuos

# En conclusión, tenemos una función de regresión dada por
# Profits = 8.543e-01(Print Media Expenses) + 4.903e+04 