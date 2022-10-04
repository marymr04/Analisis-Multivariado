#GRAPHICAL TECHNIQUES

#FIGURE 1.1 

variable1=c(3,4,2,6,8,2,5)
variable1
variable2= c(5, 5.5, 4, 7, 10, 5, 7.5)
variable2

plot(variable1, variable2)

#FIGURE 1.2

variable3=c(5,4,6,2,2,8,3)
variable3
variable4= c(5, 5.5, 4, 7, 10, 5, 7.5)
variable4

plot(variable3, variable4)

#EJERCICIO 1.1

#CONSIDERANDO LA FIGURA 1.1

mean(variable1)
var(variable1)
mean(variable2)
var(variable2)
cov(variable1,variable2)

#EJERCICIO 1.2

x1= c(1,2,3,3,4,5,6,8,9,11)
x2= c(18.95, 19, 17.95, 15.54, 14, 12.95, 8.94, 7.49, 6, 3.99)

#a) Construye un grafico de dispersion de los datos y los diagramas de puntos marginales

plot(x1,x2)

#b) infiera el signo de la covarianza de la muestra del grafico de dispersion 

-cov(x1,x2)

