install.packages("MVA")  #1
library("MVA")#2
x11()#3
demo("Ch-MVA")#4
install.packages("mvtnorm")
hypo
class("hypo")
class(hypo)
hypo[1:5,"health"]
hypo[,c("sex","age")]
x=hypo[,c("sex","age")]
x
hypo["IQ"]
class(hypo["IQ"])
#Tarea: como trabajar con los NA y con ejemplo.
#CONJUNTO DE DATOS MULTIVARIADOS
measure #5
#pregunta 1: como hacer que todas las medidas se hagan una: Combinaciones lineales
#(componentes principales)
#Y1= C1X1+C2X2+C3X3
#hay subtipos de formas corporales entre los hombres y mujeres dentro de las cuales
#los individuos tienen formas similares y entre las cualeslas formas corporales difieran
#para responder necesitamos el analisis cluster
pottery #6
#los perfiles quimicos sugieren diferentes tipos de ollas y  si alguno de estos tipos de ollas y si alguno de estos tipos esta
#relacionado con el horno de la region?
exam #7
#los puntajes de los examenes reflejan algun rasgo subycente en un estudiante que no se puede medir directamente
USairpollution #7
measure[,c()]
x=measure[,c("chest","waist","hips")]
x
cov(x)
var(x)
xfemale=subset(measure,gender=="female")
xfemale
xfemale=subset(measure,gender=="female")[,c("chest","waist","hips")]
xfemale1
cov(xfemale1)
xmale=subset(measure,gender=="male")[,c("chest","waist","hips")]
xmale
cov(xmale)
cor(xfemale1)
cor(x)
cor(xmale)
scale(x,center=FALSE)#aqui se estandariza
dist(scale(x,center=FALSE))
