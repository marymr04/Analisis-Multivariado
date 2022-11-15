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


#CLASE 27 DE SEPTIEMBRE

#DIAGRAMA DE DISPERSION

mlab="Empresas manufactureras con 20 empleados o mas"
plab="Tamaño de la poblacion en miles(censo 1970"
plot(popul~manu, data = USairpollution, xlab=mlab, ylab=plab)
x11()

# Lo que se ve en la grafica la mayoria se concentra en 500 empleados por empresa

#DIAGRAMA DE CAJA BIVARIADA

#con la densidad podemos afirmar un outlier para una variables aleatoria y graficarlo

rug(USairpollution$manu, side = 1)
rug(USairpollution$popul, side = 2)

#histograma
hist(USairpollution$manu, main="")

#Boxplot
boxplot(USairpollution$popul)
#Dispersion y densidades marginales
layout(matrix(c(2,0,1,3), nrow=2, byrow=TRUE), widths=c(2,1), heights = c(1,2),respect=TRUE)
#?layout
xlim=range(USairpollution$manu)* 1.1
xlim
xlim2=with(USairpollution, range(manu))*1.1
#?with
xlim2
plot(popul~manu, data = USairpollution, cex.lab=0.9, xlab=mlab, ylab=plab, type="n", xlim=xlim)
with(USairpollution, text(manu, popul, cex=0.6, labels=abbreviate(row.names(USairpollution))))

#CLASE DE 3 DE OCTUBRE

hist(USairpollution$manu, main="", xlim=xlim)
boxplot(USairpollution$popul)
## DIAGRAMA DE CAJA BIVARIADO (metodo de outliers 1)
lab=c("Chicago","Detroit","Cleveland","Philadelphia","Houston")
outcity=match(lab, rownames(USairpollution))
x=USairpollution[,c("manu","popul")]
x
bvbox(x, mtitle = "",xlab=mlab, ylab=plab)
text(x$manu[outcity],x$popul[outcity], labels=lab, cex=0.7, pos=c(2,2,4,2,2))
#la bisagra concentra al menos 50% de los datos 
# la elipse de afuera se llama valla, la de adentro bisagra
# las lineas (libreta)

#CORRELACION SIN LOS OUTLIERS

cor(USairpollution)
with(USairpollution, cor(manu,popul))
outcity1= match(c("Chicago","Detroit","Cleveland","Philadelphia"),rownames(USairpollution))
with(USairpollution, cor(manu[-outcity1], popul[-outcity1]))

#El convex hull de datos bivariados (metodo de outliers 2)

hull=with(USairpollution, chull(manu,popul))
hull
with(USairpollution, plot(manu,popul,pch=1, xlab=mlab, ylab=plab))
with(USairpollution,polygon(manu[hull],popul[hull], density=15,angle=30))
with(USairpollution, cor(manu[-hull], popul[-hull]))

#4 de octubre

#scatterplot matrix

pairs(USairpollution, pch=".", cex=3)
#se interpreta con diagramas de dispersion, primero se notan los outliers, ver si la correlacion es positiva y negativa,
#en el trabajo se debe checar como se comporta el triangulo superior e inferior, y variables dependientes o independientes
pairs(USairpollution, panel= function(x,y, ...){
  points(x,y, ...)
  abline(lm(y~x),col="blue")
}, pch=".", cex=3)
#se observa SO2 contra manu y popul
# existe relacion lineal entre manu y popul, la forma en la que predicen S02
#predicen SO2 de la misma manera manu y popul, cualquiera de las dos las predice de la misma manera.


#Clase 10 de octubre

#Analisis de componentes principales
#Obtener pocas variables
#Graficar o resumir los datos

### Cual es la mejor manera de contruir un indice informativo
### del rendimiento general (prueba-examen)?

# por el promedio ponderado se puede evitar la estandarizacion 
# Uso comun de analisis de CP
# Representacion grafica o algun otro analisis (analisis de regresion(apunte en la libreta))

library("MVA")
demo("Ch-PCA")
blood_corr
blood_sd
blood_pcacov=princomp(covmat=blood_cov)
summary(blood_pcacov, loadings = TRUE)
blood_pcacor=princomp(covmat=blood_corr)
summary(blood_pcacor, loadings = TRUE)

headsize
head= headsize[,c("head1","head2")]
head
summary(head)
x11()
plot(head)
cor(head)
cov(head)
pairs(head, panel = function(x, y, ...){
  points(x, y, ...)
  abline( lm(y ~ x), col="red")
}, pch = ".", cex=5)

head_pcacov=princomp(covmat=cov(head))
summary(head_pcacov, loadings = TRUE)
head_pcacor=princomp(covmat=cor(head))
summary(head_pcacor, loadings = TRUE)

#Para graficar los componentes principales
#y1=0.71x1+0.71x2
#y2=0.71x1-0.71x2

?abline

cor(head)
#correlacion positiva
boxplot(head)


#CLASE 11 DE OCTUBRE

#Contaminacion del aire en ciudades de los EEUU

#Analizar varios aspectos de la contaminacion (SO2, TEMP,...)
#PCA: Abordar los factores determinantes de la contaminacion 

#SO2: contenido de SO2 en el aire en microgramos m3

#temp: Temperatura media anual en grados farenheit
#manu: numero de empresas manufactureras que emplean a 20 o mas trabajadores
#popul: tamaño de la poblacion (censo 1970) en miles
#wind: velocidad media anual del viento en millas/hora
#precip: precipitacion media anual en pulgadas
#predays: promedio de dias con precipitacion al año 

### De las 6 variables, 2 se relacionan con factores humanos y 4 con el clima

library("MVA")
dfsinso2= USairpollution[,-1]
dfsinso2
#analisis de componentes principales
usair_pca=princomp(dfsinso2, cor=TRUE)
summary(usair_pca, loadings = TRUE)
#y1=0.33temp-.612manu-.578popul...
#Var(y1)=1.482^2 = lambda1
#Proporcion 
#1) p1= lambda1/lambda1+...+ =.366
#Y1= e´X
# e´= [0.330, -0.612]
# solo se toman los componentes principales cuyos eigenvalores sean mayores o iguales a 1

#NOTA: podemos vernos tentados a buscar una interpretacion de los componentes que les permita ser eqtiquetados en algun sentido
# que les permita "etiquetados" en algun sentido

#Restriccion: a´a= 1 todos los componentes se elvan al cuadrado y deben dar 1

#Para las Y y la interpretacion se toman los valores mas grandes sin importar el signo 

#Y1 puede considerarse como un indice de "calidad de vida" 

#y2 "clima humedo" --- se relaciona con la cantidad de lluvia en una ciudad
#con altos coeficientes en precip y predays

#y3 = "tipo de clima" --- contraste entre precip y temp, separa a las ciudades
#que tienen temperaturas altas y mucha lluvia de las que son mas frias o mas secas

### ******* ADVERTENCIA (Peligros de la sobreinterpretacion)
# 1. No existe ningun metodo matematico diseñado para resultados fisicos significativos
# 2. Si una expresion tiene significado fisico obvio debe atribuirse a un cambio afortunado
# afortunado o al hecho de que los datos tienen una estructura fuertemente marcado 

# si no nos importa etiquetar a los componentes principales, aun pueden usarse
# como base de varias presentaciones graficas (de las ciudades)

x11()
pairs(usair_pca$scores[,1:3], ylim=c(-6,4), xlim=c(-6,4), panel=function(x,y,...){
  text(x,y, abbreviate(row.names(USairpollution)),cex=0.6)
  bvbox(cbind(x,y), add= TRUE)
})
#scores=los puntajes que tiene cada variable en los componentes principales
usair_pca$scores

#CLASE 7 DE NOVIEMBRE

library("MVA")
demo("Ch-EFA")
life

#Los datos muestran la esperanza de vida en años
#por pais edad y sexo para 1960

sapply(1:3, function(f) factanal(life, factors=f, method="mle")$ PVAL)
factanal(life, factors=3, method="mle")
#El primer factor esta dominado por la esperanza de vida de un individuo al nacer
#(tanto en hombres como en mujeres)
# f1: "fuerza de vida al nacer"
# el segundo factor refleja la esperanza de vida en edades mas avanzadas
# f2: "Fuerza de vida en las personas mayores"
# el tercer factor tiene cargas mas altas para las expectativas de vida de los hombres
# de 50 y 75 años
# f3: "fuerza de vida para hombres mayores de 50 años"

#14 DE NOVIEMBRE

scores= factanal(life, factors=3, method="mle", scores="regression")$scores
scores

pairs(scores, panel= function(x,y, ...){
  points(x,y, ...)
  abline(lm(y~x),col="blue")
}, pch=".", cex=5)

scores2= as.data.frame(scores)
plot(Factor2~Factor1, data = scores2, xlab="Factor1", ylab="Factor2")
plot(Factor2~Factor1, data = scores2, cex.lab=0.9, xlab="Factor1", ylab="Factor2", type="n")
text(scores2$Factor1, scores2$Factor2, cex=.8, labels=abbreviate(row.names(scores2)))
#la fuerza de vida al nacer de camerun y madagascar es menor a la de usa y canada

plot(Factor3~Factor2, data = scores2, xlab="Factor2", ylab="Factor3")
plot(Factor3~Factor2, data = scores2, cex.lab=0.9, xlab="Factor2", ylab="Factor3", type="n")
text(scores2$Factor2, scores2$Factor3, cex=.8, labels=abbreviate(row.names(scores2)))

plot(Factor3~Factor1, data = scores2, xlab="Factor1", ylab="Factor3")
plot(Factor3~Factor1, data = scores2, cex.lab=0.9, xlab="Factor1", ylab="Factor3", type="n")
text(scores2$Factor1, scores2$Factor3, cex=.8, labels=abbreviate(row.names(scores2)))

druguse
sapply(1:7, function(nf) factanal(covmat=druguse, factors=nf, method="mle", n.obs=1634)$ PVAL)
#se toman si son menores a .05
factanal(covmat=druguse, factors=6, method="mle")
#F1 = USO DE DROGAS SOCIALES (VINO, CERVEZA, LICOR , CIGARROS Y MARIHUANA)
#F2 = USO DROGAS DURAS (TRANQUILIZANTES, COCAINA Y HEROINA)
#F3 = USO DE ANPHETAMINAS
#F4 = USO DE HASHISH
#NOS QUEDAMOS HASTA EL 4 PORQUE EN EL 5 SE REPITE MARIHUANA 




