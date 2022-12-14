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
plab="Tama?o de la poblacion en miles(censo 1970"
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
