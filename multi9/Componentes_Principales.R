#####################################################
#                                                   #
#        ANALISIS DE COMPONENTES PRINCIPALES        # 
#              EJEMPLO Postulantes                  #
#   Mg.Sc. Clodomiro Fernando Miranda Villagomez    #
#           cfmiranda@lamolina.edu.pe               #
#                                                   #                     
#####################################################


#---------------------------------------------------------
# Para limpiar el workspace, por si hubiera algun dataset 
# o informacion cargada
rm(list = ls())


###############
#  Paquetes   #
###############

library(car)
library(GGally)
library(MVN)
library(epiDisplay)
library(scales)
library(rgl)
library(psych)
library(hrbrthemes)
library(gganimate)
library(png)
library(gifski)
library (dplyr)
library(viridis)
library(tidyverse)
library(forcats)
library(BSDA)
library(dlookr)
library(ggpubr)
library(summarytools)
library(pastecs)
library(corrplot)
library(mvnormtest)
library(PerformanceAnalytics)
library(ggcorrplot)
library(ade4)
library(factoextra)
library(patchwork)
library (adegenet)
library(FactoMineR)



# Instalar y cargar GGally
install.packages("GGally")
library(GGally)

# Instalar y cargar epiDisplay
install.packages("epiDisplay")
library(epiDisplay)

# Instalar y cargar viridis
install.packages("viridis")
library(viridis)

# Instalar y cargar dlookr
install.packages("dlookr")
library(dlookr)

# Instalar y cargar pastecs
install.packages("pastecs")
library(pastecs)

# Instalar y cargar PerformanceAnalytics
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

# Instalar y cargar ade4
install.packages("ade4")
library(ade4)

# Instalar y cargar factoextra
install.packages("factoextra")
library(factoextra)

# Instalar y cargar adegenet
install.packages("adegenet")
library(adegenet)

# Instalar y cargar FactoMineR
install.packages("FactoMineR")
library(FactoMineR)







####################
# Lectura de datos #
####################

library(foreign)
datos <-read.spss("Postulantes.sav",
                  use.value.labels=TRUE, 
                  to.data.frame=TRUE)

datos1=datos[,-1]


#Verificando si hay datos perdidos
per.miss.col=100*colSums(is.na(datos1))/dim(datos1)[1]
per.miss.col

#Aplicando el criterio de la puntuacion Z (para 
#Observaciones outliers de cada variable)
is_outlier2 <- function(x,k = 2) {
  return(abs(scale(x)) > k)
}

datosP2=datos1[,-10]

datosP2[is_outlier2(datosP2$RV,3),]
datosP2[is_outlier2(datosP2$RM,3),]
datosP2[is_outlier2(datosP2$MAT,3),]
datosP2[is_outlier2(datosP2$PSI,3),]
datosP2[is_outlier2(datosP2$FIS,3),]
datosP2[is_outlier2(datosP2$LOG,3),]
datosP2[is_outlier2(datosP2$BIO,3),]
datosP2[is_outlier2(datosP2$HIS,3),]
datosP2[is_outlier2(datosP2$QUI,3),]

#Outliers multivariados

## sROC 0.1-2 loaded
outliers <- mvn(data = datos1[,-10], mvnTest = "hz", 
                multivariateOutlierMethod = "quan") 


# Grafico de barras y pie para la condicion del postulante

# Gráfico de barras

tabla2=tab1(datos1$CON,cum.percent = TRUE,main="Tabla de Condicion",
            col = c('green2','yellow'))

ppCON=round((prop.table(table(datos1$CON))*100),1)

barplot(ppCON,ylab = "Porcentaje (%)",
        xlab = "Condicion",col=c('green2','yellow'),
        main="Proporción de postulantes por Condicion",
        cex.names =0.75)

# Grafico pie
mi_tabla <-table(datos$CON)
mi_tabla
mi_tabla1 <-prop.table(table(datos$CON))
mi_tabla1
pie(mi_tabla1,col=c("tomato","steelblue"))

#Lo primero es tener el  conjunto de datos con categorias y valores 
# los valores  son porcentajes y deben sumar 100
df<-data.frame(categorias=c("ingreso","noingreso"),
               porcentaje=c(61,39))
# despues de esto  ya podemos  hacer nuestro grafico de pie
ggplot(df,aes(x="",y=porcentaje, fill=categorias))+
  geom_bar(stat = "identity",
           color="white")+
  geom_text(aes(label=porcentaje),
            position=position_stack(vjust=0.5),color="white",size=6)+
  coord_polar(theta = "y")

# grafica de dispersion basica#

head(datos1)
with(datos1, plot3d(RV,RM,MAT, type="s",
                  col=as.integer(datos$CON)))
legend3d("topright", legend = levels(datos$CON), pch = 16, col=as.integer(datos$CON))


plot(datos1[,1:9],pch=as.numeric(datos1$CON),col=datos1$CON)

pairs.panels(datos1[,-10])

pairs(x = datos1[, -10], col = c("firebrick", "green3", "blue")[datos1$CON],
      pch = 20)

# Un scatterplot basico de RM y MAT con el color que depende de la condicion
p1=ggplot(datos1, aes(x=RM, y=MAT)) +
  geom_point(aes(color = CON), size = 4, alpha = 0.7) +theme_ipsum()+
  #geom_point(size=6) +  theme_ipsum()+
  labs(colour="CONDICION")+
  xlab("Razonamiento Matematico")+
  ylab( "Matematicas " )
  
p1

anim1=p1+
  transition_states(CON,
                    transition_length = 2,
                    state_length = 1)+
  ease_aes(y='bounce-out')+
  ggtitle('Ahora Muestra {closest_state}',
          subtitle = 'Frame {frame} of {nframes}')
anim1

# B
ggplot(datos1, aes(x=MAT, y=RM, shape=CON,color=CON,size=CON)) + 
  geom_point() +  theme_ipsum()+
  xlab("Matematica")+
  ylab( "Razonamiento Matematico" )+
  labs(colour="CONDICION")
  
# BOXPLOT

boxplot(datos1$RM~datos1$CON,
        main="Razonamiento Matematico",xlab="Condicion",
        ylab = "Nota",col=c(4,15))

head(datos1)

p2=datos1 %>%
  ggplot( aes(x=datos1$CON, y=datos1$RM, 
              fill=datos1$CON)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="green3", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Boxplot de Razonamiento Matematico") +
  xlab("CONDICION")+
  ylab( "Nota " )

p2

anim2=p2+
  transition_states(
    CON,
    transition_length = 3,
    state_length = 2
  ) +
  enter_fade() +
  exit_shrink() +
  ease_aes('sine-in-out')
anim2

##HISTOGRAMAS

hist(datos1$RM,col="steelblue",main = "RAZONAMIENTO MATEMATICO",
     xlab = "Notas en RM",ylab = "Frecuencia")

plot(density(datos1$RM))
polygon(density(datos1$RM),col='#f8f025',border = 'white')
lines(density(datos1$RM),col='#000000')

multi.hist(x = datos1[,1:9], dcol = c("blue", "red"), dlty = c("dotted", "solid"),
           main = "")


head(datos1)

data <- datos1[,-10]
head(data)
data <- data %>%
  gather(key="text", value="value") %>%
  mutate(text = gsub("\\.", " ",text)) %>%
  mutate(value = round(as.numeric(value),0))

# plot
p <- data %>%
  mutate(text = fct_reorder(text, value)) %>%
  ggplot( aes(x=value, color=text, fill=text)) +
  geom_histogram(alpha=0.6, binwidth = 5) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("") +
  ylab("Assigned Probability (%)") +
  facet_wrap(~text)
p

EDA(datos$RV)

plot_normality(datos,RV,col = "green2")
plot_normality(datos,MAT,col = "violet")
plot_normality(datos,QUI,col = "purple")
?plot_normality

# Histogramas espejo#
p <- datos1 %>%
  ggplot( aes(x=datos1$RM, fill=datos1$CON)) +
  geom_histogram( color="#e9ecef", alpha=0.7, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="CONDICION")+
  xlab("Notas en Razonamiento Matematico")+
  ylab( "Frecuencia de Postulantes" )

p

anim3=p+
  transition_states(
    CON,
    transition_length = 3,
    state_length = 2
  ) +
  enter_fade() +
  exit_shrink() +
  ease_aes('sine-in-out')
anim3

#Exploracion grafica (Densidades)

plot1 <- ggplot(data = datos1, aes(x = RV)) +
  geom_density(aes(colour = CON)) + theme_bw()
plot2 <- ggplot(data = datos1, aes(x = RM)) +
  geom_density(aes(colour = CON)) + theme_bw()
plot3 <- ggplot(data = datos1, aes(x = MAT)) +
  geom_density(aes(colour = CON)) + theme_bw()

ggarrange(plot1, plot2, plot3, common.legend = TRUE, legend = "bottom")


# No considerar la primera ni la columna once: Id y condicion.

datosacp <- datos[,c(-1,-11)]
head(datosacp)
str(datosacp)

########################
# Analisis Descriptivo #
########################

summary(datosacp)

summarytools::descr(datosacp)

X=datosacp$RM
Xbar=mean(datosacp$RM)
Sd=sd(X)
mean(((X-Xbar)/Sd)^3)# Asimetria Muestral
mean(((X-Xbar)/Sd)^4)-3# Curtosis muestral

# Grouped statistics

head(datos1)
with(datos1, stby(RM, CON, descr))

with(datos1, stby(MAT, CON, descr))

describe(datosacp)

round(stat.desc(datosacp),2)
round(stat.desc(datosacp,basic=FALSE),2)

###########################
# Analisis de Correlacion #
###########################

#---------------------------------
# Matriz de Variancia-Covariancia
head(datosacp)
round(cov(datosacp),2)
options(digits = 3)
cov(datosacp)
1.9541545/(8.0235003*18.2783825)^0.5
diag(cov(datosacp))
sum(diag(cov(datosacp)))


#-----------------------------
# Coeficientes de Correlacion 
round(cor(datosacp),3)
diag(cor(datosacp))
sum(diag(cor(datosacp)))

# Prueba estadistica

pairs.panels(datosacp)
corr.test(datosacp)

#----------------------------------------------
# Graficos de Correlacion

i=cor(datosacp,method="pearson")
corrplot(i,sig.level=0.05,type="lower")
?corrplot
corrplot(i,method = "number",order = "original")
corrplot(i,method = "ellipse",order = "original")
corrplot(i,method = "ellipse",order = "original",addCoef.col = "magenta")
corrplot(i,method = "ellipse",order = "original",addCoef.col = "magenta",type = "upper")
corrplot(i,method = "ellipse",order = "original",addCoef.col = "magenta",type = "lower")
corrplot(i,method = "square",order = "original",tl.pos = "d",addCoef.col = "magenta")
corrplot(i,method = "square",order = "original",tl.pos = "d")
corrplot(i,method = "pie",order = "AOE",tl.pos = "d",addCoef.col = "green2")
corrplot(i,method = "circle",order = "FPC",tl.pos = "d",addCoef.col = "green2")
corrplot(i,method = "circle",order = "FPC",tl.pos = "d")
corrplot(i,method = "color",order = "original",tl.pos = "d",addCoef.col = "green2")
corrplot(i,method = "color",order = "original",tl.pos = "d")
corrplot(i,method = "ellipse",order = "AOE",type="upper",tl.pos = "d")
corrplot(i,add=TRUE,type="lower",method = "number",order = "AOE",
         diag = FALSE,tl.pos = "n",cl.pos = "n")

res1=cor.mtest(datosacp,conf.level=0.05)
res1
a=datosacp$RV
b=datosacp$PSI
c=cbind(a,b)
head(c,10)

mshapiro.test(t(c))

corrplot(i, p.mat = res1$p, sig.level = 0.05)
corrplot(i, p.mat = res1$p, order = "hclust", insig = "pch", addrect = 3)
corrplot(i, p.mat = res1$p, insig = "p-value",sig.level = -1)#agrega los pvalores

# Primera forma
col1 <- colorRampPalette(c("#7F0000","red","#FF7F00","yellow","white", 
                           "cyan", "#007FFF", "blue","#00007F"))
corrplot(cor(datosacp),
         title = "Matriz de correlacion", mar=c(0,0,1,0),
         method = "color", outline = T, addgrid.col = "darkgray",
         order = "hclust", addrect = 3, col=col1(100),
         tl.col='black', tl.cex=.75)

# Segunda forma
pairs(datosacp,col="green2")

# Tercera forma

chart.Correlation(datosacp, histogram=TRUE, pch=20)

# Cuarta forma - Mapas de Calor
cor.plot(cor(datosacp),
         main="Mapa de Calor", 
         diag=TRUE,
         show.legend = TRUE)  

data2=datosacp

head(data2)
corr=cor(data2)

## Correlogramas

ggcorrplot(corr) +
  ggtitle("Correlograma de Postulantes") +
  theme_minimal()

ggcorrplot(corr, method = 'circle') +
  ggtitle("Correlograma de Postulantes") +
  theme_minimal()

ggcorrplot(corr, method = 'circle', type = 'lower') +
  ggtitle("Correlograma de Postulantes") +
  theme_minimal()

ggcorrplot(corr, method = 'circle', type = 'lower', lab = TRUE) +
  ggtitle("Correlograma de Postulantes") +
  theme_minimal() +
  theme(legend.position="none")

## Incluyendo pvalores

# Calculando los pvalores
p.mat <- cor_pmat(datosacp)
p.mat

# Agregando la No significacion (X) de las correlaciones
# --------------------------------

ggcorrplot(corr, hc.order = TRUE,
           type = "lower", p.mat = p.mat)

# Dejar en blanco si el coeficiente es no significativo
ggcorrplot(corr, p.mat = p.mat, hc.order = TRUE,
           type = "lower", insig = "blank")

##################################################################
# 1. Analisis de Componentes Principales usando la libreria ade4 #
#                   Matriz de Correlaciones                      #
##################################################################
head(datosacp)

acp <- dudi.pca(datosacp,
                scannf=FALSE, scale=TRUE,
                nf=ncol(datosacp))# Con scale se tipifican las variables
summary(acp)
3.6617/9
acp[["eig"]]
str(acp)
print(acp)
3.6617/9
(3.6617+1.5135)/9
# Valores propios (autovalores)
acp$eig
sum(acp$eig)

inertia.dudi(acp)

# Vectores propios
acp$c1

# Correlaciones entre las variables originales y las componentes principales
acp$co
?cor
# Grafica de Valores propios - ScreePlot

# Primera forma
plot(acp$eig,type="b",pch=20,col="blue",lwd=2)
abline(h=1,lty=3,col="red",lwd=3.5)

a=fviz_eig(acp,choice='eigenvalue',geom="line",linecolor = '#3A5FCD',xlab = 'Componentes Principales')+
  geom_hline(yintercept = 1,color='#EE6363')+
  theme_grey()
a
?fviz_eig

# Segunda forma

eig.val <- get_eigenvalue(acp)
eig.val

barplot(eig.val[, 2], names.arg=1:nrow(eig.val), 
        main = "Autovalores",
        xlab = "Componentes Principales",
        ylab = "Porcentaje de variancias",
        col ="steelblue")
lines(x = 1:nrow(eig.val), eig.val[, 2], 
      type="b", pch=19, col = "red")

# Tercera forma

fviz_screeplot(acp)
fviz_screeplot(acp, ncp=6)
fviz_eig(acp, addlabels=TRUE, hjust = 0.5)

fviz_eig(acp, addlabels=TRUE, hjust = 0.5,
              barfill="white", barcolor ="darkblue",
              linecolor ="red") + ylim(0,50) + theme_minimal()

b=fviz_screeplot(acp, ncp=9, addlabels=TRUE,hjust = 0.5,linecolor = "#FC4E07",
               barfill = "#00AFBB",xlab = "Componentes Principales")
b

# Grafica de Variables sobre el circulo de correlaciones

# Primera forma 
s.corcircle(acp$co,grid=FALSE,xax = 1, yax = 2)
?s.corcircle
# Segunda forma
fviz_pca_var(acp,col.var = '#EE8262',axes = c(1, 3))
?fviz_pca_var
c=fviz_pca_var(acp, col.var="#FF3030")+theme_minimal()
c
dim(datosacp)

# Scores o Puntuaciones de cada individuo
acp$li[1:10,]

options(scipen=999)
round(cov(acp$li),4)
acp$eig
round(cor(acp$li),4)

describe(acp$li)

# Grafica de individuos sobre el primer plano de componentes

# Primera forma
s.label(acp$li,xax=1,yax=2,clabel=0.7,grid=FALSE,boxes=FALSE)

# Segunda forma 
fviz_pca_ind(acp,col.ind = "steelblue")

# Grafica de individuos sobre los componentes 2 y 3
s.label(acp$li,xax=2,yax=3,clabel=0.7,grid=FALSE,boxes=FALSE)


# Grafica de individuos sobre el primer plano con biplot

# Primera forma
s.label(acp$li,clabel=0.7,grid=FALSE,boxes=FALSE)
s.corcircle(acp$co,grid=FALSE,add.plot = TRUE,clabel=0.7)

# Segunda forma
d=fviz_pca_biplot(acp, repel = F,
                col.var = "#EE3A8C",
                col.ind = "green" )
d

(a + b)/(c + d)
a | b / c | d

# Grabar los datos y los resultados de los scores en un archivo CSV
salidaacp=cbind(datosacp,acp$li[,c(1,2,3)])
head(salidaacp)
str(salidaacp)
write.csv(salidaacp,"P.csv")

fviz_eig(acp, ncp = 9, addlabels=TRUE, hjust = 0.5,barfill = "violet",
         barcolor = "blue")
fviz_pca_ind(acp, repel = F,col.ind = 'steelblue')# Evitar superposicion de texto

#otra opcion de grafico

#Pareciera que hay dos grupos de postulantes
colorplot (acp$li, acp$li,transp = F, cex = 3,
           xlab="PC1", ylab = "PC2")
title ("Analisis PCA de Admision")
abline (v=0, h=0, col="black", lty=2)

# Formando grupos de postulantes

head(datosacp)
datosacp1=scale(datosacp)
round(head(datosacp1),2)
round(colMeans(datosacp1),8)
cov(datosacp1)

#Darle 3 componentes principales retenidos y 2 grupos
grp <- find.clusters(datosacp1, max.n.clust=8)

head(grp)

#DAPC: Analisis discriminante de Componentes Principales
#Darle 3 componentes y 1 funcion discriminante
dapc.WIDIV <- dapc(datosacp1, grp$grp)
?dapc
scatter (dapc.WIDIV, posi.da = "bottomright", bg = "white", pch = 17:22,
         cstar = 0)

# Cursos que discriminan mejor. Barras mas altas indican mejor discriminacion
contrib <- loadingplot(dapc.WIDIV$var.contr, axis=1,
                       thres=.07, lab.jitter=1)
is.data.frame(contrib)
contrib1=tibble(nombre=contrib$var.names,valor=contrib$var.values)

orden=contrib1 %>%
  arrange(desc(valor))

orden

## Otros graficos en 3D y 2D

######################################################
#OTRA ALTERNATIVA PARA HACER COMPONENTES PRINCIPALES #
######################################################

acp=PCA(datosacp,scale.unit = TRUE,ncp=9,graph = TRUE)
summary(acp)
str(acp)
sum(acp$ind$contrib[,1])#suma de ctr de los individuos sobre la componente 1
sum(acp$ind$cos2[1,])#suma de los cos2 del individuo 1 sobre todas las componentes
sum(acp$var$contrib[,3])#suma de los ctr de los cursos sobre la componente 3
sum(acp$var$cos2[3,])#suma de los cos2 del curso MAT sobre todas las componentes

head(datosacp)

respca = PCA(datosacp, scale.unit=TRUE, ncp=9, graph=TRUE)
summary(respca)
plot(respca, label = "none")
?plot
fviz_pca_var(respca, col.var = "steelblue")
respca$eig
respca$var

fviz_pca_ind(respca, label="none", habillage=datos1$CON)

fviz_pca_ind(respca, label="none", habillage=datos1$CON,
             addEllipses=TRUE, ellipse.level=0.95)

fviz_pca_biplot(respca, label = "var", habillage=datos1$CON,
                addEllipses=TRUE, ellipse.level=0.95,
                ggtheme = theme_minimal())

fviz_pca_biplot(respca, 
                # Individuals
                geom.ind = "point",
                fill.ind = datos1$CON, col.ind = "black",
                pointshape = 21, pointsize = 2,
                palette = "RdBu",
                addEllipses = TRUE,
                # Variables
                alpha.var ="contrib", col.var = "contrib",
                gradient.cols = "RdYlBu",
                
                legend.title = list(fill = "v1", color = "Contrib",
                                    alpha = "Contrib"))

fviz_pca_ind(respca, col.ind = "#00AFBB", repel = TRUE)

#Con la libreria ExPosition
library(ExPosition)
ePCA <- epPCA(datosacp)
summary(datosacp)
#Editar las etiquetas
library(explor)
explor(respca)

#Realizando el PCA en Factoshiny
library(Factoshiny)
result=Factoshiny(datosacp)
#res.shiny=PCAshiny(respca)

#Determinando conglomerados(clusters) jerarquicos con componentes principales
res.hcpc <- HCPC(respca,nb.clust = 3)
?HCPC

str(res.hcpc)
res.hcpc$data.clust
Grupos=res.hcpc$data.clust$clust
table(Grupos)

##################################
# 2. DESCRIPCION DE LOS CLUSTERS #
##################################

datosf=cbind(datos1,Grupos)
head(datosf)

# Diagrama de Cajas de variable RM segun Cluster
boxplot(datosf$RM ~ datosf$Grupos, 
        main= "BoxPlot de RM  vs CLUSTER",
        xlab = "Cluster", 
        names=c("Cluster 1", "Cluster 2", "Cluster 3"),
        col = c("red","blue","peru"))


# Diagrama de Cajas de la variable MAT segun Cluster
boxplot(datosf$MAT ~ datosf$Grupos, 
        main= "BoxPlot de MAT  vs CLUSTER",
        xlab = "Cluster", 
        names=c("Cluster 1", "Cluster 2", "Cluster 3"),
        col = c("red","blue","peru"))

# Perfil en base a las medias de los resultados
attach(datosf)
rv <- tapply(RV,Grupos,mean) ; rv
rm <- tapply(RM,Grupos,mean)  ; rm
mat <- tapply(MAT,Grupos,mean) ; mat
psi <- tapply(PSI,Grupos,mean) ; psi
fis <- tapply(FIS,Grupos,mean) ; fis
log <- tapply(LOG,Grupos,mean)   ; log
bio <- tapply(BIO,Grupos,mean) ; bio
his <- tapply(HIS,Grupos,mean)  ; his
qui <- tapply(QUI,Grupos,mean) ; qui

medias <- rbind(rv,rm,mat,psi,fis,log,bio,his,qui)   ; medias
general <- c(mean(RV),mean(RM),mean(MAT),
             mean(PSI),mean(FIS),mean(LOG),
             mean(BIO),mean(HIS),mean(QUI))   ; general
medias <- cbind(medias,general)
str(medias)
medias

matplot(medias,
        main = "Grafico de promedios de Variables segun Cluster",
        xlab = "Variables",
        ylab = "Promedios",
        type="l",
        xaxt="n",         # Permite eliminar los nombres del eje X
        ylim=c(-2,20), 
        col=c("blue","red","green2","black"))
axis(1,at=1:9,labels=c("RV","RM","MAT","PSI","FIS","LOG","BIO","HIS","QUI"))

legend("topright", c("Cluster 1", "Cluster 2", "Cluster 3","General"), 
       pch=c(5,5,5,5), ncol=4, cex=0.8, 
       col=c("blue","red","green2","black"), bty="n")

#Para obtener conglomerados con los componentes principles tambien se puede
#usar la libreria Factoshiny. Entrar a "Principal Component Analysis", hacer 
#check en "perform clustering after leaving PCA app?", escoger el numero de 
#cluster en "Number of dimensions kept for clustering" y despues 
#"quit the app", finalmente explorar los cluster.
#los cluster que se haya elegido.
library(Factoshiny)
result1=Factoshiny(datosacp)

## MISCELANEA

#Metodo Paralelo para la retencion de componentes principales.
#Cuando hay subjetividad en el grafico de sedimentacion (Scree Plot) respecto al numero de CP a retener
#se puede recurrir al Metodo Paralelo

library(paran)
paran(datosacp,iterations=5000,graph=TRUE,color=2)
#se confirma que se debe retener 3 CP.

#Test Estadistico para retener m CP (H0:lamda(m+1)=lamda(m+2)=...=lamda(p)=0)
#Esta prueba tiene la limitacion de aconsejar la retencion de demasiadas Componentes Principales.
library(nFactors)
nBartlett(cor(datosacp),N=541,alpha=0.01,cor=TRUE,details=TRUE)
#con este test se recomienda retener 7 u 8 CP.

