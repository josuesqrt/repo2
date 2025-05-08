#######################################################################################
############################ DISEÑOS EXPERIMENTALES II ################################
################## Lic. Mauricio Maguiña Melgar #######################################
################# Email: mmmelgar@lamolina.edu.pe #####################################
############### Practica Dirigida 1: Experimento Factorial en DCA & DBCA   ############
#######################################################################################

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

library(pacman)
p_load(readxl,mosaic,DescTools,ggpubr,dplyr,agricolae,nortest,car,
       lmtest,phia,rstatix,ggpubr)

####################################################################################
########################### EJEMPLO 1  - Exp Fact en DCA  ##########################
####################################################################################

'El departamento de nutricion humana y alimentos de una reconocida universidad
realizo un estudio sobre la estabilidad de la vitamina C en el concentrado del
jugo de naranja congelado reconstituido, que se almacena en un refrigerador
durante un periodo de hasta una semana. Se probaron dos marcas de concentrados
de jugo de naranja congelado reconstituido con tres periodos distintos,
los cuales se refieren al numero de dias desde que se mezcló el jugo hasta que
se probo (0 dias, 3 dias y 7 dias). Se registraron los resultados, en miligramos
de acido ascorbico por litro. Se decidio usar un Diseño Completamente al 
Azar (DCA) con 4 repeticiones para cada uno de los tratamientos.'

ejemplo1=read_excel("ejm de aplicacion pag 156.xlsx"); ejemplo1

names(ejemplo1)

View(ejemplo1)
head(ejemplo1)
attach(ejemplo1)

str(ejemplo1)

ejemplo1$Marca<-as.factor(ejemplo1$Marca)
ejemplo1$Periodo <-as.factor(ejemplo1$Periodo)

str(ejemplo1)

favstats(resultados~Marca+Periodo , data = ejemplo1)

summarytools::descr(ejemplo1)

######################################
###### Graficas de Interaccion #######
######################################

ggline(ejemplo1,x="Marca",y="resultados",color = "Periodo",
       title = "Grafico de interaccion de Marca vs Periodo",palette = "Set2")

ggline(ejemplo1,x="Periodo",y="resultados",color = "Marca",
       title = "Grafico de interaccion de Periodo vs Marca",palette = "Set2")

################################
###### Tabla de Totales  #######
################################

# Yi.. 

aggregate(resultados ~ Marca, FUN = sum)

# Y.j.

aggregate(resultados ~ Periodo, FUN = sum)

# Yij.

aggregate(resultados ~ Marca*Periodo, FUN = sum)

################################
###### Tabla de Promedios  #####
################################

# Prom(Yi..)

aggregate(resultados ~ Marca, FUN = mean)

# Prom(Y.j.)

aggregate(resultados ~ Periodo, FUN = mean)

# Prom(Yij.)

aggregate(resultados ~ Marca*Periodo, FUN = mean)


#####################
###### ANOVA  #######
#####################

modelo<-lm(resultados ~ Marca + Periodo + Marca*Periodo,ejemplo1) ;modelo
anova(modelo)

modelo %>% cv.model()

round (qf(0.95,2,18),4 )

####################################
############## SUPUESTOS  ##########
####################################

# Prueba de Normalidad:

shapiro.test(residuals(modelo))

ad.test(residuals(modelo))
lillie.test(residuals(modelo))

# Prueba de Homogeneidad de Varianzas:

ncvTest(modelo)

# Independencia de Errores:

dwtest(modelo,alternative = c("two.sided"))

######################################
########## Efectos Simples  ##########
######################################

testInteractions(modelo,fixed = "Periodo",across = "Marca")
testInteractions(modelo,fixed = "Marca",across = "Periodo")

######################################
########## Prueba de Tukey  ##########
######################################

phtukey=ejemplo1 %>%
  group_by(Periodo) %>%
  tukey_hsd(resultados~Marca)
phtukey

phtukey<- phtukey %>% add_xy_position(x="Periodo")

res.aov4a= ejemplo1 %>% anova_test(resultados~Marca*Periodo)
res.aov4a

plot(res.aov4a)

########################
### Factor: Periodo ####
########################

fig= ggbarplot(ejemplo1,x="Periodo",y="resultados",fill = "Marca",
               add="mean_sd",palette = "Spectral",position = position_dodge(0.8),
               ylab = c("resultados"),xlab = c("Periodo"))
fig + labs(subtitle = get_test_label(res.aov4a, detailed = TRUE))


fig + stat_pvalue_manual(phtukey) + labs(subtitle = get_test_label(res.aov4a,detailed = TRUE))

fig + stat_pvalue_manual((phtukey), hide.ns=TRUE) + labs(subtitle = 
                                                           get_test_label(res.aov4a,detailed = TRUE))

########################
##### Factor: Marca ####
########################

fig= ggbarplot(ejemplo1,x="Marca",y="resultados",fill = "Periodo",
               add="mean_sd",palette = "Spectral",position = position_dodge(0.8),
               ylab = c("resultados"),xlab = c("Marca"))
fig + labs(subtitle = get_test_label(res.aov4a, detailed = TRUE))


fig + stat_pvalue_manual(phtukey) + labs(subtitle = get_test_label(res.aov4a,detailed = TRUE))
fig + stat_pvalue_manual((phtukey), hide.ns=TRUE) + labs(subtitle = get_test_label(res.aov4a,detailed = TRUE))


####################################################################################
############################# Problema 1 - Exp Fact DCA ############################
####################################################################################

library(readxl)
prob1=read_excel("Prob1 - Exp Fact en DCA.xlsx"); prob1

names(prob1)

View(prob1)

head(prob1)
attach(prob1)

str(prob1)

prob1$Dosis<-as.factor(prob1$Dosis)
prob1$Manejos <-as.factor(prob1$Manejos)

str(prob1)

favstats(porcentajes~ Dosis+ Manejos, data = prob1)

summarytools::descr(prob1)

######################################
###### Graficas de Interaccion #######
######################################

ggline(prob1,x="Dosis",y="porcentajes",color = "Manejos",
       title = "Grafico de interaccion de Dosis vs Manejos",palette = "Set2")

ggline(prob1,x="Manejos",y="porcentajes",color = "Dosis",
       title = "Grafico de interaccion de Manejos vs Dosis",palette = "Set2")

################################
###### Tabla de Totales  #######
################################

# Yi.. 

aggregate(porcentajes ~ Dosis, FUN = sum)

# Y.j.

aggregate(porcentajes ~ Manejos, FUN = sum)

# Yij.

aggregate(porcentajes ~ Dosis*Manejos, FUN = sum)

################################
###### Tabla de Promedios  #####
################################

# Prom(Yi..)

aggregate(porcentajes ~ Dosis, FUN = mean)

# Prom(Y.j.)

aggregate(porcentajes ~ Manejos, FUN = mean)

# Prom(Yij.)

aggregate(porcentajes ~ Dosis*Manejos, FUN = mean)


#####################
###### ANOVA  #######
#####################

modelo1<-lm(porcentajes ~ Dosis + Manejos + Dosis*Manejos,prob1) ;modelo1
anova(modelo1)

################################
########## SUPUESTOS  ##########
################################

# Prueba de Normalidad:

shapiro.test(residuals(modelo1))

library(nortest)
ad.test(residuals(modelo1))
lillie.test(residuals(modelo1))

# Prueba de Homogeneidad de Varianzas:

ncvTest(modelo1)

# Independencia de Errores:

dwtest(modelo1,alternative = c("two.sided"))

######################################
########## Efectos Simples  ##########
######################################

testInteractions(modelo1,fixed = "Dosis",across = "Manejos")
testInteractions(modelo1,fixed = "Manejos",across = "Dosis")

######################################
########## Prueba de Tukey  ##########
######################################

phtukey=prob1 %>%group_by(Dosis) %>% tukey_hsd(porcentajes~Manejos)
phtukey

phtukey<- phtukey %>% add_xy_position(x="Dosis")

res.aov4a= prob1 %>% anova_test(porcentajes~Dosis*Manejos)
res.aov4a

plot(res.aov4a)


########################
##### Factor: Dosis ####
########################

fig = ggbarplot(prob1,x="Dosis",y="porcentajes",fill = "Manejos",
                add="mean_sd",palette = "Spectral",position = position_dodge(0.8),
                ylab = c("porcentajes"),xlab = c("Dosis"))
fig + labs(subtitle = get_test_label(res.aov4a, detailed = TRUE))


fig + stat_pvalue_manual(phtukey) + labs(subtitle = get_test_label(res.aov4a,detailed = TRUE))

fig + stat_pvalue_manual((phtukey), hide.ns=TRUE) + labs(subtitle = 
                                                           get_test_label(res.aov4a,detailed = TRUE))

########################
### Factor: Manejos ####
########################

fig= ggbarplot(prob1,x="Manejos",y="porcentajes",fill = "Dosis",
               add="mean_sd",palette = "Spectral",position = position_dodge(0.8),
               ylab = c("porcentajes"),xlab = c("Manejos"))
fig + labs(subtitle = get_test_label(res.aov4a, detailed = TRUE))


fig + stat_pvalue_manual(phtukey) + labs(subtitle = get_test_label(res.aov4a,detailed = TRUE))
fig + stat_pvalue_manual((phtukey), hide.ns=TRUE) + labs(subtitle = get_test_label(res.aov4a,detailed = TRUE))

#####################################################################################
########################## BONUS TRACK 1 - DATASETS EN R: Warpbreaks  ###############
#####################################################################################

'La industria experiemntal examino el numero de roturas de hilo 
 dependiendo de dos tipos de lana y tres niveles de tensi?n usando 
 una m?quina de tejer. (Tukey 1977)'

?warpbreaks

library(faraway)
warpbreaks

str(warpbreaks)

library(stats)
library(graphics)
summary(warpbreaks)

library(DescTools)
summarytools::descr(warpbreaks)

opar <- par(mfrow = c(1, 2), oma = c(0, 0, 1.1, 0))
plot(breaks ~ tension, data = warpbreaks, col = "lightgray",
     varwidth = TRUE, subset = wool == "A", main = "Wool A")
plot(breaks ~ tension, data = warpbreaks, col = "lightgray",
     varwidth = TRUE, subset = wool == "B", main = "Wool B")
mtext("warpbreaks data", side = 3, outer = TRUE)
par(opar)

summary(fm1 <- lm(breaks ~ wool*tension, data = warpbreaks))
anova(fm1)

#####################################################################################
######################### BONUS TRACK 2 - Experimento Factorial DBCA  ###############
#####################################################################################

'Caso de Estudio: 

 El jengibre es una raiz en compuestos bioactivos, sin embargo, la extraccion de estos
 compuestos por lixiviacion esta relacionada con factores como el tipo de Solvente, 
 polaridad y temperatura. Se analizo el efecto del tipo de Solvente y la parte de planta 
 sobre el contenido de compuestos fen?licos del extracto, pero dado que la variedad 
 puede generar diferencias se opt? por analizar dos variedades como bloques.

 En los factores se analizaron: cuatro tipos de solvente(metanol,acetona, cloromorfo 
 y etanol),tres partes de la planta(hojas, tallo, raiz) y dos variedades de jengibre
 (Bentong y Bara). El contenido de Fenoles se determino en terminos de "miligramos 
 equivalentes de acido golico/g gengibre. 
 La unidad experimental fue de 1 ml de extracto. 
 Los resultados estan en excel sobre la CONCENTRACION DE FENOLES A DIFERENTES 
 SOLVENTES Y PARTES DE LA PLANTA.'

prob2=read_excel("bonus track 2- Exp Fact en DBCA.xlsx"); prob2

names(prob2)

head(prob2)

View(prob2)

str(prob2)

prob2$Parte=as.factor(prob2$Parte)
prob2$Solvente=as.factor(prob2$Solvente)
prob2$Variedad=as.factor(prob2$Variedad)

str(prob2)

attach(prob2)   ## Asignacion de variables

################################
###### Tabla de Totales  #######
################################

# Yi.. 

aggregate(Fenoles ~ Parte, FUN = sum)

# Y.j.

aggregate(Fenoles ~ Solvente, FUN = sum)

#  Y..k 

aggregate(Fenoles ~ Variedad, FUN = sum)

# Yij.

aggregate(Fenoles ~ Parte*Solvente, FUN = sum)


################################
###### Tabla de Promedios  #####
################################

# Prom(Yi..)

aggregate(Fenoles ~ Parte, FUN = mean)

# Prom(Y.j.)

aggregate(Fenoles ~ Solvente, FUN = mean)

#  Prom(Y..k)

aggregate(Fenoles ~ Variedad, FUN = mean)

# Prom(Yij.)

aggregate(Fenoles ~ Parte*Solvente, FUN = mean)

##############################################
######### Graficos de Interaccion ############
##############################################

ggline(prob2,x="Parte",y="Fenoles",color = "Solvente",title = "Grafico de interaccion
       de Parte vs Solvente",palette="Set2")


ggline(prob2,x="Solvente",y="Fenoles",
       color="Parte",title = "Grafico de interaccion de Solvente vs Parte",
       palette = "Set2")

#####################
###### ANOVA  #######
#####################

modelo2<-lm(Fenoles ~ Parte*Solvente + Variedad,prob2) ;modelo2
anova(modelo2)

################################
########## SUPUESTOS  ##########
################################

# Prueba de Normalidad:

shapiro.test(residuals(modelo2))

ad.test(residuals(modelo2))
lillie.test(residuals(modelo2))

# Prueba de Homogeneidad de Varianzas:

ncvTest(modelo2)

modelo=lm(Fenoles ~ Parte*Solvente,data = prob2)
levene_test(modelo,center=mean)

bptest(modelo)

# Independencia de Errores:

dwtest(modelo2,alternative = c("two.sided"))

################################################################
########## Medidas Estadisticas de las interacciones  ##########
################################################################

interactionMeans(modelo2)

###########################################################
###### Efectos Simples: P(S1)-P(S2)-P(S3)-P(S4)  ##########
###########################################################

testInteractions(modelo2,fixed = "Solvente",across = "Parte",
                 adjustment = "none")

###########################################################
###### Efectos Simples: S(P1)-S(P2)-S(P3)  ################
###########################################################

testInteractions(modelo2,fixed = "Parte",across = "Solvente",
                 adjustment = "none")


######################################
########## Prueba de TUKEY  ##########
######################################

head(prob2)

## Parte:

phtukey=prob2 %>%
  group_by(Parte) %>%
  tukey_hsd(Fenoles ~ Solvente)   ### Test apriori de Tukey
phtukey

## Solvente:

phtukey1=prob2 %>%
  group_by(Solvente) %>%
  tukey_hsd(Fenoles ~ Parte)   ### Test apriori de Tukey
phtukey1


######################################
###### Grafico de los resultados #####
######################################

## Parte:

phtukey<- phtukey %>% add_xy_position(x="Parte")
res.aov4a= prob2 %>% anova_test(Fenoles ~ Parte*Solvente+Variedad)
res.aov4a
plot(res.aov4a)

## Solvente:

phtukey1<- phtukey1 %>% add_xy_position(x="Solvente")
res.aov4a= prob2 %>% anova_test(Fenoles ~ Parte*Solvente+Variedad)
res.aov4a
plot(res.aov4a)

########################
##### Factor: Parte ####
########################

fig = ggbarplot(prob2,x="Parte",y="Fenoles",fill = "Solvente",
                add="mean_sd",palette = "Spectral",position = position_dodge(0.8),
                ylab = c("Fenoles"),xlab = c("Parte"))
fig + labs(subtitle = get_test_label(res.aov4a, detailed = TRUE))

fig +stat_pvalue_manual(phtukey)+labs(subtitle = get_test_label(res.aov4a),detailed=TRUE)

fig +stat_pvalue_manual(phtukey,hide.ns=TRUE)+labs(subtitle = get_test_label(res.aov4a),detailed=TRUE)

#########################
### Factor: Solvente ####
#########################

fig = ggbarplot(prob2,x="Solvente",y="Fenoles",fill = "Parte",
                add="mean_sd",palette = "Spectral",position = position_dodge(0.8),
                ylab = c("Fenoles"),xlab = c("Solvente"))
fig + labs(subtitle = get_test_label(res.aov4a, detailed = TRUE))

fig +stat_pvalue_manual(phtukey1)+labs(subtitle = get_test_label(res.aov4a),detailed=TRUE)

fig +stat_pvalue_manual(phtukey1,hide.ns=TRUE)+labs(subtitle = get_test_label(res.aov4a),detailed=TRUE)

##########################
####### Prueba de DLS ####
##########################

prueba_t=prob2 %>%
  group_by(Parte) %>%
  t_test(Fenoles ~ Solvente) %>%
  add_significance("p.adj")
prueba_t

prueba_t=prueba_t %>% add_xy_position(x="Parte")
res.aov4a=prob2%>% anova_test(Fenoles ~ Parte*Solvente+Variedad) 
res.aov4a
plot(res.aov4a)

#####################################################################################
######################### BONUS TRACK 3 - Experimento Factorial DBCA  ###############
#####################################################################################

datos<-read.table("factorial1.txt",header=T)
datos

View(datos)

names(datos)

head(datos)

str(datos)

datos$Bloque<-as.factor(datos$Bloque)
datos$A<-as.factor(datos$A)
datos$B<-as.factor(datos$B)

str(datos)

library(mosaic)
favstats(y~A+B , data = datos)

modelo<-lm(y~Bloque+A+B+A*B,datos)
anova(modelo)

### Emplearemos un alpha = 0.01

' Ho: No hay interacccion
  H1: Existe interaccion 

  pvalor = 0.0182032 > alpha = 0.01
  Conclusion: NRho

  A : p= 2 niveles
  B : q= 3 niveles'

#############################################
###### Analisis de efectos principales ######
#############################################

######################### 
### Para el factor A ####
#########################

'Ho: u1. = u2.
 H1:     =!

 pvalor = 0.0005869 < alpha = 0.05
 Rho: El factor A influye en la respuesta'

######################### 
### Para el factor B ####
#########################

'Ho: u.1 = u.2 = u.3
 H1: Al menos uno es diferente

 pvalor = 9.913e-06 < alpha = 0.05
 RHo: El factor B influye en la respuesta'

par(mfcol=c(1,2))
plot(modelo, 1:2)

library(phia)

modelo.means <- interactionMeans(modelo,factors=c("A","B"))
modelo.means

plot(modelo.means)


###################################################
######## Analisis de Efectos Simples ##############
###################################################

'Sera de efectos simples(RHo), si el alpha = 0.05 
 RECORDAR:

 A : p= 2 niveles
 B : q= 3 niveles'

########################################
######## Efecto simple de B en A #######
########################################

testInteractions(modelo, fixed="A", across="B")

'B(a1): Ho:u11=u12=u13           B(a2):    Ho:u21=u22=u23   
        H1: al menos una =!                H1: al menos una =! 

       pvalor=0.005428                    pvalor= 3.263e-05        
             Rho (Sig)                           Rho (Sig)     '                   

#######################################
######## Efecto simple de A en B ######
#######################################

testInteractions(modelo, fixed="B", across="A") 

' A(b1): Ho:u11=u21           A(b2):   Ho:u12=u22      A(b3):  Ho:u13=u23 
        H1:   =!                      H1:   =!                H1:   =!

        pvalor=0.8802105              pvalor= 0.0007709       pvalor= 0.0397223
            NO Rho                          Sig                    Sig            '

#######################################
######## Analisis de Residuales #######
#######################################

obs.table <- xtabs(modelo.means$"adjusted mean" ~ A + B, modelo.means)
obs.table<- addmargins(obs.table,FUN=mean,quiet=TRUE)
print(obs.table,digits=4)

res.table <- obs.table - obs.table[3,4] # restando la media
res.table <- sweep(res.table, 1, res.table[,4]) # restando media fila
res.table <- sweep(res.table, 2, res.table[3,]) # restando media comlumna
print(res.table, digits=4)
testInteractions(modelo,residual=c("A","B"))
matplot(t(res.table[-3,-4]), type="b", xaxt="n", ylab="residuales interacci?n")
axis(1, at=1:3, labels=levels(datos$B))

#####################################################
### An?lisis de diferencias en efectos principales ##
#####################################################

model2<-lm(y~Bloque+A+B,datos)

library(multcomp)
citukey <- confint(glht(model2, linfct = mcp(A = "Tukey")))
summary(citukey)

citukey <- confint(glht(model2, linfct = mcp(B = "Tukey")))
summary(citukey)



