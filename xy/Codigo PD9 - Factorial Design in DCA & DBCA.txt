################################################################################
######################  DISEÑOS EXPERIMENTALES I  ##############################
############### Lic. Mauricio Maguiña Melgar ###################################
######## Email: mmmelgar@lamolina.edu.pe / ma.maguinam@up.edu.pe ###############
########## Practica Dirigida 9: Diseño Factorial DCA & DBCA ####################
################################################################################

# Para limpiar el workspace, por si hubiera algun dataset 
# o informacion cargada
rm(list = ls())

# Para limpiar el área de gráficos
graphics.off()

# Ejecutar para evitar el mensaje de: Error in plot.new() : figure margins too large 
plot(1:10)
dev.off()
par(mar=c(1,1,1,1))

# Instalacion de librerias:

library(pacman)
p_load(readxl,mosaic,summarytools,nortest,car,lmtest,
       phia,rstatix,dplyr,ggpubr,utils)

# Ruta de la base de datos:

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

################################################################################
################## Problema 1 - Exp Fact DCA - Guia Pag 156 ####···#############
################################################################################

prob1=read_excel("Prob1 - Exp Fact en DCA.xlsx"); prob1

names(prob1)

View(prob1)

head(prob1)
attach(prob1)

str(prob1)

prob1$Marca<-as.factor(prob1$Marca)
prob1$Periodo<-as.factor(prob1$Periodo)

str(prob1)

favstats(resultados~ Marca+Periodo, data = prob1)

with(prob1,stby(prob1$resultados,prob1$Marca,descr))
boxplot(prob1$resultados ~prob1$Marca,col=3, 
        main="Boxplot de Marca")

with(prob1,stby(prob1$resultados,prob1$Periodo,descr))
boxplot(prob1$resultados~prob1$Periodo,col=3, 
        main="Boxplot de Periodos")

######################################
###### Graficas de Interaccion #######
######################################

par(mfrow=c(1,2))
ggline(prob1,x="Marca",y="resultados",color = "Periodo",
       title = "Grafico de interaccion de Marca vs Periodos",palette = "Set2")
ggline(prob1,x="Periodo",y="resultados",color = "Marca",
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

modelo1<-lm(resultados ~ Marca + Periodo + Marca*Periodo,prob1) ;modelo1
anova(modelo1)

################################
########## SUPUESTOS  ##########
################################

# Prueba de Normalidad:

shapiro.test(residuals(modelo1))

ad.test(residuals(modelo1))
lillie.test(residuals(modelo1))

# Prueba de Homogeneidad de Varianzas:

ncvTest(modelo1)

# Independencia de Errores:

dwtest(modelo1,alternative = c("two.sided"))

######################################
########## Efectos Simples  ##########
######################################

testInteractions(modelo1,fixed = "Marca",across = "Periodo")
testInteractions(modelo1,fixed = "Periodo",across = "Marca")

######################################
########## Prueba de Tukey  ##########
######################################

phtukey=prob1 %>%group_by(Marca) %>% tukey_hsd(resultados~Periodo)
phtukey

phtukey2=prob1 %>%group_by(Periodo) %>% tukey_hsd(resultados~Marca)
phtukey2

phtukey<- phtukey %>% add_xy_position(x="Marca")

res.aov4a= prob1 %>% anova_test(resultados~Marca*Periodo)
res.aov4a

plot(res.aov4a)
          
########################
##### Factor: Marca ####
########################


fig = ggbarplot(prob1,x="Marca",y="resultados",fill = "Periodo",
                add="mean_sd",palette = "Spectral",position = position_dodge(0.8),
                ylab = c("resultados"),xlab = c("Marca"))
fig + labs(subtitle = get_test_label(res.aov4a, detailed = TRUE))


fig + stat_pvalue_manual(phtukey) + labs(subtitle = get_test_label(res.aov4a,detailed = TRUE))

fig + stat_pvalue_manual((phtukey), hide.ns=TRUE) + labs(subtitle = 
                                                           get_test_label(res.aov4a,detailed = TRUE))

########################
### Factor: Periodo ####
########################

fig= ggbarplot(prob1,x="Periodo",y="resultados",fill = "Marca",
               add="mean_sd",palette = "Spectral",position = position_dodge(0.8),
               ylab = c("porcentajes"),xlab = c("Manejos"))
fig + labs(subtitle = get_test_label(res.aov4a, detailed = TRUE))


fig + stat_pvalue_manual(phtukey) + labs(subtitle = get_test_label(res.aov4a,detailed = TRUE))
fig + stat_pvalue_manual((phtukey), hide.ns=TRUE) + labs(subtitle = get_test_label(res.aov4a,detailed = TRUE))

################################################################################
######################### PROB 2 - Experimento Factorial DBCA  #################
################################################################################

datos<-read_excel("Prob2 - Exp Fact en DBCA.xlsx")
datos

View(datos)

names(datos)

head(datos)

str(datos)

datos$Bloque<-as.factor(datos$Bloque)
datos$A<-as.factor(datos$A)
datos$B<-as.factor(datos$B)

str(datos)

favstats(y~A+B , data = datos)

##################################################
########### Estadistica Descriptiva: #############
##################################################

library(summarytools)
with(datos,stby(datos$y,datos$A,descr))
boxplot(datos$y~datos$A,col=3, 
        main="Boxplot de Variedad de Lechuga")

with(datos,stby(datos$y,datos$B,descr))
boxplot(datos$y~datos$B,col=3, 
        main="Boxplot de tipos de siembra")

with(datos,stby(datos$y,datos$Bloque,descr))
boxplot(datos$y~datos$Bloque,col=3, 
        main="Boxplot de Bloques")

################################
###### Tabla de Totales  #######
################################

# Yi.. 
aggregate(datos$y ~ datos$A, FUN = sum)

# Y.j.
aggregate(datos$y ~ datos$B, FUN = sum)

#  Y..k 
aggregate(datos$y ~ datos$Bloque, FUN = sum)

# Yij.
aggregate(datos$y ~ datos$A*datos$B, FUN = sum)

################################
###### Tabla de Promedios  #####
################################

# Prom(Yi..)
aggregate(datos$y ~ datos$A, FUN = mean)

# Prom(Y.j.)
aggregate(datos$y ~ datos$B, FUN = mean)

#  Prom(Y..k)
aggregate(datos$y ~ datos$Bloque, FUN = mean)

# Prom(Yij.)
aggregate(datos$y ~ datos$A*datos$B, FUN = mean)

# Modelo de ANOVA:

modelo<-lm(y~Bloque+A+B+A*B,datos)
anova(modelo)

# Emplearemos un alpha = 0.01

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


#####################################################
### Analisis de diferencias en efectos principales ##
#####################################################

model2<-lm(y~Bloque+A+B,datos)

library(multcomp)
citukey <- confint(glht(model2, linfct = mcp(A = "Tukey")))
summary(citukey)

citukey <- confint(glht(model2, linfct = mcp(B = "Tukey")))
summary(citukey)






