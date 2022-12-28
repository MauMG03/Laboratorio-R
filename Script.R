#------------------------------------------------------#
#--- Probabilidad y Estadistica       Grupo:2       ---#
#--- Laboratorio en R                               ---#
#--- Integrantes:                                   ---#
#--- Mauricio Munoz Gutierrez - 2123687             ---#
#--- Juan Sebastian Getial Getial - 2124644         ---#
#------------------------------------------------------#

# Instalacion de paquetes
#install.packages("easypackages")
#install.packages("readxl")

library("easypackages")
library("readxl")

lib_req <- c("lubridate","dplyr","visdat","missMDA","mice","DMwR2","editrules","corrplot")
easypackages::packages(lib_req)

#Importar datos del excel

Datos <- read_excel("./paises.xls")

str(Datos)
summary(Datos) 

##--------------------- PREPROCESAMIENTO DE DATOS ---------------------##

##----------------------------------------------------##
## Formatos de variables                              ##
##----------------------------------------------------##

#Observando las etiquetas de las variables.
str(Datos)

#Cambio de etiqueta de GRUPOS a factor.
Datos = transform(Datos,
                  GRUPOS = factor(GRUPOS))

str(Datos)

#------------------------------------------------------------##
##Validacion de las reglas sobre los datos.                  ##
#------------------------------------------------------------##

# Verificación de las reglas sobres los datos
Rules <- editrules::editfile("consistencia.txt")
Valid_Data = editrules::violatedEdits(Rules, Datos)
summary(Valid_Data)

#Identificar que observaciones presentan violaciones a las reglas
which(Valid_Data)
matrix(data=1:55, 5, 11)

# Visualización del diagnóstico
x11()
plot(Valid_Data)


#------------------------------------------------------------##
##Visualizacion de datos faltantes.                          ##
#------------------------------------------------------------##

#Vista por consola, si es verdadero es un valor nulo
View(Datos)
is.na(Datos)

#Vista en ventana
x11()
visdat::vis_miss(Datos)

#Identificacion de datos faltantes por columna y registro.
miss<-function(Datos,plot=T){  
  n=nrow(Datos);p=ncol(Datos)
  names.obs<-rownames(Datos)
  
  
  nobs.comp=sum(complete.cases(Datos))         # Cuenta los registros completos
  Obs.comp=which(complete.cases(Datos))        # Identifica los registros completos
  nobs.miss = sum(!complete.cases(Datos))      # Identifica los registros con datos faltantes.
  Obs.miss=which(!complete.cases(Datos))       # Identifica los registros con datos faltantes.
  
  Datos.NA<-is.na(Datos)
  Var_Num<- sort(colSums(Datos.NA),decreasing=T)
  Var_per<-round(Var_Num/n,3)
  Obs_Num<-rowSums(Datos.NA)
  names(Obs_Num)<-names.obs
  Obs_Num<-sort(Obs_Num,decreasing=T)
  Obs_per<-round(Obs_Num/p,3)
  lista<-list(n.row = n, n.col = p,n.comp = nobs.comp,Obs.comp = Obs.comp,n.miss = nobs.miss,Obs.miss = Obs.miss, Var.n = Var_Num , Var.p = Var_per, Obs.n= Obs_Num, Obs.per= Obs_per)
  
  if(plot){
    x11(display = "", 15, 10)
    par(mfrow=c(1,2))
    coord<-barplot(Var_per,plot=F)
    barplot(Var_per,xaxt="n",horiz=T,yaxt="n",xlim=c(-0.2,1), ylim=c(0,max(coord)+1),main= "% datos faltantes por variable")
    axis(2,at=coord,labels=names(Var_per), cex.axis=0.5,pos=0,las=2)
    axis(1,seq(0,1,0.2),seq(0,1,0.2),pos=0)
    
    coord<-barplot(Obs_per,plot=F)
    barplot(Obs_per,xaxt="n",horiz=T,yaxt="n",xlim=c(-0.2,1), ylim=c(0,max(coord)+1),main= "% datos faltantes por registro")
    axis(2,at=coord,labels=names(Obs_per),cex.axis=0.5,pos=0,las=2)
    axis(1,seq(0,1,0.2),seq(0,1,0.2))
  }
  return(invisible(lista))
}

Summary.NA = miss(Datos)

#------------------------------------------------------------##
##Correccion de datos nulos e inconsistentes.                ##
#------------------------------------------------------------##

#----------------- Correcion de Datos inconsistentes -----------------#
#Declaracion de niveles correctos para las variables tipo Factor.
level_GRUPOS <- c(africa="AFRICA",
                  Africa="AFRICA",
                  AFRICA="AFRICA",
                  asia="ASIA",
                  Asia="ASIA",
                  ASIA="ASIA",
                  'EO-NA_JAPON_AUSTR_NZ'="EO-NA_JAPON_AUSTR_NZ",
                  'Europa Oriental'="EUROPA ORIENTAL",
                  EUROPA_ORIENTAL="EUROPA ORIENTAL",
                  iberoamerica="IBEROAMERICA",
                  Iberoamerica="IBEROAMERICA",
                  IBEROAMERICA="IBEROAMERICA",
                  ORIENTE_MEDIO="ORIENTE MEDIO")


#Modificacion del formato y transformacion de variables
Datos <- transform(Datos,
                   GRUPOS= factor(dplyr::recode(GRUPOS, !!!level_GRUPOS))
)
str(Datos$GRUPOS)

#Visualizacion de datos.
Valid_Data = editrules::violatedEdits(Rules, Datos)
x11()
plot(Valid_Data)

#--------------------- Correccion de datos faltantes -------------------------#

#Imputacion de datos faltantes (Regresion).
imputR = mice::mice(Datos, maxit = 1, method = "norm.predict",seed = 2018,print=F)
Datos_ImputR = mice::complete(imputR)
x11(display = "", 15, 10); visdat::vis_miss(Datos_ImputR) 

model_ImputR=lm(GRUPOS~.,Datos_ImputR[,-6]) 

Summary.NA = miss(Datos_ImputR)

#Almacenamiento de los datos limpios como objeto .RData
datosLimpios = Datos_ImputR
save(datosLimpios, file="datosLimpios.RData")


##------------------------ VISUALIZACION DE LOS DATOS ----------------------##

## Limpieza del entorno de trabajo
rm(list = ls())

## Importacion de los datos limpios
load("datosLimpios.RData")

##--1. Como está conformada la muestra (distribución) de países según grupo.--#
distribucion <- table(datosLimpios$GRUPOS)
grupos <- c("AFRICA",
            "ASIA",
            "EO-NA_JAPON_AUSTR_NZ",
            "EUROPA ORIENTAL",
            "IBEROAMERICA",
            "ORIENTE MEDIO")

x11()
barplot(distribucion,
        xlab = "Grupo",
        ylab = "Numero de paises",
        ylim = c(0,30),
        names.arg = c("6","5","3","1","2","4"),
        main = "Distribucion de paises segun grupo",
        col = c("2","3","4","5","6","7"))

legend(x=3.75,y=30,legend = grupos,
       fill = c("2","3","4","5","6","7"),
       bg = "lightgray")

##--2. Diferencia en los indicadores para los paises.##

##Coeficiente VAriacion(%) Tasa natalidad 
cvTasNat = round(
  (sd(datosLimpios$Tasa.natalidad)/mean(datosLimpios$Tasa.natalidad))*100 
, 2)

##Coeficiente VAriacion(%) Tasa Mortalidad
cvTasMort = round(
  (sd(datosLimpios$Tasa.mortalidad)/mean(datosLimpios$Tasa.mortalidad))*100 
  , 2)

##Coeficiente VAriacion(%) Mortalidad Infantil
cvMortInf = round(
  (sd(datosLimpios$Mortalidad.infantil)/mean(datosLimpios$Mortalidad.infantil))*100 
  , 2)

x11();barplot(
  c(TasaNatalidad = cvTasNat, 
    TasaMortalidad = cvTasMort, 
    MortalidadInfantil = cvMortInf),
  xlab = "Indicadores",
  ylab = "% Variacion",
  ylim = c(0,100),
  main = "Variacion de los Indicadores Entre los Paises",
  col = 2:4
)

## 3. 
