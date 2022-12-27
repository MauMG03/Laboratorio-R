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

Datos <- read_excel(".\\paises.xls")

str(Datos)
summary(Datos) 

##--------------------- PREPROCESAMIENTO DE DATOS ---------------------##

##----------------------------------------------------##
## Formatos de variables                              ##
##----------------------------------------------------##

#Observando las etiquetas de la variable GRUPOS
table(Datos$GRUPOS)

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

#------------------------------------------------------------##
##Validacion de las reglas sobre los datos.                  ##
#------------------------------------------------------------##


