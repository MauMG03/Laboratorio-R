#------------------------------------------------------#
#--- Probabilidad y Estadistica       Grupo:2       ---#
#--- Laboratorio en R                               ---#
#--- Integrantes:                                   ---#
#--- Mauricio Munoz Gutierrez - 2123687             ---#
#--- Juan Sebastian Getial Getial - XXXXXXX         ---#
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
 

