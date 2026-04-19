# ==============================================================================
# SCRIPT 01: Descarga de datos robusta
# Este script se encarga de descargar los datos desde las fuentes oficiales, ase
# gurando la creación de directorios necesarios, y registrando metadatos de la d
# escarga.
# ==============================================================================
library(here)

data_dir = here("TP2", "data")
if (!dir.exists(data_dir)) {
  dir.create(data_dir, recursive = TRUE)
}

#DESCARGO LAS LIBRERÍAS CON LAS QUE VOY A TRABAJAR: 

library(tidyverse)  # Manipulación de datos
install.packages("rvest")
library(rvest)      
# Web scraping
install.packages("httr2")
library(httr2) 
install.packages("tidytext")
library(tidytext)
install.packages("udpipe")
library(udpipe)     
# Requests HTTP
# Análisis de texto
# Lematización
install.packages("robotstxt")
library(robotstxt)  # Verificar permisos de scraping
# Manejo de rutas de archivos
install.packages("xml2")
library(xml2)       
# Manejo de HTML (guardar la página completa x ej)







