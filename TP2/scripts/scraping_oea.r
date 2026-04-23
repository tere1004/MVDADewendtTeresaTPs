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
# Manejo de HTML (guardar la página completa x ej)


#instalacion de paquetes 2:

install.packages("remotes")
remotes::install_version("rvest", version = "1.0.4")
library(rvest)

library(rvest)
library(xml2)

#GUARDAMOS EL URL DE LA PÁGINA: 

url_noticias <- "https://www.oas.org/es/centro_noticias/comunicados_prensa.asp"

# Leemos el HTML de la página
pagina_html <- read_html(url_noticias)

# Inspeccionamos la estructura
pagina_html

# Guardamos en formato rds para uso interno y registro de fecha de descarga
attr(pagina_html, "fecha_descarga") <- Sys.time()

# Guardamos en html para abrir en el navegador si queremos 
# (el atributo no se vá a guardar, pero lo vamos a usar más tarde)
write_html(pagina_html, file = file.path(data_dir, "pagina_noticias_oea.html"))


#WEB SCRAPING: 

#EXTRAEMOS TITULOS, ID Y FECHAS: 


# Función para scrapear una página de noticias: 

scrapear_oea <- function(url, first_page=FALSE) {

  # Pausa para no sobrecargar el servidor
  Sys.sleep(3)

  # Leemos la página
  html <- read_html(url)

  # Extraemos los elementos que identificamos con el Selector o el Inspector
  titulos_nodos <- html |>
    html_elements(".itemmenulink")

  titulos <- titulos_nodos |>
    html_text2() |>
    str_trim()

  urls <- titulos_nodos |>
    html_attr("href")

  cuerpo <- html |>
    html_elements("p") |>
    html_text2() |>
    str_trim()

  fechas <- html |>
    html_elements(".headlinelink") |>
    html_text2() |>
    str_trim()
  tibble(
      titulo = titulos,
      url = urls,
      fecha_raw = fechas
    )
}




#COMPROBAMOS QUE FUNCIONA 
scrapear_oea("https://www.oas.org/es/centro_noticias/comunicados_prensa.asp", first_page=TRUE)



#ITERAMOS EL SCRAPING PARA VARIOS MESES: 

scrapear_meses_oea <- function(meses, anio){
  
  noticias_oea <- tibble()
  
  for(i in meses){
    
    Sys.sleep(3)
    
    url <- paste0(
      "https://www.oas.org/es/centro_noticias/comunicados_prensa.asp?nMes=",
      i,
      "&nAnio=",
      anio
    )
    
    noticias_mes <- scrapear_oea(url)
    
    noticias_oea <- bind_rows(noticias_oea, noticias_mes)
    
    message(
      paste(
        "\nMes", i,
        "de", anio,
        "scrapeado correctamente.",
        "Total noticias:", nrow(noticias_oea)
      )
    )
  }
  
  noticias_oea <- noticias_oea |>
    mutate(id = row_number())
  
  return(noticias_oea)
}

#LA USAMOS:

noticias_oea <- scrapear_meses_oea(
  meses = 1:4,
  anio = 2026
)


#EXTRAEMOS EL CUERPO REAL DE CADA NOTICIA: 


extraer_cuerpo_noticiaoea = function(url) {
  
  Sys.sleep(3)
  
  if (!grepl("^http", url)) {
    url <- paste0("https://www.oas.org/es/centro_noticias/", url)
  }
  
  html_noticia = read_html(url)
  
  cuerpo_noticia = html_noticia |>
    html_elements("#rightmaincol") |>
    html_elements("p") |>
    html_text2() |>
    str_trim()
  
  
  # Concatenamos todos los párrafos
  cuerpo_noticia = str_c(cuerpo_noticia, collapse = " ")
  
  # Eliminamos caracteres 
  cuerpo_noticia = str_replace_all(cuerpo_noticia, "[\\r\\n\\t]+", " ")
  
  # Eliminamos todo tipo de comillas (estándar, tipográficas, simples, dobles, backticks y porcentajes)
  cuerpo_noticia = str_replace_all(cuerpo_noticia, "[\\\"'“”‘’«»`´%()]", "")
  
  # Eliminamos espacios múltiples
  cuerpo_noticia = str_squish(cuerpo_noticia)  # parecido a trim, pero además saca dos o más espacios seguidos
  
  return(cuerpo_noticia)
}




# La probamos con una noticia OEA cualquiera
extraer_cuerpo_noticiaoea(
  "https://www.oas.org/es/centro_noticias/fotonoticia.asp?sCodigo=FNC-144771"
)


#CREAMOS UNA BASE DE DATOS PARA ALMACENAR LOS CUERPOS: 

cuerpos_noticias_oea <- noticias_oea |>
  select(id, url) |>
  mutate(
    url = paste0(
      "https://www.oas.org/es/centro_noticias/",
      url
    ),
    cuerpo = map_chr(url, extraer_cuerpo_noticiaoea)
  ) |>
  select(id, cuerpo)



#AHORA UNIMOS TODO: 

noticias_oea <- noticias_oea |>
  left_join(cuerpos_noticias_oea, by = "id", suffix = c("", "_nuevo")) |>
  select(id, titulo, cuerpo)


# Guardamos esta tabla por separado para ahorrar memoria y 
#no tener que volver a hacer scraping cada vez que queramos analizar el texto

# Le asignamos la misma fecha de descarga que el dataset original por consistencia
attr(noticias_oea, "fecha_descarga") <- pagina_html |> attr("fecha_descarga")

noticias_oea |> write_rds(
  file.path(data_dir, "noticias_oea.rds")
)


















#ITERAMOS EL SCRAPING SOBRE VARIAS PAGINAS: 


scrapear_n_oea <- function(n) {
  
  # Primera página (mes 1)
  url <- paste0("https://www.oas.org/es/centro_noticias/comunicados_prensa.asp?nMes=", 1, "&nAnio=2026")
  
  noticias_pagina <- scrapear_oea(url, first_page = TRUE)
  
  mensaje <- paste("\nPágina", 1, "de", n, "scrapeada correctamente. Total noticias hasta ahora:", nrow(noticias_pagina))
  message(mensaje)
  
  # Loop para las demás páginas
  if (n > 1) {
    
    for (i in 2:n) {
      
      url <- paste0("https://www.oas.org/es/centro_noticias/comunicados_prensa.asp?nMes=", i, "&nAnio=2026")
      
      nuevas_noticias <- scrapear_oea(url, first_page = FALSE)
      
      noticias_pagina <- rbind(noticias_pagina, nuevas_noticias)
      
      mensaje <- paste("\nPágina", i, "de", n, "scrapeada correctamente. Total noticias hasta ahora:", nrow(noticias_pagina))
      message(mensaje)
    }
  }
  
  # Agregar id
  noticias_pagina <- noticias_pagina |> dplyr::mutate(id = dplyr::row_number())
  
  return(noticias_pagina)
}




noticias_oea = scrapear_n_oea(n=4) 


# Definimos una función que extrae el texto de cada noticia haciendo request al url
extraer_cuerpo_noticia = function(url) {
  Sys.sleep(1) # Pausa para no sobrecargar el servidor
  html_noticia = read_html(url)
  
  # El selector para el texto completo de la noticia puede variar según la estructura del sitio
  cuerpo_noticia = html_noticia |>
    html_elements(".entry-container") |> # Selector para el contenido de la noticia
    html_elements(".entry p" ) |> # Extraemos los párrafos para obtener el texto completo (sin headers ni partes que puedan dificultar mucho el análisis)
    html_text2() |>
    str_trim()
  
  # Concatenamos todos los párrafos
  cuerpo_noticia = str_c(cuerpo_noticia, collapse = " ")
  
  # Eliminamos caracteres 
  cuerpo_noticia = str_replace_all(cuerpo_noticia, "[\\r\\n\\t]+", " ")
  
  # Eliminamos todo tipo de comillas (estándar, tipográficas, simples, dobles, backticks y porcentajes)
  cuerpo_noticia = str_replace_all(cuerpo_noticia, "[\\\"'“”‘’«»`´%()]", "")
  
  # Eliminamos espacios múltiples
  cuerpo_noticia = str_squish(cuerpo_noticia)  # parecido a trim, pero además saca dos o más espacios seguidos
  
  return(cuerpo_noticia)
}

# La probamos con una noticia cualquiera
extraer_cuerpo_noticia(
  "https://es.globalvoices.org/2025/07/29/moldavia-se-queda-vacia-las-cuatro-olas-de-emigracion-de-moldavia/"
)


#EXTRAEMOS EL CUERPO DE LAS NOTICIAS. 

# Definimos una función que extrae el texto de cada noticia haciendo request al url
extraer_cuerpo_noticia = function(url) {
  Sys.sleep(1) # Pausa para no sobrecargar el servidor
  html_noticia = read_html(url)
  
  # El selector para el texto completo de la noticia puede variar según la estructura del sitio
  cuerpo_noticia = html_noticia |>
    html_elements("#rightmaincol") |> # Selector para el contenido de la noticia
    html_elements("p" ) |> # Extraemos los párrafos para obtener el texto completo (sin headers ni partes que puedan dificultar mucho el análisis)
    html_text2() |>
    str_trim()
  
  # Concatenamos todos los párrafos
  cuerpo_noticia = str_c(cuerpo_noticia, collapse = " ")
  
  # Eliminamos caracteres 
  cuerpo_noticia = str_replace_all(cuerpo_noticia, "[\\r\\n\\t]+", " ")
  
  # Eliminamos todo tipo de comillas (estándar, tipográficas, simples, dobles, backticks y porcentajes)
  cuerpo_noticia = str_replace_all(cuerpo_noticia, "[\\\"'“”‘’«»`´%()]", "")
  
  # Eliminamos espacios múltiples
  cuerpo_noticia = str_squish(cuerpo_noticia)  # parecido a trim, pero además saca dos o más espacios seguidos
  
  return(cuerpo_noticia)
}

# La probamos con una noticia cualquiera
extraer_cuerpo_noticia(
  "https://www.oas.org/es/centro_noticias/comunicado_prensa.asp?sCodigo=C-043/26"
)
extraer_cuerpo_noticia("https://www.oas.org/es/centro_noticias/comunicado_prensa.asp?sCodigo=C-047/26")


#Creamos otra base de datos para almacenar los cuerpos
cuerpos_noticias_oea<- noticias_oea |>
  select(id, url) |> # mismo ID que el dataset original para hacer un join más tarde
  mutate(cuerpo = map_chr(url, extraer_cuerpo_noticia)) |> 
  select(-url) # tiene id y cuerpo nada más

# Guardamos esta tabla por separado para ahorrar memoria y no tener que volver a hacer scraping cada vez que queramos analizar el texto

# Le asignamos la misma fecha de descarga que el dataset original por consistencia
attr(cuerpos_noticias_oea, "fecha_descarga") <- pagina_html |> attr("fecha_descarga")

cuerpos_noticias_oea |> write_rds(
  file.path(data_dir, "cuerpos_noticias_oea")
)








