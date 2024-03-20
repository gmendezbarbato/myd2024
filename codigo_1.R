##----------------------------------------------------------------------------##
#                                                                              #
#    Medición y Diseño 2024 | Carneiro y Méndez                                #
#    LIVE CODING CLASE 1. INTRODUCCIÓN A R Y RSTUDIO                           #
#                                                                              #
##----------------------------------------------------------------------------##

## Para correr código es:
### Cursor en la línea o selección y ctrl + enter (o botón Run)

## ANOTACIONES ----

# Esta línea es una anotación. 
# R ignora todo lo que está acá adentro (tiene que empezar con #)
# Podemos escribir nombres de funciones u objetos y R no las va a interpetar
# Usar anotaciones es clave para poder entender qué fue lo que hicimos anteriormente


## AYUDA ----

help() ## documentación de la función help
help(mean) ## ayuda de la función mean (media) de Rbase
?mean ## otra forma de hacer lo mismo


## R COMO CALCULADORA ----

# Operaciones sencillas

2 + 2
## [1] 4

20 - 10

## [1] 10

10 / 2
## [1] 5

10 * 10
## [1] 100

## OBJETOS EN R ----

# Una operación sencilla:
17*17*7 # Se imprime el resultado

anio <- 17*17*7 # Se crea un objeto

(anio <- 17*17*7) # Se crea un objeto y se imprime


## ALGUNAS FUNCIONES BÁSICAS ----

ls() # Lista los objetos en el ambiente
rm(anio) # Borra objeto del ambiente
rm(list=ls()) # Borra todos los objetos del ambiente
help(ls) # Buscar ayuda sobre una función


## TIPOS Y CLASES ----

obj_1 <- "10"

typeof(obj_1)
## [1] "character"

obj_1 + 20 # Da error
## Error in obj_1 + 20: argumento no-numérico para operador binario


obj_1 <- 10
typeof(obj_1)
## [1] "double"

obj_1 + 20 # Funciona
## [1] 30

## COERCIONADORES ----

obj_1 <- "10"
typeof(obj_1)
## [1] "character"

obj_1 <- as.numeric(obj_1)
typeof(obj_1)
## [1] "double"

is.numeric(obj_1) # Podemos verificarlo directamente también
## [1] TRUE


## VECTORES ----

mi_primer_vector <- c(1, 3, 5, 7, 143) 
print(mi_primer_vector)

## [1]   1   3   5   7 143


## Otras formas de crear vectores con : y seq()

v1 <- c(1:5) # Todos los números de 1 a 5
v1
## [1] 1 2 3 4 5
v2 <- seq(0, 50, 10) # De 0 a 50 de a 10 números
v2
## [1]  0 10 20 30 40 50


# Indexación:
mi_primer_vector
## [1]   1   3   5   7 143

mi_primer_vector[1] # El primer elemento dentro del vector
## [1] 1

# Nos sirve por ejemplo para extraer partes del vector:
v3 <- mi_primer_vector[1:3] # Creo nuevo vector con los elementos del 1 al 3
v3
## [1] 1 3 5

## EJERCICIO (abrir archivo ejercicios_1.R)
