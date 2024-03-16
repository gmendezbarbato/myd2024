
## ***************************************************************************
##  Módulo 2: Manipulación de datos          
##  Ejercicio                                          
##  Medición y diseño de investigación
##  Martín Opertti y Fabricio Carneiro - 2023                                         
## ***************************************************************************

## Trabajaremos con una base de datos con datos económicos y sociales de 
# distintos países, descargada del banco mundial con datos de 2018.

# La base se llama "wb_paises" y esta en formato excel (.xlsx)

## 1. Importar dataframe "wb_paises" y asignarla con el nombre df_paises.
# Cargar el paquete tidyverse y readxl  (usar funcion read_excel() para 
# importar la base de datos)


## 2. Usa la función glimpse() para explorar "df_paises" y explorar  las variables 
# y su tipo (puedes ayudarte con el codebook que está en la carpeta "data")


## 3. ¿Cuál es el rango de la variable de acceso a electricidad? 
# Estar atento a usar el argumento na.rm = TRUE


## 4. ¿Cuál es el promedio de población total?


## 5. ¿Cuál es la relación entre el gasto en educación y el gasto en salud? 
# Explorar con un gráfico de dispersión 


## 6. Crea una variable "pbi_pc" que calcule el PBI per capita (utilizando las
# variables pbi y pob_total)


## 7. Crear una nueva variable llamada gini_rec que codifique la  variable 
# en tres grupos "bajo", "medio" y "alto". Crear un histrograma para 
# elegir los puntos de corte (definirlos ustedes)


## 8. Crear un nuevo dataframe a partir de df_paises eliminando las  
# observaciones de los paises grupo de ingresos bajos ("Low income") en la 
# variable grupo ingreso y luego seleccionar unicamente las variables:
# pais, region grupo_ingreso, acceso_electricidad y indice_de_gini


## 9. A partir de este nuevo dataframe crear una tabla cruzada entre 
# region y grupo_ingreso


## 10. Crear la misma tabla pero con los % por fila. ¿Qué región tiene mayor
# % de países de alto ingreso (high income)?


# La región es América del Norte (North America) con el 100%

## 11. A partir del dataframe creado en el punto anterior, Crear una tabla
# resumen con la media y desvío del acceso a electricidad e indice de gini
# por region (calcular las estadísticas sin tener en cuenta los datos perdidos)


## 12. A partir de la tabla anterior, ¿cuál es la región más desigual (con el
# indice de gini más alto)? (escribir respuesta)


## 13. ¿Cuál es la región con mayor variación en el acceso a electricidad?


## 14. Exportar la tabla del punto anterior en un archivo excel 

