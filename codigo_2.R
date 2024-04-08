##----------------------------------------------------------------------------##
#                                                                              #
#    Medición y Diseño 2024 | Carneiro y Méndez                                #
#    LIVE CODING CLASE 2. INTRODUCCIÓN A R Y RSTUDIO                           #
#                                                                              #
##----------------------------------------------------------------------------##

# Puedo fijar el directorio de trabajo con la función setwd()
# Fijar la carpeta donde vamos a importar y exportar los archivos:
setwd("micompu/micarpeta") # ojo con la / no es \
getwd() # Con está función puedo consultar el directorio

# Ahora, si quiero leer un archivo que esté en "micompu/micarpeta" simplemente 
# escribo su nombre dentro de la función, en el lugar del "path".
# Supongamos que tengo dentro de la carpeta "micarpeta" un excel con datos
# de desempleo en Uruguay:
library(readxl)
desempleo_uru <- read_excel("data/desempleo.xlsx")
head(desempleo_uru, 4)


## ARREGLAR ESTO ----
setwd("C:/Usuario/Martin/Dropbox/cursos/Medicion y diseño/cacrpeta_del_curso") 

datauru <- read_excel("data/datauru.xlsx")


# Con la función read_csv() del paquete readr importamos archivos .csv
library(tidyverse)
gapminder_csv <- read_csv("data/gapminder.csv")
# Con la función read_excel() del paquete readxl importamos archivos excel
library(readxl)
gapminder_excel <- read_excel("data/gapminder.xlsx")
# Vemos que los dataframes son iguales, tienen la mismas filas y columnas
dim(gapminder_csv)
## [1] 1704    6
dim(gapminder_excel)
## [1] 1704    6


#install.packages("gampminder")
library(gapminder)
data_gapminder <- gapminder
head(data_gapminder)

library(haven)
## Warning: package 'haven' was built under R version 4.2.3
# SPSS
gapminder_spss <- read_spss("data/gapminder.sav") 
# STATA
gapminder_stata <- read_stata("data/gapminder.dta")

# SPSS
gapminder_spss <- haven::read_spss("data/gapminder.sav") 
# STATA
gapminder_stata <- haven::read_stata("data/gapminder.dta")


# Para esto no necesitamos cargar paquetes. 
# Guardar un objeto como .rds:
saveRDS(object = data_gapminder,
        file = "resultados/data_gapminder.rds") 
# Leemos un archivo .rds
miobjeto_rds <- readRDS(file = "resultados/data_gapminder.rds")
# Con .rda se pueden guardar varios objetos al mismo tiempo!
# Exportamos un archivo .Rdata
save(data_gapminder, miobjeto_rds,
     file = "resultados/dos_dataframes.Rdata") 
# Importamos un archivo .Rdata
load("resultados/dos_dataframes.Rdata")


# Guardar .csv
library(gapminder)
data_gapminder <- gapminder
write_excel_csv(data_gapminder, "resultados/gapminder.csv")
# Guardar excel
library(writexl)
write_xlsx(data_gapminder, "resultados/gapminder.xlsx")
# Guardar .dta (Stata)
library(haven)
write_dta(data_gapminder, "resultados/gapminder.dta")
# Guardar .sav (SPSS)
write_sav(data_gapminder, "resultados/gapminder.sav")
# Guardar .sas (SAS)
write_sas(data_gapminder, "resultados/gapminder.sas")


dim(data_gapminder) # Número de filas y columnas
## [1] 1704    6
names(data_gapminder) # Nombre de variables
## [1] "country"   "continent" "year"      "lifeExp"   "pop"       "gdpPercap"
head(data_gapminder, 3) # Imprime primeras filas (3 en este caso)
## # A tibble: 3 × 6
##   country     continent  year lifeExp      pop gdpPercap
##   <fct>       <fct>     <int>   <dbl>    <int>     <dbl>
## 1 Afghanistan Asia       1952    28.8  8425333      779.
## 2 Afghanistan Asia       1957    30.3  9240934      821.
## 3 Afghanistan Asia       1962    32.0 10267083      853.


## Rows: 1,704
## Columns: 6
## $ country   <fct> "Afghanistan", "Afghanistan", "Afghanistan", "Afghanistan", …
## $ continent <fct> Asia, Asia, Asia, Asia, Asia, Asia, Asia, Asia, Asia, Asia, …
## $ year      <int> 1952, 1957, 1962, 1967, 1972, 1977, 1982, 1987, 1992, 1997, …
## $ lifeExp   <dbl> 28.801, 30.332, 31.997, 34.020, 36.088, 38.438, 39.854, 40.8…
## $ pop       <int> 8425333, 9240934, 10267083, 11537966, 13079460, 14880372, 12…
## $ gdpPercap <dbl> 779.4453, 820.8530, 853.1007, 836.1971, 739.9811, 786.1134, …



# Para obtener una tabla de frecuencias de una variable usamos la función
# table() de R Base
tabla_1 <- table(data_gapminder$continent) # Frecuencia simple
tabla_1
## 
##   Africa Americas     Asia   Europe  Oceania 
##      624      300      396      360       24

prop.table(tabla_1) # Proporciones
## 
##     Africa   Americas       Asia     Europe    Oceania 
## 0.36619718 0.17605634 0.23239437 0.21126761 0.01408451
addmargins(tabla_1) # Totales
## 
##   Africa Americas     Asia   Europe  Oceania      Sum 
##      624      300      396      360       24     1704
addmargins(prop.table(tabla_1)) # Proporciones y totales
## 
##     Africa   Americas       Asia     Europe    Oceania        Sum 
## 0.36619718 0.17605634 0.23239437 0.21126761 0.01408451 1.00000000


## Medidas de tendencia central

mean(data_gapminder$lifeExp) # Media
## [1] 59.47444
median(data_gapminder$lifeExp) # Mediana
## [1] 60.7125
sd(data_gapminder$lifeExp) # Desvío estandar
## [1] 12.91711


## Rangos

range(data_gapminder$lifeExp) # Rango
## [1] 23.599 82.603
max(data_gapminder$lifeExp)

## [1] 82.603
min(data_gapminder$lifeExp)
## [1] 23.599


## Histogramas

hist(data_gapminder$lifeExp,
     main = "Distribución de expectativa de vida (Gapminder)")


## Gráfico de dispersión (scatterplot)
plot(data_gapminder$lifeExp, data_gapminder$gdpPercap,
     main = "Relación entre expectativa de vida y PBI per cápita")


## Ejercicio (Abrir ejercicios_2.R)


## Filtrar

if(!require(tidyverse)){
  install.packages("tidyverse")
  library(tidyverse)
}

# Tenemos datos de muchos años:
table(gapminder$year)
## 
## 1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 2002 2007 
##  142  142  142  142  142  142  142  142  142  142  142  142
# Filtremos para con los datos a partir de 2007
gapminder_07 <- filter(gapminder, year == 2007)
table(gapminder_07$year)
## 
## 2007 
##  142

## Filtrar

# Todas los años menos 2007
gapminder_pre07 <- filter(gapminder, year != 2007)
table(gapminder_pre07$year)
## 
## 1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 2002 
##  142  142  142  142  142  142  142  142  142  142  142
# Solo los siguientes años: 1952, 1992 y 2007
anios_especificos <- c(1952, 1992, 2007)
gapminder_esp <- filter(gapminder, year %in% anios_especificos)
table(gapminder_esp$year)
## 
## 1952 1992 2007 
##  142  142  142

## Seleccionar variables
# Selccionar un conjunto de variables (país, año, población)
select(gapminder, country, year, pop)
# Selccionar todas las variables menos las especificadas
select(gapminder, -continent)
# Seleccionar un rango de variables según orden
select(gapminder, country:lifeExp)
select(gapminder, 1:3) # Orden numérico


## Pipeline %>%
  
gapminder_07_america <- gapminder %>% 
  filter(year == 2007 & continent == "Americas") %>% 
  select(-continent)
print(gapminder_07_america)


## Crear variables con mutate()

data_gapminder <- gapminder
# Variable de caracteres
data_gapminder <- mutate(data_gapminder, var1 = "Valor fijo") 
# Variable numérica
data_gapminder <- mutate(data_gapminder, var2 = 7) 
head(data_gapminder, 3)
## # A tibble: 3 × 8
##   country     continent  year lifeExp      pop gdpPercap var1        var2
##   <fct>       <fct>     <int>   <dbl>    <int>     <dbl> <chr>      <dbl>
## 1 Afghanistan Asia       1952    28.8  8425333      779. Valor fijo     7
## 2 Afghanistan Asia       1957    30.3  9240934      821. Valor fijo     7
## 3 Afghanistan Asia       1962    32.0 10267083      853. Valor fijo     7
## Podemos escribir lo mismo de distinta manera:
data_gapminder <- mutate(data_gapminder, var1 = "Valor fijo",
                         var2 = 7)

## Recodificar variables con mutate()

## Podemos recodificar usando variables y operadores aritméticos
# Calculemos el pbi total (pbi per capita * población)
d_gap <- mutate(gapminder, gdp = gdpPercap * pop)
head(d_gap, 3)
## # A tibble: 3 × 7
##   country     continent  year lifeExp      pop gdpPercap         gdp
##   <fct>       <fct>     <int>   <dbl>    <int>     <dbl>       <dbl>
## 1 Afghanistan Asia       1952    28.8  8425333      779. 6567086330.
## 2 Afghanistan Asia       1957    30.3  9240934      821. 7585448670.
## 3 Afghanistan Asia       1962    32.0 10267083      853. 8758855797.


# Podemos calcular el logaritmo 
d_gap <- mutate(d_gap, gdp_log = log(gdp))
head(d_gap, 2)
## # A tibble: 2 × 8
##   country     continent  year lifeExp     pop gdpPercap         gdp gdp_log
##   <fct>       <fct>     <int>   <dbl>   <int>     <dbl>       <dbl>   <dbl>
## 1 Afghanistan Asia       1952    28.8 8425333      779. 6567086330.    22.6
## 2 Afghanistan Asia       1957    30.3 9240934      821. 7585448670.    22.7



## Transformaciones de tipo

# Exploro tipo de variables
glimpse(d_gap)
## Rows: 1,704
## Columns: 3
## $ continent <fct> Asia, Asia, Asia, Asia, Asia, Asia, Asia, Asia, Asia, Asia, …
## $ year      <int> 1952, 1957, 1962, 1967, 1972, 1977, 1982, 1987, 1992, 1997, …
## $ lifeExp   <dbl> 28.801, 30.332, 31.997, 34.020, 36.088, 38.438, 39.854, 40.8…
# Variable continente a caracteres y año a factor


d_gap <- d_gap %>% 
  mutate(continent = as.character(continent),
         year = as.factor(year))
glimpse(d_gap)
## Rows: 1,704
## Columns: 3
## $ continent <chr> "Asia", "Asia", "Asia", "Asia", "Asia", "Asia", "Asia", "Asi…
## $ year      <fct> 1952, 1957, 1962, 1967, 1972, 1977, 1982, 1987, 1992, 1997, …
## $ lifeExp   <dbl> 28.801, 30.332, 31.997, 34.020, 36.088, 38.438, 39.854, 40.8…

## Recodificaciones condicionales

data %>% 
  mutate(var_nueva = case_when(var_original == "Valor 1" ~ "Valor A",
                               var_original == "Valor 2" ~ "Valor B",
                               TRUE ~ "Otros"))


## Recodificación condicional con case_when y mutate

d_gap <- gapminder
# Creemos una variable que indique si el país es Uruguay o no
d_gap <- d_gap %>% 
  mutate(uruono = case_when(
    country == "Uruguay" ~ "Si",
    TRUE ~ "No")
  )
table(d_gap$uruono)
## 
##   No   Si 
## 1692   12


d_gap <- gapminder
d_gap <- d_gap %>% 
  mutate(mercosur = case_when(country == "Uruguay" ~ 1,
                              country == "Argentina" ~ 1,
                              country == "Paraguay" ~ 1,
                              country == "Brazil" ~ 1, 
                              TRUE ~ 0))
table(d_gap$mercosur)
## 
##    0    1 
## 1656   48


d_gap <- d_gap %>% 
  mutate(mercosur = case_when(
    country %in% c("Argentina", "Paraguay", "Brazil", "Uruguay") ~ 1,
    TRUE ~ 0)
  ) 
d_gap <- d_gap %>% 
  mutate(mercosur2 = case_when(
    country == "Argentina" | country == "Paraguay" |
      country == "Brazil" | country == "Uruguay" ~ 1,
    TRUE ~ 0)
  )
identical(d_gap$mercosur, d_gap$mercosur2)
## [1] TRUE

d_gap <- d_gap %>% 
  mutate(pob_rec = case_when(
    pop >= 20000000 ~ "Grande",
    pop >= 5000000 & pop < 20000000 ~ "Mediana",
    pop < 5000000 ~ "Pequeña",
    TRUE ~ "Error")
  ) 
table(d_gap$pob_rec)
## 
##  Grande Mediana Pequeña 
##     419     580     705


d_gap <- d_gap %>% 
  mutate(var1 = case_when(gdpPercap > 20000 ~ 1,
                          lifeExp > 75 ~ 1,
                          TRUE ~ 0))
table(d_gap$var1)