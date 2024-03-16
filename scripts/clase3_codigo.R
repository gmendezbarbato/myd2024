
## ***************************************************************************
##  Módulo 3: Análisis de datos          
##  Código de la presentación                                      
##  Medición y diseño de investigación
##  Martín Opertti y Fabricio Carneiro - 2023                                         
## ***************************************************************************


##  1. Importar datos con etiquetas  =======================================

rm(list = ls())

library(tidyverse)
library(haven)

## 1.1. Cargar base de datos ----
data_uru <- read_dta("data_lapop/md_lapop_uruguay_2019.dta")
glimpse(data_uru) # ver el tipo de variables

# 1.2. Pasar todas las variables a factores ----
data_uru <- read_dta("data_lapop/md_lapop_uruguay_2019.dta") %>% 
  as_factor()

# 1.3. Factores  ----
# Podemos chequear y coercionar factores
is.factor(data_uru$q1) # Chequeo si es factor

levels(data_uru$q1) # Chequeo los niveles

# Transformo a caracter
data_uru$q1 <- as.character(data_uru$q1) 
class(data_uru$q1)

# De vuelta a factor
data_uru$q1 <- as.factor(data_uru$q1)
class(data_uru$q1)



##  2. Estimación intervalo de confianza  =================================

# 2.1. Intervalo de confianza para una media ----

# Estimación media de ideología con intervalo de confianza 
data_uru %>% 
  mutate(l1 = as.numeric(l1)) %>% # pasar ideología a variable numérica
  summarise(media = mean(l1),
            sd = sd(l1),
            n = n()) %>% 
  mutate(se = sd / sqrt(n),
         ci_inf = media - qnorm(.975) * (sd/sqrt(n)), # Intervalo inferior
         ci_sup = media + qnorm(.975) * (sd/sqrt(n)) # Intervalo superior
         )

# 2.2. Intervalo de confianza para una media según grupo ----

# Calcular la media de ideología según identificación partidaria. Notar como 
# podemos manipular la tabla como cualquier dataframe
data_uru %>% 
  mutate(l1 = as.numeric(l1)) %>% # pasar ideología a numérica
  group_by(id_par) %>% 
  summarise(media = mean(l1),
            sd = sd(l1),
            n = n()) %>% 
  mutate(se = sd / sqrt(n),
         ci_inf = media - qnorm(.975) * (sd/sqrt(n)), # Intervalo inferior
         ci_sup = media + qnorm(.975) * (sd/sqrt(n)) # Intervalo superior
         ) %>% 
  select(-sd, -n, -se) %>% # (ya quitamos las variables que sobran)
  filter(id_par %in% c("Frente Amplio", "Partido Nacional", "Partido Colorado")) 

# 2.3. Intervalo de confianza para una proporción ----

# Proporciones simples con su intervalo de confianza
data_uru %>% 
  count(id_par) %>% 
  mutate(
    prop = n/sum(n), # Proporción de cada categoría
    moe = (qnorm(0.975) * sqrt(prop*(1-prop)/nrow(data_uru))), # margen de error al 95% confianza   
    ci_inf =  prop - moe, # Intervalo superior
    ci_sup =  prop + moe # Intervalo superior
  )



##  3. Regresión lineal  ================================================

# En esta sección queremos estudiar el efecto de las preferencias por 
# redistribución en la autoidentificación ideológica. Para ello, estimaremos
# un modelo de regresión lineal para ver el efecto de las preferencias por 
# políticas  redistributivas en la autoidentificación ideológica.´

# 3.1. Chequear el formato de las variables que vamos a utilizar ----

# Chequear el tipo de la variable de preferencias por redistribución
class(data_uru$redist3)

# Tirar tabla de frecuencia simple
count(data_uru, redist3)

# Esta variable, al ser una escala del 1 al 7 (ordinal) la vamos a utilizar 
# como numérica. Antes de eso debemos filtrar el dataframe para eliminar
# las observaciones que tienen valor "No sabe" o "No contesta"
data_uru <- data_uru %>% 
  filter(redist3 != "No sabe") %>% 
  filter(redist3 != "No responde")

# Chequeamos
count(data_uru, redist3)

# Ahora si podemos pasar la variable a numérica
data_uru <- data_uru %>% 
  mutate(redist3_num = as.numeric(redist3))

# Ahora tiramos una tabla cruzada entre la variable original y la numérica
# para chequear que la recodificación hayan funcionado:
data_uru %>% 
  count(redist3, redist3_num) %>% 
  spread(redist3_num, n)

# Ahora repetimos el proceso con la variable de ideología
data_uru %>% 
  count(l1)

data_uru <- data_uru %>% 
  filter(l1 != "No responde") %>% 
  filter(l1 != "No sabe") %>% 
  mutate(l1_num = as.numeric(l1))

data_uru %>% 
  count(l1, l1_num) %>% 
  spread(l1_num, n)


# 3.2. Tabla cruzada o descriptiva entre las dos variables ----

# Antes de estimar el modelo de regresión tiremos una tabla cruzada y una tabla
# con medias para ver de forma preliminar datos sobre la relación entre las dos 
# variables que queremos estudiar

# 3.2.1. Tabla cruzada
data_uru <- data_uru %>% 
  mutate(l1_cat = case_when(
    l1_num <= 4 ~ "Izquierda",
    l1_num %in% c(5, 6) ~ "Centro",
    l1_num >= 7 ~ "Derecha",
  )) %>% 
  mutate(redist3_cat = case_when(
    redist3_num <= 3 ~ "Pro-redistribucion",
    redist3_num == 4 ~ "Neutro",
    redist3_num >= 5 ~ "Anti-redistribucion",
  ))

# Tabla con % a nivel de fila
data_uru %>% 
  count(redist3_cat, l1_cat) %>% 
  mutate(n = n/sum(n)*100, .by = redist3_cat) %>% 
  spread(l1_cat, n)

# 3.2.2. Tabla con medias
data_uru %>%
  group_by(redist3_cat) %>% 
  summarize(media_ideologia = mean(l1_num))


# 3.3. Estimar modelo de regresión ----

# 3.3.1. Estimar modelo entre dos variables
reg <- lm(l1_num ~ redist3_num, data = data_uru)
# Con summary podemos ver los resultados
summary(reg) 

# 3.3.2. Agregar variables de control
# Primero chequeo el tipo de las variables de control a agregar (edad y sexo)
class(data_uru$q1) # Sexo es factor (como es variable categórica está ok)
class(data_uru$q2) # Edad es factor (cómo es variable continua debemos pasar a numérica)

data_uru <- data_uru %>% 
  mutate(edad_num = as.numeric(q2))

reg <- lm(l1_num ~ redist3_num + q1 + edad_num, data = data_uru)
summary(reg)

# 3.3.3. Exportamos resultados a excel
library(broom)
coef <- tidy(reg, conf.int = TRUE) 
print(coef)

writexl::write_xlsx(coef, "resultads/reg.xlsx")












