# 0. LIBRERIAS ----

library(tidyverse)
library(haven)
library(forcats)
library(broom)
library(DT)
library(ggplot2)
library(plotly)

# 1. DATA ----

## 1.1 bases olas lapop ----
uru_2007 <- haven::read_dta("data_lapop/uru/uru_2007.dta") 
uru_2008 <- haven::read_dta("data_lapop/uru/uru_2008.dta") 
uru_2010 <- haven::read_dta("data_lapop/uru/uru_2010.dta") 
uru_2012 <- haven::read_dta("data_lapop/uru/uru_2012.dta")
uru_2014 <- haven::read_dta("data_lapop/uru/uru_2014.dta") 
uru_2017 <- haven::read_dta("data_lapop/uru/uru_2017.dta") 
uru_2019 <- haven::read_dta("data_lapop/uru/uru_2019.dta") 


## 1.2 selección de variables ----
# selecciono variables de interés como factores para quedarme con las etiquetas de los valores
uru_07 <- uru_2007 %>% 
          select(Q1, Q2, ED, IT1, ING4, ETID, VIC1, IDIO2, M1) %>% # selecciono variables
          drop_na () %>% # dado que es un ejercicio, elimino filas con NA
          rename_all(tolower) %>% # paso a minúsculas las variables para coincidir con bases más recientes
          rename("vic1ext" = vic1) %>% # renombro variable para coincidir con bases más recientes
          as_factor() %>% # convierto a factor todas la variables
          mutate(year = 2007)  # identifico año para unificación de base
  

uru_08 <- uru_2008 %>% 
  select(q1, q2, ed, it1, ing4, etid, vic1, idio2, m1) %>% # selecciono variables
  drop_na () %>% # dado que es un ejercicio, elimino filas con NA
  rename("vic1ext" = vic1) %>% # renombro variable para coincidir con bases más recientes
  as_factor() %>%  # convierto a factor todas la variables
  mutate(year = 2008) # identifico año para unificación de base

uru_10 <- uru_2010 %>% 
  select(q1, q2, ed, it1, ing4, etid, vic1ext, idio2, m1) %>% # selecciono variables
  drop_na () %>% # dado que es un ejercicio, elimino filas con NA
  as_factor() %>% # convierto a factor todas la variables
  mutate(year = 2010) # identifico año para unificación de base


uru_12 <- uru_2012 %>% 
  select(q1, q2, ed, it1, ing4, etid, vic1ext, idio2, m1) %>% # selecciono variables
  drop_na () %>% # dado que es un ejercicio, elimino filas con NA
  as_factor() %>%  # convierto a factor todas la variables
  mutate(q2 = haven::as_factor(q2)) %>% # fuerzo conversión a factor
  mutate(year = 2012) # identifico año para unificación de base

sum(is.na(uru_12))

uru_14 <- uru_2014 %>% 
  select(q1, q2, ed, it1, ing4, etid, vic1ext, idio2, m1) %>% # selecciono variables
         drop_na () %>% # dado que es un ejercicio, elimino filas con NA
         as_factor() %>%  # convierto a factor todas la variables
         mutate(q2 = haven::as_factor(q2)) %>% # fuerzo conversión a factor
         mutate(year = 2014) # identifico año para unificación de base

uru_17 <- uru_2017 %>% 
  select(q1, q2, ed, it1, ing4, etid, vic1ext, idio2, m1) %>% # selecciono variables
  drop_na () %>% # dado que es un ejercicio, elimino filas con NA
  as_factor()  %>% # convierto a factor todas la variables
  mutate(q2 = haven::as_factor(q2)) %>% # fuerzo conversión a factor
  mutate(year = 2017) # identifico año para unificación de base

uru_19 <- uru_2019 %>% 
  select(q1, q2, ed, it1, ing4, etid, vic1ext, idio2, m1) %>% # selecciono variables
  drop_na () %>% # dado que es un ejercicio, elimino filas con NA
  as_factor()  %>% # convierto a factor todas la variables
  mutate(q2 = haven::as_factor(q2)) %>% # fuerzo conversión a factor
  mutate(year = 2019) # identifico año para unificación de base

# unifico base
data_uru <- dplyr::bind_rows(uru_07, uru_08, uru_10, uru_12, uru_14, uru_17, uru_19) 

# elimino objetos y dejo solo la base que voy a usar
rm(list=setdiff(ls(), "data_uru"))

# 2. LIMPIEZA ----

# elimino niveles no usados
data_uru <- droplevels(data_uru) 

## 2.1 Variable dependiente ----

### 2.1.1 variable ing4 (apoyo_democracia_fct) ----
levels(data_uru$ing4) # niveles
# recodifico x idioma
data_uru$ing4 <- fct_recode(data_uru$ing4, 
                            `Muy en desacuerdo` = "1 Muy en desacuerdo", 
                            `Muy en desacuerdo` = "Strongly Disagree", 
                            "2" = "( 2 )",
                            "3" = "( 3 )",
                            "4" = "( 4 )",
                            "5" = "( 5 )",
                            "6" = "( 6 )",
                            `Muy de acuerdo` = "7 Muy de acuerdo",
                            `Muy de acuerdo` = "Strongly Agree") 
# chequeo con tidy
count(data_uru, ing4)

### 2.1.2 variable ing4_num (apoyo_democracia_num) ----
# creo ing4_num como numerica porque es la variable dependiente en la regresion
data_uru$ing4_num <- as.numeric(data_uru$ing4)
# chequeo con tidy
count(data_uru, ing4_num )

# chequeo que este ok el pasaje a numerica cruzando con la var fct
data_uru %>% 
  count(ing4, ing4_num) %>% 
  spread(ing4_num, n)

### 2.1.3 variable ing4_bin (apoyo_democracia_bin) ----
# creo ing4_num como binaria
data_uru <-  data_uru %>% 
  mutate(ing4_bin = factor(case_when(# elimino var de origen para simplificar base
    ing4_num %in% c(0:5) ~ "No apoyo",
    ing4_num %in% c(6:7) ~ "Apoyo",
  ),
  levels = c("No apoyo", "Apoyo")))
# chequeo con tidy
count(data_uru, ing4_bin)

# chequeo que este ok el pasaje a numerica cruzando con la var fct
data_uru %>% 
  count(ing4, ing4_num) %>% 
  spread(ing4_num, n)


## 2.2 Variables independientes ----


### 2.2.1 variable q1 (sexo)----
levels(data_uru$q1) # niveles
# recodifico x idioma
data_uru$q1 <- fct_recode(data_uru$q1, Hombre = "Male", Mujer = "Female") 
# chequeo con R base
table(data_uru$q1)


### 2.2.2 variable q2 (edad) ----
levels(data_uru$q2) # niveles
# chequeo 
table(data_uru$q2) 

# creo una nueva variable g2 (edad_grupos)
data_uru <- data_uru %>%
  mutate(q2_grupos = factor(case_when(# elimino var de origen para simplificar base
  q2 %in% c(18:25) ~ "18-25",
  q2 %in% c(26:35) ~ "26-35",
  q2 %in% c(36:45) ~ "36-45",
  q2 %in% c(46:55) ~ "46-55",
  q2 %in% c(56:65) ~ "56-65",
  q2 %in% c(66:150) ~ "66 y más",
),
levels = c("18-25", "26-35", "36-45", "46-55", "56-65", "66 y más")))


### 2.2.3 variable ed (nivel_educ) ----
levels(data_uru$ed) # niveles
# chequeo con R base
table(data_uru$ed)
# muchos niveles, convierto a numerica y recodifico
data_uru <- data_uru %>%
  mutate(ed = as.numeric(ed)) %>% 
  mutate(ed = factor(case_when(# elimino var de origen para simplificar base
    ed %in% c(1:6) ~ "Primaria",
    ed %in% c(7:12) ~ "Secundaria",
    ed %in% c(13:100) ~ "Terciaria",
  ),
  levels = c("Primaria", "Secundaria", "Terciaria")))

### 2.2.4 variable etid (etnia_raza) ----
levels(data_uru$etid) # niveles
# recodifico x idioma
data_uru$etid <- fct_recode(data_uru$etid, 
                            Blanca = "White", 
                            Indigena = "Indigenous", 
                            Indigena = "Indígena", 
                            Otra = "Other",
                            `Negra (afrouruguaya)` = "Negra (agrouruguaya)",
                            `Negra (afrouruguaya)` = "Negra",
                            `Negra (afrouruguaya)` = "Black", 
                            Mulata = "Mulatto", 
                            Mestiza = "Mestizo")
# chequeo con tidy
count(data_uru, etid)

### 2.2.5 variable it1 (conf_interpersonal) ----
levels(data_uru$it1) # niveles
# recodifico x idioma
data_uru$it1 <- fct_recode(data_uru$it1,  
                           `Nada confiable` = "Untrustworthy", 
                           `Poco confiable` = "Not Very Trustworthy", 
                           `Algo confiable` = "Somewhat Trustworthy",
                           `Muy confiable` = "Very Trustworthy")
# chequeo con R base
table(data_uru$it1)

### 2.2.6 variable vic1ext (victima_delito) ----
levels(data_uru$vic1ext) # niveles
# recodifico x idioma
data_uru$vic1ext <- fct_recode(data_uru$vic1ext, 
                            Si = "Sí", 
                            Si = "Yes")
# chequeo con tidy
count(data_uru, vic1ext)

### 2.2.7 variable idio2 (sit_economica) ----
levels(data_uru$idio2) # niveles
# recodifico x idioma
data_uru$idio2 <- fct_recode(data_uru$idio2, 
                             Mejor = "Better", 
                             Igual = "Same",
                             Peor = "Worse")
# chequeo con tidy
count(data_uru, idio2)

### 2.2.8 variable m1 (eval_gobierno) ----
levels(data_uru$m1) # niveles
# recodifico x idioma
data_uru$m1 <- fct_recode(data_uru$m1, 
                          `Muy bueno` = "Very Good", 
                           Bueno = "Good", 
                          `Ni bueno, ni malo` = "Neither Good nor Bad (Fair)", 
                          `Ni bueno, ni malo` = "Ni bueno ni malo", 
                          `Ni bueno, ni malo` = "Ni bueno ni malo (regular)", 
                           Malo = "Bad",
                          `Muy malo` = "Muy malo (pésimo)",
                          `Muy malo` = "Very Bad")

### 2.2.9 variable year (año) ----
# chequeo con tidy
count(data_uru, year)


# 3. RENOMBRO VARIABLES ----

data_uru <- data_uru %>% 
            rename(
             "sexo" = q1,
             "edad" = q2,
             "edad_grupos" = q2_grupos,
             "nivel_educ" = ed,
             "conf_interpersonal" = it1,
             "apoyo_democracia_fct" = ing4,
             "apoyo_democracia_num" = ing4_num,
             "apoyo_democracia_bin" = ing4_bin,
             "etnia_raza" = etid,
             "victima_delito" = vic1ext,
             "sit_economica" = idio2,
             "eval_gobierno" = m1,
             "año" = year)
             

# 4. TABLAS CRUZADAS DE APOYO A LA DEMOCRACIA ----

## 4.1 sexo ----
t_sexo <- data_uru %>% 
  count(apoyo_democracia_num, sexo) %>% 
  mutate(n = round((n/sum(n)*100),2), .by = sexo) %>% 
  spread(apoyo_democracia_num, n) %>% 
  DT::datatable(.,
                caption = "Tabla. Apoyo a la democracia según sexo. Uruguay 2007-2019.", 
                options = list(searching = FALSE,
                               info = FALSE,
                               dom = "rtip"))

## 4.2 edad_grupos ----
t_edad_grupos <- data_uru %>% 
  count(apoyo_democracia_num, edad_grupos) %>% 
  mutate(n = round((n/sum(n)*100),2), .by = edad_grupos) %>% 
  spread(apoyo_democracia_num, n) %>% 
  DT::datatable(.,
                caption = "Tabla. Apoyo a la democracia según grupos de edad. Uruguay 2007-2019.", 
                options = list(searching = FALSE,
                               info = FALSE,
                               dom = "rtip"))         

## 4.3 nivel_educ ----
t_nivel_educ <- data_uru %>% 
  count(apoyo_democracia_num, nivel_educ) %>% 
  mutate(n = round((n/sum(n)*100),2), .by = nivel_educ) %>% 
  spread(apoyo_democracia_num, n) %>% 
  DT::datatable(.,
                caption = "Tabla. Apoyo a la democracia según nivel educativo. Uruguay 2007-2019.", 
                options = list(searching = FALSE,
                               info = FALSE,
                               dom = "rtip"))           


## 4.4 etnia_raza ----
t_etnia_raza <- data_uru %>% 
  count(apoyo_democracia_num, etnia_raza) %>% 
  mutate(n = round((n/sum(n)*100),2), .by = etnia_raza) %>% 
  spread(apoyo_democracia_num, n) %>% 
  DT::datatable(.,
                caption = "Tabla. Apoyo a la democracia según ascendencia étnico-racial. Uruguay 2007-2019.", 
                options = list(searching = FALSE,
                               info = FALSE,
                               dom = "rtip"))          

## 4.5 conf_interpersonal ----
t_conf_interpersonal <- data_uru %>% 
  count(apoyo_democracia_num, conf_interpersonal) %>% 
  mutate(n = round((n/sum(n)*100),2), .by = conf_interpersonal) %>% 
  spread(apoyo_democracia_num, n)  %>% 
  DT::datatable(.,
                caption = "Tabla. Apoyo a la democracia según confianza interpersonal. Uruguay 2007-2019.", 
                options = list(searching = FALSE,
                               info = FALSE,
                               dom = "rtip")) 


## 4.6 victima_delito ----
t_victima_delito <- data_uru %>% 
  count(apoyo_democracia_num, victima_delito) %>% 
  mutate(n = round((n/sum(n)*100),2), .by = victima_delito) %>% 
  spread(apoyo_democracia_num, n) %>% 
  DT::datatable(.,
                caption = "Tabla. Apoyo a la democracia según si fue víctima de delitos. Uruguay 2007-2019.", 
                options = list(searching = FALSE,
                               info = FALSE,
                               dom = "rtip"))          

## 4.7 sit_economica ----
t_sit_economica <- data_uru %>% 
  count(apoyo_democracia_num, sit_economica) %>% 
  mutate(n = round((n/sum(n)*100),2), .by = sit_economica) %>% 
  spread(apoyo_democracia_num, n)  %>% 
  DT::datatable(.,
                caption = "Tabla. Apoyo a la democracia según evolución de situación económica personal. Uruguay 2007-2019.", 
                options = list(searching = FALSE,
                               info = FALSE,
                               dom = "rtip"))         

## 4.8 eval_gobierno ----
t_eval_gobierno <- data_uru %>% 
  count(apoyo_democracia_num, eval_gobierno) %>% 
  mutate(n = round((n/sum(n)*100),2), .by = eval_gobierno) %>% 
  spread(apoyo_democracia_num, n) %>% 
  DT::datatable(.,
                caption = "Tabla. Apoyo a la democracia según evaluación de gobierno. Uruguay 2007-2019.", 
                options = list(searching = FALSE,
                               info = FALSE,
                               dom = "rtip"))          

## 4.9 año ----
t_año <- data_uru %>% 
  count(apoyo_democracia_num, año) %>% 
  mutate(n = round((n/sum(n)*100),2), .by = año) %>% 
  spread(apoyo_democracia_num, n)  %>% 
  DT::datatable(.,
                caption = "Tabla. Apoyo a la democracia según año. Uruguay 2007-2019.", 
                options = list(searching = FALSE,
                               info = FALSE,
                               dom = "rtip"))         



# 5. GRAFICOS DE APOYO A LA DEMOCRACIA  ----

library(ggplot2)
library(plotly)

## 4.1 sexo ----
g_sexo <- data_uru %>% 
  count(apoyo_democracia_bin, sexo) %>% 
  mutate(n = round((n/sum(n)*100),2), .by = sexo) %>% 
  spread(apoyo_democracia_bin, n) %>% 
  ggplot(., aes(x=sexo, y=Apoyo, fill = sexo,
                text= paste("</br>Sexo:", sexo,"</br>Apoyo a la democracia:", paste0(Apoyo),"%"))) +
  geom_col(width = 0.5, color = "black", size = 0.3) +
  scale_fill_brewer(palette = "Pastel1") +
  ylim(0, NA) +
  ggtitle("Apoyo a la democracia según sexo (Uruguay 2007-2019)") +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())

#grafico interactivo con plotly
g_sexo <- plotly::ggplotly(g_sexo, hoverinfo = 'text',tooltip = c("text")) %>% 
  plotly::layout(annotations = 
                   list(x = 1, y = -0.1, 
                        text = "Fuente: The Americas Barometer by the LAPOP Lab www.vanderbilt.edu/lapop", 
                        showarrow = F, xref='paper', yref='paper', 
                        xanchor='right', yanchor='auto', xshift=0, yshift=0,
                        font=list(size=10)))


## 4.2 edad_grupos ----
g_edad_grupos <- data_uru %>% 
  count(apoyo_democracia_bin, edad_grupos) %>% 
  mutate(n = round((n/sum(n)*100),2), .by = edad_grupos) %>% 
  spread(apoyo_democracia_bin, n) %>% 
  ggplot(., aes(x=edad_grupos, y=Apoyo, fill = edad_grupos,
                text= paste("</br>Grupos de edad:", edad_grupos,"</br>Apoyo a la democracia:", paste0(Apoyo),"%"))) +
  geom_col(width = 0.5, color = "black", size = 0.3) +
  scale_fill_brewer(palette = "Pastel1") +# scale_fill_manual(
  ylim(0, NA) +
  ggtitle("Apoyo a la democracia según grupos de edad (Uruguay 2007-2019)") +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(plot.caption.position = "panel") +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())

#grafico interactivo con plotly
g_edad_grupos <- plotly::ggplotly(g_edad_grupos, hoverinfo = 'text',tooltip = c("text")) %>% 
  plotly::layout(annotations = 
           list(x = 1, y = -0.1, text = "Fuente: The Americas Barometer by the LAPOP Lab www.vanderbilt.edu/lapop", 
                showarrow = F, xref='paper', yref='paper', 
                xanchor='right', yanchor='bottom', xshift=0, yshift=0,
                font=list(size=10)))
         
## 4.3 nivel_educ ----
g_nivel_educ <- data_uru %>% 
  count(apoyo_democracia_bin, nivel_educ) %>% 
  mutate(n = round((n/sum(n)*100),2), .by = nivel_educ) %>% 
  spread(apoyo_democracia_bin, n) %>% 
  ggplot(., aes(x=nivel_educ, y=Apoyo, fill = nivel_educ,
                text= paste("</br>Nivel educativo:", nivel_educ,"</br>Apoyo a la democracia:", paste0(Apoyo),"%"))) +
  geom_col(width = 0.5, color = "black", size = 0.3) +
  scale_fill_brewer(palette = "Pastel1") +
  ylim(0, NA) +
  ggtitle("Apoyo a la democracia según nivel educativo (Uruguay 2007-2019)") +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(plot.caption.position = "panel") +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())

#grafico interactivo con plotly
g_nivel_educ <- plotly::ggplotly(g_nivel_educ, hoverinfo = 'text',tooltip = c("text")) %>% 
  plotly::layout(annotations = 
                   list(x = 1, y = -0.1, text = "Fuente: The Americas Barometer by the LAPOP Lab www.vanderbilt.edu/lapop", 
                        showarrow = F, xref='paper', yref='paper', 
                        xanchor='right', yanchor='bottom', xshift=0, yshift=0,
                        font=list(size=10)))   

## 4.4 etnia_raza ----
g_etnia_raza <- data_uru %>% 
  count(apoyo_democracia_bin, etnia_raza) %>% 
  mutate(n = round((n/sum(n)*100),2), .by = etnia_raza) %>% 
  spread(apoyo_democracia_bin, n) %>% 
  ggplot(., aes(x=etnia_raza, y=Apoyo, fill = etnia_raza,
                text= paste("</br>Ascendencia étnico racial:", etnia_raza,"</br>Apoyo a la democracia:", paste0(Apoyo),"%"))) +
  geom_col(width = 0.5, color = "black", size = 0.3) +
  scale_fill_brewer(palette = "Pastel1") +
  ylim(0, NA) +
  ggtitle("Apoyo a la democracia según ascendencia étnico racial (Uruguay 2007-2019)") +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(plot.caption.position = "panel") +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())

#grafico interactivo con plotly
g_etnia_raza <- plotly::ggplotly(g_etnia_raza, hoverinfo = 'text',tooltip = c("text")) %>% 
  plotly::layout(annotations = 
                   list(x = 1, y = -0.1, text = "Fuente: The Americas Barometer by the LAPOP Lab www.vanderbilt.edu/lapop", 
                        showarrow = F, xref='paper', yref='paper', 
                        xanchor='right', yanchor='bottom', xshift=0, yshift=0,
                        font=list(size=10)))   


## 4.5 conf_interpersonal ----
g_conf_interpersonal <- data_uru %>% 
  count(apoyo_democracia_bin, conf_interpersonal) %>% 
  mutate(n = round((n/sum(n)*100),2), .by = conf_interpersonal) %>% 
  spread(apoyo_democracia_bin, n) %>% 
  ggplot(., aes(x=conf_interpersonal, y=Apoyo, fill = conf_interpersonal,
                text= paste("</br>Confianza interpersonal:", conf_interpersonal,"</br>Apoyo a la democracia:", paste0(Apoyo),"%"))) +
  geom_col(width = 0.5, color = "black", size = 0.3) +
  scale_fill_brewer(palette = "Pastel1") +
  ylim(0, NA) +
  ggtitle("Apoyo a la democracia según confianza interpersonal (Uruguay 2007-2019)") +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(plot.caption.position = "panel") +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())

#grafico interactivo con plotly
g_conf_interpersonal <- plotly::ggplotly(g_conf_interpersonal, hoverinfo = 'text',tooltip = c("text")) %>% 
  plotly::layout(annotations = 
                   list(x = 1, y = -0.1, text = "Fuente: The Americas Barometer by the LAPOP Lab www.vanderbilt.edu/lapop", 
                        showarrow = F, xref='paper', yref='paper', 
                        xanchor='right', yanchor='bottom', xshift=0, yshift=0,
                        font=list(size=10)))    

## 4.6 victima_delito ----
g_victima_delito <- data_uru %>% 
  count(apoyo_democracia_bin, victima_delito) %>% 
  mutate(n = round((n/sum(n)*100),2), .by = victima_delito) %>% 
  spread(apoyo_democracia_bin, n) %>% 
  ggplot(., aes(x=victima_delito, y=Apoyo, fill = victima_delito,
                text= paste("</br>Victima de delitos:", victima_delito,"</br>Apoyo a la democracia:", paste0(Apoyo),"%"))) +
  geom_col(width = 0.5, color = "black", size = 0.3) +
  scale_fill_brewer(palette = "Pastel1") +
  ylim(0, NA) +
  ggtitle("Apoyo a la democracia según si la persona fue víctima de delito en el último año (Uruguay 2007-2019)") +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(  plot.caption.position = "panel") +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())

#grafico interactivo con plotly
g_victima_delito <- plotly::ggplotly(g_victima_delito, hoverinfo = 'text',tooltip = c("text")) %>% 
  plotly::layout(annotations = 
                   list(x = 1, y = -0.1, text = "Fuente: The Americas Barometer by the LAPOP Lab www.vanderbilt.edu/lapop", 
                        showarrow = F, xref='paper', yref='paper', 
                        xanchor='right', yanchor='bottom', xshift=0, yshift=0,
                        font=list(size=10)))   


## 4.7 sit_economica ----
g_sit_economica <- data_uru %>% 
  count(apoyo_democracia_bin, sit_economica) %>% 
  mutate(n = round((n/sum(n)*100),2), .by = sit_economica) %>% 
  spread(apoyo_democracia_bin, n) %>% 
  ggplot(., aes(x=sit_economica, y=Apoyo, fill = sit_economica,
                text= paste("</br>Situación económica:", sit_economica,"</br>Apoyo a la democracia:", paste0(Apoyo),"%"))) +
  geom_col(width = 0.5, color = "black", size = 0.3) +
  scale_fill_brewer(palette = "Pastel1") +
  ylim(0, NA) +
  ggtitle("Apoyo a la democracia según evolución de la situación económica (Uruguay 2007-2019)") +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(  plot.caption.position = "panel") +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())

#grafico interactivo con plotly
g_sit_economica <- plotly::ggplotly(g_sit_economica, hoverinfo = 'text',tooltip = c("text")) %>% 
  plotly::layout(annotations = 
                   list(x = 1, y = -0.1, text = "Fuente: The Americas Barometer by the LAPOP Lab www.vanderbilt.edu/lapop", 
                        showarrow = F, xref='paper', yref='paper', 
                        xanchor='right', yanchor='bottom', xshift=0, yshift=0,
                        font=list(size=10)))  

## 4.8 eval_gobierno ----
g_eval_gobierno <- data_uru %>% 
  count(apoyo_democracia_bin, eval_gobierno) %>% 
  mutate(n = round((n/sum(n)*100),2), .by = eval_gobierno) %>% 
  spread(apoyo_democracia_bin, n) %>% 
  ggplot(., aes(x=eval_gobierno, y=Apoyo, fill = eval_gobierno,
                text= paste("</br>Evaluación del gobierno:", eval_gobierno,"</br>Apoyo a la democracia:", paste0(Apoyo),"%"))) +
  geom_col(width = 0.5, color = "black", size = 0.3) +
  scale_fill_brewer(palette = "Pastel1") +
  ylim(0, NA) +
  ggtitle("Apoyo a la democracia según evaluación del gobierno (Uruguay 2007-2019)") +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(  plot.caption.position = "panel") +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())

#grafico interactivo con plotly
g_eval_gobierno <- plotly::ggplotly(g_eval_gobierno, hoverinfo = 'text',tooltip = c("text")) %>% 
  plotly::layout(annotations = 
                   list(x = 1, y = -0.1, text = "Fuente: The Americas Barometer by the LAPOP Lab www.vanderbilt.edu/lapop", 
                        showarrow = F, xref='paper', yref='paper', 
                        xanchor='right', yanchor='bottom', xshift=0, yshift=0,
                        font=list(size=10)))  


## 4.9 año ----
g_año <- data_uru %>% 
  count(apoyo_democracia_bin, año) %>% 
  mutate(n = round((n/sum(n)*100),2), .by = año) %>% 
  spread(apoyo_democracia_bin, n) %>% 
  ggplot(., aes(x=as.factor(año), y=Apoyo, fill = as.factor(año),
                text= paste("</br>Año:", as.factor(año),"</br>Apoyo a la democracia:", paste0(Apoyo),"%"))) +
  geom_col(width = 0.5, color = "black", size = 0.3) +
  scale_fill_brewer(palette = "Pastel1") +
  ylim(0, NA) +
  ggtitle("Apoyo a la democracia según año de ola de Lapop (Uruguay 2007-2019)") +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(  plot.caption.position = "panel") +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())

#grafico interactivo con plotly
g_año <- plotly::ggplotly(g_año, hoverinfo = 'text',tooltip = c("text")) %>% 
  plotly::layout(annotations = 
                   list(x = 1, y = -0.1, text = "Fuente: The Americas Barometer by the LAPOP Lab www.vanderbilt.edu/lapop", 
                        showarrow = F, xref='paper', yref='paper', 
                        xanchor='right', yanchor='bottom', xshift=0, yshift=0,
                        font=list(size=10))) 

# 6.MODELOS ----

## 6.1 Regresión lineal simple ----

reg <- lm(apoyo_democracia_num ~ eval_gobierno, data = data_uru)

summary(reg) # imprimo resultados

coef <- broom::tidy(reg, conf.int = TRUE) 
print(coef)


# Grafico simple de la relación a partir de los promedios

data_grafico <- data_uru %>%
  group_by(eval_gobierno) %>%
  summarise(apoyo_democracia_num_prom = mean(apoyo_democracia_num), n = n())

g <- ggplot(data_grafico, aes(x = desc(eval_gobierno), y = apoyo_democracia_num_prom)) +
  geom_point(size = 2) +
  ggtitle("Promedio de apoyo a la democracia según evaluación del gobierno (Uruguay 2007-2019)") +
  theme_minimal() +
  geom_smooth(method = "lm", se = TRUE, color = "#138D75") +
  labs(x = "Promedio de evaluación del gobierno",
       y = "Apoyo a la democracia",
       caption = "Fuente: Elaboración propia en base a LAPOP") +
  scale_x_continuous(label = c("Muy malo", "Malo", "Ni bueno, ni malo", "Bueno", "Muy bueno")) 

## 6.2 Regresión lineal múltiple ----

reg2 <- lm(apoyo_democracia_num ~ eval_gobierno + sit_economica + conf_interpersonal + victima_delito + as.numeric(año) + as.numeric(edad) + sexo + nivel_educ + etnia_raza, data = data_uru)

summary(reg2) # imprimo resultados

print(broom::tidy(reg2, conf.int = TRUE), n = 40)

## 6.3 Probit ----

reg3 <- glm(apoyo_democracia_bin ~ eval_gobierno + sit_economica + conf_interpersonal + as.numeric(año) + as.numeric(edad) + nivel_educ + etnia_raza, family=binomial(link="probit"), data = data_uru)

summary(reg3) # imprimo resultados

print(broom::tidy(reg3, conf.int = TRUE), n = 40)








