
library(tidyverse)
library(ggpubr)
library(stargazer)

rm(list=ls())

df_test <- tibble(x = c(2, -1),
                  y = c(5, -1),
                  etiqueta = c("(2, 5)", "(-1, -1)"))

ggplot(df_test, aes(x = x, y = y)) +
  theme_minimal(base_size = 15) +
  geom_point(size = 3) +
  geom_text(aes(label = etiqueta), vjust = 1.5, size = 5) +
  xlim(-10, 10) +
  ylim(-10, 10) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed")

ggsave("figures/regresion_1.png", width = 20, height = 12, units = "cm")


ggplot(df_test, aes(x = x, y = y)) +
  theme_minimal(base_size = 15) +
  stat_smooth(method = "lm", se = FALSE, fullrange = T, color = "#138D75") +
  geom_point(size = 3) +
  xlim(-10, 10) +
  ylim(-10, 10) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line()

ggsave("figures/regresion_2.png", width = 20, height = 12, units = "cm")


ggplot(df_test %>% 
         add_row(x = 0,
                 y = 1,
                 etiqueta = "(0, 1)"), aes(x = x, y = y)) +
  geom_text(aes(label = etiqueta), hjust = 1.2, size = 5) +
  theme_minimal(base_size = 15) +
  stat_smooth(method = "lm", se = FALSE, fullrange = T, color = "#138D75") +
  geom_point(size = 3) +
  xlim(-10, 10) +
  ylim(-10, 10) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line() +
  annotate("text", x = -4, y = 4, label = "y = 2x + 1", size = 6)

ggsave("figures/regresion_3or.png", width = 20, height = 12, units = "cm")


df <- tibble(pais = c("Argentina", "Chile", "Paraguay", "Brasil", "Uruguay",
                      "Bolivia", "Mexico", "Peru", "Ecuador", "Colombia",
                      "Costa Rica", "Guatemala", "Canada", "RD"),
             id_par = c(22.9, 10.7, 35.4, 23.4, 47.7,
                        18.7, 19.7, 10.7, 22.8, 26,
                        35.9, 12.7, 37.4, 54.5),
             confianza = c(29, 28.8, 27, 26.7, 43,
                           34.4, 32.7, 24.7, 36.5, 31.9,
                           30.1, 26, 42, 40.7))

ggplot(df, aes(x = id_par, y = confianza)) +
  theme_minimal(base_size = 13) +
  geom_point(size = 2) +
  geom_text(aes(label = pais), vjust = 1.5) +
  # geom_smooth(method = "lm", se = FALSE) +
  labs(title = "",
       x = "\n Identificación partidaria (% de la población)",
       y = "Confianza política (% de la población)\n ",
       caption = "Fuente: LAPOP 2018") +
  ylim(10, 55) +
  xlim(10, 55)

ggsave("figures/regresion_4.png", width = 20, height = 12, units = "cm")


ggplot(df, aes(x = id_par, y = confianza)) +
  theme_minimal(base_size = 13) +
  geom_point(size = 2) +
  stat_smooth(method = "lm", se = FALSE, fullrange = T, color = "#138D75") +
  geom_text(aes(label = pais), vjust = 1.5) +
  labs(title = "",
       x = "\n Identificación partidaria (% de la población)",
       y = "Confianza política (% de la población)\n ",
       caption = "Fuente: LAPOP 2018") +
  stat_regline_equation(label.x = 40, label.y = 50, size = 6, color = "#138D75") +
  ylim(10, 65) +
  xlim(10, 65)

ggsave("figures/regresion_10.png", width = 20, height = 12, units = "cm")

ggplot(df %>% 
         add_row(pais = "EEUU",
                 id_par = 64,
                 confianza = 25), aes(x = id_par, y = confianza)) +
  theme_minimal(base_size = 13) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "#138D75") +
  geom_text(aes(label = pais), vjust = 1.5) +
  stat_regline_equation(label.x = 40, label.y = 50, size = 6, color = "#138D75") +
  labs(title = "",
       x = "\n Identificación partidaria (% de la población)",
       y = "Confianza política (% de la población)\n ",
       caption = "Fuente: LAPOP 2018") +
  ylim(10, 65) +
  xlim(10, 65)

ggsave("figures/regresion_10b.png", width = 20, height = 12, units = "cm")



intercept <- 23.6882
slope <- 0.3220
df$fitted <- intercept + slope * df$id_par

ggplot(df, aes(x = id_par, y = confianza)) +
  geom_abline(slope = slope, intercept = intercept, color = "#138D75") +
  geom_segment(aes(xend = id_par, yend = fitted), color = "#1F618D", linetype = "dashed") +
  theme_minimal(base_size = 13) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "#138D75") +
  # geom_text(aes(label = pais), vjust = -1.5) +
  labs(title = "",
       x = "\n Identificación partidaria (% de la población)",
       y = "Confianza política (% de la población)\n ",
       caption = "Fuente: LAPOP 2018") +
  ylim(20, 50) +
  xlim(10, 55) +
  theme(legend.position = "none") +
  annotate("text", x =38.5, y =38, label = "μ3") +
  annotate("text", x =37, y =32, label = "μ2") +
  annotate("text", x =34.5, y =32, label = "μ1") 
  
ggsave("figures/regresion_5_b.png", width = 20, height = 12, units = "cm")


ggplot(df, aes(x = id_par, y = confianza)) +
  theme_minimal(base_size = 13) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "#138D75") +
  geom_text(aes(label = pais), vjust = 1.5) +
  labs(title = "",
       x = "\n Identificación partidaria (% de la población)",
       y = "Confianza política (% de la población)\n ",
       caption = "Fuente: LAPOP 2018") +
  stat_regline_equation(label.x=30, label.y=50, size = 6, color = "#138D75") +
  ylim(10, 55) +
  xlim(10, 55)

ggsave("figures/regresion_6.png", width = 20, height = 12, units = "cm")


m1 <- glm(confianza ~ id_par, data = df)
summary(m1)

stargazer(m1, type='latex', summary=FALSE)


# Quien se anima a pasar y decirme cuál es la confianza polítia esperada
# para un país donde el 30% de la población tiene se identifica con un partido?
24 + (0.32*30)

df <- readxl::read_excel("figures/data_simulada.xlsx")
df

# Relación simple
ggplot(df, aes(x = eco, y = aprobacion)) +
  geom_point(size = 2) +
  theme_minimal(base_size = 13) +
  geom_smooth(method = "lm", se = FALSE, color = "#138D75") +
  labs(x = "Percepción sobre la situación económica del país",
       y = "Evaluación del gobierno",
       caption = "Datos simulados")

ggsave("figures/regresion_7.png", width = 20, height = 12, units = "cm")

m2 <- glm(aprobacion ~ eco, data = df)
summary(m2)
stargazer(m2, type='latex', summary=FALSE, font.size = "footnotesize", p.auto = T)


ggplot(df, aes(x = eco, y = aprobacion, color = id_partidaria)) +
  geom_point(size = 2) +
  theme_minimal(base_size = 13) +
  # geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Percepción sobre la situación económica del país",
       y = "Evaluación del gobierno",
       color = "Identificación partidaria",
       caption = "Datos simulados") +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c("#1F618D", "#616A6B", "#AF601A"))

ggsave("figures/regresion_8.png", width = 20, height = 12, units = "cm")


ggplot(df, aes(x = eco, y = aprobacion, color = id_partidaria)) +
  geom_point(size = 2) +
  theme_minimal(base_size = 13) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Percepción sobre la situación económica del país",
       y = "Evaluación del gobierno",
       color = "Identificación partidaria",
       caption = "Datos simulados") +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c("#1F618D", "#616A6B", "#AF601A"))

ggsave("figures/regresion_9.png", width = 20, height = 12, units = "cm")

m3 <- glm(aprobacion ~ eco + id_partidaria, data = df)
summary(m3)
stargazer(m3, type='latex', summary=FALSE, font.size = "footnotesize", p.auto = T)





## Simular relaciones (linear no linear)

# No hay relación
df_1 <- tibble(x = seq(-100, 100),
             y = (x^2 + rnorm(length(x), 0, 5000))) %>% 
  mutate(x = x + 100,
         y = y + 10000)
  
ggplot(df_1, aes(x = x, y = y)) +
  geom_point(size = 2) +
  theme_minimal(base_size = 13) +
  labs(x = "Variable independiente",
       y = "Variable dependiente") 

ggsave("figures/linear_1.png", width = 20, height = 12, units = "cm")

ggplot(df_1, aes(x = x, y = y)) +
  geom_point(size = 2) +
  theme_minimal(base_size = 13) +
  geom_smooth(method = "lm", se = FALSE, color = "#138D75") +
  labs(x = "Variable independiente",
       y = "Variable dependiente") 

ggsave("figures/linear_2.png", width = 20, height = 12, units = "cm")

# Relación linear
df_2 <- tibble(x = seq(-100, 100),
               y = 100 + (12*x) + rnorm(length(x), 400, 200)) %>% 
  mutate(x = x + 100)

ggplot(df_2, aes(x = x, y = y)) +
  geom_point(size = 2) +
  theme_minimal(base_size = 13) +
  labs(x = "Variable independiente",
       y = "Variable dependiente") 

ggsave("figures/linear_3.png", width = 20, height = 12, units = "cm")

ggplot(df_2, aes(x = x, y = y)) +
  geom_point(size = 2) +
  theme_minimal(base_size = 13) +
  geom_smooth(method = "lm", se = FALSE, color = "#138D75") +
  labs(x = "Variable independiente",
       y = "Variable dependiente") 

ggsave("figures/linear_4.png", width = 20, height = 12, units = "cm")


#  Relación no lineal
df_3 <- tibble(x = seq(-100, 100),
             y = (x^2 + rnorm(length(x), 0, 1000))) %>% 
  mutate(y = case_when(
    x < -95 ~ (y - 7000),
    x >= -95 & x < -75 ~ (y - 6000),
    x >= -75 & x < -50 ~ (y - 4000),
    x > 40 & x < 55 ~ (y + 2000),
    TRUE ~ y
  )) %>% 
  mutate(x = x + 100,
         y = y + 10000)

ggplot(df_3, aes(x = x, y = y)) +
  geom_point(size = 2) +
  theme_minimal(base_size = 13) +
  labs(x = "Variable independiente",
       y = "Variable dependiente") 
  
ggsave("figures/linear_5.png", width = 20, height = 12, units = "cm")

ggplot(df_3, aes(x = x, y = y)) +
  geom_point(size = 2) +
  theme_minimal(base_size = 13) +
  stat_smooth(method = "lm", se = FALSE, fullrange = T, color = "#138D75") +
  stat_regline_equation(label.x = 80, label.y = 15000, size = 6, color = "#138D75") +
  labs(x = "Variable independiente",
       y = "Variable dependiente") 

ggsave("figures/linear_6.png", width = 20, height = 12, units = "cm")

ggplot(df_3, aes(x = x, y = y)) +
  geom_point(size = 2) +
  theme_minimal(base_size = 13) +
  geom_smooth(method = "loess", se = FALSE, color = "#138D75", span = 2) +
  labs(x = "Variable independiente",
       y = "Variable dependiente") 

ggsave("figures/linear_7.png", width = 20, height = 12, units = "cm")


