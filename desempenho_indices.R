
# Carregando pacotes ------------------------------------------------------

library(tidyverse)

# Setup -------------------------------------------------------------------

rm(list = ls())

Sys.setenv("LANGUAGE" = "Pt")
Sys.setlocale("LC_ALL", "Portuguese")

# Coleta de dados ---------------------------------------------------------

df <- read.csv("index.csv", header = TRUE, sep = ";", dec = ",", check.names = FALSE) |>
  `colnames<-`(c("Nome do Ativo",
                 "Data",
                 "Cota")) |>
  as_tibble() |>
  mutate(Data = as.Date(Data, format = "%d/%m/%Y"))

# Classe
class(df)

# Estrutura
str(df)

# Tratamento de dados -----------------------------------------------------
data <- df |>
  group_by(`Nome do Ativo`) |>
  mutate(var_12M = round(((Cota / lag(Cota,252)) - 1)*100,2)) |>
  mutate(var_ano = round(((Cota / Cota[which(Data == "2023-01-02")]) - 1)*100,2)) |>
  mutate(var_mes = round(((Cota / Cota[which(Data == "2023-10-31")]) - 1)*100,2))

write.csv2(data, "desempenho_indices.csv")

# Visualização de dados ---------------------------------------------------

## Variação anual -------------------------------------------------------

data |> 
  filter(Data >= "2018-01-01") |>
  ggplot() +
  geom_line(data = . %>% filter(`Nome do Ativo` != "CDI"), 
            aes(Data, var_12M, colour = `Nome do Ativo`), size = .75) +
  geom_line(data = . %>% filter(`Nome do Ativo` == "CDI"), 
            aes(Data, var_12M, colour = "CDI"), size = .5, linetype = "longdash") +
  theme_bw() + theme(panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     legend.position = "bottom", 
                     legend.title = element_blank(), 
                     axis.title = element_blank(), 
                     strip.background = element_blank()) + 
  scale_x_date(expand = c(0,0), date_labels = "%b/%Y", breaks = "6 months",) +
  scale_colour_manual(values = c("black",
                                 "#4e5579",
                                 "#dc7a3a",
                                 "#a74b2d",
                                 "#6b5b95",
                                 "#3e4651",
                                 "#e77e52",
                                 "#b83b5e",
                                 "#4eadde")) + 
  labs(title = "Índices", 
       subtitle = "Variação anual",
       caption = "Fonte: Capri com dados da Quantum Axis")

ggsave("variacao anual.png", width = 21, height = 11.900, units = "in", dpi = 800, path = paste(getwd(),
                                                                                                "/Gráficos/Índices",
                                                                                                sep = ""))

## Variação anual acumulada  -------------------------------------------------------

data |> 
  filter(Data >= "2023-01-01") |>
  ggplot() +
  geom_line(data = . %>% filter(`Nome do Ativo` != "CDI"), 
            aes(Data, var_ano, colour = `Nome do Ativo`), size = .75) +
  geom_line(data = . %>% filter(`Nome do Ativo` == "CDI"), 
            aes(Data, var_ano, colour = "CDI"), size = .5, linetype = "longdash") +
  theme_bw() + theme(panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     legend.position = "bottom", 
                     legend.title = element_blank(), 
                     axis.title = element_blank(), 
                     strip.background = element_blank()) + 
  scale_x_date(expand = c(0,0), date_labels = "%b", breaks = "1 months",) +
  scale_colour_manual(values = c("black",
                                 "#4e5579",
                                 "#dc7a3a",
                                 "#a74b2d",
                                 "#6b5b95",
                                 "#3e4651",
                                 "#e77e52",
                                 "#b83b5e",
                                 "#4eadde")) + 
  labs(title = "Índices",
       subtitle = "Variação acumulada no ano", 
       caption = "Fonte: Capri com dados da Quantum Axis")

ggsave("variacao anual acumulada.png", width = 21, height = 11.900, units = "in", dpi = 800, path = paste(getwd(),
                                                                                                          "/Gráficos/Índices",
                                                                                                          sep = ""))

## Variação mensal acumulada  -------------------------------------------------------

data |> 
  filter(Data >= "2023-10-31") |>
  ggplot() +
  geom_line(data = . %>% filter(`Nome do Ativo` != "CDI"), 
            aes(Data, var_mes, colour = `Nome do Ativo`), size = .75) +
  geom_line(data = . %>% filter(`Nome do Ativo` == "CDI"), 
            aes(Data, var_mes, colour = "CDI"), size = .5, linetype = "longdash") +
  theme_bw() + theme(panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     legend.position = "bottom", 
                     legend.title = element_blank(), 
                     axis.title = element_blank(), 
                     strip.background = element_blank()) + 
  scale_x_date(expand = c(0,0), date_labels = "%d", breaks = "1 day",) +
  scale_colour_manual(values = c("black",
                                 "#4e5579",
                                 "#dc7a3a",
                                 "#a74b2d",
                                 "#6b5b95",
                                 "#3e4651",
                                 "#e77e52",
                                 "#b83b5e",
                                 "#4eadde")) +  
  labs(title = "Índices",
       subtitle = "Variação acumulada no mês", 
       caption = "Fonte: Capri com dados da Quantum Axis")

ggsave("variacao mensal acumulada.png", width = 21, height = 11.900, units = "in", dpi = 800, path = paste(getwd(),
                                                                                                           "/Gráficos/Índices",
                                                                                                           sep = ""))
