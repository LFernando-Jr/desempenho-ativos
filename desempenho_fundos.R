
# Carregando pacotes ------------------------------------------------------

library(tidyverse)

# Setup -------------------------------------------------------------------

rm(list = ls())

Sys.setenv("LANGUAGE" = "Pt")
Sys.setlocale("LC_ALL", "Portuguese")

# Coleta de dados ---------------------------------------------------------

funds <- read.csv(paste0(getwd(), "/Dados/fundos.csv"), header = TRUE, sep = ";", dec = ",", check.names = FALSE) |>
  `colnames<-`(c("Nome do Ativo",
                 "Data",
                 "Cota")) |>
  as_tibble() |>
  mutate(Data = as.Date(Data, format = "%d/%m/%Y"))

df <- rbind(funds, read.csv(paste0(getwd(), "/Dados/index.csv"), header = TRUE, sep = ";", dec = ",", check.names = FALSE) |>
              `colnames<-`(c("Nome do Ativo",
                             "Data",
                             "Cota")) |>
              as_tibble() |>
              mutate(Data = as.Date(Data, format = "%d/%m/%Y"))  |>
              filter(`Nome do Ativo` == "IHFA"))

cdi <- read.csv(paste0(getwd(), "/Dados/index.csv"), header = TRUE, sep = ";", dec = ",", check.names = FALSE) |>
  `colnames<-`(c("Nome do Ativo",
                 "Data",
                 "CDI")) |>
  as_tibble() |>
  mutate(Data = as.Date(Data, format = "%d/%m/%Y"))  |>
  filter(`Nome do Ativo` == "CDI") |>
  select(Data, CDI)

df <- merge(df, cdi)

# Classe
class(df)

# Estrutura
str(df)

# Tratamento de dados -----------------------------------------------------

data <- df |>
  group_by(`Nome do Ativo`) |>
  #fundos
  mutate(var_36M = round(((Cota / lag(Cota,252*3)) - 1)*100,2)) |>
  mutate(var_ano = round(((Cota / Cota[which(Data == "2023-01-02")]) - 1)*100,2)) |>
  mutate(var_mes = round(((Cota / Cota[which(Data == "2023-11-30")]) - 1)*100,2)) |>
  #cdi
  mutate(cdi_36M = round(((CDI / lag(CDI,252*3)) - 1)*100,2)) |>
  mutate(cdi_ano = round(((CDI / CDI[which(Data == "2023-01-02")]) - 1)*100,2)) |>
  mutate(cdi_mes = round(((CDI / CDI[which(Data == "2023-11-30")]) - 1)*100,2)) |>
  #excesso
  mutate(excess_var_36M = var_36M - cdi_36M) |>
  mutate(excess_var_ano = var_ano - cdi_ano) |>
  mutate(excess_var_mes = var_mes - cdi_mes)

tbl <- data[,c(1,2,12,13)] |>
  arrange(desc(Data)) |>
  group_by(`Nome do Ativo`) |>
  slice(1) |>
  ungroup() |>
  select(-Data) |>
  arrange(desc(excess_var_ano)) |>
  rename(`% No mês ` = excess_var_mes,
         `% No ano ` = excess_var_ano)

# write.csv2(data, "desempenho_fundos.csv")
 write.csv2(tbl, "desempenho_fundos.csv")

# Visualização de dados ---------------------------------------------------

## Variação Trianual ------------------------------------------------------

data |> 
  filter(Data >= "2018-01-01") |>
  ggplot() +
  geom_line(data = . %>% filter(`Nome do Ativo` != "IHFA"), 
            aes(Data, excess_var_36M, colour = `Nome do Ativo`), linewidth = .75) +
  geom_line(data = . %>% filter(`Nome do Ativo` == "IHFA"), 
            aes(Data, excess_var_36M, colour = "IHFA"), linewidth = .5, linetype = "longdash") +
  geom_hline(yintercept = 0) +
  theme_bw() + theme(panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     legend.position = "bottom", 
                     legend.title = element_blank(), 
                     axis.title = element_blank(), 
                     strip.background = element_blank()) + 
  scale_x_date(expand = c(0,0), date_labels = "%m/%Y", breaks = "6 months",) +
  scale_colour_manual(values = c("#2F47AD",
                                 "black",
                                 "#8C977D",
                                 "#31AFE0",
                                 "#E47632",
                                 "#AD4728",
                                 "#3BA58B",
                                 "#D4A83F",
                                 "#8057A5")) + 
  labs(title = "Fundos",
       subtitle = "Variação trianual do excesso de retorno", 
       caption = "Fonte: Capri com dados da Quantum Axis")
  
ggsave("variacao trianual.png", width = 15, height = 8.661, units = "in", dpi = 800, path = paste(getwd(),
                                                                                                   "/Gráficos/Fundos",
                                                                                                   sep = ""))

## Variação anual acumulada  -------------------------------------------------------

data |> 
  filter(Data >= "2023-01-01") |>
  ggplot() +
  geom_line(data = . %>% filter(`Nome do Ativo` != "IHFA"), 
            aes(Data, excess_var_ano, colour = `Nome do Ativo`), linewidth = .75) +
  geom_line(data = . %>% filter(`Nome do Ativo` == "IHFA"), 
            aes(Data, excess_var_ano, colour = "IHFA"), linewidth = .5, linetype = "longdash") +
  geom_hline(yintercept = 0) +
  theme_bw() + theme(panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     legend.position = "bottom", 
                     legend.title = element_blank(), 
                     axis.title = element_blank(), 
                     strip.background = element_blank()) + 
  scale_x_date(expand = c(0,0), date_labels = "%b", breaks = "1 months",) +
  scale_colour_manual(values = c("#2F47AD",
                                 "black",
                                 "#8C977D",
                                 "#31AFE0",
                                 "#E47632",
                                 "#AD4728",
                                 "#3BA58B",
                                 "#D4A83F",
                                 "#8057A5")) + 
  labs(title = "Fundos",
       subtitle = "Excesso de retorno acumulado no ano",
       caption = "Fonte: Capri com dados da Quantum Axis")

ggsave("variacao anual acumulada.png", width = 15, height = 8.661, units = "in", dpi = 800, path = paste(getwd(),
                                                                                                          "/Gráficos/Fundos",
                                                                                                          sep = ""))

## Variação mensal acumulada  -------------------------------------------------------

data |> 
  filter(Data >= "2023-11-30") |>
  ggplot() +
  geom_line(data = . %>% filter(`Nome do Ativo` != "IHFA"), 
            aes(Data, excess_var_mes, colour = `Nome do Ativo`), linewidth = .75) +
  geom_line(data = . %>% filter(`Nome do Ativo` == "IHFA"), 
            aes(Data, excess_var_mes, colour = "IHFA"), linewidth = .5, linetype = "longdash") +
  geom_hline(yintercept = 0) +
  theme_bw() + theme(panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     legend.position = "bottom", 
                     legend.title = element_blank(), 
                     axis.title = element_blank(), 
                     strip.background = element_blank()) + 
  scale_x_date(expand = c(0,0), date_labels = "%d", breaks = "1 day",) +
  scale_colour_manual(values = c("#2F47AD",
                                 "black",
                                 "#8C977D",
                                 "#31AFE0",
                                 "#E47632",
                                 "#AD4728",
                                 "#3BA58B",
                                 "#D4A83F",
                                 "#8057A5")) + 
  labs(title = "Fundos",
       subtitle = "Excesso de retorno acumulado no mês",
       caption = "Fonte: Capri com dados da Quantum Axis")

ggsave("variacao mensal acumulada.png", width = 15, height = 8.661, units = "in", dpi = 800, path = paste(getwd(),
                                                                                                           "/Gráficos/Fundos",
                                                                                                           sep = ""))
