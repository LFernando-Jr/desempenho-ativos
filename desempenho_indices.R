
# Carregando pacotes ------------------------------------------------------

library(tidyverse)

# Setup -------------------------------------------------------------------

rm(list = ls())

Sys.setenv("LANGUAGE" = "Pt")
Sys.setlocale("LC_ALL", "Portuguese")

# Coleta de dados ---------------------------------------------------------

df <- read.csv(paste0(getwd(), "/Dados/index.csv"), header = TRUE, sep = ";", dec = ",") |>
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
  mutate(var_mes = round(((Cota / Cota[which(Data == "2023-11-30")]) - 1)*100,2))

tbl <- data[,c(1,2,5,6)] |>
  arrange(desc(Data)) |>
  group_by(`Nome do Ativo`) |>
  slice(1) |>
  ungroup() |>
  select(-Data) |>
  arrange(desc(var_mes)) |>
  rename(`% No mês ` = var_mes,
         `% No ano ` = var_ano)

# Visualização de dados ---------------------------------------------------

## Variação anual -------------------------------------------------------

data |> 
  filter(Data >= "2018-01-01") |>
  ggplot() +
  geom_line(data = . %>% filter(`Nome do Ativo` != "CDI"), 
            aes(Data, var_12M, colour = `Nome do Ativo`), linewidth = .75) +
  geom_line(data = . %>% filter(`Nome do Ativo` == "CDI"), 
            aes(Data, var_12M, colour = "CDI"), linewidth = .5, linetype = "longdash") +
  theme_bw() + theme(panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     legend.title = element_blank(), 
                     axis.title = element_blank(), 
                     strip.background = element_blank()) + 
  scale_x_date(expand = c(0,0), date_labels = "%b/%Y", breaks = "6 months",) +
  scale_colour_manual(values = c("black",
                                 "#2F47AD",
                                 "#8C977D",
                                 "#31AFE0",
                                 "#E47632",
                                 "#AD4728",
                                 "#3BA58B",
                                 "#D4A83F",
                                 "#8057A5")) +
  labs(title = "Índices", 
       subtitle = "Variação anual",
       caption = "Fonte: Capri com dados da Quantum Axis")

ggsave("variacao anual.png", width = 4800, height = 2160, units = "px", dpi = 576, path = paste(getwd(),
                                                                                                "/Gráficos/Índices",
                                                                                                sep = ""))

# ggsave("variacao anual.png", width = 15, height = 8.661, units = "in", dpi = 800, path = paste(getwd(),
#                                                                                                           "/Gráficos/Fundos",
#                                                                                                           sep = ""))

## Variação anual acumulada  -------------------------------------------------------

data |>
  filter(Data >= "2023-01-01") |>
  mutate(`Nome do Ativo` = factor(`Nome do Ativo`, levels = arrange(tbl, desc(`% No ano `))$`Nome do Ativo`)) |>
  ggplot() +
  aes(Data, var_ano, colour = `Nome do Ativo`, linetype = `Nome do Ativo`) +
  geom_line(linewidth = .75) +
  theme_bw() + theme(panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     legend.title = element_blank(), 
                     axis.title = element_blank(), 
                     strip.background = element_blank()) + 
  scale_x_date(expand = c(0,0), date_labels = "%b", breaks = "1 months") +
  scale_colour_manual(values = c("Ibovespa" = "#2F47AD",
                                 "CDI" = "black",
                                 "IDA-DI" = "#8C977D",
                                 "Idex-CDI Geral JGP" = "#31AFE0",
                                 "IHFA" = "#E47632",
                                 "IMA-B" = "#AD4728",
                                 "IMA-B 5" = "#3BA58B",
                                 "IRF-M" = "#D4A83F"),
                      labels = c("Ibovespa" = paste0("Ibovespa: ", tbl$`% No ano `[which(tbl$`Nome do Ativo` == "Ibovespa")], "%"),
                               "CDI" = paste0("CDI: ", tbl$`% No ano `[which(tbl$`Nome do Ativo` == "CDI")], "%"),
                               "IDA-DI" = paste0("IDA-DI: ", tbl$`% No ano `[which(tbl$`Nome do Ativo` == "IDA-DI")], "%"),
                               "Idex-CDI Geral JGP" = paste0("IDEX: ", tbl$`% No ano `[which(tbl$`Nome do Ativo` == "Idex-CDI Geral JGP")], "%"),
                               "IHFA" = paste0("IHFA: ", tbl$`% No ano `[which(tbl$`Nome do Ativo` == "IHFA")], "%"),
                               "IMA-B" = paste0("IMA-B: ", tbl$`% No ano `[which(tbl$`Nome do Ativo` == "IMA-B")], "%"),
                               "IMA-B 5" = paste0("IMA-B 5: ", tbl$`% No ano `[which(tbl$`Nome do Ativo` == "IMA-B 5")], "%"),
                               "IRF-M" = paste0("IRF-M: ", tbl$`% No ano `[which(tbl$`Nome do Ativo` == "IRF-M")], "%"))) +
  scale_linetype_manual(values = c("CDI" = "longdash", 
                                   "Ibovespa" = "solid",
                                   "CDI" = "solid",
                                   "IDA-DI" = "solid",
                                   "Idex-CDI Geral JGP" = "solid",
                                   "IHFA" = "solid",
                                   "IMA-B" = "solid",
                                   "IMA-B 5" = "solid",
                                   "IRF-M" = "solid")) +
  guides(linetype = "none") +
  labs(title = NULL,
       subtitle = "Variação acumulada no ano", 
       caption = "Fonte: Capri com dados da Quantum Axis")

ggsave("variacao anual acumulada.png", width = 4800, height = 2160, units = "px", dpi = 576, path = paste(getwd(),
                                                                                                          "/Gráficos/Índices",
                                                                                                          sep = ""))

## Variação mensal acumulada  -------------------------------------------------------

data |>
  filter(Data >= "2023-11-30") |>
  mutate(`Nome do Ativo` = factor(`Nome do Ativo`, levels = arrange(tbl, desc(`% No mês `))$`Nome do Ativo`)) |>
  ggplot() +
  aes(Data, var_mes, colour = `Nome do Ativo`, linetype = `Nome do Ativo`) +
  geom_line(linewidth = .75) +
  theme_bw() + theme(panel.grid.minor = element_blank(),
                     axis.line = element_line(colour = "black"),
                     legend.title = element_blank(),
                     axis.title = element_blank(),
                     strip.background = element_blank()) +
  scale_x_date(expand = c(0,0), date_labels = "%d", breaks = "1 day",) +
  scale_colour_manual(values = c("Ibovespa" = "#2F47AD",
                                 "CDI" = "black",
                                 "IDA-DI" = "#8C977D",
                                 "Idex-CDI Geral JGP" = "#31AFE0",
                                 "IHFA" = "#E47632",
                                 "IMA-B" = "#AD4728",
                                 "IMA-B 5" = "#3BA58B",
                                 "IRF-M" = "#D4A83F"),
                      labels = c("Ibovespa" = paste0("Ibovespa: ", tbl$`% No mês `[which(tbl$`Nome do Ativo` == "Ibovespa")], "%"),
                                 "CDI" = paste0("CDI: ", tbl$`% No mês `[which(tbl$`Nome do Ativo` == "CDI")], "%"),
                                 "IDA-DI" = paste0("IDA-DI: ", tbl$`% No mês `[which(tbl$`Nome do Ativo` == "IDA-DI")], "%"),
                                 "Idex-CDI Geral JGP" = paste0("IDEX: ", tbl$`% No mês `[which(tbl$`Nome do Ativo` == "Idex-CDI Geral JGP")], "%"),
                                 "IHFA" = paste0("IHFA: ", tbl$`% No mês `[which(tbl$`Nome do Ativo` == "IHFA")], "%"),
                                 "IMA-B" = paste0("IMA-B: ", tbl$`% No mês `[which(tbl$`Nome do Ativo` == "IMA-B")], "%"),
                                 "IMA-B 5" = paste0("IMA-B 5: ", tbl$`% No mês `[which(tbl$`Nome do Ativo` == "IMA-B 5")], "%"),
                                 "IRF-M" = paste0("IRF-M: ", tbl$`% No mês `[which(tbl$`Nome do Ativo` == "IRF-M")], "%"))) +
  scale_linetype_manual(values = c("CDI" = "longdash", 
                                   "Ibovespa" = "solid",
                                   "CDI" = "solid",
                                   "IDA-DI" = "solid",
                                   "Idex-CDI Geral JGP" = "solid",
                                   "IHFA" = "solid",
                                   "IMA-B" = "solid",
                                   "IMA-B 5" = "solid",
                                   "IRF-M" = "solid")) +
  guides(linetype = "none") +
  labs(title = NULL,
       subtitle = "Variação acumulada no mês", 
       caption = "Fonte: Capri com dados da Quantum Axis")

ggsave("variacao mensal acumulada.png", width = 9720, height = 3920, units = "px", dpi = 1152, path = paste(getwd(),
                                                                                                           "/Gráficos/Índices",
                                                                                                           sep = ""))
