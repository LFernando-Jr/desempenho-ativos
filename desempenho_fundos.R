
# Carregando pacotes ------------------------------------------------------

library(tidyverse)

# Setup -------------------------------------------------------------------

rm(list = ls())

Sys.setenv("LANGUAGE" = "Pt")
Sys.setlocale("LC_ALL", "Portuguese")

# Coleta de dados ---------------------------------------------------------

funds <- read.csv(paste0(getwd(), "/Dados/fundos.csv"), header = TRUE, sep = ";", dec = ",", check.names = FALSE) %>%
  `colnames<-`(c("Nome do Ativo",
                 "Data",
                 "Cota")) %>%
  as_tibble() %>%
  mutate(Data = as.Date(Data, format = "%d/%m/%Y"))

df <- rbind(funds, read.csv(paste0(getwd(), "/Dados/index.csv"), header = TRUE, sep = ";", dec = ",", check.names = FALSE) %>%
              `colnames<-`(c("Nome do Ativo",
                             "Data",
                             "Cota")) %>%
              as_tibble() %>%
              mutate(Data = as.Date(Data, format = "%d/%m/%Y"))  %>%
              filter(`Nome do Ativo` == "IHFA"))

cdi <- read.csv(paste0(getwd(), "/Dados/index.csv"), header = TRUE, sep = ";", dec = ",", check.names = FALSE) %>%
  `colnames<-`(c("Nome do Ativo",
                 "Data",
                 "CDI")) %>%
  as_tibble() %>%
  mutate(Data = as.Date(Data, format = "%d/%m/%Y"))  %>%
  filter(`Nome do Ativo` == "CDI") %>%
  select(Data, CDI)

df <- merge(df, cdi)

# Classe
class(df)

# Estrutura
str(df)

# Tratamento de dados -----------------------------------------------------

data <- df %>%
  group_by(`Nome do Ativo`) %>%
  #fundos
  mutate(var_36M = round(((Cota / lag(Cota,252*3)) - 1)*100,2)) %>%
  mutate(var_ano = round(((Cota / Cota[which(Data == "2023-01-02")]) - 1)*100,2)) %>%
  mutate(var_mes = round(((Cota / Cota[which(Data == "2023-12-01")]) - 1)*100,2)) %>%
  #cdi
  mutate(cdi_36M = round(((CDI / lag(CDI,252*3)) - 1)*100,2)) %>%
  mutate(cdi_ano = round(((CDI / CDI[which(Data == "2023-01-02")]) - 1)*100,2)) %>%
  mutate(cdi_mes = round(((CDI / CDI[which(Data == "2023-12-01")]) - 1)*100,2)) %>%
  #excesso
  mutate(excess_var_36M = var_36M - cdi_36M) %>%
  mutate(excess_var_ano = var_ano - cdi_ano) %>%
  mutate(excess_var_mes = var_mes - cdi_mes)

tbl <- data[,c(1,2,11,12,13)] %>%
  filter(Data < "2024-01-01") %>% 
  arrange(desc(Data)) %>%
  group_by(`Nome do Ativo`) %>%
  slice(1) %>%
  ungroup() %>%
  select(-Data) %>%
  arrange(desc(excess_var_ano)) %>%
  rename(`% No mês ` = excess_var_mes,
         `% No ano ` = excess_var_ano,
         `% em 36 meses ` = excess_var_36M )

# Visualização de dados ---------------------------------------------------

## Variação Trianual ------------------------------------------------------

data %>% 
  filter(Data >= "2023-01-01") %>%
  mutate(`Nome do Ativo` = factor(`Nome do Ativo`, levels = arrange(tbl, desc(`% em 36 meses `))$`Nome do Ativo`)) %>%
  ggplot() +
  aes(Data, excess_var_36M, colour = `Nome do Ativo`, linetype = `Nome do Ativo`) +
  geom_line(linewidth = .75) +
  geom_hline(yintercept = 0) +
  theme_bw() + theme(panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     legend.title = element_blank(), 
                     axis.title = element_blank(), 
                     strip.background = element_blank()) + 
  scale_x_date(expand = c(0,0), date_labels = "%b", breaks = "1 month",) +
  scale_colour_manual(values = c("ABSOLUTE VERTEX FIC MULTIMERCADO" = "#2F47AD",
                                 "IHFA" = "black",
                                 "JGP STRATEGY FIC MULTIMERCADO" = "#8C977D",
                                 "KINEA ATLAS II FI MULTIMERCADO" = "#31AFE0",
                                 "SPX NIMITZ FEEDER FIC MULTIMERCADO" = "#E47632",
                                 "KAPITALO ZETA FIC MULTIMERCADO" = "#AD4728",
                                 "OCCAM RETORNO ABSOLUTO ADVISORY FIC MULTIMERCADO" = "#3BA58B",
                                 "LEGACY CAPITAL ADVISORY FIC MULTIMERCADO" = "#D4A83F",
                                 "VERDE AM X60 ADVISORY FIC MULTIMERCADO" = "#2f5a3d"),
                      labels = c("ABSOLUTE VERTEX FIC MULTIMERCADO" = paste0("Absolute: ", round(tbl$`% em 36 meses `[which(tbl$`Nome do Ativo` == "ABSOLUTE VERTEX FIC MULTIMERCADO")],2), "%"),
                                 "IHFA" = paste0("IHFA: ", round(tbl$`% em 36 meses `[which(tbl$`Nome do Ativo` == "IHFA")],2), "%"),
                                 "JGP STRATEGY FIC MULTIMERCADO" = paste0("JGP: ", round(tbl$`% em 36 meses `[which(tbl$`Nome do Ativo` == "JGP STRATEGY FIC MULTIMERCADO")],2), "%"),
                                 "KINEA ATLAS II FI MULTIMERCADO" = paste0("Kinea: ", round(tbl$`% em 36 meses `[which(tbl$`Nome do Ativo` == "KINEA ATLAS II FI MULTIMERCADO")],2), "%"),
                                 "SPX NIMITZ FEEDER FIC MULTIMERCADO" = paste0("SPX: ", round(tbl$`% em 36 meses `[which(tbl$`Nome do Ativo` == "SPX NIMITZ FEEDER FIC MULTIMERCADO")],2), "%"),
                                 "KAPITALO ZETA FIC MULTIMERCADO" = paste0("Kapitalo: ", round(tbl$`% em 36 meses `[which(tbl$`Nome do Ativo` == "KAPITALO ZETA FIC MULTIMERCADO")],2), "%"),
                                 "OCCAM RETORNO ABSOLUTO ADVISORY FIC MULTIMERCADO" = paste0("Occam: ", round(tbl$`% em 36 meses `[which(tbl$`Nome do Ativo` == "OCCAM RETORNO ABSOLUTO ADVISORY FIC MULTIMERCADO")],2), "%"),
                                 "LEGACY CAPITAL ADVISORY FIC MULTIMERCADO" = paste0("Legacy: ", round(tbl$`% em 36 meses `[which(tbl$`Nome do Ativo` == "LEGACY CAPITAL ADVISORY FIC MULTIMERCADO")],2), "%"),
                                 "VERDE AM X60 ADVISORY FIC MULTIMERCADO" = paste0("Verde: ", round(tbl$`% em 36 meses `[which(tbl$`Nome do Ativo` == "VERDE AM X60 ADVISORY FIC MULTIMERCADO")],2), "%"))) + 
  scale_linetype_manual(values = c("ABSOLUTE VERTEX FIC MULTIMERCADO" = "solid",
                                 "IHFA" = "longdash",
                                 "JGP STRATEGY FIC MULTIMERCADO" = "solid",
                                 "KINEA ATLAS II FI MULTIMERCADO" = "solid",
                                 "SPX NIMITZ FEEDER FIC MULTIMERCADO" = "solid",
                                 "KAPITALO ZETA FIC MULTIMERCADO" = "solid",
                                 "OCCAM RETORNO ABSOLUTO ADVISORY FIC MULTIMERCADO" = "solid",
                                 "LEGACY CAPITAL ADVISORY FIC MULTIMERCADO" = "solid",
                                 "VERDE AM X60 ADVISORY FIC MULTIMERCADO" = "solid")) +
  guides(linetype = "none") +
  labs(title = NULL,
       subtitle = "Variação trianual do excesso de retorno", 
       caption = "Fonte: Capri com dados da Quantum Axis")
  
ggsave("variacao trianual.png", width = 4800, height = 2160, units = "px", dpi = 576, path = paste(getwd(),
                                                                                                   "/Gráficos/Fundos",
                                                                                                   sep = ""))

## Variação anual acumulada  -------------------------------------------------------

data %>% 
  filter(Data >= "2023-01-01" & Data < "2024-01-01") %>%
  mutate(`Nome do Ativo` = factor(`Nome do Ativo`, levels = arrange(tbl, desc(`% No ano `))$`Nome do Ativo`)) %>%
  ggplot() +
  aes(Data, excess_var_ano, colour = `Nome do Ativo`, linetype = `Nome do Ativo`) +
  geom_line(linewidth = .75) +
  geom_hline(yintercept = 0) +
  theme_bw() + theme(panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     legend.title = element_blank(), 
                     axis.title = element_blank(), 
                     strip.background = element_blank()) + 
  scale_x_date(expand = c(0,0), date_labels = "%b", breaks = "1 months",) +
  scale_colour_manual(values = c("ABSOLUTE VERTEX FIC MULTIMERCADO" = "#2F47AD",
                                 "IHFA" = "black",
                                 "JGP STRATEGY FIC MULTIMERCADO" = "#8C977D",
                                 "KINEA ATLAS II FI MULTIMERCADO" = "#31AFE0",
                                 "SPX NIMITZ FEEDER FIC MULTIMERCADO" = "#E47632",
                                 "KAPITALO ZETA FIC MULTIMERCADO" = "#AD4728",
                                 "OCCAM RETORNO ABSOLUTO ADVISORY FIC MULTIMERCADO" = "#3BA58B",
                                 "LEGACY CAPITAL ADVISORY FIC MULTIMERCADO" = "#D4A83F",
                                 "VERDE AM X60 ADVISORY FIC MULTIMERCADO" = "#2f5a3d"),
                      labels = c("ABSOLUTE VERTEX FIC MULTIMERCADO" = paste0("Absolute: ", round(tbl$`% No ano `[which(tbl$`Nome do Ativo` == "ABSOLUTE VERTEX FIC MULTIMERCADO")],2), "%"),
                                 "IHFA" = paste0("IHFA: ", round(tbl$`% No ano `[which(tbl$`Nome do Ativo` == "IHFA")],2), "%"),
                                 "JGP STRATEGY FIC MULTIMERCADO" = paste0("JGP: ", round(tbl$`% No ano `[which(tbl$`Nome do Ativo` == "JGP STRATEGY FIC MULTIMERCADO")],2), "%"),
                                 "KINEA ATLAS II FI MULTIMERCADO" = paste0("Kinea: ", round(tbl$`% No ano `[which(tbl$`Nome do Ativo` == "KINEA ATLAS II FI MULTIMERCADO")],2), "%"),
                                 "SPX NIMITZ FEEDER FIC MULTIMERCADO" = paste0("SPX: ", round(tbl$`% No ano `[which(tbl$`Nome do Ativo` == "SPX NIMITZ FEEDER FIC MULTIMERCADO")],2), "%"),
                                 "KAPITALO ZETA FIC MULTIMERCADO" = paste0("Kapitalo: ", round(tbl$`% No ano `[which(tbl$`Nome do Ativo` == "KAPITALO ZETA FIC MULTIMERCADO")],2), "%"),
                                 "OCCAM RETORNO ABSOLUTO ADVISORY FIC MULTIMERCADO" = paste0("Occam: ", round(tbl$`% No ano `[which(tbl$`Nome do Ativo` == "OCCAM RETORNO ABSOLUTO ADVISORY FIC MULTIMERCADO")],2), "%"),
                                 "LEGACY CAPITAL ADVISORY FIC MULTIMERCADO" = paste0("Legacy: ", round(tbl$`% No ano `[which(tbl$`Nome do Ativo` == "LEGACY CAPITAL ADVISORY FIC MULTIMERCADO")],2), "%"),
                                 "VERDE AM X60 ADVISORY FIC MULTIMERCADO" = paste0("Verde: ", round(tbl$`% No ano `[which(tbl$`Nome do Ativo` == "VERDE AM X60 ADVISORY FIC MULTIMERCADO")],2), "%"))) + 
  scale_linetype_manual(values = c("ABSOLUTE VERTEX FIC MULTIMERCADO" = "solid",
                                   "IHFA" = "longdash",
                                   "JGP STRATEGY FIC MULTIMERCADO" = "solid",
                                   "KINEA ATLAS II FI MULTIMERCADO" = "solid",
                                   "SPX NIMITZ FEEDER FIC MULTIMERCADO" = "solid",
                                   "KAPITALO ZETA FIC MULTIMERCADO" = "solid",
                                   "OCCAM RETORNO ABSOLUTO ADVISORY FIC MULTIMERCADO" = "solid",
                                   "LEGACY CAPITAL ADVISORY FIC MULTIMERCADO" = "solid",
                                   "VERDE AM X60 ADVISORY FIC MULTIMERCADO" = "solid")) +
  guides(linetype = "none") +
  labs(title = NULL,
       subtitle = "Excesso de retorno acumulado no ano",
       caption = "Fonte: Capri com dados da Quantum Axis")

ggsave("variacao anual acumulada.png", width = 4800, height = 2160, units = "px", dpi = 576, path = paste(getwd(),
                                                                                                          "/Gráficos/Fundos",
                                                                                                          sep = ""))

## Variação mensal acumulada  -------------------------------------------------------

data %>% 
  filter(Data >= "2023-12-01" & Data < "2024-01-01") %>%
  mutate(`Nome do Ativo` = factor(`Nome do Ativo`, levels = arrange(tbl, desc(`% No mês `))$`Nome do Ativo`)) %>%
  ggplot() +
  aes(Data, excess_var_mes, colour = `Nome do Ativo`, linetype = `Nome do Ativo`) +
  geom_line(linewidth = .75) + geom_hline(yintercept = 0) +
  theme_bw() + theme(panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     legend.title = element_blank(), 
                     axis.title = element_blank(), 
                     strip.background = element_blank()) + 
  scale_x_date(expand = c(0,0), date_labels = "%d", breaks = "1 day",) +
  scale_colour_manual(values = c("ABSOLUTE VERTEX FIC MULTIMERCADO" = "#2F47AD",
                                 "IHFA" = "black",
                                 "JGP STRATEGY FIC MULTIMERCADO" = "#8C977D",
                                 "KINEA ATLAS II FI MULTIMERCADO" = "#31AFE0",
                                 "SPX NIMITZ FEEDER FIC MULTIMERCADO" = "#E47632",
                                 "KAPITALO ZETA FIC MULTIMERCADO" = "#AD4728",
                                 "OCCAM RETORNO ABSOLUTO ADVISORY FIC MULTIMERCADO" = "#3BA58B",
                                 "LEGACY CAPITAL ADVISORY FIC MULTIMERCADO" = "#D4A83F",
                                 "VERDE AM X60 ADVISORY FIC MULTIMERCADO" = "#2f5a3d"),
                      labels = c("ABSOLUTE VERTEX FIC MULTIMERCADO" = paste0("Absolute: ", round(tbl$`% No mês `[which(tbl$`Nome do Ativo` == "ABSOLUTE VERTEX FIC MULTIMERCADO")],2), "%"),
                                 "IHFA" = paste0("IHFA: ", round(tbl$`% No mês `[which(tbl$`Nome do Ativo` == "IHFA")],2), "%"),
                                 "JGP STRATEGY FIC MULTIMERCADO" = paste0("JGP: ", round(tbl$`% No mês `[which(tbl$`Nome do Ativo` == "JGP STRATEGY FIC MULTIMERCADO")],2), "%"),
                                 "KINEA ATLAS II FI MULTIMERCADO" = paste0("Kinea: ", round(tbl$`% No mês `[which(tbl$`Nome do Ativo` == "KINEA ATLAS II FI MULTIMERCADO")],2), "%"),
                                 "SPX NIMITZ FEEDER FIC MULTIMERCADO" = paste0("SPX: ", round(tbl$`% No mês `[which(tbl$`Nome do Ativo` == "SPX NIMITZ FEEDER FIC MULTIMERCADO")],2), "%"),
                                 "KAPITALO ZETA FIC MULTIMERCADO" = paste0("Kapital: ", round(tbl$`% No mês `[which(tbl$`Nome do Ativo` == "KAPITALO ZETA FIC MULTIMERCADO")],2), "%"),
                                 "OCCAM RETORNO ABSOLUTO ADVISORY FIC MULTIMERCADO" = paste0("Occam: ", round(tbl$`% No mês `[which(tbl$`Nome do Ativo` == "OCCAM RETORNO ABSOLUTO ADVISORY FIC MULTIMERCADO")],2), "%"),
                                 "LEGACY CAPITAL ADVISORY FIC MULTIMERCADO" = paste0("Legacy: ", round(tbl$`% No mês `[which(tbl$`Nome do Ativo` == "LEGACY CAPITAL ADVISORY FIC MULTIMERCADO")],2), "%"),
                                 "VERDE AM X60 ADVISORY FIC MULTIMERCADO" = paste0("Verde: ", round(tbl$`% No mês `[which(tbl$`Nome do Ativo` == "VERDE AM X60 ADVISORY FIC MULTIMERCADO")],2), "%"))) + 
  scale_linetype_manual(values = c("ABSOLUTE VERTEX FIC MULTIMERCADO" = "solid",
                                   "IHFA" = "longdash",
                                   "JGP STRATEGY FIC MULTIMERCADO" = "solid",
                                   "KINEA ATLAS II FI MULTIMERCADO" = "solid",
                                   "SPX NIMITZ FEEDER FIC MULTIMERCADO" = "solid",
                                   "KAPITALO ZETA FIC MULTIMERCADO" = "solid",
                                   "OCCAM RETORNO ABSOLUTO ADVISORY FIC MULTIMERCADO" = "solid",
                                   "LEGACY CAPITAL ADVISORY FIC MULTIMERCADO" = "solid",
                                   "VERDE AM X60 ADVISORY FIC MULTIMERCADO" = "solid")) +
  guides(linetype = "none") +
  labs(title = NULL,
       subtitle = "Excesso de retorno acumulado no mês",
       caption = "Fonte: Capri com dados da Quantum Axis")

ggsave("variacao mensal acumulada.png", width = 4800, height = 2160, units = "px", dpi = 576, path = paste(getwd(),
                                                                                                           "/Gráficos/Fundos",
                                                                                                           sep = ""))
