
# Carregando pacotes ------------------------------------------------------

library(tidyverse)

# Setup -------------------------------------------------------------------

rm(list = ls())

Sys.setenv("LANGUAGE" = "Pt")
Sys.setlocale("LC_ALL", "Portuguese")

# Coleta de dados ---------------------------------------------------------

funds <- readxl::read_excel(paste0(getwd(), "/Dados/fundos.xlsx"), sheet = 1) %>%
  `colnames<-`(c("Ativo",
                 "Data",
                 "Cota")) %>%
  mutate(Data = as.Date(Data, format = "%d/%m/%Y"))

df <- rbind(funds, readxl::read_excel(paste0(getwd(), "/Dados/index.xlsx"), sheet = 1) %>%
              `colnames<-`(c("Ativo",
                             "Data",
                             "Cota")) %>%
              mutate(Data = as.Date(Data, format = "%d/%m/%Y"))  %>%
              filter(Ativo == "IHFA"))

cdi <- readxl::read_excel(paste0(getwd(), "/Dados/index.xlsx"), sheet = 1) %>%
  `colnames<-`(c("Ativo",
                 "Data",
                 "CDI")) %>%
  mutate(Data = as.Date(Data, format = "%d/%m/%Y"))  %>%
  filter(Ativo == "CDI") %>%
  select(Data, CDI)

df <- inner_join(df, cdi)

# Classe
class(df)

# Estrutura
glimpse(df)

# Tratamento de dados -----------------------------------------------------

data <- df %>%
  #fundos
  arrange(Ativo, Data) %>%
  group_by(Ativo) %>% 
  mutate(var = (Cota/lag(Cota, 1) - 1) * 100,
         acumulado_36_meses = (zoo::rollapply(1 + var/100, width = 756, FUN = prod, align = 'right', fill = NA) - 1)*100) %>%
  group_by(Ativo, year(Data), month(Data)) %>%
  mutate(acumulado_mes = round((cumprod(1 + var/100) - 1) * 100, 2)) %>%
  group_by(Ativo, year(Data)) %>%
  mutate(acumulado_ano = round((cumprod(1 + var/100) - 1) * 100, 2)) %>% 
  ungroup() %>% 
  #cdi
  arrange(Ativo, Data) %>%
  group_by(Ativo) %>% 
  mutate(var_cdi = (CDI/lag(CDI, 1) - 1) * 100,
         acumulado_36_meses_cdi = (zoo::rollapply(1 + var_cdi/100, width = 756, FUN = prod, align = 'right', fill = NA) - 1)*100) %>%
  group_by(Ativo, year(Data), month(Data)) %>%
  mutate(acumulado_mes_cdi = round((cumprod(1 + var_cdi/100) - 1) * 100, 2)) %>%
  group_by(Ativo, year(Data)) %>%
  mutate(acumulado_ano_cdi = round((cumprod(1 + var_cdi/100) - 1) * 100, 2)) %>%
  ungroup() %>% 
  #excesso
  mutate(excess_var_36M = ((1 + acumulado_36_meses/100)/(1 + acumulado_36_meses_cdi/100) - 1)*100) %>%
  mutate(excess_var_ano = ((1 + acumulado_ano/100)/(1 + acumulado_ano_cdi/100) - 1)*100) %>%
  mutate(excess_var_mes = ((1 + acumulado_mes/100)/(1 + acumulado_mes_cdi/100) - 1)*100)

tbl <- data[,c(1:2,15:17)] %>%
  arrange(desc(Data)) %>%
  group_by(Ativo) %>%
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
  mutate(Ativo = factor(Ativo, levels = arrange(tbl, desc(`% em 36 meses `))$Ativo)) %>%
  ggplot() +
  aes(Data, excess_var_36M, colour = Ativo, linetype = Ativo) +
  geom_line(linewidth = .75) +
  geom_hline(yintercept = 0) +
  theme_bw() + theme(panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     legend.title = element_blank(), 
                     axis.title = element_blank(), 
                     strip.background = element_blank()) + 
  scale_x_date(expand = c(0,0), date_labels = "%b-%y", breaks = "1 month",) +
  scale_colour_manual(values = c("ABSOLUTE VERTEX FIC MULTIMERCADO" = "#2F47AD",
                                 "IHFA" = "black",
                                 "JGP STRATEGY FIC MULTIMERCADO" = "#8C977D",
                                 "KINEA ATLAS II FI MULTIMERCADO" = "#31AFE0",
                                 "SPX NIMITZ FEEDER FIC MULTIMERCADO" = "#E47632",
                                 "KAPITALO ZETA FIC MULTIMERCADO" = "#AD4728",
                                 "OCCAM RETORNO ABSOLUTO ADVISORY FIC MULTIMERCADO" = "#3BA58B",
                                 "LEGACY CAPITAL ADVISORY FIC MULTIMERCADO" = "#D4A83F",
                                 "VERDE AM X60 ADVISORY FIC MULTIMERCADO" = "#2f5a3d"),
                      labels = c("ABSOLUTE VERTEX FIC MULTIMERCADO" = paste0("Absolute: ", round(tbl$`% em 36 meses `[which(tbl$Ativo == "ABSOLUTE VERTEX FIC MULTIMERCADO")],2), "%"),
                                 "IHFA" = paste0("IHFA: ", round(tbl$`% em 36 meses `[which(tbl$Ativo == "IHFA")],2), "%"),
                                 "JGP STRATEGY FIC MULTIMERCADO" = paste0("JGP: ", round(tbl$`% em 36 meses `[which(tbl$Ativo == "JGP STRATEGY FIC MULTIMERCADO")],2), "%"),
                                 "KINEA ATLAS II FI MULTIMERCADO" = paste0("Kinea: ", round(tbl$`% em 36 meses `[which(tbl$Ativo == "KINEA ATLAS II FI MULTIMERCADO")],2), "%"),
                                 "SPX NIMITZ FEEDER FIC MULTIMERCADO" = paste0("SPX: ", round(tbl$`% em 36 meses `[which(tbl$Ativo == "SPX NIMITZ FEEDER FIC MULTIMERCADO")],2), "%"),
                                 "KAPITALO ZETA FIC MULTIMERCADO" = paste0("Kapitalo: ", round(tbl$`% em 36 meses `[which(tbl$Ativo == "KAPITALO ZETA FIC MULTIMERCADO")],2), "%"),
                                 "OCCAM RETORNO ABSOLUTO ADVISORY FIC MULTIMERCADO" = paste0("Occam: ", round(tbl$`% em 36 meses `[which(tbl$Ativo == "OCCAM RETORNO ABSOLUTO ADVISORY FIC MULTIMERCADO")],2), "%"),
                                 "LEGACY CAPITAL ADVISORY FIC MULTIMERCADO" = paste0("Legacy: ", round(tbl$`% em 36 meses `[which(tbl$Ativo == "LEGACY CAPITAL ADVISORY FIC MULTIMERCADO")],2), "%"),
                                 "VERDE AM X60 ADVISORY FIC MULTIMERCADO" = paste0("Verde: ", round(tbl$`% em 36 meses `[which(tbl$Ativo == "VERDE AM X60 ADVISORY FIC MULTIMERCADO")],2), "%"))) + 
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
  filter(Data >= floor_date(Sys.Date(), "year")) %>%
  # filter(Data >= floor_date(Sys.Date(), "year") & Data < "2024-05-01") %>%
  mutate(Ativo = factor(Ativo, levels = arrange(tbl, desc(`% No ano `))$Ativo)) %>%
  ggplot() +
  aes(Data, excess_var_ano, colour = Ativo, linetype = Ativo) +
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
                      labels = c("ABSOLUTE VERTEX FIC MULTIMERCADO" = paste0("Absolute: ", round(tbl$`% No ano `[which(tbl$Ativo == "ABSOLUTE VERTEX FIC MULTIMERCADO")],2), "%"),
                                 "IHFA" = paste0("IHFA: ", round(tbl$`% No ano `[which(tbl$Ativo == "IHFA")],2), "%"),
                                 "JGP STRATEGY FIC MULTIMERCADO" = paste0("JGP: ", round(tbl$`% No ano `[which(tbl$Ativo == "JGP STRATEGY FIC MULTIMERCADO")],2), "%"),
                                 "KINEA ATLAS II FI MULTIMERCADO" = paste0("Kinea: ", round(tbl$`% No ano `[which(tbl$Ativo == "KINEA ATLAS II FI MULTIMERCADO")],2), "%"),
                                 "SPX NIMITZ FEEDER FIC MULTIMERCADO" = paste0("SPX: ", round(tbl$`% No ano `[which(tbl$Ativo == "SPX NIMITZ FEEDER FIC MULTIMERCADO")],2), "%"),
                                 "KAPITALO ZETA FIC MULTIMERCADO" = paste0("Kapitalo: ", round(tbl$`% No ano `[which(tbl$Ativo == "KAPITALO ZETA FIC MULTIMERCADO")],2), "%"),
                                 "OCCAM RETORNO ABSOLUTO ADVISORY FIC MULTIMERCADO" = paste0("Occam: ", round(tbl$`% No ano `[which(tbl$Ativo == "OCCAM RETORNO ABSOLUTO ADVISORY FIC MULTIMERCADO")],2), "%"),
                                 "LEGACY CAPITAL ADVISORY FIC MULTIMERCADO" = paste0("Legacy: ", round(tbl$`% No ano `[which(tbl$Ativo == "LEGACY CAPITAL ADVISORY FIC MULTIMERCADO")],2), "%"),
                                 "VERDE AM X60 ADVISORY FIC MULTIMERCADO" = paste0("Verde: ", round(tbl$`% No ano `[which(tbl$Ativo == "VERDE AM X60 ADVISORY FIC MULTIMERCADO")],2), "%"))) + 
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
  filter(Data >= floor_date(Sys.Date(), "month")) %>%
  # filter(Data >= as.Date("2024-04-01") & Data < floor_date(Sys.Date(), "month")) %>%
  mutate(Ativo = factor(Ativo, levels = arrange(tbl, desc(`% No mês `))$Ativo)) %>%
  ggplot() +
  aes(Data, excess_var_mes, colour = Ativo, linetype = Ativo) +
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
                      labels = c("ABSOLUTE VERTEX FIC MULTIMERCADO" = paste0("Absolute: ", round(tbl$`% No mês `[which(tbl$Ativo == "ABSOLUTE VERTEX FIC MULTIMERCADO")],2), "%"),
                                 "IHFA" = paste0("IHFA: ", round(tbl$`% No mês `[which(tbl$Ativo == "IHFA")],2), "%"),
                                 "JGP STRATEGY FIC MULTIMERCADO" = paste0("JGP: ", round(tbl$`% No mês `[which(tbl$Ativo == "JGP STRATEGY FIC MULTIMERCADO")],2), "%"),
                                 "KINEA ATLAS II FI MULTIMERCADO" = paste0("Kinea: ", round(tbl$`% No mês `[which(tbl$Ativo == "KINEA ATLAS II FI MULTIMERCADO")],2), "%"),
                                 "SPX NIMITZ FEEDER FIC MULTIMERCADO" = paste0("SPX: ", round(tbl$`% No mês `[which(tbl$Ativo == "SPX NIMITZ FEEDER FIC MULTIMERCADO")],2), "%"),
                                 "KAPITALO ZETA FIC MULTIMERCADO" = paste0("Kapitalo: ", round(tbl$`% No mês `[which(tbl$Ativo == "KAPITALO ZETA FIC MULTIMERCADO")],2), "%"),
                                 "OCCAM RETORNO ABSOLUTO ADVISORY FIC MULTIMERCADO" = paste0("Occam: ", round(tbl$`% No mês `[which(tbl$Ativo == "OCCAM RETORNO ABSOLUTO ADVISORY FIC MULTIMERCADO")],2), "%"),
                                 "LEGACY CAPITAL ADVISORY FIC MULTIMERCADO" = paste0("Legacy: ", round(tbl$`% No mês `[which(tbl$Ativo == "LEGACY CAPITAL ADVISORY FIC MULTIMERCADO")],2), "%"),
                                 "VERDE AM X60 ADVISORY FIC MULTIMERCADO" = paste0("Verde: ", round(tbl$`% No mês `[which(tbl$Ativo == "VERDE AM X60 ADVISORY FIC MULTIMERCADO")],2), "%"))) + 
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
