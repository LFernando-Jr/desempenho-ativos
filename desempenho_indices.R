
# Carregando pacotes ------------------------------------------------------

library(tidyverse)

# Setup -------------------------------------------------------------------

rm(list = ls())

Sys.setenv("LANGUAGE" = "Pt")
Sys.setlocale("LC_ALL", "Portuguese")

# Coleta de dados ---------------------------------------------------------

df <- readxl::read_excel(paste0(getwd(), "/Dados/index.xlsx"), sheet = 1) %>%
  `colnames<-`(c("Ativo",
                 "Data",
                 "Cota")) %>%
  mutate(Data = as.Date(Data, format = "%d/%m/%Y"))

# Classe
class(df)

# Estrutura
glimpse(df)

# Tratamento de dados -----------------------------------------------------

data <- df %>% 
  arrange(Ativo, Data) %>%
  group_by(Ativo) %>% 
  mutate(var = (Cota/lag(Cota, 1) - 1) * 100,
         acumulado_12_meses = (zoo::rollapply(1 + var/100, width = 252, FUN = prod, align = 'right', fill = NA) - 1)*100) %>%
  group_by(Ativo, year(Data), month(Data)) %>%
  mutate(acumulado_mes = round((cumprod(1 + var/100) - 1) * 100, 2)) %>%
  group_by(Ativo, year(Data)) %>%
  mutate(acumulado_ano = round((cumprod(1 + var/100) - 1) * 100, 2)) %>% 
  ungroup()

tbl <- data[,-c(3,4,6,7)] %>%
  # filter(Data <= "2024-02-29") %>%
  arrange(desc(Data)) %>%
  group_by(`Ativo`) %>%
  slice(1) %>%
  ungroup() %>%
  select(-Data) %>%
  arrange(desc(acumulado_mes)) %>%
  rename(`Retorno acumulado no mês` = acumulado_mes,
         `Retorno acumulado no ano` = acumulado_ano,
         `Retorno acumulado em 12 meses` = acumulado_12_meses)

tbl

# Visualização de dados ---------------------------------------------------

## Variação anual -------------------------------------------------------

data %>% 
  filter(Data >= last(data$Data) - 360,
         !(Ativo %in% c("IDA-IPCA Infraestrutura", "IDA-DI"))) %>%
  ggplot() +
  geom_line(data = . %>% filter(Ativo != "CDI"), 
            aes(Data, acumulado_12_meses, colour = Ativo), linewidth = .75) +
  geom_line(data = . %>% filter(Ativo == "CDI"), 
            aes(Data, acumulado_12_meses, colour = "CDI"), linewidth = .5, linetype = "longdash") +
  theme_bw() + theme(panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     legend.title = element_blank(), 
                     axis.title = element_blank(), 
                     strip.background = element_blank()) + 
  scale_x_date(expand = c(0,0), date_labels = "%b/%Y", breaks = "3 months",) +
  scale_colour_manual(values = c("black",
                                 "#FF6F61",
                                 "#2F47AD",
                                 "#8C977D",
                                 "#31AFE0",
                                 "#E47632",
                                 "#AD4728",
                                 "#3BA58B",
                                 "#D4A83F",
                                 "#8057A5")) +
  labs(title = "Índices", 
       subtitle = "Retorno acumulado em 12 meses",
       caption = "Fonte: Capri com dados da Quantum Axis")

ggsave("acumulado em 12 mesess.png", width = 4800, height = 2160, units = "px", dpi = 576, path = paste0(getwd(),
                                                                                                        "/Gráficos/Índices"))

# ggsave("variacao anual.png", width = 15, height = 8.661, units = "in", dpi = 800, path = paste(getwd(),
#                                                                                                           "/Gráficos/Fundos",
#                                                                                                           sep = ""))

## Variação anual acumulada  -------------------------------------------------------

data %>% 
  filter(Data >= floor_date(Sys.Date(), "year")) %>%
  mutate(Ativo = factor(Ativo, levels = arrange(tbl, desc(`Retorno acumulado no ano`))$Ativo)) %>%
  ggplot() +
  aes(Data, acumulado_ano, colour = Ativo, linetype = Ativo) +
  geom_line(linewidth = .75) +
  theme_bw() + theme(panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     legend.title = element_blank(), 
                     axis.title = element_blank(), 
                     strip.background = element_blank()) + 
  scale_x_date(expand = c(0,0), date_labels = "%b", breaks = "1 months") +
  scale_colour_manual(values = c("Ibovespa" = "#2F47AD",
                                 "CDI" = "black",
                                 "Idex-Infra Geral JGP" = "#8C977D",
                                 "Idex-CDI Geral JGP" = "#31AFE0",
                                 "IHFA" = "#E47632",
                                 "IMA-B" = "#AD4728",
                                 "IMA-B 5" = "#3BA58B",
                                 "IRF-M" = "#D4A83F",
                                 "Dólar" = "#FF6F61"),
                      labels = c("Ibovespa" = paste0("Ibovespa: ", tbl$`Retorno acumulado no ano`[which(tbl$Ativo == "Ibovespa")], "%"),
                               "CDI" = paste0("CDI: ", tbl$`Retorno acumulado no ano`[which(tbl$Ativo == "CDI")], "%"),
                               "Idex-Infra Geral JGP" = paste0("IDEX-Infra: ", tbl$`Retorno acumulado no ano`[which(tbl$Ativo == "Idex-Infra Geral JGP")], "%"),
                               "Idex-CDI Geral JGP" = paste0("IDEX: ", tbl$`Retorno acumulado no ano`[which(tbl$Ativo == "Idex-CDI Geral JGP")], "%"),
                               "IHFA" = paste0("IHFA: ", tbl$`Retorno acumulado no ano`[which(tbl$Ativo == "IHFA")], "%"),
                               "IMA-B" = paste0("IMA-B: ", tbl$`Retorno acumulado no ano`[which(tbl$Ativo == "IMA-B")], "%"),
                               "IMA-B 5" = paste0("IMA-B 5: ", tbl$`Retorno acumulado no ano`[which(tbl$Ativo == "IMA-B 5")], "%"),
                               "IRF-M" = paste0("IRF-M: ", tbl$`Retorno acumulado no ano`[which(tbl$Ativo == "IRF-M")], "%"),
                               "Dólar" = paste0("Dólar: ", tbl$`Retorno acumulado no ano`[which(tbl$Ativo == "Dólar")], "%"))) +
  scale_linetype_manual(values = c("CDI" = "longdash", 
                                   "Ibovespa" = "solid",
                                   "CDI" = "solid",
                                   "Idex-Infra Geral JGP" = "solid",
                                   "Idex-CDI Geral JGP" = "solid",
                                   "IHFA" = "solid",
                                   "IMA-B" = "solid",
                                   "IMA-B 5" = "solid",
                                   "IRF-M" = "solid",
                                   "Dólar" = "solid")) +
  guides(linetype = "none") +
  labs(title = NULL,
       subtitle = "Retorno acumulado no ano", 
       caption = "Fonte: Capri com dados da Quantum Axis")

ggsave("retorno anual acumulado.png", width = 4800, height = 2160, units = "px", dpi = 576, path = paste0(getwd(),
                                                                                                         "/Gráficos/Índices"))

## Variação mensal acumulada  -------------------------------------------------------

data %>%
  filter(Data >= floor_date(Sys.Date(), "month")) %>%
  # filter(Data >= as.Date("2024-02-01") & Data < floor_date(Sys.Date(), "month")) %>%
  mutate(Ativo = factor(Ativo, levels = arrange(tbl, desc(`Retorno acumulado no mês`))$Ativo)) %>%
  ggplot() +
  aes(Data, acumulado_mes, colour = Ativo, linetype = Ativo) +
  geom_line(linewidth = .75) +
  theme_bw() + theme(panel.grid.minor = element_blank(),
                     axis.line = element_line(colour = "black"),
                     legend.title = element_blank(),
                     axis.title = element_blank(),
                     strip.background = element_blank()) +
  scale_x_date(expand = c(0,0), date_labels = "%d", breaks = "1 day",) +
  scale_colour_manual(values = c("Ibovespa" = "#2F47AD",
                                 "CDI" = "black",
                                 "Idex-Infra Geral JGP" = "#8C977D",
                                 "Idex-CDI Geral JGP" = "#31AFE0",
                                 "IHFA" = "#E47632",
                                 "IMA-B" = "#AD4728",
                                 "IMA-B 5" = "#3BA58B",
                                 "IRF-M" = "#D4A83F",
                                 "Dólar" = "#FF6F61"),
                      labels = c("Ibovespa" = paste0("Ibovespa: ", tbl$`Retorno acumulado no mês`[which(tbl$Ativo == "Ibovespa")], "%"),
                                 "CDI" = paste0("CDI: ", tbl$`Retorno acumulado no mês`[which(tbl$Ativo == "CDI")], "%"),
                                 "Idex-Infra Geral JGP" = paste0("Idex-Infra: ", tbl$`Retorno acumulado no mês`[which(tbl$Ativo == "Idex-Infra Geral JGP")], "%"),
                                 "Idex-CDI Geral JGP" = paste0("IDEX: ", tbl$`Retorno acumulado no mês`[which(tbl$Ativo == "Idex-CDI Geral JGP")], "%"),
                                 "IHFA" = paste0("IHFA: ", tbl$`Retorno acumulado no mês`[which(tbl$Ativo == "IHFA")], "%"),
                                 "IMA-B" = paste0("IMA-B: ", tbl$`Retorno acumulado no mês`[which(tbl$Ativo == "IMA-B")], "%"),
                                 "IMA-B 5" = paste0("IMA-B 5: ", tbl$`Retorno acumulado no mês`[which(tbl$Ativo == "IMA-B 5")], "%"),
                                 "IRF-M" = paste0("IRF-M: ", tbl$`Retorno acumulado no mês`[which(tbl$Ativo == "IRF-M")], "%"),
                                 "Dólar" = paste0("Dólar: ", tbl$`Retorno acumulado no mês`[which(tbl$Ativo == "Dólar")], "%"))) +
  scale_linetype_manual(values = c("CDI" = "longdash", 
                                   "Ibovespa" = "solid",
                                   "CDI" = "solid",
                                   "Idex-Infra Geral JGP" = "solid",
                                   "Idex-CDI Geral JGP" = "solid",
                                   "IHFA" = "solid",
                                   "IMA-B" = "solid",
                                   "IMA-B 5" = "solid",
                                   "IRF-M" = "solid",
                                   "Dólar" = "solid")) +
  guides(linetype = "none") +
  labs(title = NULL,
       subtitle = "Retorno acumulado no mês", 
       caption = "Fonte: Capri com dados da Quantum Axis")

ggsave("retorno mensal acumulado.png", width = 9720, height = 3920, units = "px", dpi = 1152, path = paste0(getwd(),
                                                                                                            "/Gráficos/Índices"))
