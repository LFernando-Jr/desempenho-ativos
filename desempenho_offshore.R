
# Carregando pacotes ------------------------------------------------------

library(tidyverse)
library(quantmod)
library(Quandl)

# Setup -------------------------------------------------------------------

rm(list = ls())

Sys.setenv("LANGUAGE" = "Pt")
Sys.setlocale("LC_ALL", "Portuguese")

Quandl.api_key('on_Vk-ogkmufJBMudwhZ')

# Coleta de dados ---------------------------------------------------------


getSymbols(c("^SPX", "^DJI", "^RUT", "^IXIC", "AGG", "STIP", "TIP", "SGOV", "DX-Y.NYB"), src = 'yahoo', return.class = "data.frame")
getSymbols(c("BAMLHYH0A0HYM2TRIV", "BAMLCC0A0CMTRIV"), src = 'FRED', return.class = "data.frame")

getSymbols(c("^SPX", "^DJI", "^RUT", "^NDX", "AGG", "STIP", "TIP", "SGOV", "DX-Y.NYB"), src = 'yahoo', return.class = "data.frame")
getSymbols(c("DGS2","DGS10", "BAMLHYH0A0HYM2TRIV", "BAMLCC0A0CMTRIV"), src = 'FRED', return.class = "data.frame")


SPX <- data.frame(date = as.Date(rownames(SPX)), SPX[,4])
IXIC <- data.frame(date = as.Date(rownames(IXIC)), IXIC[,4])
DJI <- data.frame(date = as.Date(rownames(DJI)), DJI[,4])
RUT <- data.frame(date = as.Date(rownames(RUT)), RUT[,4])
AGG <- data.frame(date = as.Date(rownames(AGG)), AGG[,6])
TIP <- data.frame(date = as.Date(rownames(TIP)), TIP[,6])
STIP <- data.frame(date = as.Date(rownames(STIP)), STIP[,6])
SGOV <- data.frame(date = as.Date(rownames(SGOV)), SGOV[,6])
DXY <- data.frame(date = as.Date(rownames(`DX-Y.NYB`)), `DX-Y.NYB`[,4])
DGS2 <- data.frame(date = as.Date(rownames(DGS2)), DGS2)[,-c(2,3)]
DGS10 <- data.frame(date = as.Date(rownames(DGS10)), DGS10)[,-c(2,3)]
HG <- data.frame(date = as.Date(rownames(BAMLCC0A0CMTRIV)), BAMLCC0A0CMTRIV)
HY <- data.frame(date = as.Date(rownames(BAMLHYH0A0HYM2TRIV)), BAMLHYH0A0HYM2TRIV)


df <- Reduce(function(x, y) merge(x, y, by = "date", all = TRUE), list(SPX, IXIC, DJI, RUT, AGG, TIP, STIP, SGOV, DXY, HG, HY))
df <- df %>% 
  `colnames<-`(c("date", "spx", "ixic", "dji", "rut", "agg", "tip", "stip", "sgov", "dxy", "hg", "hy")) %>% 

df <- Reduce(function(x, y) merge(x, y, by = "date", all = TRUE), list(SPX, NDX, DJI, RUT, AGG, TIP, STIP, SGOV, DXY, DGS2, DGS10, HG, HY))
df <- df %>% 
  `colnames<-`(c("date", "spx", "ndx", "dji", "rut", "agg", "tip", "stip", "sgov", "dxy", "DGS2", "DGS10", "hg", "hy")) %>% 
  as_tibble()

# Classe
class(df)

# Estrutura
glimpse(df)

# Tratamento de dados -----------------------------------------------------

data <- df %>%
  pivot_longer(cols = -1, values_drop_na = TRUE) %>% 
  arrange(name, date) %>%
  group_by(name) %>%
  mutate(var = (value/lag(value, 1) - 1) * 100,
         acumulado_12_meses = (zoo::rollapply(1 + var/100, width = 252, FUN = prod, align = 'right', fill = NA) - 1)*100) %>%
  group_by(name, year(date), month(date)) %>%
  mutate(acumulado_mes = round((cumprod(1 + var/100) - 1) * 100, 2)) %>%
  group_by(name, year(date)) %>%
  mutate(acumulado_ano = round((cumprod(1 + var/100) - 1) * 100, 2)) %>% 
  ungroup()

tbl <- data[,-c(3,4,6,7)] %>%
  # filter(date <= "2024-02-29") %>% 
  arrange(desc(date)) %>%
  arrange(desc(date)) %>%
  group_by(name) %>%
  slice(1) %>%
  ungroup() %>%
  select(-date) %>%
  arrange(desc(acumulado_mes)) %>%
  rename(`Retorno acumulado no mês` = acumulado_mes,
         `Retorno acumulado no ano` = acumulado_ano,
         `Retorno acumulado em 12 meses` = acumulado_12_meses)

tbl

(df$DGS2[which(df$date == "2024-02-27")] - df$DGS2[which(df$date == "2024-01-31")]) * 100

(df$DGS10[which(df$date == "2024-02-27")] - df$DGS10[which(df$date == "2024-01-31")]) * 100


# Visualização de dados ---------------------------------------------------

## Variação anual -------------------------------------------------------

data %>% 
  filter(date >= last(data$date) - 360) %>% 
  ggplot() +
  aes(date, acumulado_12_meses, colour = name) +
  geom_line(linewidth = .75) +
  theme_bw() + theme(panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     legend.title = element_blank(), 
                     axis.title = element_blank(), 
                     strip.background = element_blank()) + 
  scale_x_date(expand = c(0,0), date_labels = "%b-%y", breaks = "1 months") +
  scale_colour_manual(values = c("spx" = "#2F47AD",
                                 "ixic" = "black",
                                 "rut" = "#8C977D",
                                 "dji" = "#31AFE0",
                                 "agg" = "#E47632",
                                 "hg" = "#AD4728",
                                 "hy" = "#3BA58B",
                                 "sgov" = "#D4A83F",
                                 "tip" = "#8057A5",
                                 "stip" = "#FF6F61",
                                 "dxy" = "#00796B"),
                      labels = c("spx" = "S&P",
                                 "ixic" = "Nasdaq",
                                 "rut" = "Russell",
                                 "dji" = "Dow Jones",
                                 "agg" = "AAG",
                                 "hg" = "HG",
                                 "hy" = "HY",
                                 "sgov" = "SGOV",
                                 "tip" = "TIP",
                                 "stip" = "STIP",
                                 "dxy" = "DXY")) +
  labs(title = "Índices", 
       subtitle = "Retorno acumulado em 12 meses",
       caption = "Fonte: Capri com dados da Quandl")

ggsave("acumulado em 12 meses.png", width = 4800, height = 2160, units = "px", dpi = 576, path = paste0(getwd(),
                                                                                                       "/Gráficos/Offshore"))

# ggsave("variacao anual.png", width = 15, height = 8.661, units = "in", dpi = 800, path = paste(getwd(),
#                                                                                                           "/Gráficos/Fundos",
#                                                                                                           sep = ""))

## Variação anual acumulada  -------------------------------------------------------

data %>%
  filter(date >= floor_date(Sys.Date(), "year")) %>%
  mutate(name = factor(name, levels = arrange(tbl, desc(`Retorno acumulado no ano`))$name)) %>%
  ggplot() +
  aes(date, acumulado_ano, colour = name) +
  geom_line(linewidth = .75) +
  theme_bw() + theme(panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     legend.title = element_blank(), 
                     axis.title = element_blank(), 
                     strip.background = element_blank()) + 
  scale_x_date(expand = c(0,0), date_labels = "%b", breaks = "1 months") +
  scale_colour_manual(values = c("spx" = "#2F47AD",
                                 "ixic" = "black",
                                 "rut" = "#8C977D",
                                 "dji" = "#31AFE0",
                                 "agg" = "#E47632",
                                 "hg" = "#AD4728",
                                 "hy" = "#3BA58B",
                                 "sgov" = "#D4A83F",
                                 "tip" = "#8057A5",
                                 "stip" = "#FF6F61",
                                 "dxy" = "#00796B"),
                      labels = c("spx" = paste0("S&P: ", tbl$`Retorno acumulado no ano`[which(tbl$name == "spx")], "%"),
                                 "ixic" = paste0("Nasdaq: ", tbl$`Retorno acumulado no ano`[which(tbl$name == "ixic")], "%"),
                                 "rut" = paste0("Russell: ", tbl$`Retorno acumulado no ano`[which(tbl$name == "rut")], "%"),
                                 "dji" = paste0("Dow Jones: ", tbl$`Retorno acumulado no ano`[which(tbl$name == "dji")], "%"),
                                 "agg" = paste0("AAG: ", tbl$`Retorno acumulado no ano`[which(tbl$name == "agg")], "%"),
                                 "hg" = paste0("HG: ", tbl$`Retorno acumulado no ano`[which(tbl$name == "hg")], "%"),
                                 "hy" = paste0("HY: ", tbl$`Retorno acumulado no ano`[which(tbl$name == "hy")], "%"),
                                 "sgov" = paste0("SGOV: ", tbl$`Retorno acumulado no ano`[which(tbl$name == "sgov")], "%"),
                                 "tip" = paste0("TIP: ", tbl$`Retorno acumulado no ano`[which(tbl$name == "tip")], "%"),
                                 "stip" = paste0("STIP: ", tbl$`Retorno acumulado no ano`[which(tbl$name == "stip")], "%"),
                                 "dxy" = paste0("DXY: ", tbl$`Retorno acumulado no ano`[which(tbl$name == "dxy")], "%"))) +
  labs(title = NULL,
       subtitle = "Retorno acumulado no ano", 
       caption = "Fonte: Capri com dados da Quandl")

ggsave("retorno anual acumulado.png", width = 4800, height = 2160, units = "px", dpi = 576, path = paste0(getwd(),
                                                                                                          "/Gráficos/Offshore"))

## Variação mensal acumulada  -------------------------------------------------------

data %>%
  filter(date >= floor_date(Sys.Date(), "month")) %>%
  # filter(date >= as.Date("2024-02-01") & date < floor_date(Sys.Date(), "month")) %>%
  mutate(name = factor(name, levels = arrange(tbl, desc(`Retorno acumulado no mês`))$name)) %>%
  ggplot() +
  aes(date, acumulado_mes, colour = name) +
  geom_line(linewidth = .75) +
  theme_bw() + theme(panel.grid.minor = element_blank(),
                     axis.line = element_line(colour = "black"),
                     legend.title = element_blank(),
                     axis.title = element_blank(),
                     strip.background = element_blank()) +
  scale_x_date(expand = c(0,0), date_labels = "%d", breaks = "1 day",) +
  scale_colour_manual(values = c("spx" = "#2F47AD",
                                 "ixic" = "black",
                                 "rut" = "#8C977D",
                                 "dji" = "#31AFE0",
                                 "agg" = "#E47632",
                                 "hg" = "#AD4728",
                                 "hy" = "#3BA58B",
                                 "sgov" = "#D4A83F",
                                 "tip" = "#8057A5",
                                 "stip" = "#FF6F61",
                                 "dxy" = "#00796B"),
                      labels = c("spx" = paste0("S&P: ", tbl$`Retorno acumulado no mês`[which(tbl$name == "spx")], "%"),
                                 "ixic" = paste0("Nasdaq: ", tbl$`Retorno acumulado no mês`[which(tbl$name == "ixic")], "%"),
                                 "rut" = paste0("Russell: ", tbl$`Retorno acumulado no mês`[which(tbl$name == "rut")], "%"),
                                 "dji" = paste0("Dow Jones: ", tbl$`Retorno acumulado no mês`[which(tbl$name == "dji")], "%"),
                                 "agg" = paste0("AAG: ", tbl$`Retorno acumulado no mês`[which(tbl$name == "agg")], "%"),
                                 "hg" = paste0("HG: ", tbl$`Retorno acumulado no mês`[which(tbl$name == "hg")], "%"),
                                 "hy" = paste0("HY: ", tbl$`Retorno acumulado no mês`[which(tbl$name == "hy")], "%"),
                                 "sgov" = paste0("SGOV: ", tbl$`Retorno acumulado no mês`[which(tbl$name == "sgov")], "%"),
                                 "tip" = paste0("TIP: ", tbl$`Retorno acumulado no mês`[which(tbl$name == "tip")], "%"),
                                 "stip" = paste0("STIP: ", tbl$`Retorno acumulado no mês`[which(tbl$name == "stip")], "%"),
                                 "dxy" = paste0("DXY: ", tbl$`Retorno acumulado no mês`[which(tbl$name == "dxy")], "%"))) +
  labs(title = NULL,
       subtitle = "Retorno acumulado no mês", 
       caption = "Fonte: Capri com dados da Quandl")

ggsave("retorno acumulado no mês.png", width = 9720, height = 3920, units = "px", dpi = 1152, path = paste0(getwd(),
                                                                                                    "/Gráficos/Offshore"))
