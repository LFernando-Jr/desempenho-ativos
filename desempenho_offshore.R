
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

getSymbols(c("^SPX", "^DJI", "^RUT", "^NDX", "AGG", "STIP", "TIP", "SGOV", "DX-Y.NYB"), src = 'yahoo', return.class = "data.frame")
getSymbols(c("BAMLHYH0A0HYM2TRIV", "BAMLCC0A0CMTRIV"), src = 'FRED', return.class = "data.frame")

SPX <- data.frame(date = as.Date(rownames(SPX)), SPX[,4])
NDX <- data.frame(date = as.Date(rownames(NDX)), NDX[,4])
DJI <- data.frame(date = as.Date(rownames(DJI)), DJI[,4])
RUT <- data.frame(date = as.Date(rownames(RUT)), RUT[,4])
AGG <- data.frame(date = as.Date(rownames(AGG)), AGG[,6])
TIP <- data.frame(date = as.Date(rownames(TIP)), TIP[,6])
STIP <- data.frame(date = as.Date(rownames(STIP)), STIP[,6])
SGOV <- data.frame(date = as.Date(rownames(SGOV)), SGOV[,6])
DXY <- data.frame(date = as.Date(rownames(`DX-Y.NYB`)), `DX-Y.NYB`[,4])
HG <- data.frame(date = as.Date(rownames(BAMLCC0A0CMTRIV)), BAMLCC0A0CMTRIV)
HY <- data.frame(date = as.Date(rownames(BAMLHYH0A0HYM2TRIV)), BAMLHYH0A0HYM2TRIV)

df <- Reduce(function(x, y) merge(x, y, by = "date", all = TRUE), list(SPX, NDX, DJI, RUT, AGG, TIP, STIP, SGOV, DXY, HG, HY))
df <- df %>% 
  `colnames<-`(c("date", "spx", "ndx", "dji", "rut", "agg", "tip", "stip", "sgov", "dxy", "hg", "hy")) %>% 
  as_tibble()

# Classe
class(df)

# Estrutura
str(df)

# Tratamento de dados -----------------------------------------------------

data <- df %>%
  filter(date <= "2024-01-31") %>% 
  pivot_longer(cols = -1) %>% 
  group_by(name) %>%
  mutate(var_12M = round(((value / lag(value,252)) - 1)*100,2)) %>%
  mutate(var_ano = round(((value / value[which(date == "2024-01-02")]) - 1)*100,2)) %>%
  mutate(var_mes = round(((value / value[which(date == "2024-01-02")]) - 1)*100,2)) %>% 
  na.omit()

tbl <- data[,c(1,2,5,6)] %>%
  arrange(desc(date)) %>%
  group_by(name) %>%
  slice(1) %>%
  ungroup() %>%
  select(-date) %>%
  arrange(desc(var_mes)) %>%
  rename(`% No mês ` = var_mes,
         `% No ano ` = var_ano)

# Visualização de dados ---------------------------------------------------

## Variação anual -------------------------------------------------------

data %>% 
  na.omit() %>% 
  filter(date >= "2018-01-01") %>%
  ggplot() +
  aes(date, var_12M, colour = name) +
  geom_line(linewidth = .75) +
  theme_bw() + theme(panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     legend.title = element_blank(), 
                     axis.title = element_blank(), 
                     strip.background = element_blank()) + 
  scale_x_date(expand = c(0,0), date_labels = "%Y", breaks = "12 months") +
  scale_colour_manual(values = c("spx" = "#2F47AD",
                                 "ndx" = "black",
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
                                 "ndx" = "Nasdaq",
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
       subtitle = "Variação anual",
       caption = "Fonte: Capri com dados da Quandl")

ggsave("variacao anual.png", width = 4800, height = 2160, units = "px", dpi = 576, path = paste(getwd(),
                                                                                                "/Gráficos/Offshore",
                                                                                                sep = ""))

# ggsave("variacao anual.png", width = 15, height = 8.661, units = "in", dpi = 800, path = paste(getwd(),
#                                                                                                           "/Gráficos/Fundos",
#                                                                                                           sep = ""))

## Variação anual acumulada  -------------------------------------------------------

data %>%
  filter(date >= "2024-01-02") %>%
  mutate(name = factor(name, levels = arrange(tbl, desc(`% No ano `))$name)) %>%
  ggplot() +
  aes(date, var_ano, colour = name) +
  geom_line(linewidth = .75) +
  theme_bw() + theme(panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     legend.title = element_blank(), 
                     axis.title = element_blank(), 
                     strip.background = element_blank()) + 
  scale_x_date(expand = c(0,0), date_labels = "%b", breaks = "1 months") +
  scale_colour_manual(values = c("spx" = "#2F47AD",
                                 "ndx" = "black",
                                 "rut" = "#8C977D",
                                 "dji" = "#31AFE0",
                                 "agg" = "#E47632",
                                 "hg" = "#AD4728",
                                 "hy" = "#3BA58B",
                                 "sgov" = "#D4A83F",
                                 "tip" = "#8057A5",
                                 "stip" = "#FF6F61",
                                 "dxy" = "#00796B"),
                      labels = c("spx" = paste0("S&P: ", tbl$`% No ano `[which(tbl$name == "spx")], "%"),
                                 "ndx" = paste0("Nasdaq: ", tbl$`% No ano `[which(tbl$name == "ndx")], "%"),
                                 "rut" = paste0("Russell: ", tbl$`% No ano `[which(tbl$name == "rut")], "%"),
                                 "dji" = paste0("Dow Jones: ", tbl$`% No ano `[which(tbl$name == "dji")], "%"),
                                 "agg" = paste0("AAG: ", tbl$`% No ano `[which(tbl$name == "agg")], "%"),
                                 "hg" = paste0("HG: ", tbl$`% No ano `[which(tbl$name == "hg")], "%"),
                                 "hy" = paste0("HY: ", tbl$`% No ano `[which(tbl$name == "hy")], "%"),
                                 "sgov" = paste0("SGOV: ", tbl$`% No ano `[which(tbl$name == "sgov")], "%"),
                                 "tip" = paste0("TIP: ", tbl$`% No ano `[which(tbl$name == "tip")], "%"),
                                 "stip" = paste0("STIP: ", tbl$`% No ano `[which(tbl$name == "stip")], "%"),
                                 "dxy" = paste0("DXY: ", tbl$`% No ano `[which(tbl$name == "dxy")], "%"))) +
  labs(title = NULL,
       subtitle = "Variação acumulada no ano", 
       caption = "Fonte: Capri com dados da Quandl")

ggsave("variacao anual acumulada.png", width = 4800, height = 2160, units = "px", dpi = 576, path = paste(getwd(),
                                                                                                          "/Gráficos/Offshore",
                                                                                                          sep = ""))

## Variação mensal acumulada  -------------------------------------------------------

data %>%
  filter(date >= "2024-01-01") %>%
  mutate(name = factor(name, levels = arrange(tbl, desc(`% No mês `))$name)) %>%
  ggplot() +
  aes(date, var_mes, colour = name) +
  geom_line(linewidth = .75) +
  theme_bw() + theme(panel.grid.minor = element_blank(),
                     axis.line = element_line(colour = "black"),
                     legend.title = element_blank(),
                     axis.title = element_blank(),
                     strip.background = element_blank()) +
  scale_x_date(expand = c(0,0), date_labels = "%d", breaks = "1 day",) +
  scale_colour_manual(values = c("spx" = "#2F47AD",
                                 "ndx" = "black",
                                 "rut" = "#8C977D",
                                 "dji" = "#31AFE0",
                                 "agg" = "#E47632",
                                 "hg" = "#AD4728",
                                 "hy" = "#3BA58B",
                                 "sgov" = "#D4A83F",
                                 "tip" = "#8057A5",
                                 "stip" = "#FF6F61",
                                 "dxy" = "#00796B"),
                      labels = c("spx" = paste0("S&P: ", tbl$`% No mês `[which(tbl$name == "spx")], "%"),
                                 "ndx" = paste0("Nasdaq: ", tbl$`% No mês `[which(tbl$name == "ndx")], "%"),
                                 "rut" = paste0("Russell: ", tbl$`% No mês `[which(tbl$name == "rut")], "%"),
                                 "dji" = paste0("Dow Jones: ", tbl$`% No mês `[which(tbl$name == "dji")], "%"),
                                 "agg" = paste0("AAG: ", tbl$`% No mês `[which(tbl$name == "agg")], "%"),
                                 "hg" = paste0("HG: ", tbl$`% No mês `[which(tbl$name == "hg")], "%"),
                                 "hy" = paste0("HY: ", tbl$`% No mês `[which(tbl$name == "hy")], "%"),
                                 "sgov" = paste0("SGOV: ", tbl$`% No mês `[which(tbl$name == "sgov")], "%"),
                                 "tip" = paste0("TIP: ", tbl$`% No mês `[which(tbl$name == "tip")], "%"),
                                 "stip" = paste0("STIP: ", tbl$`% No mês `[which(tbl$name == "stip")], "%"),
                                 "dxy" = paste0("DXY: ", tbl$`% No mês `[which(tbl$name == "dxy")], "%"))) +
  labs(title = NULL,
       subtitle = "Variação acumulada no mês", 
       caption = "Fonte: Capri com dados da Quandl")

ggsave("variacao mensal acumulada.png", width = 9720, height = 3920, units = "px", dpi = 1152, path = paste(getwd(),
                                                                                                            "/Gráficos/Offshore",
                                                                                                            sep = ""))
