
# Carregando pacotes ------------------------------------------------------

library(tidyverse)
library(openxlsx)
library(quantmod)
library(Quandl)

# Setup -------------------------------------------------------------------

rm(list = ls())

Sys.setenv("LANGUAGE" = "Pt")
Sys.setlocale("LC_ALL", "Portuguese")

Quandl.api_key('on_Vk-ogkmufJBMudwhZ')

# Coleta de dados ---------------------------------------------------------

onshore <- readxl::read_excel(paste0(getwd(), "/Dados/index.xlsx"), sheet = 1) %>%
  `colnames<-`(c("Ativo",
                 "Data",
                 "Cota")) %>%
  mutate(Data = as.Date(Data, format = "%d/%m/%Y"))

getSymbols(c("^SPX", "^DJI", "^RUT", "^IXIC", "AGG", "STIP", "TIP", "SGOV", "DX-Y.NYB"), src = 'yahoo', return.class = "data.frame")
getSymbols(c("BAMLHYH0A0HYM2TRIV", "BAMLCC0A0CMTRIV"), src = 'FRED', return.class = "data.frame")

SPX <- data.frame(date = as.Date(rownames(SPX)), SPX[,4])
IXIC <- data.frame(date = as.Date(rownames(IXIC)), IXIC[,4])
DJI <- data.frame(date = as.Date(rownames(DJI)), DJI[,4])
RUT <- data.frame(date = as.Date(rownames(RUT)), RUT[,4])
AGG <- data.frame(date = as.Date(rownames(AGG)), AGG[,6])
TIP <- data.frame(date = as.Date(rownames(TIP)), TIP[,6])
STIP <- data.frame(date = as.Date(rownames(STIP)), STIP[,6])
SGOV <- data.frame(date = as.Date(rownames(SGOV)), SGOV[,6])
DXY <- data.frame(date = as.Date(rownames(`DX-Y.NYB`)), `DX-Y.NYB`[,4])
HG <- data.frame(date = as.Date(rownames(BAMLCC0A0CMTRIV)), BAMLCC0A0CMTRIV)
HY <- data.frame(date = as.Date(rownames(BAMLHYH0A0HYM2TRIV)), BAMLHYH0A0HYM2TRIV)

offshore <- Reduce(function(x, y) merge(x, y, by = "date", all = TRUE), list(SPX, IXIC, DJI, RUT, AGG, TIP, STIP, SGOV, DXY, HG, HY))
offshore <- offshore %>% 
  `colnames<-`(c("date", "SPX", "IXIC", "DJI", "RUT", "AGG", "TIP", "STIP", "SGOV", "DXY", "HG", "HY")) %>% 
  pivot_longer(cols = -1, 
               names_to = "Ativo",
               values_to = "Cota") %>% 
  rename("Data" = "date") %>% 
  as_tibble()

df <- rbind(onshore, offshore) %>% 
  drop_na()

# Classe
class(df)

# Estrutura
glimpse(df)

# Tratamento de dados -----------------------------------------------------

data = df %>% 
  # filter(Data < "2024-05-01") %>% 
  arrange(Ativo, Data) %>%
  group_by(Ativo) %>% 
  mutate(var = (Cota/lag(Cota, 1) - 1) * 100,
         acumulado_12_meses = (zoo::rollapply(1 + var/100, width = 252, FUN = prod, align = 'right', fill = NA) - 1)*100) %>%
  group_by(Ativo, year(Data), month(Data)) %>%
  mutate(acumulado_mes  = round((cumprod(1 + var/100) - 1) * 100, 2),
         var_anualizado = ((1 + acumulado_mes/100)^12 - 1) * 100) %>%
  group_by(Ativo, year(Data)) %>%
  mutate(acumulado_ano = round((cumprod(1 + var/100) - 1) * 100, 2)) %>% 
  ungroup()

tbl = data %>%
  arrange(desc(Data)) %>%
  group_by(`Ativo`) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(desc(acumulado_mes)) %>%
  rename(`Var% no mes` = acumulado_mes,
         `Var% no mes anualizado` = var_anualizado,
         `Var% no ano` = acumulado_ano,
         `Var% em 12 meses` = acumulado_12_meses,
         `Número índice` = Cota) %>% 
  select(Data, 
         Ativo, 
         `Número índice`,
         `Var% no mes`,
         `Var% no mes anualizado`,
         `Var% no ano`,
         `Var% em 12 meses`)

tbl

wb <- loadWorkbook(paste0(getwd(), "/Dados/fechamento_mes.xlsx"))

writeData(wb, "Fechamento", tbl)

saveWorkbook(wb, paste0(getwd(), "/Dados/fechamento_mes.xlsx"), overwrite = TRUE)
