
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

offshore <- Reduce(function(x, y) merge(x, y, by = "date", all = TRUE), list(SPX, NDX, DJI, RUT, AGG, TIP, STIP, SGOV, DXY, HG, HY))
offshore <- offshore %>% 
  `colnames<-`(c("date", "SPX", "NDX", "DJI", "RUT", "AGG", "TIP", "STIP", "SGOV", "DXY", "HG", "HY")) %>% 
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
str(df)

# Tratamento de dados -----------------------------------------------------

data <- df %>%
  group_by(Ativo) %>%
  mutate(var_mes = round(((Cota / Cota[which(Data == "2024-01-02")]) - 1)*100, 2),
         var_anualizado = ((1 + var_mes/100)^12 - 1) * 100)

tbl <- data %>%
  arrange(desc(Data)) %>%
  group_by(`Ativo`) %>%
  slice(1) %>%
  ungroup() %>%
  select(-Data) %>%
  arrange(desc(var_mes)) %>%
  rename(`% Acumulado No mes ` = var_mes,
         `% Acumulado No mes Anualizado` = var_anualizado,
         `Numero Indice` = Cota)

tbl

wb <- loadWorkbook(paste0(getwd(), "/Dados/fechamento_mes.xlsx"))

writeData(wb, "Fechamento", tbl)

saveWorkbook(wb, paste0(getwd(), "/Dados/fechamento_mes.xlsx"), overwrite = TRUE)
