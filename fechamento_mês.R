
# Pacotes -----------------------------------------------------------------

library(tidyverse)
library(openxlsx)
library(quantmod)

# Setup -------------------------------------------------------------------

rm(list = ls())

Sys.setenv("LANGUAGE" = "Pt")
Sys.setlocale("LC_ALL", "Portuguese")

simplify_dfs = function(df_list, col_indices) {
  simplified_dfs = lapply(seq_along(df_list), function(i) {
    df = df_list[[i]] 
    col_index = col_indices[i] 
    simplified_df = data.frame(date = as.Date(rownames(df)), 
                               value = df[, col_index]) 
    colnames(simplified_df)[2] = names(df_list)[i] 
    return(simplified_df)
  })
  
  merged_df = Reduce(function(x, y) merge(x, y, by = "date", all = TRUE), 
                     simplified_dfs) %>% 
    as_tibble()
  return(merged_df)
}

# Coleta de dados ---------------------------------------------------------

onshore <- readxl::read_excel(paste0(getwd(), 
                                     "/Dados/index.xlsx"), 
                              sheet = 1) %>%
  `colnames<-`(c("Ativo",
                 "Data",
                 "Cota")) %>%
  mutate(Data = as.Date(Data, 
                        format = "%d/%m/%Y"))

getSymbols(c(
  "^SPX",
  "^DJI",
  "^RUT", 
  "^IXIC",
  "^SPXEW",
  "AGG",
  "STIP",
  "TIP",
  "SGOV",
  "DX-Y.NYB"
  ), 
  src = 'yahoo', 
  return.class = "data.frame")

getSymbols(c(
  "BAMLHYH0A0HYM2TRIV", 
  "BAMLCC0A0CMTRIV"
  ), 
  src = 'FRED',
  return.class = "data.frame")

offshore_list = list(
  AGG   = `AGG`,
  HG    =  BAMLCC0A0CMTRIV,
  HY    =  BAMLHYH0A0HYM2TRIV,
  DJI   = `DJI`,
  DXY   = `DX-Y.NYB`,
  IXIC  = `IXIC`,
  RUT   = `RUT`,
  SGOV  = `SGOV`,
  SPX   = `SPX`,
  SPXEW = `SPXEW`,
  STIP  = `STIP`,
  TIP   = `TIP`
  )

col_indices = c(
  6,
  1, 
  1,
  6,
  6,
  6,
  6,
  6,
  6,
  6, 
  6,
  6)

offshore_df = simplify_dfs(offshore_list, col_indices)

rm(
  `AGG`,
  BAMLCC0A0CMTRIV,
  BAMLHYH0A0HYM2TRIV,
  `DJI`,
  `DX-Y.NYB`,
  `IXIC`,
  `RUT`,
  `SGOV`,
  `SPX`,
  `SPXEW`,
  `STIP`,
  `TIP`
)
offshore <- offshore_df %>% 
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
