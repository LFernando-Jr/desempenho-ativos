
# Pacotes -----------------------------------------------------------------

library(tidyverse)
library(magrittr)
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

data_list = list(
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

data_df = simplify_dfs(data_list, col_indices) %>% 
  janitor::clean_names()

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

# Estrutura
glimpse(data_df)

# Tratamento de dados -----------------------------------------------------

data = data_df %>%
  pivot_longer(cols = -1, values_drop_na = TRUE) %>% 
  arrange(name, date) %>%
  mutate(var = (value/lag(value, 1) - 1) * 100,
         acumulado_12_meses = (zoo::rollapply(1 + var/100, 
                                              width = 252, 
                                              FUN   = prod, 
                                              align = 'right', 
                                              fill  = NA) - 1)*100,
        .by = name) %>%
  group_by(name, year(date), month(date)) %>%
  mutate(acumulado_mes = round((cumprod(1 + var/100) - 1) * 100, 2)) %>%
  group_by(name, year(date)) %>%
  mutate(acumulado_ano = round((cumprod(1 + var/100) - 1) * 100, 2)) %>% 
  ungroup() %>% 
  select(-c(3,4,6,7)) %>% 
  pivot_longer(cols = -c(1:2),
               names_to = "retorno")

lst_dt = data %>% 
  arrange(desc(date)) %>% 
  group_by(name, retorno) %>%
  slice(1) %>%
  ungroup()

# Visualização de dados ---------------------------------------------------

## Linhas -----------------------------------------------------------------

retorno = c("acumulado_12_meses",
            "acumulado_ano",
            "acumulado_mes")

for (i in retorno) {
  
  g = data %>% 
    filter(., case_when(
      i == "acumulado_12_meses" ~ date >= last(data$date) - 360,
      i == "acumulado_ano" ~ date >= floor_date(Sys.Date(), "year"),
      i == "acumulado_mes" ~ date >= floor_date(Sys.Date(), "month")),
      retorno == i) %>% 
    mutate(name = factor(name, 
                         levels = arrange(filter(lst_dt, retorno == i), 
                                          desc(value)
                         )$name)) %>%
    ggplot() +
    aes(date, value, colour = name) +
    geom_line(linewidth = .75) +
    theme_bw() + theme(panel.grid.minor = element_blank(), 
                       axis.line        = element_line(colour = "black"),
                       legend.title     = element_blank(), 
                       axis.title       = element_blank(), 
                       strip.background = element_blank()) + 
    scale_x_date(expand = c(0,0), 
                 date_labels = case_when(
                   i == "acumulado_12_meses" ~ "%b-%y",
                   i == "acumulado_ano" ~ "%b-%y",
                   i == "acumulado_mes" ~ "%d"), 
                 breaks = case_when(
                   i == "acumulado_12_meses" ~ "1 month",
                   i == "acumulado_ano" ~ "1 month",
                   i == "acumulado_mes" ~ "1 day")) +
    scale_colour_manual(
      values = c(
        "spx"   = "#2F47AD",
        "spxew" = "#1F99FF",
        "ixic"  = "black",
        "rut"   = "#8C977D",
        "dji"   = "#31AFE0",
        "acwi"  = "#F4A261",
        "cbu7"  = "#7F5A58",
        "agg"   = "#E47632",
        "hg"    = "#AD4728",
        "hy"    = "#3BA58B",
        "sgov"  = "#D4A83F",
        "tip"   = "#8057A5",
        "stip"  = "#FF6F61",
        "dxy"   = "#00796B"),
      labels = c(
        "spx" = paste0(
          "S&P: ", 
          round(lst_dt$value[which(lst_dt$name == "spx" & 
                                     lst_dt$retorno == i)],2), "%"),
        "spxew" = paste0(
          "S&P EW: ", 
          round(lst_dt$value[which(lst_dt$name == "spxew" & 
                                     lst_dt$retorno == i)],2), "%"),
        "ixic" = paste0(
          "Nasdaq: ", 
          round(lst_dt$value[which(lst_dt$name == "ixic" & 
                                     lst_dt$retorno == i)],2), "%"),
        "rut" = paste0(
          "Russell: ",
          round(lst_dt$value[which(lst_dt$name == "rut" & 
                                     lst_dt$retorno == i)],2), "%"),
        "dji" = paste0(
          "Dow Jones: ", 
          round(lst_dt$value[which(lst_dt$name == "dji" & 
                                     lst_dt$retorno == i)],2), "%"),
        "agg" = paste0(
          "AGG: ", 
          round(lst_dt$value[which(lst_dt$name == "agg" & 
                                     lst_dt$retorno == i)],2), "%"),
        "hg" = paste0(
          "HG: ", 
          round(lst_dt$value[which(lst_dt$name == "hg" & 
                                     lst_dt$retorno == i)],2), "%"),
        "hy" = paste0(
          "HY: ", 
          round(lst_dt$value[which(lst_dt$name == "hy" & 
                                     lst_dt$retorno == i)],2), "%"),
        "sgov" = paste0(
          "SGOV: ", 
          round(lst_dt$value[which(lst_dt$name == "sgov" & 
                                     lst_dt$retorno == i)],2), "%"),
        "tip" = paste0(
          "TIP: ", 
          round(lst_dt$value[which(lst_dt$name == "tip" & 
                                     lst_dt$retorno == i)],2), "%"),
        "stip" = paste0(
          "STIP: ", 
          round(lst_dt$value[which(lst_dt$name == "stip" & 
                                     lst_dt$retorno == i)],2), "%"),
        "dxy" = paste0(
          "DXY: ", 
          round(lst_dt$value[which(lst_dt$name == "dxy" & 
                                     lst_dt$retorno == i)],2), "%"))) + 
    labs(title    = NULL, 
         subtitle = case_when(
           i == "acumulado_12_meses" ~ "Retorno acumulado em 12 meses",
           i == "acumulado_ano" ~ "Retorno acumulado no ano",
           i == "acumulado_mes" ~ "Retorno acumulado no mês"),
         caption = paste0("Capri FO com dados da Quandl até ", 
                          as.Date(lst_dt$date, format = "%dd-%mm-yy%")))
  
  print(g)
  
  ggsave(paste0(i, ".png"), 
         width = 4800, 
         # width = 15,
         height = 2160, 
         # height = 8.661,
         units = "px",
         # units = "in",
         dpi = 576, 
         # dpi = 800,
         path = paste0(getwd(), "/gráficos/offshore"))
  
}

## Barras -----------------------------------------------------------------

for (i in retorno) {
  
  g = lst_dt %>%
    filter(retorno == i) %>% 
    ggplot() +
    aes(x = reorder(name, value), 
        y = value, fill = value > 0) +
    geom_bar(stat = "identity") +
    coord_flip(
      ylim = c(min(lst_dt$value[which(lst_dt$retorno == i)]) 
               - ifelse(i == "acumulado_mes", 2, 5),
               max(lst_dt$value[which(lst_dt$retorno == i)]) 
               + ifelse(i == "acumulado_mes", 2, 5))
    ) +
    geom_text(
      aes(label = paste0(round(value, 2), "%")), 
      hjust = ifelse(lst_dt$value[which(lst_dt$retorno == i)] > 0, 
                     -0.1, 1.1)) +
    scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "red")) + 
    scale_x_discrete(label = c("spx"    = "S&P", 
                               "spxew"  = "S&P EW", 
                               "ixic"   = "NASDAQ", 
                               "rut"    = "Russell",
                               "dji"    = "Dow Jones",
                               "agg"    = "AGG", 
                               "hg"     = "High Grade",
                               "hy"     = "High Yield",
                               "sgov"   = "Juros de Curto Prazo",
                               "tip"    = "Inflação Longa",
                               "stip"   = "Inflação Curta",
                               "dxy"    = "Índice do Dólar")) +
    theme_bw() + 
    theme(legend.position = "none", 
          panel.border = element_blank(), 
          axis.line.x.bottom = element_line(color = "black"), 
          axis.line.y.left =  element_line(color = "black")) + 
    labs(subtitle = case_when( 
      i == "acumulado_12_meses" ~ "Retorno acumulado em 12 meses", 
      i == "acumulado_ano" ~ "Retorno acumulado no ano", 
      i == "acumulado_mes" ~ "Retorno acumulado no mês"), 
      x = NULL, 
      y = "Retorno (%)",
      caption = paste0("Capri FO com dados da Quandl até ", 
                       as.Date(lst_dt$date, format = "%dd-%mm-yy%")))
  
  print(g)
  
  ggsave(paste0(i, "_barras.png"), 
         width = 4800, 
         # width = 15,
         height = 2160, 
         # height = 8.661,
         units = "px",
         # units = "in",
         dpi = 576, 
         # dpi = 800,
         path = paste0(getwd(), "/gráficos/offshore"))
  
}
