
# Carregando pacotes ------------------------------------------------------

library(tidyverse)
library(magrittr)
library(quantmod)
library(Quandl)

# Setup -------------------------------------------------------------------

rm(list = ls())

Sys.setenv("LANGUAGE" = "Pt")
Sys.setlocale("LC_ALL", "Portuguese")

Quandl.api_key('on_Vk-ogkmufJBMudwhZ')

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
  "AGG",
  "STIP",
  "TIP",
  "SGOV",
  "DX-Y.NYB",
  "CBU7.L",
  "ACWI"
  ), 
  src = 'yahoo', 
  return.class = "data.frame")

getSymbols(c(
  "BAMLHYH0A0HYM2TRIV", 
  "BAMLCC0A0CMTRIV"
  ), 
  src = 'FRED',
  return.class = "data.frame")

df_list = list(
  ACWI  = `ACWI`,
  AGG   = `AGG`,
  HG    = BAMLCC0A0CMTRIV,
  HY    = BAMLHYH0A0HYM2TRIV,
  CBU7  = CBU7.L,
  DJI   = `DJI`,
  DXY   = `DX-Y.NYB`,
  IXIC  = `IXIC`,
  RUT   = `RUT`,
  SGOV  = `SGOV`,
  SPX   = `SPX`,
  STIP  = `STIP`,
  TIP   = `TIP`
  )

col_indices = c(6, 6, 1, 1, 6, 6, 6, 6, 6, 6, 6, 6, 6)

df = simplify_dfs(df_list, col_indices) %>% 
  janitor::clean_names()

# Estrutura
glimpse(df)

# Tratamento de dados -----------------------------------------------------

data = df %>%
  pivot_longer(cols = -1, values_drop_na = TRUE) %>% 
  arrange(name, date) %>%
  mutate(var = (value/lag(value, 1) - 1) * 100,
         acumulado_12_meses = (zoo::rollapply(1 + var/100, 
                                              width = 252, 
                                              FUN = prod, 
                                              align = 'right', 
                                              fill = NA) - 1)*100,
        .by = name) %>%
  group_by(name, year(date), month(date)) %>%
  mutate(acumulado_mes = round((cumprod(1 + var/100) - 1) * 100, 2)) %>%
  group_by(name, year(date)) %>%
  mutate(acumulado_ano = round((cumprod(1 + var/100) - 1) * 100, 2)) %>% 
  ungroup()

tbl <- data[,-c(3,4,6,7)] %>%
  # filter(date < "2024-05-01") %>%
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
                                 "acwi" = "#F4A261",
                                 "cbu7" = "#7F5A58",
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
                                 "agg" = "AGG",
                                 "hg" = "HG",
                                 "hy" = "HY",
                                 "acwi" = "MSCI ACWI",
                                 "cbu7" = "CBU7",
                                 "sgov" = "SGOV",
                                 "tip" = "TIP",
                                 "stip" = "STIP",
                                 "dxy" = "DXY")) +
  labs(title = "Índices", 
       subtitle = "Retorno acumulado em 12 meses",
       caption = "Fonte: Capri com dados da Quandl")

ggsave("acumulado em 12 meses.png", 
       width = 4800, 
       # width = 15,
       height = 2160, 
       # height = 8.661,
       units = "px",
       # units = "in",
       dpi = 576, 
       # dpi = 800,
       path = paste0(getwd(), "/Gráficos/Offshore"))


## Variação anual acumulada -----------------------------------------------

data %>%
  filter(date >= floor_date(Sys.Date(), "year")) %>%
  # filter(date >= floor_date(Sys.Date(), "year") & date < "2024-05-01") %>%
  mutate(name = factor(name, 
                       levels = arrange(
                         tbl, 
                         desc(`Retorno acumulado no ano`)
                         )$name)) %>%
  ggplot() +
  aes(date, acumulado_ano, colour = name) +
  geom_line(linewidth = .75) +
  theme_bw() + theme(panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     legend.title = element_blank(), 
                     axis.title = element_blank(), 
                     strip.background = element_blank()) +
  scale_x_date(expand = c(0,0), date_labels = "%b", breaks = "1 months") +
  scale_colour_manual(
    values = c(
      "spx" = "#2F47AD",
      "ixic" = "black",
      "rut" = "#8C977D",
      "dji" = "#31AFE0",
      "acwi" = "#F4A261",
      "cbu7" = "#7F5A58",
      "agg" = "#E47632",
      "hg" = "#AD4728",
      "hy" = "#3BA58B",
      "sgov" = "#D4A83F",
      "tip" = "#8057A5",
      "stip" = "#FF6F61",
      "dxy" = "#00796B"),
    labels = c(
      "spx" = paste0(
        "S&P: ", 
        tbl$`Retorno acumulado no ano`[which(tbl$name == "spx")], "%"
        ),
      "acwi" = paste0(
        "MSCI ACWI: ", 
        tbl$`Retorno acumulado no ano`[which(tbl$name == "acwi")], "%"
        ),
      "cbu7" = paste0(
        "CBU7: ", 
        tbl$`Retorno acumulado no ano`[which(tbl$name == "cbu7")], "%"
        ),
      "ixic" = paste0(
        "Nasdaq: ", 
        tbl$`Retorno acumulado no ano`[which(tbl$name == "ixic")], "%"
        ),
      "rut" = paste0(
        "Russell: ",
        tbl$`Retorno acumulado no ano`[which(tbl$name == "rut")], "%"
        ),
      "dji" = paste0(
        "Dow Jones: ", 
        tbl$`Retorno acumulado no ano`[which(tbl$name == "dji")], "%"
        ),
      "agg" = paste0(
        "AGG: ", 
        tbl$`Retorno acumulado no ano`[which(tbl$name == "agg")], "%"
        ),
      "hg" = paste0(
        "HG: ", 
        tbl$`Retorno acumulado no ano`[which(tbl$name == "hg")], "%"
        ),
      "hy" = paste0(
        "HY: ", 
        tbl$`Retorno acumulado no ano`[which(tbl$name == "hy")], "%"
        ),
      "sgov" = paste0(
        "SGOV: ", 
        tbl$`Retorno acumulado no ano`[which(tbl$name == "sgov")], "%"
        ),
      "tip" = paste0(
        "TIP: ", 
        tbl$`Retorno acumulado no ano`[which(tbl$name == "tip")], "%"
        ),
      "stip" = paste0(
        "STIP: ", 
        tbl$`Retorno acumulado no ano`[which(tbl$name == "stip")], "%"
        ),
      "dxy" = paste0(
        "DXY: ", 
        tbl$`Retorno acumulado no ano`[which(tbl$name == "dxy")], "%"
        ))) +
  labs(title = NULL,
       subtitle = "Retorno acumulado no ano", 
       caption = "Fonte: Capri com dados da Quandl")

ggsave("retorno anual acumulado.png", 
       width = 4800, 
       # width = 15,
       height = 2160, 
       # height = 8.661,
       units = "px",
       # units = "in",
       dpi = 576, 
       # dpi = 800,
       path = paste0(getwd(), "/Gráficos/Offshore"))


## Variação mensal acumulada ----------------------------------------------

data %>%
  filter(date >= floor_date(Sys.Date(), "month")) %>%
  # filter(date >= as.Date("2024-06-01") & date < floor_date(Sys.Date(), 
  # "month")) %>%
  mutate(name = factor(name, 
                       levels = arrange(
                         tbl, 
                         desc(`Retorno acumulado no mês`)
                         )$name)) %>%
  ggplot() +
  aes(date, acumulado_mes, colour = name) +
  geom_line(linewidth = .75) +
  theme_bw() + theme(panel.grid.minor = element_blank(),
                     axis.line = element_line(colour = "black"),
                     legend.title = element_blank(),
                     axis.title = element_blank(),
                     strip.background = element_blank()) +
  scale_x_date(expand = c(0,0), date_labels = "%d", breaks = "1 day",) +
  scale_colour_manual(
    values = c(
      "spx" = "#2F47AD",
      "ixic" = "black",
      "rut" = "#8C977D",
      "dji" = "#31AFE0",
      "acwi" = "#F4A261",
      "cbu7" = "#7F5A58",
      "agg" = "#E47632",
      "hg" = "#AD4728",
      "hy" = "#3BA58B",
      "sgov" = "#D4A83F",
      "tip" = "#8057A5",
      "stip" = "#FF6F61",
      "dxy" = "#00796B"),
    labels = c(
      "spx" = paste0(
        "S&P: ", 
        tbl$`Retorno acumulado no mês`[which(tbl$name == "spx")], "%"
        ),
      "acwi" = paste0(
        "MSCI ACWI: ", 
        tbl$`Retorno acumulado no mês`[which(tbl$name == "acwi")], "%"
      ),
      "cbu7" = paste0(
        "CBU7: ", 
        tbl$`Retorno acumulado no mês`[which(tbl$name == "cbu7")], "%"
      ),
      "ixic" = paste0(
        "Nasdaq: ", 
        tbl$`Retorno acumulado no mês`[which(tbl$name == "ixic")], "%"
        ),
      "rut" = paste0(
        "Russell: ", 
        tbl$`Retorno acumulado no mês`[which(tbl$name == "rut")], "%"
        ),
      "dji" = paste0(
        "Dow Jones: ", 
        tbl$`Retorno acumulado no mês`[which(tbl$name == "dji")], "%"
        ),
      "agg" = paste0(
        "AGG: ", 
        tbl$`Retorno acumulado no mês`[which(tbl$name == "agg")], "%"
        ),
      "hg" = paste0(
        "HG: ", 
        tbl$`Retorno acumulado no mês`[which(tbl$name == "hg")], "%"
        ),
      "hy" = paste0(
        "HY: ", 
        tbl$`Retorno acumulado no mês`[which(tbl$name == "hy")], "%"
        ),
      "sgov" = paste0(
        "SGOV: ", 
        tbl$`Retorno acumulado no mês`[which(tbl$name == "sgov")], "%"
        ),
      "tip" = paste0(
        "TIP: ", 
        tbl$`Retorno acumulado no mês`[which(tbl$name == "tip")], "%"
        ),
      "stip" = paste0(
        "STIP: ", 
        tbl$`Retorno acumulado no mês`[which(tbl$name == "stip")], "%"
        ),
      "dxy" = paste0(
        "DXY: ", 
        tbl$`Retorno acumulado no mês`[which(tbl$name == "dxy")], "%"
        ))) +
  labs(title = NULL,
       subtitle = "Retorno acumulado no mês", 
       caption = "Fonte: Capri com dados da Quandl")

ggsave("retorno acumulado no mês.png", 
       width = 4800, 
       # width = 15,
       height = 2160, 
       # height = 8.661,
       units = "px",
       # units = "in",
       dpi = 576, 
       # dpi = 800,
       path = paste0(getwd(), "/Gráficos/Offshore"))
