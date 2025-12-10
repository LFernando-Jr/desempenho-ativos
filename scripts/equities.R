
# Setup -------------------------------------------------------------------

rm(list = ls())

# --------------------------- Coleta de dados ------------------------------

# Yahoo (índices/ETFs/FX)
tickers_yahoo <- c(
  "^GSPC",   # S&P 500
  "^DJI",    # Dow Jones
  "^RUT",    # Russell 2000
  "^IXIC",   # Nasdaq
  "^SPXEW",  # S&P 500 Equal Weight
  "AGG", "STIP", "TIP", "SGOV",  # ETFs de renda fixa
  "DX-Y.NYB" # DXY (US Dollar Index) - via ICE/Yahoo
)

yahoo_raw <-
  tq_get(tickers_yahoo) %>%                 # get = "stock.prices" (default)
  janitor::clean_names()

# padroniza (pega adjusted se existir; senão close)
yahoo_tbl <-
  yahoo_raw %>%
  transmute(
    name = case_match(symbol,
                      "^GSPC" ~ "spx",
                      "^DJI" ~ "dji",
                      "^RUT" ~ "rut",
                      "^IXIC" ~ "ixic",
                      "^SPXEW" ~ "spxew",
                      "AGG" ~ "agg",
                      "STIP" ~ "stip",
                      "TIP" ~ "tip",
                      "SGOV" ~ "sgov",
                      "DX-Y.NYB" ~ "dxy",
                      .default = symbol |> tolower()
    ),
    date,
    value = coalesce(adjusted, close)       # índices têm close; ETFs têm adjusted
  )

# FRED (High Yield e Investment Grade – séries de TR index value)
tickers_fred <- c(
  "BAMLHYH0A0HYM2TRIV",  # ICE BofA US High Yield TR Index Value (daily)
  "BAMLCC0A0CMTRIV"      # ICE BofA US Corporate (IG) TR Index Value
)

fred_tbl <- tq_get(tickers_fred, 
                   get = "economic.data") %>%   # coluna 'price'
  transmute(
    name  = case_when(symbol == "BAMLHYH0A0HYM2TRIV" ~ "hy",
                      symbol == "BAMLCC0A0CMTRIV"   ~ "hg"),
    date  = date,
    value = price
  )

# Merge “bronze”
data_raw <- bind_rows(yahoo_tbl, fred_tbl) %>%
  arrange(name, date)

glimpse(data_raw)

# ------------------------ Tratamento / Métricas ---------------------------

# Retorno diário simples; MTD, YTD e 12m (252 úteis) em %
data <-
  data_raw %>%
  group_by(name) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(
    var = value / lag(value) - 1,
    acumulado_12_meses = rollapply(1 + var, width = 252, FUN = prod,
                                   align = "right", fill = NA) - 1
  ) %>%
  ungroup() %>%
  group_by(name, year(date), month(date)) %>%
  mutate(acumulado_mes = cumprod(1 + var) - 1) %>%
  ungroup() %>%
  group_by(name, year(date)) %>%
  mutate(acumulado_ano = cumprod(1 + var) - 1) %>%
  ungroup() %>%
  select(date, name, var, acumulado_12_meses, acumulado_mes, acumulado_ano) %>%
  pivot_longer(cols = -c(date, name), names_to = "retorno") %>%
  mutate(value = round(value * 100, 2))

# Último valor por (ativo x métrica) para ordenar legenda e barras
lst_dt <-
  data %>%
  arrange(desc(date)) %>%
  group_by(name, retorno) %>%
  slice_head(n = 1) %>%
  ungroup()

# ----------------------------- Visualização -------------------------------

# Paleta fixa (mantive as tuas cores)
pal <- c(
  "spx"   = "#2F47AD",
  "spxew" = "#1F99FF",
  "ixic"  = "black",
  "rut"   = "#8C977D",
  "dji"   = "#31AFE0",
  "agg"   = "#E47632",
  "hg"    = "#AD4728",
  "hy"    = "#3BA58B",
  "sgov"  = "#D4A83F",
  "tip"   = "#8057A5",
  "stip"  = "#FF6F61",
  "dxy"   = "#00796B"
)

# Função para plot de linhas por métrica
plot_linhas <- function(metric) {
  # janela do eixo x
  filt <- case_when(
    metric == "acumulado_12_meses" ~ data$date >= (max(data$date) - 360),
    metric == "acumulado_ano"      ~ data$date >= floor_date(Sys.Date(), "year"),
    metric == "acumulado_mes"      ~ data$date >= floor_date(Sys.Date(), "month")
  )
  
  # labels dinâmicos na legenda
  lab_fun <- function(key) {
    val <- lst_dt |> filter(name == key, retorno == metric) |> pull(value)
    nm  <- c(
      spx = "S&P", spxew = "S&P EW", ixic = "Nasdaq", rut = "Russell",
      dji = "Dow Jones", agg = "AGG", hg = "HG", hy = "HY",
      sgov = "SGOV", tip = "TIP", stip = "STIP", dxy = "DXY"
    )[key]
    paste0(nm, ": ", round(val, 2), "%")
  }
  
  ord <- lst_dt |> filter(retorno == metric) |> arrange(desc(value)) |> pull(name)
  
  g <-
    data %>%
    filter(!!filt, retorno == metric) %>%
    mutate(name = factor(name, levels = ord)) %>%
    ggplot(aes(date, value, colour = name)) +
    geom_line(linewidth = 0.75) +
    scale_colour_manual(values = pal,
                        labels = setNames(map_chr(names(pal), lab_fun), names(pal))) +
    scale_x_date(
      expand = c(0, 0),
      date_labels = if (metric == "acumulado_mes") "%d" else "%b-%y",
      breaks = if (metric == "acumulado_mes") "1 day" else "1 month"
    ) +
    theme_bw() +
    theme(
      panel.grid.minor = element_blank(),
      axis.line        = element_line(colour = "black"),
      legend.title     = element_blank(),
      axis.title       = element_blank(),
      strip.background = element_blank()
    ) +
    labs(
      subtitle = case_when(
        metric == "acumulado_12_meses" ~ "Retorno acumulado em 12 meses",
        metric == "acumulado_ano"      ~ "Retorno acumulado no ano",
        metric == "acumulado_mes"      ~ "Retorno acumulado no mês"
      ),
      caption = paste0("Capri FO • Yahoo Finance & FRED • até ", format(max(data$date), "%d-%m-%Y"))
    )
  
  g
}

# Gera e salva
for (m in c("acumulado_12_meses","acumulado_ano","acumulado_mes")) {
  g <- plot_linhas(m)
  print(g)
  ggsave(filename = paste0(m, ".png"),
         width = 4800, height = 2160, units = "px", dpi = 576,
         path = file.path(getwd(), "output"))
}

# ---------------------------- Barras (snap) -------------------------------

for (m in c("acumulado_12_meses","acumulado_ano","acumulado_mes")) {
  
  snap <- lst_dt %>% filter(retorno == m)
  
  g <- snap %>%
    ggplot(aes(x = reorder(name, value), y = value, fill = value > 0)) +
    geom_col() +
    coord_flip(
      ylim = c(min(snap$value) - ifelse(m == "acumulado_mes", 2, 5),
               max(snap$value) + ifelse(m == "acumulado_mes", 2, 5))
    ) +
    geom_text(aes(label = paste0(round(value, 2), "%")),
              hjust = ifelse(snap$value > 0, -0.1, 1.1)) +
    scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "red")) +
    scale_x_discrete(labels = c(
      "spx"="S&P","spxew"="S&P EW","ixic"="NASDAQ","rut"="Russell","dji"="Dow Jones",
      "agg"="AGG","hg"="High Grade","hy"="High Yield","sgov"="Juros Curto Prazo",
      "tip"="Inflação Longa","stip"="Inflação Curta","dxy"="Índice do Dólar"
    )) +
    theme_bw() +
    theme(legend.position = "none",
          panel.border = element_blank(),
          axis.line.x.bottom = element_line(color = "black"),
          axis.line.y.left   = element_line(color = "black")) +
    labs(
      subtitle = case_when(
        m == "acumulado_12_meses" ~ "Retorno acumulado em 12 meses",
        m == "acumulado_ano"      ~ "Retorno acumulado no ano",
        m == "acumulado_mes"      ~ "Retorno acumulado no mês"
      ),
      x = NULL, y = "Retorno (%)",
      caption = paste0("Capri FO • Yahoo Finance & FRED • até ", format(max(data$date), "%d-%m-%Y"))
    )
  
  print(g)
  ggsave(filename = paste0(m, "_barras.png"),
         width = 4800, height = 2160, units = "px", dpi = 576,
         path = file.path(getwd(), "output"))
}
