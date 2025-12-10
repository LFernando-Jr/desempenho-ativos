# -------------------------------------------------------------------------
# ÍNDICES GLOBAIS → convertidos para USD
# -------------------------------------------------------------------------

idx_global <- c(
  "^GSPC",     # EUA (já USD)
  "^STOXX50E", # Euro Stoxx 50 (EUR)
  "^GDAXI",    # DAX (EUR)
  "^FCHI",     # CAC 40 (EUR)
  "^FTSE",     # FTSE 100 (GBP)
  "^N225",     # Nikkei 225 (JPY)
  "^HSI",      # Hang Seng (HKD)
  "^BVSP"      # Ibovespa (BRL)
)

fx_needed <- c(
  "EURUSD=X",  # EUR -> USD
  "GBPUSD=X",  # GBP -> USD
  "JPY=X",     # USD/JPY
  "HKD=X",     # USD/HKD
  "BRL=X"      # USD/BRL
)

idx_raw  <- tq_get(idx_global)
fx_raw   <- tq_get(fx_needed)

# deixar arrumadinho
idx_tbl <- idx_raw %>%
  clean_names() %>%
  transmute(symbol,
            date,
            price_local = coalesce(adjusted, close)
  )

fx_tbl <- fx_raw %>%
  clean_names() %>%
  transmute(
    fx = symbol,
    date,
    fx_value = coalesce(adjusted, close)
  )

# helper pra saber a moeda de cada índice
idx_currency_map <- tribble(
  ~symbol,       ~fx,
  "^GSPC",       "USD",
  "^STOXX50E",   "EURUSD=X",
  "^GDAXI",      "EURUSD=X",
  "^FCHI",       "EURUSD=X",
  "^FTSE",       "GBPUSD=X",
  "^N225",       "JPY=X",
  "^HSI",        "HKD=X",
  "^BVSP",       "BRL=X"
)

idx_tbl <- idx_tbl %>%
  left_join(idx_currency_map, 
            by = c("symbol"))

# junta com as FX por data
idx_usd <- idx_tbl %>%
  left_join(
    fx_tbl,
    by = c("date","fx")
  ) %>%
  # vamos criar uma fx certa pra cada linha
  # rowwise() %>%
  mutate(
    # pega a fx certa daquele dia praquela moeda
    # fx_ccy = case_when(
    #   ccy == "EUR" ~ (fx_raw %>% filter(symbol == "EURUSD=X", date == !!date) %>% pull(adjusted) %>% first()),
    #   ccy == "GBP" ~ (fx_raw %>% filter(symbol == "GBPUSD=X", date == !!date) %>% pull(adjusted) %>% first()),
    #   ccy == "JPY" ~ (fx_raw %>% filter(symbol == "JPY=X",     date == !!date) %>% pull(adjusted) %>% first()),
    #   ccy == "HKD" ~ (fx_raw %>% filter(symbol == "HKD=X",     date == !!date) %>% pull(adjusted) %>% first()),
    #   ccy == "BRL" ~ (fx_raw %>% filter(symbol == "BRL=X",     date == !!date) %>% pull(adjusted) %>% first()),
    #   TRUE         ~ NA_real_
    # ),
    price_usd = case_when(
      fx == "USD" ~ price_local,
      fx == "EURUSD=X" ~ price_local * fx_value,          # EURUSD
      fx == "GBPUSD=X" ~ price_local * fx_value,          # GBPUSD
      fx == "BRL=X" ~ price_local / fx_value,          # USD/BRL -> divide
      fx == "HKD=X" ~ price_local / fx_value,          # USD/HKD -> divide
      fx == "JPY=X" ~ price_local / fx_value,          # USD/JPY -> divide
      TRUE         ~ NA_real_
    )
  ) %>%
  ungroup()

# renomeia para os teus nomes
idx_usd_clean <- idx_usd %>%
  transmute(
    name = case_match(symbol,
                      "^GSPC"     ~ "usa_spx_usd",
                      "^STOXX50E" ~ "eur_stoxx50_usd",
                      "^GDAXI"    ~ "deu_dax_usd",
                      "^FCHI"     ~ "fra_cac40_usd",
                      "^FTSE"     ~ "uk_ftse_usd",
                      "^N225"     ~ "jpn_nikkei_usd",
                      "^HSI"      ~ "hkg_hsi_usd",
                      "^BVSP"     ~ "bra_ibov_usd",
                      .default = symbol
    ),
    date,
    value = price_usd
  ) %>%
  drop_na(value) %>%
  arrange(name, date)

# agora fazemos os mesmos retornos ------------------------------

data_global <-
  idx_usd_clean %>%
  group_by(name) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(
    var = value / lag(value) - 1,
    acumulado_12_meses = rollapply(1 + var, width = 252,
                                   FUN = prod, align = "right", fill = NA) - 1
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

lst_dt_global <-
  data_global %>%
  arrange(desc(date)) %>%
  group_by(name, retorno) %>%
  slice_head(n = 1) %>%
  ungroup()

# paleta global (pode reaproveitar e só mudar nomes)
pal_global <- c(
  "usa_spx_usd"     = "#2F47AD",
  "eur_stoxx50_usd" = "#1F99FF",
  "deu_dax_usd"     = "#F4A261",
  "fra_cac40_usd"   = "#E76F51",
  "uk_ftse_usd"     = "#31AFE0",
  "jpn_nikkei_usd"  = "#8057A5",
  "hkg_hsi_usd"     = "#3BA58B",
  "bra_ibov_usd"    = "#AD4728"
)

plot_linhas_global <- function(metric) {
  
  filt <- case_when(
    metric == "acumulado_12_meses" ~ data_global$date >= (max(data_global$date) - 360),
    metric == "acumulado_ano"      ~ data_global$date >= floor_date(Sys.Date(), "year"),
    metric == "acumulado_mes"      ~ data_global$date >= floor_date(Sys.Date(), "month")
  )
  
  ord <- lst_dt_global %>%
    filter(retorno == metric) %>%
    arrange(desc(value)) %>%
    pull(name)
  
  g <-
    data_global %>%
    filter(!!filt, retorno == metric) %>%
    mutate(name = factor(name, levels = ord)) %>%
    ggplot(aes(date, value, colour = name)) +
    geom_line(linewidth = 0.75) +
    scale_colour_manual(
      values = pal_global,
      labels = c(
        "usa_spx_usd"     = paste0("S&P 500 (USD): ",      round(lst_dt_global$value[lst_dt_global$name=="usa_spx_usd"     & lst_dt_global$retorno==metric],2), "%"),
        "eur_stoxx50_usd" = paste0("Euro Stoxx 50 (USD): ", round(lst_dt_global$value[lst_dt_global$name=="eur_stoxx50_usd" & lst_dt_global$retorno==metric],2), "%"),
        "deu_dax_usd"     = paste0("DAX (USD): ",           round(lst_dt_global$value[lst_dt_global$name=="deu_dax_usd"     & lst_dt_global$retorno==metric],2), "%"),
        "fra_cac40_usd"   = paste0("CAC 40 (USD): ",        round(lst_dt_global$value[lst_dt_global$name=="fra_cac40_usd"   & lst_dt_global$retorno==metric],2), "%"),
        "uk_ftse_usd"     = paste0("FTSE 100 (USD): ",      round(lst_dt_global$value[lst_dt_global$name=="uk_ftse_usd"     & lst_dt_global$retorno==metric],2), "%"),
        "jpn_nikkei_usd"  = paste0("Nikkei 225 (USD): ",    round(lst_dt_global$value[lst_dt_global$name=="jpn_nikkei_usd"  & lst_dt_global$retorno==metric],2), "%"),
        "hkg_hsi_usd"     = paste0("Hang Seng (USD): ",     round(lst_dt_global$value[lst_dt_global$name=="hkg_hsi_usd"     & lst_dt_global$retorno==metric],2), "%"),
        "bra_ibov_usd"    = paste0("Ibovespa (USD): ",      round(lst_dt_global$value[lst_dt_global$name=="bra_ibov_usd"    & lst_dt_global$retorno==metric],2), "%")
      )
    ) +
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
        metric == "acumulado_12_meses" ~ "Índices globais em USD — 12 meses",
        metric == "acumulado_ano"      ~ "Índices globais em USD — no ano",
        metric == "acumulado_mes"      ~ "Índices globais em USD — no mês"
      ),
      caption = paste0("Capri FO • Yahoo Finance • índices convertidos p/ USD • até ", format(max(data_global$date), "%d-%m-%Y"))
    )
  
  g
}

for (m in c("acumulado_12_meses", "acumulado_ano", "acumulado_mes")) {
  g <- plot_linhas_global(m)
  print(g)
  ggsave(filename = paste0(m, "_global.png"),
         width = 4800, height = 2160, units = "px", dpi = 576,
         path = file.path(getwd(), "output"))
}

# Barras globais ----------------------------------------------------------
for (m in c("acumulado_12_meses","acumulado_ano","acumulado_mes")) {
  
  snap <- lst_dt_global %>% filter(retorno == m)
  
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
      "usa_spx_usd"     = "S&P 500 (USD)",
      "eur_stoxx50_usd" = "Euro Stoxx 50 (USD)",
      "deu_dax_usd"     = "DAX (USD)",
      "fra_cac40_usd"   = "CAC 40 (USD)",
      "uk_ftse_usd"     = "FTSE 100 (USD)",
      "jpn_nikkei_usd"  = "Nikkei 225 (USD)",
      "hkg_hsi_usd"     = "Hang Seng (USD)",
      "bra_ibov_usd"    = "Ibovespa (USD)"
    )) +
    theme_bw() +
    theme(legend.position = "none",
          panel.border = element_blank(),
          axis.line.x.bottom = element_line(color = "black"),
          axis.line.y.left   = element_line(color = "black")) +
    labs(
      subtitle = case_when(
        m == "acumulado_12_meses" ~ "Índices globais em USD — 12 meses",
        m == "acumulado_ano"      ~ "Índices globais em USD — no ano",
        m == "acumulado_mes"      ~ "Índices globais em USD — no mês"
      ),
      x = NULL, y = "Retorno (%)",
      caption = paste0("Capri FO • Yahoo Finance • índices convertidos p/ USD • até ", format(max(data_global$date), "%d-%m-%Y"))
    )
  
  print(g)
  ggsave(filename = paste0(m, "_global_barras.png"),
         width = 4800, height = 2160, units = "px", dpi = 576,
         path = file.path(getwd(), "saídas/offshore"))
}