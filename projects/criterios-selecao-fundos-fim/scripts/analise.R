# =========================================================================
# Seleção quantitativa de fundos de crédito privado — v3
# -------------------------------------------------------------------------
# Pacotes carregados via .Rprofile.
# Output em XLSX multi-aba + PNGs.
# =========================================================================

# Setup -------------------------------------------------------------------

rm(list = ls())

Sys.setenv("LANGUAGE" = "Pt")
Sys.setlocale("LC_ALL", "Portuguese")

# Parâmetros globais ------------------------------------------------------

PARAMS = list(
  janelas_dias = c(`1M` = 21, `12M` = 252, `36M` = 756),
  janela_hit_rate_meses = 36,
  janela_vol_rolling = 252,
  cvar_p = 0.95,
  marking_lag_atencao = 2.5, # marking_lag_ratio > 2.5 → atenção
  marking_lag_alerta = 3.5, # > 3.5 → alerta (carteira muito ilíquida)
  data_corte = as.Date("2022-10-27")
)

# Output paths ------------------------------------------------------------

DIR_OUT = "criterios-selecao-fundos-cp/output"
DIR_GRAF = file.path(DIR_OUT, "graphs")
ARQ_XLSX = file.path(DIR_OUT, "selecao_credito.xlsx")

dir.create(DIR_OUT, showWarnings = FALSE, recursive = TRUE)
dir.create(DIR_GRAF, showWarnings = FALSE, recursive = TRUE)


# Coleta de dados ---------------------------------------------------------

fund = readxl::read_excel(
  "criterios-selecao-fundos-cp/data/sample_funds.xlsx",
  sheet = 1
) %>%
  `colnames<-`(c("name", "Data", "value")) %>%
  mutate(
    date = as.Date(Data, format = "%d/%m/%Y"),
    .before = "name",
    .keep = "unused"
  )

gauge = readxl::read_excel(
  "criterios-selecao-fundos-cp/data/gauge.xlsx",
  sheet = 1
) %>%
  `colnames<-`(c("name", "Data", "value")) %>%
  mutate(
    date = as.Date(Data, format = "%d/%m/%Y"),
    .before = "name",
    .keep = "unused"
  )

benchmarks = unique(gauge$name)
message("Benchmarks disponíveis: ", paste(benchmarks, collapse = ", "))


# Tratamento de dados -----------------------------------------------------

raw = bind_rows(fund, gauge) %>%
  filter(date >= PARAMS$data_corte) %>%
  arrange(name, date)

message(
  "Corte aplicado em: ",
  PARAMS$data_corte,
  ". Observações restantes: ",
  nrow(raw)
)

ret_d = raw %>%
  group_by(name) %>%
  mutate(ret_d = value / lag(value, 1) - 1) %>%
  ungroup()

ret_rolling = ret_d %>%
  group_by(name) %>%
  mutate(
    acum_1M = zoo::rollapplyr(
      1 + ret_d,
      width = PARAMS$janelas_dias["1M"],
      FUN = prod,
      fill = NA
    ) -
      1,
    acum_12M = zoo::rollapplyr(
      1 + ret_d,
      width = PARAMS$janelas_dias["12M"],
      FUN = prod,
      fill = NA
    ) -
      1,
    acum_36M = zoo::rollapplyr(
      1 + ret_d,
      width = PARAMS$janelas_dias["36M"],
      FUN = prod,
      fill = NA
    ) -
      1
  ) %>%
  ungroup()

fundos_long = ret_rolling %>%
  filter(!name %in% benchmarks) %>%
  rename(
    fund = name,
    fund_value = value,
    fund_ret_d = ret_d,
    fund_1M = acum_1M,
    fund_12M = acum_12M,
    fund_36M = acum_36M
  )

gauge_long = ret_rolling %>%
  filter(name %in% benchmarks) %>%
  rename(
    gauge = name,
    gauge_value = value,
    gauge_ret_d = ret_d,
    gauge_1M = acum_1M,
    gauge_12M = acum_12M,
    gauge_36M = acum_36M
  )

data = fundos_long %>%
  inner_join(gauge_long, by = "date", relationship = "many-to-many") %>%
  mutate(
    excess_d = (1 + fund_ret_d) / (1 + gauge_ret_d) - 1,
    excess_1M = (1 + fund_1M) / (1 + gauge_1M) - 1,
    excess_12M = (1 + fund_12M) / (1 + gauge_12M) - 1,
    excess_36M = (1 + fund_36M) / (1 + gauge_36M) - 1
  ) %>%
  arrange(fund, gauge, date)

data = data %>%
  group_by(fund, gauge, mes = floor_date(date, "month")) %>%
  mutate(is_eom = date == max(date)) %>%
  ungroup() %>%
  select(-mes)

# Inspeção exploratória (rodar manualmente quando necessário) -------------

fund[, c(1:3)] %>%
  pivot_wider(id_cols = date)

data[, c(1:7)] %>%
  pivot_longer(
    cols = 3:7,
    names_to = "type",
    values_to = "return",
    values_drop_na = TRUE
  ) %>%
  filter(type == "fund_12M") %>%
  ggplot() +
  aes(date, return, color = fund) +
  geom_line() +
  theme(legend.position = "none")

# =========================================================================
# CÁLCULO DAS MÉTRICAS -----------------------------------------------------
# =========================================================================

# Nível 1: por fundo (não depende de benchmark) ---------------------------

dd_metrics = ret_d %>%
  filter(!name %in% benchmarks, !is.na(ret_d)) %>%
  group_by(fund = name) %>%
  arrange(date, .by_group = TRUE) %>%
  group_modify(
    ~ {
      x = xts::xts(.x$ret_d, order.by = .x$date)
      dd = PerformanceAnalytics::findDrawdowns(x)
      tibble(
        max_drawdown = as.numeric(PerformanceAnalytics::maxDrawdown(x)),
        avg_drawdown = as.numeric(PerformanceAnalytics::AverageDrawdown(x)),
        n_drawdowns = sum(dd$return < 0, na.rm = TRUE)
      )
    }
  ) %>%
  ungroup()

marking_lag = ret_d %>%
  filter(
    !name %in% benchmarks,
    !is.na(ret_d),
    date >= max(date) %m-% years(3)
  ) %>%
  group_by(fund = name) %>%
  summarise(
    vol_d_anual = sd(ret_d, na.rm = TRUE) * sqrt(252),
    vol_m_anual = {
      ret_mensal = tibble(date, ret_d) %>%
        mutate(mes = floor_date(date, "month")) %>%
        group_by(mes) %>%
        summarise(r_m = prod(1 + ret_d, na.rm = TRUE) - 1, .groups = "drop") %>%
        pull(r_m)
      sd(ret_mensal, na.rm = TRUE) * sqrt(12)
    },
    marking_lag_ratio = vol_m_anual / vol_d_anual,
    .groups = "drop"
  )

shape_fund = ret_d %>%
  filter(
    !name %in% benchmarks,
    !is.na(ret_d),
    date >= max(date) %m-% years(3)
  ) %>%
  group_by(fund = name) %>%
  summarise(
    skew_fund = PerformanceAnalytics::skewness(ret_d, na.rm = TRUE),
    kurt_fund = PerformanceAnalytics::kurtosis(
      ret_d,
      na.rm = TRUE,
      method = "excess"
    ),
    .groups = "drop"
  )

track_info = ret_d %>%
  filter(!name %in% benchmarks, !is.na(ret_d)) %>%
  group_by(fund = name) %>%
  summarise(
    data_inicio = min(date),
    data_fim = max(date),
    n_dias = n(),
    cobre_americanas = min(date) <= as.Date("2023-01-11"),
    .groups = "drop"
  )

dd_episodes = ret_d %>%
  filter(!name %in% benchmarks, !is.na(ret_d)) %>%
  group_by(fund = name) %>%
  arrange(date, .by_group = TRUE) %>%
  group_modify(
    ~ {
      x = xts::xts(.x$ret_d, order.by = .x$date)
      tab = try(
        PerformanceAnalytics::table.Drawdowns(x, top = 5),
        silent = TRUE
      )
      if (inherits(tab, "try-error") || is.null(tab) || nrow(tab) == 0) {
        return(tibble())
      }
      as_tibble(tab)
    }
  ) %>%
  ungroup()

# Nível 2 e 3: por par (fundo, benchmark) ---------------------------------

prob_metrics = data %>%
  filter(date >= max(date) %m-% years(3), !is.na(excess_d)) %>%
  group_by(fund, gauge) %>%
  summarise(
    p_excess_lt_25bps = ecdf(excess_d)(-0.0025),
    p_excess_lt_50bps = ecdf(excess_d)(-0.0050),
    p_excess_lt_100bps = ecdf(excess_d)(-0.0100),
    .groups = "drop"
  )

shape_metrics = data %>%
  filter(date >= max(date) %m-% years(3), !is.na(excess_d)) %>%
  group_by(fund, gauge) %>%
  summarise(
    median_excess = median(excess_d, na.rm = TRUE),
    skew_excess = PerformanceAnalytics::skewness(excess_d, na.rm = TRUE),
    kurt_excess = PerformanceAnalytics::kurtosis(
      excess_d,
      na.rm = TRUE,
      method = "excess"
    ),
    .groups = "drop"
  )

sortino_metrics = data %>%
  filter(date >= max(date) %m-% years(3), !is.na(fund_ret_d)) %>%
  group_by(fund, gauge) %>%
  arrange(date, .by_group = TRUE) %>%
  summarise(
    sortino = {
      r = fund_ret_d
      mar = gauge_ret_d
      excess = r - mar
      downside = excess[excess < 0]
      mean(excess, na.rm = TRUE) /
        sqrt(mean(downside^2, na.rm = TRUE)) *
        sqrt(252)
    },
    .groups = "drop"
  )

es_metrics = ret_d %>%
  filter(
    !name %in% benchmarks,
    !is.na(ret_d),
    date >= max(date) %m-% years(3)
  ) %>%
  group_by(fund = name) %>%
  arrange(date, .by_group = TRUE) %>%
  group_modify(
    ~ {
      x = xts::xts(.x$ret_d, order.by = .x$date)
      tibble(
        var_95 = -as.numeric(PerformanceAnalytics::VaR(
          x,
          p = PARAMS$cvar_p,
          method = "historical"
        )),
        es_95 = -as.numeric(PerformanceAnalytics::ES(
          x,
          p = PARAMS$cvar_p,
          method = "historical"
        ))
      )
    }
  ) %>%
  ungroup()

perf_metrics = data %>%
  filter(is_eom, date >= max(date) %m-% years(3), !is.na(excess_1M)) %>%
  group_by(fund, gauge) %>%
  summarise(
    excess_anualizado = prod(1 + excess_1M, na.rm = TRUE)^(12 /
      sum(!is.na(excess_1M))) -
      1,
    pct_gauge_medio = mean(fund_1M / gauge_1M * 100, na.rm = TRUE),
    .groups = "drop"
  )

hit_rate = data %>%
  filter(is_eom, !is.na(excess_1M)) %>%
  group_by(fund, gauge) %>%
  arrange(date) %>%
  mutate(
    pos_m = as.integer(excess_1M > 0),
    hit_total = cumsum(pos_m) / row_number(),
    hit_rolling = zoo::rollapplyr(
      pos_m,
      width = PARAMS$janela_hit_rate_meses,
      FUN = mean,
      partial = TRUE
    )
  ) %>%
  ungroup() %>%
  select(fund, gauge, date, hit_total, hit_rolling)

ultimo_hit = hit_rate %>%
  group_by(fund, gauge) %>%
  filter(date == max(date)) %>%
  ungroup() %>%
  select(fund, gauge, hit_total, hit_rolling)

snapshot_rolling = ret_rolling %>%
  filter(!name %in% benchmarks) %>%
  group_by(name) %>%
  filter(date == max(date)) %>%
  ungroup() %>%
  select(fund = name, ret_1M = acum_1M, ret_12M = acum_12M, ret_36M = acum_36M)


# =========================================================================
# MONTAGEM DAS ABAS --------------------------------------------------------
# =========================================================================

# Aba 1: FILTROS ---------------------------------------------------------
aba_filtros = shape_fund %>%
  left_join(dd_metrics, by = "fund") %>%
  left_join(marking_lag %>% select(fund, marking_lag_ratio), by = "fund") %>%
  left_join(track_info, by = "fund") %>%
  mutate(
    flag_marking_lag_atencao = marking_lag_ratio > PARAMS$marking_lag_atencao,
    flag_marking_lag_alerta = marking_lag_ratio > PARAMS$marking_lag_alerta,
    flag_track_curto = n_dias < 252 * 3
  ) %>%
  select(
    fund,
    marking_lag_ratio,
    flag_marking_lag_atencao,
    flag_marking_lag_alerta,
    max_drawdown,
    avg_drawdown,
    n_drawdowns,
    skew_fund,
    kurt_fund,
    n_dias,
    cobre_americanas,
    flag_track_curto,
    data_inicio,
    data_fim
  ) %>%
  arrange(
    desc(flag_marking_lag_alerta),
    desc(flag_marking_lag_atencao),
    desc(flag_track_curto)
  )

# Aba 2: SHORTLIST -------------------------------------------------------
aba_shortlist = perf_metrics %>%
  left_join(dd_metrics %>% select(fund, max_drawdown), by = "fund") %>%
  left_join(sortino_metrics, by = c("fund", "gauge")) %>%
  left_join(ultimo_hit, by = c("fund", "gauge")) %>%
  select(
    fund,
    gauge,
    excess_anualizado,
    max_drawdown,
    sortino,
    hit_rolling,
    hit_total
  ) %>%
  arrange(gauge, desc(excess_anualizado))

# Aba 3: DESEMPATE -------------------------------------------------------
aba_desempate = prob_metrics %>%
  left_join(shape_metrics, by = c("fund", "gauge")) %>%
  left_join(
    perf_metrics %>% select(fund, gauge, excess_anualizado, pct_gauge_medio),
    by = c("fund", "gauge")
  ) %>%
  left_join(
    dd_metrics %>% select(fund, avg_drawdown, n_drawdowns),
    by = "fund"
  ) %>%
  left_join(es_metrics, by = "fund") %>%
  arrange(gauge, desc(excess_anualizado)) %>%
  select(
    fund,
    gauge,
    p_excess_lt_25bps,
    p_excess_lt_50bps,
    p_excess_lt_100bps,
    pct_gauge_medio,
    avg_drawdown,
    n_drawdowns,
    kurt_excess,
    median_excess,
    var_95,
    es_95
  )

# Aba 4: INSUMOS ---------------------------------------------------------
aba_insumos = snapshot_rolling %>% arrange(desc(ret_36M))

# Aba 5: DRAWDOWNS TOP 5 -------------------------------------------------
aba_drawdowns = dd_episodes %>% arrange(fund, desc(abs(Depth)))


# =========================================================================
# EXPORTAÇÃO XLSX ----------------------------------------------------------
# =========================================================================

wb = createWorkbook()

header_style = createStyle(
  fontName = "Arial",
  fontSize = 10,
  textDecoration = "bold",
  fgFill = "#2C3E50",
  fontColour = "#FFFFFF",
  halign = "center",
  valign = "center",
  border = "TopBottom",
  borderColour = "#000000",
  borderStyle = "medium"
)
body_style = createStyle(fontName = "Arial", fontSize = 10, valign = "center")
pct_style = createStyle(
  fontName = "Arial",
  fontSize = 10,
  numFmt = "0.00%",
  valign = "center"
)
pct4_style = createStyle(
  fontName = "Arial",
  fontSize = 10,
  numFmt = "0.0000%",
  valign = "center"
)
num_style = createStyle(
  fontName = "Arial",
  fontSize = 10,
  numFmt = "0.00",
  valign = "center"
)
date_style = createStyle(
  fontName = "Arial",
  fontSize = 10,
  numFmt = "dd/mm/yyyy",
  valign = "center"
)
flag_true_style = createStyle(
  fontName = "Arial",
  fontSize = 10,
  fgFill = "#E74C3C",
  fontColour = "#FFFFFF",
  halign = "center"
)
flag_attn_style = createStyle(
  fontName = "Arial",
  fontSize = 10,
  fgFill = "#F39C12",
  fontColour = "#FFFFFF",
  halign = "center"
)

write_sheet = function(
  wb,
  sheet_name,
  df,
  pct_cols = NULL,
  pct4_cols = NULL,
  num_cols = NULL,
  date_cols = NULL,
  flag_cols = NULL,
  flag_attn_cols = NULL,
  intro_text = NULL
) {
  addWorksheet(wb, sheet_name, gridLines = FALSE)
  start_row = 1
  if (!is.null(intro_text)) {
    writeData(wb, sheet_name, intro_text, startRow = 1, startCol = 1)
    addStyle(
      wb,
      sheet_name,
      createStyle(
        fontName = "Arial",
        fontSize = 11,
        textDecoration = "italic",
        fontColour = "#555555"
      ),
      rows = 1,
      cols = 1
    )
    setRowHeights(wb, sheet_name, rows = 1, heights = 30)
    start_row = 3
  }
  writeData(
    wb,
    sheet_name,
    df,
    startRow = start_row,
    startCol = 1,
    headerStyle = header_style
  )
  n_rows = nrow(df)
  n_cols = ncol(df)
  data_rows = (start_row + 1):(start_row + n_rows)

  addStyle(
    wb,
    sheet_name,
    body_style,
    rows = data_rows,
    cols = 1:n_cols,
    gridExpand = TRUE
  )

  apply_fmt = function(cols_vec, style) {
    if (is.null(cols_vec)) {
      return(invisible())
    }
    idx = which(names(df) %in% cols_vec)
    if (length(idx) > 0) {
      addStyle(
        wb,
        sheet_name,
        style,
        rows = data_rows,
        cols = idx,
        gridExpand = TRUE,
        stack = TRUE
      )
    }
  }
  apply_fmt(pct_cols, pct_style)
  apply_fmt(pct4_cols, pct4_style)
  apply_fmt(num_cols, num_style)
  apply_fmt(date_cols, date_style)

  paint_flag = function(cols_vec, style) {
    if (is.null(cols_vec)) {
      return(invisible())
    }
    for (col_name in cols_vec) {
      col_idx = which(names(df) == col_name)
      if (length(col_idx) == 0) {
        next
      }
      flagged_rows = which(df[[col_name]] == TRUE)
      if (length(flagged_rows) > 0) {
        addStyle(
          wb,
          sheet_name,
          style,
          rows = start_row + flagged_rows,
          cols = col_idx,
          stack = TRUE
        )
      }
    }
  }
  paint_flag(flag_cols, flag_true_style)
  paint_flag(flag_attn_cols, flag_attn_style)

  freezePane(wb, sheet_name, firstActiveRow = start_row + 1, firstActiveCol = 2)
  setColWidths(wb, sheet_name, cols = 1, widths = 45)
  setColWidths(wb, sheet_name, cols = 2:n_cols, widths = 18)
}

write_sheet(
  wb,
  "1_Filtros",
  aba_filtros,
  pct_cols = c("max_drawdown", "avg_drawdown"),
  num_cols = c("marking_lag_ratio", "skew_fund", "kurt_fund"),
  date_cols = c("data_inicio", "data_fim"),
  flag_cols = c("flag_marking_lag_alerta", "flag_track_curto"),
  flag_attn_cols = c("flag_marking_lag_atencao"),
  intro_text = paste0(
    "Nível 1 — Filtros. Vermelho = alerta (marking_lag > ",
    PARAMS$marking_lag_alerta,
    " ou track < 3A). Laranja = atenção (marking_lag > ",
    PARAMS$marking_lag_atencao,
    "). marking_lag mede defasagem efetiva de marcação por iliquidez ",
    "dos papéis — não juízo sobre o gestor."
  )
)
write_sheet(
  wb,
  "2_Shortlist",
  aba_shortlist,
  pct_cols = c("excess_anualizado", "max_drawdown", "hit_rolling", "hit_total"),
  num_cols = c("sortino"),
  intro_text = paste0(
    "Nível 2 — Eixos primários. Comparação entre fundos ",
    "do mesmo peer group (filtre por gauge e categoria). ",
    "max_drawdown substitui ES na régua primária."
  )
)
write_sheet(
  wb,
  "3_Desempate",
  aba_desempate,
  pct_cols = c("avg_drawdown"),
  pct4_cols = c(
    "p_excess_lt_25bps",
    "p_excess_lt_50bps",
    "p_excess_lt_100bps",
    "var_95",
    "es_95",
    "median_excess"
  ),
  num_cols = c("kurt_excess", "pct_gauge_medio"),
  intro_text = paste0(
    "Nível 3 — Contexto e desempate. Ordenado igual à aba 2 para ",
    "correlação visual. ES NaN indica fundo sem cauda esquerda ",
    "observável (informação relevante)."
  )
)
write_sheet(
  wb,
  "4_Insumos",
  aba_insumos,
  pct_cols = c("ret_1M", "ret_12M", "ret_36M"),
  intro_text = paste0(
    "Insumos descritivos — snapshot dos retornos rolling ",
    "no último dia da série. Não são métricas de seleção."
  )
)
if (nrow(aba_drawdowns) > 0) {
  write_sheet(
    wb,
    "5_Drawdowns_Top5",
    aba_drawdowns,
    pct_cols = c("Depth"),
    date_cols = c("From", "Trough", "To"),
    intro_text = paste0(
      "Cinco piores drawdowns por fundo, com datas e ",
      "tempo de recuperação. Diagnóstico qualitativo."
    )
  )
}

saveWorkbook(wb, ARQ_XLSX, overwrite = TRUE)
message("XLSX salvo em: ", ARQ_XLSX)


# =========================================================================
# GRÁFICOS -----------------------------------------------------------------
# =========================================================================

theme_capri = theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.title = element_blank(),
    strip.background = element_blank()
  )

# G1
g1 = data %>%
  filter(gauge == "CDI", !is.na(excess_12M)) %>%
  ggplot(aes(date, excess_12M)) +
  geom_line() +
  geom_hline(yintercept = 0, color = "blue", linewidth = .25) +
  facet_wrap(~fund, scales = "free_y") +
  scale_y_continuous(labels = scales::percent) +
  theme_capri +
  labs(
    title = "Excesso de retorno 12M vs CDI",
    caption = "Elaboração: Capri Family Office"
  )
ggsave(
  file.path(DIR_GRAF, "01_excesso_12M_vs_CDI.png"),
  g1,
  width = 14,
  height = 9,
  dpi = 200
)

# G2
g2 = hit_rate %>%
  filter(gauge == "CDI") %>%
  ggplot(aes(date, hit_rolling)) +
  geom_line() +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
  facet_wrap(~fund, scales = "free_x") +
  scale_y_continuous(labels = scales::percent) +
  theme_capri +
  labs(
    title = paste0(
      "Hit rate mensal rolling ",
      PARAMS$janela_hit_rate_meses,
      "M vs CDI"
    )
  )
ggsave(
  file.path(DIR_GRAF, "02_hit_rate_mensal.png"),
  g2,
  width = 14,
  height = 9,
  dpi = 200
)

# G3
g3 = data %>%
  filter(gauge == "CDI", date >= max(date) %m-% years(3), !is.na(excess_d)) %>%
  ggplot(aes(excess_d)) +
  geom_histogram(aes(y = after_stat(density)), fill = "grey", bins = 60) +
  geom_density() +
  geom_vline(xintercept = 0) +
  facet_wrap(~fund, scales = "free") +
  geom_text(
    data = shape_metrics %>% filter(gauge == "CDI"),
    aes(
      x = Inf,
      y = Inf,
      hjust = 1,
      vjust = 1,
      label = paste0(
        " skew: ",
        round(skew_excess, 2),
        "\n",
        " kurt: ",
        round(kurt_excess, 2)
      )
    ),
    colour = "red",
    size = 3.5,
    inherit.aes = FALSE
  ) +
  theme_capri +
  labs(title = "Distribuição do excesso diário vs CDI (3A)")
ggsave(
  file.path(DIR_GRAF, "03_distribuicao_excesso.png"),
  g3,
  width = 14,
  height = 9,
  dpi = 200
)

# G4
g4 = ret_d %>%
  filter(!name %in% benchmarks, !is.na(ret_d)) %>%
  group_by(name) %>%
  arrange(date) %>%
  mutate(
    cota_ix = cumprod(1 + ret_d),
    hwm = cummax(cota_ix),
    dd = cota_ix / hwm - 1
  ) %>%
  ungroup() %>%
  ggplot(aes(date, dd)) +
  geom_area(fill = "darkred", alpha = .6) +
  facet_wrap(~name, scales = "free") +
  scale_y_continuous(labels = scales::percent) +
  theme_capri +
  labs(title = "Drawdown (underwater)")
ggsave(
  file.path(DIR_GRAF, "04_drawdown_underwater.png"),
  g4,
  width = 14,
  height = 9,
  dpi = 200
)

# G5
g5 = aba_shortlist %>%
  filter(gauge == "CDI") %>%
  ggplot(aes(sortino, excess_anualizado, label = fund)) +
  geom_point(size = 3, alpha = .7) +
  ggrepel::geom_text_repel(size = 3, max.overlaps = 20) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_y_continuous(labels = scales::percent) +
  theme_capri +
  labs(
    title = "Sortino × Excesso anualizado (3A, vs CDI)",
    x = "Sortino ratio",
    y = "Excesso anualizado"
  )
ggsave(
  file.path(DIR_GRAF, "05_sortino_vs_excesso.png"),
  g5,
  width = 12,
  height = 8,
  dpi = 200
)

# G6: ECDF sobreposto (substitui CDF em zero pelo objeto completo)
g6 = data %>%
  filter(gauge == "CDI", date >= max(date) %m-% years(3), !is.na(excess_d)) %>%
  ggplot(aes(excess_d, colour = fund)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  stat_ecdf(geom = "step", linewidth = .6) +
  scale_x_continuous(labels = scales::percent, limits = c(-0.02, 0.02)) +
  scale_y_continuous(labels = scales::percent) +
  theme_capri +
  labs(
    title = "ECDF do excesso diário vs CDI (3A) — sobreposição",
    subtitle = "Curvas mais à esquerda = mais cauda esquerda gorda",
    x = "Excesso diário",
    y = "F(x)"
  )
ggsave(
  file.path(DIR_GRAF, "06_ecdf_sobreposto.png"),
  g6,
  width = 12,
  height = 8,
  dpi = 200
)

# G7: Sharpe × Sortino (diagnóstico de assimetria)
sharpe_ratio_calc = data %>%
  filter(
    gauge == "CDI",
    date >= max(date) %m-% years(3),
    !is.na(fund_ret_d),
    !is.na(gauge_ret_d)
  ) %>%
  group_by(fund) %>%
  summarise(
    sharpe = mean(fund_ret_d - gauge_ret_d, na.rm = TRUE) /
      sd(fund_ret_d - gauge_ret_d, na.rm = TRUE) *
      sqrt(252),
    .groups = "drop"
  )

g7 = sharpe_ratio_calc %>%
  left_join(sortino_metrics %>% filter(gauge == "CDI"), by = "fund") %>%
  ggplot(aes(sharpe, sortino, label = fund)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
  geom_point(size = 3, alpha = .7) +
  ggrepel::geom_text_repel(size = 3, max.overlaps = 20) +
  theme_capri +
  labs(
    title = "Sharpe × Sortino (3A, vs CDI)",
    subtitle = "Pontos abaixo da diagonal indicam assimetria negativa",
    x = "Sharpe ratio",
    y = "Sortino ratio"
  )
ggsave(
  file.path(DIR_GRAF, "07_sharpe_vs_sortino.png"),
  g7,
  width = 12,
  height = 8,
  dpi = 200
)

message("Gráficos salvos em: ", DIR_GRAF)
message("Pronto.")
