# =============================================================================
# Carteira sintética modelo — offshore
# Capri FO
# -----------------------------------------------------------------------------
# Constrói a série diária de retorno da alocação-alvo e a injeta no objeto
# `data` do script de classes de ativos offshore (mesmo formato long:
# date / name / retorno / value), substituindo o add_row manual.
# =============================================================================

library(tidyverse)
library(quantmod)
library(lubridate)
library(zoo)

# Parâmetros ------------------------------------------------------------------

par_inicio    <- as.Date("2021-01-01")  # tentativa; o início efetivo é o max dos inícios
par_rebal     <- "M"                    # "D", "M", "Q", "A" ou "none"
par_cash_fixo <- FALSE                  # TRUE = CASH acumula par_cash_taxa (act/360)
par_cash_taxa <- 0.038
par_gap_max   <- 5                      # dias corridos de LOCF antes de avisar staleness
par_nav_file  <- "input/navs_offshore.csv"   # colunas: date, sleeve, nav
par_ref       <- "AGG"                  # calendário de referência

# Composição ------------------------------------------------------------------
# fonte: "yahoo"  -> preço ajustado do ticker
#        "manual" -> NAV em par_nav_file; se ausente, cai no proxy abaixo

composicao <- tribble(
  ~sleeve,                  ~rotulo,                          ~ticker,  ~fonte,   ~peso, ~ytm, ~duration,
  "cash",                   "Cash",                           "SGOV",   "yahoo",  0.250,  3.8,  0.25,
  "pimco_income",           "PIMCO Income",                   "PIMIX",  "yahoo",  0.100,  6.8,  6.31,
  "la_ultra_short",         "LA Ultra Short Bond",            "LUBYX",  "yahoo",  0.150,  4.7,  0.66,
  "wellington_credit_tr",   "Wellington Credit Total Return", NA,       "manual", 0.050,  5.0,  4.80,
  "agg",                    "AGG",                            "AGG",    "yahoo",  0.050,  4.8,  5.74,
  "tip",                    "TIP",                            "TIP",    "yahoo",  0.050,  4.4,  6.47,
  "stip",                   "STIP",                           "STIP",   "yahoo",  0.100,  4.3,  2.44,
  "la_sd_high_yield",       "LA Short Duration HY",           "LSYIX",  "yahoo",  0.050,  8.3,  3.20,
  "oaktree_global_credit",  "Oaktree Global Credit",          NA,       "manual", 0.100,  7.0,  2.65,
  "nb_sd_emd",              "NB Short Duration EMD",          NA,       "manual", 0.050,  5.8,  2.70,
  "emca",                   "EMCA",                           "EMCA.L", "yahoo",  0.050,  6.0,  4.24
)

# Proxies para os sleeves sem NAV disponível.
# ATENÇÃO: construção minha, não do gestor. Calibrada grosseiramente por
# duration/perfil de crédito. Substituir por NAV real assim que possível.
proxies <- list(
  wellington_credit_tr  = c(LQD  = 0.70, HYG  = 0.30),
  oaktree_global_credit = c(HYG  = 0.60, BKLN = 0.40),
  nb_sd_emd             = c(EMB  = 0.40, SGOV = 0.60)
)

stopifnot(abs(sum(composicao$peso) - 1) < 1e-9)

# Métricas ex-ante da carteira ------------------------------------------------

metricas <- composicao %>%
  summarise(ytm_carteira      = sum(peso * ytm),
            duration_carteira = sum(peso * duration))

cat(sprintf("\nYTM da carteira: %.2f%% | Duration: %.2f anos\n\n",
            metricas$ytm_carteira, metricas$duration_carteira))

# Coleta ----------------------------------------------------------------------

tickers <- unique(na.omit(c(composicao$ticker, unlist(lapply(proxies, names)))))

baixar <- function(tk, from) {
  px <- try(getSymbols(tk, src = "yahoo", from = from, auto.assign = FALSE),
            silent = TRUE)
  if (inherits(px, "try-error") || is.null(px) || nrow(px) == 0) {
    warning("Falha em ", tk, call. = FALSE)
    return(NULL)
  }
  tibble(date = as.Date(index(px)), ticker = tk, px = as.numeric(Ad(px)))
}

px_raw <- map(tickers, baixar, from = par_inicio) %>%
  compact() %>%
  bind_rows() %>%
  filter(!is.na(px), px > 0)

ret_ticker <- px_raw %>%
  arrange(ticker, date) %>%
  mutate(ret = px / lag(px) - 1, .by = ticker) %>%
  filter(!is.na(ret)) %>%
  select(date, ticker, ret)

# Calendário de referência
calendario <- ret_ticker %>% filter(ticker == par_ref) %>% pull(date) %>% sort()

# Retorno por sleeve ----------------------------------------------------------

## 1. NAV manual (prioridade máxima)
nav_manual <- if (file.exists(par_nav_file)) {
  read_csv(par_nav_file, show_col_types = FALSE) %>%
    mutate(date = as.Date(date)) %>%
    arrange(sleeve, date) %>%
    mutate(ret = nav / lag(nav) - 1, .by = sleeve) %>%
    filter(!is.na(ret)) %>%
    transmute(date, sleeve, ret, origem = "nav")
} else {
  message("Sem ", par_nav_file, " — sleeves manuais rodando em proxy.")
  tibble(date = as.Date(character()), sleeve = character(),
         ret = numeric(), origem = character())
}

## 2. Mercado direto
ret_direto <- composicao %>%
  filter(fonte == "yahoo") %>%
  select(sleeve, ticker) %>%
  inner_join(ret_ticker, by = "ticker") %>%
  transmute(date, sleeve, ret, origem = "mercado")

## 3. Proxy (blend em espaço de retorno; só vale o dia em que todas as pernas existem)
ret_proxy <- imap(proxies, ~ tibble(sleeve = .y, ticker = names(.x),
                                    w = as.numeric(.x))) %>%
  bind_rows() %>%
  inner_join(ret_ticker, by = "ticker", relationship = "many-to-many") %>%
  summarise(ret = sum(w * ret), n_pernas = n(), peso_total = sum(w),
            .by = c(sleeve, date)) %>%
  filter(abs(peso_total - 1) < 1e-9) %>%
  transmute(date, sleeve, ret, origem = "proxy")

ret_sleeve <- bind_rows(nav_manual, ret_direto, ret_proxy) %>%
  mutate(prio = match(origem, c("nav", "mercado", "proxy"))) %>%
  arrange(sleeve, date, prio) %>%
  distinct(sleeve, date, .keep_all = TRUE) %>%
  select(date, sleeve, ret, origem)

## 4. Cash sintético (opcional): acrual act/360 em vez de SGOV
if (par_cash_fixo) {
  ret_sleeve <- ret_sleeve %>%
    filter(sleeve != "cash") %>%
    bind_rows(
      tibble(date = calendario) %>%
        mutate(dias   = as.numeric(date - lag(date)),
               ret    = par_cash_taxa * dias / 360,
               sleeve = "cash",
               origem = "sintetico") %>%
        filter(!is.na(ret)) %>%
        select(date, sleeve, ret, origem)
    )
}

# Alinhamento de calendário ---------------------------------------------------
# Feriado local (ex.: EMCA.L em feriado do Reino Unido) vira retorno 0;
# o movimento acumulado aparece no pregão seguinte. Correto, mas monitorar gaps.

ret_grid <- ret_sleeve %>%
  group_by(sleeve) %>%
  group_modify(~ {
    rng <- range(.x$date)
    tibble(date = calendario[calendario >= rng[1] & calendario <= rng[2]]) %>%
      left_join(.x, by = "date") %>%
      mutate(preenchido = is.na(ret),
             ret        = coalesce(ret, 0),
             origem     = zoo::na.locf(origem, na.rm = FALSE))
  }) %>%
  ungroup()

# Diagnóstico
gap_maximo <- function(x) {
  r <- rle(x)
  if (!any(r$values)) 0L else max(r$lengths[r$values])
}

diagnostico <- ret_grid %>%
  summarise(inicio      = min(date),
            fim         = max(date),
            n           = n(),
            pct_preench = round(100 * mean(preenchido), 1),
            gap_max     = gap_maximo(preenchido),
            origem      = paste(unique(na.omit(origem)), collapse = "/"),
            .by = sleeve) %>%
  left_join(composicao %>% select(sleeve, rotulo, peso), by = "sleeve") %>%
  arrange(desc(peso))

print(diagnostico, n = Inf)

if (any(diagnostico$gap_max > par_gap_max)) {
  warning("Sleeve(s) com sequência de preenchimento acima de ", par_gap_max,
          " dias — checar a fonte.", call. = FALSE)
}

faltando <- setdiff(composicao$sleeve, diagnostico$sleeve)
if (length(faltando)) stop("Sem série para: ", paste(faltando, collapse = ", "))

inicio_carteira <- max(diagnostico$inicio)
cat(sprintf("\nInício efetivo da carteira modelo: %s (limitado por %s)\n\n",
            inicio_carteira,
            paste(diagnostico$rotulo[diagnostico$inicio == inicio_carteira],
                  collapse = ", ")))

# Construção da carteira ------------------------------------------------------

bloco_rebal <- function(d, freq) {
  switch(freq,
         "D"    = as.integer(d),
         "M"    = as.integer(floor_date(d, "month")),
         "Q"    = as.integer(floor_date(d, "quarter")),
         "A"    = as.integer(floor_date(d, "year")),
         "none" = 0L,
         stop("par_rebal inválido"))
}

pesos_diarios <- ret_grid %>%
  filter(date >= inicio_carteira) %>%
  left_join(composicao %>% select(sleeve, rotulo, peso), by = "sleeve") %>%
  mutate(bloco = bloco_rebal(date, par_rebal)) %>%
  arrange(sleeve, date) %>%
  # crescimento acumulado do sleeve até a abertura de t, dentro do bloco
  mutate(cresc = cumprod(1 + ret) / (1 + ret), .by = c(sleeve, bloco)) %>%
  mutate(peso_t = peso * cresc) %>%
  mutate(peso_t = peso_t / sum(peso_t), .by = date)

carteira_ret <- pesos_diarios %>%
  summarise(ret = sum(peso_t * ret), .by = date) %>%
  arrange(date)

# Atribuição de performance (Carino) — soma exatamente o retorno da carteira
atribuicao <- function(pesos_df, ret_df, de = min(ret_df$date), ate = max(ret_df$date)) {
  rp <- ret_df %>% filter(date >= de, date <= ate)
  R  <- prod(1 + rp$ret) - 1
  k  <- if (abs(R) < 1e-12) 1 else log(1 + R) / R
  rp <- rp %>% mutate(kt = ifelse(abs(ret) < 1e-12, 1, log(1 + ret) / ret))

  pesos_df %>%
    filter(date >= de, date <= ate) %>%
    left_join(rp %>% select(date, kt), by = "date") %>%
    summarise(contrib = sum((kt / k) * peso_t * ret), .by = c(sleeve, rotulo)) %>%
    mutate(contrib = round(100 * contrib, 2)) %>%
    arrange(desc(contrib)) %>%
    add_row(rotulo = "TOTAL", contrib = round(100 * R, 2))
}

cat("\nAtribuição no ano:\n")
print(atribuicao(pesos_diarios, carteira_ret,
                 de = floor_date(Sys.Date(), "year")), n = Inf)

# Séries acumuladas no formato do script principal ----------------------------

carteira_series <- carteira_ret %>%
  mutate(name = "carteira",
         ano  = year(date),
         mes  = month(date),
         acumulado_12_meses = zoo::rollapply(1 + ret, width = 252, FUN = prod,
                                             align = "right", fill = NA) - 1) %>%
  group_by(ano, mes) %>%
  mutate(acumulado_mes = cumprod(1 + ret) - 1) %>%
  group_by(ano) %>%
  mutate(acumulado_ano = cumprod(1 + ret) - 1) %>%
  ungroup() %>%
  select(date, name, acumulado_12_meses, acumulado_mes, acumulado_ano) %>%
  pivot_longer(cols = -c(date, name), names_to = "retorno") %>%
  mutate(value = round(value * 100, 2)) %>%
  # o primeiro mês e o primeiro ano da série são parciais — descarta
  filter(!(retorno == "acumulado_ano" & year(date) == year(inicio_carteira)),
         !(retorno == "acumulado_mes" &
             floor_date(date, "month") == floor_date(inicio_carteira, "month")))

# Índice base 100 (para gráfico de nível)
carteira_indice <- carteira_ret %>%
  mutate(indice = 100 * cumprod(1 + ret))

# Janelas de retorno da carteira ----------------------------------------------
# Mesmas três janelas do script principal.

janelas <- c("acumulado_12_meses", "acumulado_ano", "acumulado_mes")

carteira_janelas <- carteira_series %>%
  filter(retorno %in% janelas, !is.na(value)) %>%
  arrange(desc(date)) %>%
  group_by(retorno) %>%
  slice(1) %>%
  ungroup() %>%
  select(date, name, retorno, value) %>%
  arrange(match(retorno, janelas))

print(carteira_janelas)

# Atalhos escalares no environment
carteira_12m <- carteira_janelas$value[carteira_janelas$retorno == "acumulado_12_meses"]
carteira_ano <- carteira_janelas$value[carteira_janelas$retorno == "acumulado_ano"]
carteira_mes <- carteira_janelas$value[carteira_janelas$retorno == "acumulado_mes"]

if (!"acumulado_12_meses" %in% carteira_janelas$retorno) {
  warning("Menos de 252 pregões desde ", inicio_carteira,
          " — acumulado em 12 meses ainda indisponível.", call. = FALSE)
}

# Injeção no objeto `data` e no lst_dt do script principal ---------------------
# Rodar depois do bloco "Tratamento de dados" e ANTES dos loops de gráfico.

if (exists("data")) {

  data <- data %>%
    filter(name != "carteira") %>%
    bind_rows(carteira_series) %>%
    arrange(name, retorno, date)

  # filter(!is.na(value)) evita que o slice pegue os NA do rollapply de 252 dias
  lst_dt <- data %>%
    filter(!is.na(value)) %>%
    arrange(desc(date)) %>%
    group_by(name, retorno) %>%
    slice(1) %>%
    ungroup() %>%
    arrange(name, retorno)

  # a carteira só existe até a última data disponível de TODOS os sleeves;
  # se algum NAV estiver atrasado, ela fica defasada frente às demais classes
  defasagem <- as.numeric(max(data$date[data$name != "carteira"]) -
                            max(carteira_janelas$date))
  if (defasagem > 0) {
    warning("Carteira modelo defasada em ", defasagem,
            " dia(s) frente às demais classes.", call. = FALSE)
  }
}

# -----------------------------------------------------------------------------
# Ajustes nas escalas do script principal:
#
# Gráfico de LINHAS — scale_colour_manual():
#   values = c(..., "emca" = "#6A9955", "carteira" = "#111111")
#   labels = c(..., "carteira" = paste0("Carteira Modelo: ", carteira_12m, "%"))
#   (o "emca" está faltando hoje — a linha some do gráfico)
#
# Destaque da linha da carteira:
#   geom_line(aes(linewidth = name == "carteira")) +
#   scale_linewidth_manual(values = c("TRUE" = 1.4, "FALSE" = 0.75), guide = "none")
#
# Gráfico de BARRAS — scale_x_discrete():
#   label = c(..., "carteira" = "Carteira Modelo")
# -----------------------------------------------------------------------------

# Gráfico de barras corrigido -------------------------------------------------
# Diferenças em relação ao loop atual:
#  - hjust calculado dentro do aes(), a partir do próprio data frame filtrado
#    (hoje ele vem de lst_dt$value[which(...)] sem o filtro de brent, o que
#     desalinha os rótulos assim que qualquer série for excluída do gráfico)
#  - limites do coord_flip derivados do mesmo subset que é plotado
#  - caption com max(date) em vez do vetor inteiro

for (i in janelas) {

  df_i <- lst_dt %>%
    filter(retorno == i, !name %in% c("brent"))

  folga <- ifelse(i == "acumulado_mes", 2, 5)

  g <- df_i %>%
    ggplot(aes(x = reorder(name, value), y = value, fill = value > 0)) +
    geom_bar(stat = "identity") +
    coord_flip(ylim = c(min(df_i$value) - folga, max(df_i$value) + folga)) +
    geom_text(aes(label = paste0(round(value, 2), "%"),
                  hjust = ifelse(value > 0, -0.1, 1.1))) +
    scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "red")) +
    scale_x_discrete(label = c("spx"      = "S&P",
                               "spxew"    = "S&P EW",
                               "ixic"     = "NASDAQ",
                               "rut"      = "Russell",
                               "dji"      = "Dow Jones",
                               "agg"      = "AGG",
                               "hg"       = "High Grade",
                               "hy"       = "High Yield",
                               "sgov"     = "Juros de Curto Prazo",
                               "emca"     = "Emerging Markets",
                               "tip"      = "Inflação Longa",
                               "stip"     = "Inflação Curta",
                               "dxy"      = "Índice do Dólar",
                               "carteira" = "Carteira Modelo")) +
    theme_bw() +
    theme(legend.position    = "none",
          panel.border       = element_blank(),
          axis.line.x.bottom = element_line(color = "black"),
          axis.line.y.left   = element_line(color = "black")) +
    labs(subtitle = case_when(
           i == "acumulado_12_meses" ~ "Retorno acumulado em 12 meses",
           i == "acumulado_ano"      ~ "Retorno acumulado no ano",
           i == "acumulado_mes"      ~ "Retorno acumulado no mês"),
         x = NULL,
         y = "Retorno (%)",
         caption = paste0("Capri FO com dados da Quandl até ", max(df_i$date)))

  print(g)

  ggsave(paste0(i, "_barras.png"),
         width = 4800, height = 2160, units = "px", dpi = 576,
         path = paste0(getwd(), "/output/offshore"))
}

# Gráfico da carteira modelo --------------------------------------------------

g_carteira <- carteira_indice %>%
  ggplot(aes(date, indice)) +
  geom_line(linewidth = 0.9, colour = "#E47632") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.line        = element_line(colour = "black"),
        panel.border     = element_blank(),
        axis.title       = element_blank()) +
  scale_x_date(expand = c(0, 0), date_labels = "%b-%y", breaks = "3 months") +
  labs(subtitle = paste0("Carteira modelo offshore — base 100 (rebal. ",
                         par_rebal, ")"),
       caption  = paste0("Capri FO | YTM ",
                         sprintf("%.2f%%", metricas$ytm_carteira),
                         " | duration ",
                         sprintf("%.2f", metricas$duration_carteira),
                         " anos | até ", max(carteira_indice$date)))

print(g_carteira)

ggsave("carteira_modelo.png",
       width = 4800, height = 2160, units = "px", dpi = 576,
       path = paste0(getwd(), "/output/offshore"))
