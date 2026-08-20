# ==============================================================================
# PIPELINE QUANTITATIVO — FUNDOS DE CRÉDITO LÍQUIDO HIGH GRADE
# Autor: ChatGPT
# Objetivo:
#   1) aproveitar a planilha de ranking já existente;
#   2) incorporar séries diárias de fundos, CDI, IDA-DI e IDA LIQ-DI;
#   3) medir retorno relativo, risco de cauda, beta de crédito e alfa líquido;
#   4) medir eficiência de custo;
#   5) opcionalmente, calcular overlap de carteira por ativo/grupo econômico.
#
# INPUTS ESPERADOS
# ------------------------------------------------------------------------------
# 1. analise_quantitativa_fundos_high_grade.xlsx
#    - abas: "Base Tratada" e "Ranking Quantitativo"
#
# 2. series_fundos_quantum.xlsx | aba "Cotas"
#    Formato longo preferencial:
#       data | cnpj | cota
#    Alternativamente:
#       data | cnpj | retorno
#    Retornos devem estar em decimal: 0,0005 = 0,05%.
#
# 3. benchmarks_quantum.xlsx | aba "Benchmarks"
#    Colunas mínimas:
#       data | cdi_ret | ida_liq_di_nivel | ida_di_nivel
#    Opcional:
#       irfm1_nivel
#    Também são aceitas colunas de retorno: ida_liq_di_ret, ida_di_ret, irfm1_ret.
#
# 4. carteiras_quantum.xlsx | aba "Carteiras" (OPCIONAL)
#       data | cnpj | ativo_id | emissor | grupo_economico | peso
#    peso em decimal ou percentual; o código identifica automaticamente.
#
# OUTPUT
# ------------------------------------------------------------------------------
# resultados_high_grade.xlsx
#   - Ranking_novo
#   - Metricas
#   - Regressoes
#   - Overlap_pares (se houver carteira)
#   - Overlap_ativo (se houver carteira)
#   - Overlap_grupo (se houver carteira)
# ==============================================================================

# ---- 0. Pacotes ---------------------------------------------------------------
pacotes <- c(
  "tidyverse", "readxl", "janitor", "lubridate", "slider",
  "broom", "lmtest", "sandwich", "openxlsx"
)

instalar_ausentes <- FALSE
faltantes <- pacotes[!vapply(pacotes, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltantes) > 0 && instalar_ausentes) install.packages(faltantes)
if (length(faltantes) > 0) {
  stop(
    "Pacotes ausentes: ", paste(faltantes, collapse = ", "),
    ". Instale-os ou defina instalar_ausentes <- TRUE."
  )
}

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(janitor)
  library(lubridate)
  library(slider)
  library(broom)
  library(lmtest)
  library(sandwich)
  library(openxlsx)
})

# ---- 1. Configuração ----------------------------------------------------------

setwd("C:\\Users\\nandd\\Downloads\\selecao_credito")

ARQ_META       <- "analise_quantitativa_fundos_high_grade.xlsx"
ABA_META       <- "Base Tratada"
ABA_RANK       <- "Ranking Quantitativo"

ARQ_COTAS      <- "series_fundos_quantum.xlsx"
ABA_COTAS      <- "Cotas"

ARQ_BENCH      <- "benchmarks_quantum.xlsx"
ABA_BENCH      <- "Benchmarks"

ARQ_CARTEIRAS  <- "carteiras_quantum.xlsx"   # opcional
ABA_CARTEIRAS  <- "Carteiras"

ARQ_SAIDA      <- "resultados_high_grade.xlsx"

MIN_OBS_METRICAS  <- 126L   # aproximadamente 6 meses úteis
MIN_OBS_REGRESSAO <- 252L   # aproximadamente 1 ano útil
LAG_NEWEY_WEST    <- 5L

# Pesos do score novo — altere conforme a filosofia de alocação.
PESOS <- c(
  alpha_custo  = 0.35,
  risco        = 0.25,
  consistencia = 0.20,
  liquidez     = 0.10,
  retorno      = 0.10
)
stopifnot(abs(sum(PESOS) - 1) < 1e-10)

# ---- 2. Funções auxiliares ----------------------------------------------------
normaliza_cnpj <- function(x) {
  x <- as.character(x)
  x <- stringr::str_replace_all(x, "[^0-9]", "")
  x <- stringr::str_pad(x, width = 14, side = "left", pad = "0")
  dplyr::na_if(x, stringr::str_dup("0", 14))
}

normaliza_taxa <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  # 0,60% pode vir como 0,006 ou 0,60. Valores acima de 10% são mantidos
  # para que o usuário perceba eventual erro de origem.
  if_else(!is.na(x) & x > 0.10 & x <= 10, x / 100, x)
}

normaliza_data <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, c("POSIXct", "POSIXt"))) return(as.Date(x))
  if (is.numeric(x)) return(as.Date(x, origin = "1899-12-30"))
  as.Date(x)
}

ret_acum <- function(r) {
  r <- r[is.finite(r)]
  if (length(r) == 0) return(NA_real_)
  prod(1 + r) - 1
}

ret_anual <- function(r, escala = 252) {
  r <- r[is.finite(r)]
  if (length(r) < 2) return(NA_real_)
  prod(1 + r)^(escala / length(r)) - 1
}

ret_relativo <- function(r_ativo, r_base) {
  (1 + r_ativo) / (1 + r_base) - 1
}

vol_anual <- function(r, escala = 252) {
  r <- r[is.finite(r)]
  if (length(r) < 3) return(NA_real_)
  sd(r) * sqrt(escala)
}

max_drawdown <- function(r) {
  r <- r[is.finite(r)]
  if (length(r) == 0) return(NA_real_)
  riqueza <- cumprod(1 + r)
  min(riqueza / cummax(riqueza) - 1, na.rm = TRUE)
}

max_duracao_drawdown <- function(r) {
  r <- r[is.finite(r)]
  if (length(r) == 0) return(NA_integer_)
  riqueza <- cumprod(1 + r)
  abaixo_max <- riqueza < cummax(riqueza)
  runs <- rle(abaixo_max)
  if (!any(runs$values)) return(0L)
  max(runs$lengths[runs$values])
}

expected_shortfall <- function(r, p = 0.05) {
  r <- r[is.finite(r)]
  if (length(r) < 20) return(NA_real_)
  q <- quantile(r, probs = p, na.rm = TRUE, names = FALSE)
  mean(r[r <= q], na.rm = TRUE)
}

pior_janela <- function(r, n) {
  r <- r[is.finite(r)]
  if (length(r) < n) return(NA_real_)
  min(slider::slide_dbl(r, ret_acum, .before = n - 1, .complete = TRUE), na.rm = TRUE)
}

ultimo_retorno <- function(r, n) {
  r <- r[is.finite(r)]
  if (length(r) < n) return(NA_real_)
  ret_acum(tail(r, n))
}

ac_lag1 <- function(r) {
  r <- r[is.finite(r)]
  if (length(r) < 20 || sd(r) == 0) return(NA_real_)
  unname(acf(r, lag.max = 1, plot = FALSE, na.action = na.pass)$acf[2])
}

pct_rank_seguro <- function(x, maior_melhor = TRUE) {
  z <- if (maior_melhor) x else -x
  out <- dplyr::percent_rank(z) * 100
  out[is.na(x)] <- NA_real_
  out
}

media_disponivel <- function(...) {
  m <- cbind(...)
  apply(m, 1, function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE))
}

media_ponderada_disponivel <- function(m, pesos) {
  m <- as.matrix(m)
  apply(m, 1, function(x) {
    ok <- is.finite(x) & is.finite(pesos)
    if (!any(ok)) return(NA_real_)
    sum(x[ok] * pesos[ok]) / sum(pesos[ok])
  })
}

coalesce_col <- function(df, candidatos, default = NA_real_) {
  existentes <- intersect(candidatos, names(df))
  if (length(existentes) == 0) return(rep(default, nrow(df)))
  Reduce(dplyr::coalesce, df[existentes])
}

# Converte nível de índice em retorno, usando retorno informado quando existente.
cria_retorno_benchmark <- function(df, nome) {
  col_ret   <- paste0(nome, "_ret")
  col_nivel <- paste0(nome, "_nivel")

  if (col_ret %in% names(df)) {
    return(as.numeric(df[[col_ret]]))
  }
  if (col_nivel %in% names(df)) {
    return(as.numeric(df[[col_nivel]]) / dplyr::lag(as.numeric(df[[col_nivel]])) - 1)
  }
  rep(NA_real_, nrow(df))
}

# ---- 3. Importação dos metadados já existentes --------------------------------
if (!file.exists(ARQ_META)) stop("Arquivo de metadados não encontrado: ", ARQ_META)

meta_base <- readxl::read_excel(ARQ_META, sheet = ABA_META) |>
  janitor::clean_names()

meta_ids <- readxl::read_excel(ARQ_META, sheet = ABA_RANK) |>
  janitor::clean_names() |>
  transmute(
    fundo = as.character(fundo),
    cnpj = normaliza_cnpj(cnpj)
  ) |>
  distinct(fundo, .keep_all = TRUE)

meta <- meta_base |>
  mutate(fundo = as.character(fundo)) |>
  left_join(meta_ids, by = "fundo") |>
  transmute(
    grupo,
    fundo,
    gestor,
    administrador,
    cnpj,
    risco_xp = suppressWarnings(as.numeric(risco_xp)),
    liquidez_dias = suppressWarnings(as.numeric(
      liquidez_total_cotizacao_liquidacao_em_numero_aproximacao_para_filtros
    )),
    taxa_adm = normaliza_taxa(taxa_de_administracao),
    benchmark_declarado = benchmark,
    data_inicio = normaliza_data(data_de_inicio)
  ) |>
  filter(!is.na(cnpj))

# Caso você acrescente à planilha colunas como taxa_total_maxima ou taxa_performance,
# troque taxa_adm por uma coluna all-in. Em FICs, a taxa da classe pode não capturar
# integralmente os custos dos fundos investidos.

# ---- 4. Importação das cotas/retornos dos fundos -------------------------------
if (!file.exists(ARQ_COTAS)) {
  stop(
    "Arquivo de séries dos fundos não encontrado: ", ARQ_COTAS,
    ". Exporte da Quantum data, CNPJ e cota (ou retorno diário)."
  )
}

cotas_raw <- readxl::read_excel(ARQ_COTAS, sheet = ABA_COTAS) |>
  janitor::clean_names()

colunas_obrigatorias_cotas <- c("data", "cnpj")
if (!all(colunas_obrigatorias_cotas %in% names(cotas_raw))) {
  stop("A aba de cotas precisa conter ao menos as colunas: data e cnpj.")
}
if (!any(c("cota", "retorno", "ret_fundo") %in% names(cotas_raw))) {
  stop("A aba de cotas precisa conter cota, retorno ou ret_fundo.")
}

cotas_raw$cota_padrao <- if ("cota" %in% names(cotas_raw)) {
  as.numeric(cotas_raw$cota)
} else {
  rep(NA_real_, nrow(cotas_raw))
}
cotas_raw$ret_informado_padrao <- coalesce_col(
  cotas_raw, c("ret_fundo", "retorno")
)

cotas <- cotas_raw |>
  mutate(
    data = normaliza_data(data),
    cnpj = normaliza_cnpj(cnpj),
    cota = cota_padrao,
    ret_informado = ret_informado_padrao
  ) |>
  arrange(cnpj, data) |>
  group_by(cnpj) |>
  mutate(
    ret_calculado = cota / lag(cota) - 1,
    ret_fundo = coalesce(ret_informado, ret_calculado)
  ) |>
  ungroup() |>
  select(data, cnpj, ret_fundo) |>
  filter(!is.na(data), !is.na(cnpj), is.finite(ret_fundo), ret_fundo > -1)

# Proteção contra retorno exportado em porcentagem inteira.
if (quantile(abs(cotas$ret_fundo), 0.99, na.rm = TRUE) > 0.20) {
  warning(
    "Os retornos dos fundos parecem estar em percentual inteiro. ",
    "Confirme se 0,05% foi exportado como 0,0005 e não como 0,05."
  )
}

# ---- 5. Importação dos benchmarks ---------------------------------------------
if (!file.exists(ARQ_BENCH)) {
  stop(
    "Arquivo de benchmarks não encontrado: ", ARQ_BENCH,
    ". Exporte CDI, IDA LIQ-DI e IDA-DI da Quantum."
  )
}

bench_raw <- readxl::read_excel(ARQ_BENCH, sheet = ABA_BENCH) |>
  janitor::clean_names() |>
  mutate(data = normaliza_data(data)) |>
  arrange(data)

if (!("cdi_ret" %in% names(bench_raw))) {
  stop("A aba de benchmarks precisa conter cdi_ret em retorno diário decimal.")
}

bench_raw$ida_liq_di_ret_padrao <- cria_retorno_benchmark(bench_raw, "ida_liq_di")
bench_raw$ida_di_ret_padrao <- cria_retorno_benchmark(bench_raw, "ida_di")
bench_raw$irfm1_ret_padrao <- cria_retorno_benchmark(bench_raw, "irfm1")

bench <- bench_raw |>
  mutate(
    cdi_ret = as.numeric(cdi_ret),
    ida_liq_di_ret = ida_liq_di_ret_padrao,
    ida_di_ret = ida_di_ret_padrao,
    irfm1_ret = irfm1_ret_padrao
  ) |>
  transmute(data, cdi_ret, ida_liq_di_ret, ida_di_ret, irfm1_ret) |>
  filter(!is.na(data), is.finite(cdi_ret), cdi_ret > -1)

if (all(is.na(bench$ida_liq_di_ret))) {
  stop("Informe ida_liq_di_nivel ou ida_liq_di_ret na base de benchmarks.")
}

# ---- 6. Painel diário e fatores ------------------------------------------------
painel <- cotas |>
  inner_join(bench, by = "data") |>
  inner_join(meta, by = "cnpj") |>
  arrange(cnpj, data) |>
  mutate(
    excesso_cdi = ret_relativo(ret_fundo, cdi_ret),
    fator_credito_liq = ret_relativo(ida_liq_di_ret, cdi_ret),
    # Spread entre o universo amplo e o líquido: proxy imperfeita para beta de
    # papéis menos líquidos / composição fora do núcleo mais negociado.
    fator_iliquidez = if_else(
      !is.na(ida_di_ret), ret_relativo(ida_di_ret, ida_liq_di_ret), NA_real_
    ),
    fator_juros = if_else(
      !is.na(irfm1_ret), ret_relativo(irfm1_ret, cdi_ret), NA_real_
    )
  )

# ---- 7. Painel mensal ----------------------------------------------------------
painel_mensal <- painel |>
  mutate(mes = floor_date(data, "month")) |>
  group_by(cnpj, fundo, mes) |>
  summarise(
    excesso_cdi = ret_acum(excesso_cdi),
    fator_credito_liq = ret_acum(fator_credito_liq),
    fator_iliquidez = if (all(is.na(fator_iliquidez))) NA_real_ else ret_acum(fator_iliquidez),
    .groups = "drop"
  ) |>
  group_by(cnpj) |>
  arrange(mes, .by_group = TRUE) |>
  mutate(
    excesso_12m = slider::slide_dbl(
      excesso_cdi, ret_acum, .before = 11, .complete = TRUE
    )
  ) |>
  ungroup()

# ---- 8. Métricas de performance, risco e consistência -------------------------
metricas_diarias <- painel |>
  group_by(cnpj, fundo) |>
  filter(n() >= MIN_OBS_METRICAS) |>
  summarise(
    data_inicial = min(data),
    data_final = max(data),
    n_dias = n(),

    retorno_fundo_anual = ret_anual(ret_fundo),
    retorno_cdi_anual = ret_anual(cdi_ret),
    excesso_cdi_anual = ret_anual(excesso_cdi),

    excesso_12m = ultimo_retorno(excesso_cdi, 252),
    excesso_24m_anual = if (n() >= 504) ret_anual(tail(excesso_cdi, 504)) else NA_real_,
    excesso_36m_anual = if (n() >= 756) ret_anual(tail(excesso_cdi, 756)) else NA_real_,

    tracking_error = vol_anual(excesso_cdi),
    information_ratio = excesso_cdi_anual / tracking_error,
    max_dd_rel_cdi = max_drawdown(excesso_cdi),
    duracao_max_dd_dias = max_duracao_drawdown(excesso_cdi),
    es_5_diario = expected_shortfall(excesso_cdi, 0.05),
    pior_21d_rel_cdi = pior_janela(excesso_cdi, 21),
    pior_63d_rel_cdi = pior_janela(excesso_cdi, 63),
    autocorrelacao_1d = ac_lag1(excesso_cdi),
    .groups = "drop"
  )

metricas_mensais <- painel_mensal |>
  group_by(cnpj, fundo) |>
  summarise(
    n_meses = n(),
    hit_rate_mensal = mean(excesso_cdi > 0, na.rm = TRUE),
    hit_rate_12m = if (all(is.na(excesso_12m))) NA_real_ else mean(excesso_12m > 0, na.rm = TRUE),
    pior_mes_rel_cdi = min(excesso_cdi, na.rm = TRUE),
    es_5_mensal = expected_shortfall(excesso_cdi, 0.05),
    up_capture_credito = if (
      sum(fator_credito_liq > 0, na.rm = TRUE) >= 6 &&
      mean(fator_credito_liq[fator_credito_liq > 0], na.rm = TRUE) != 0
    ) {
      mean(excesso_cdi[fator_credito_liq > 0], na.rm = TRUE) /
        mean(fator_credito_liq[fator_credito_liq > 0], na.rm = TRUE)
    } else NA_real_,
    down_capture_credito = if (
      sum(fator_credito_liq < 0, na.rm = TRUE) >= 6 &&
      mean(fator_credito_liq[fator_credito_liq < 0], na.rm = TRUE) != 0
    ) {
      mean(excesso_cdi[fator_credito_liq < 0], na.rm = TRUE) /
        mean(fator_credito_liq[fator_credito_liq < 0], na.rm = TRUE)
    } else NA_real_,
    .groups = "drop"
  )

metricas <- metricas_diarias |>
  left_join(metricas_mensais, by = c("cnpj", "fundo")) |>
  left_join(meta, by = c("cnpj", "fundo"))

# ---- 9. Regressões fatoriais com Newey-West ----------------------------------
regressao_um_fundo <- function(df) {
  df <- df |>
    select(excesso_cdi, fator_credito_liq, fator_iliquidez, fator_juros) |>
    filter(is.finite(excesso_cdi), is.finite(fator_credito_liq))

  if (nrow(df) < MIN_OBS_REGRESSAO) return(tibble())

  # Inclui fatores opcionais apenas quando há observações suficientes e variância.
  vars <- "fator_credito_liq"
  if (sum(is.finite(df$fator_iliquidez)) >= MIN_OBS_REGRESSAO &&
      sd(df$fator_iliquidez, na.rm = TRUE) > 0) {
    vars <- c(vars, "fator_iliquidez")
  }
  if (sum(is.finite(df$fator_juros)) >= MIN_OBS_REGRESSAO &&
      sd(df$fator_juros, na.rm = TRUE) > 0) {
    vars <- c(vars, "fator_juros")
  }

  df_fit <- df |>
    select(all_of(c("excesso_cdi", vars))) |>
    drop_na()

  if (nrow(df_fit) < MIN_OBS_REGRESSAO) return(tibble())

  formula_fit <- reformulate(vars, response = "excesso_cdi")
  fit <- lm(formula_fit, data = df_fit)
  nw <- sandwich::NeweyWest(
    fit, lag = LAG_NEWEY_WEST, prewhite = FALSE, adjust = TRUE
  )

  coef_nw <- lmtest::coeftest(fit, vcov. = nw) |>
    broom::tidy() |>
    select(term, estimate, std_error = std.error, statistic, p_value = p.value)

  resid_vol <- sd(residuals(fit), na.rm = TRUE) * sqrt(252)
  alpha_dia <- coef(fit)[["(Intercept)"]]

  coef_nw |>
    mutate(
      n_obs = nobs(fit),
      r2_ajustado = summary(fit)$adj.r.squared,
      resid_vol_anual = resid_vol,
      alpha_anual = if_else(
        term == "(Intercept)", (1 + estimate)^252 - 1, NA_real_
      ),
      alpha_information_ratio = if_else(
        term == "(Intercept)" & resid_vol > 0,
        ((1 + alpha_dia)^252 - 1) / resid_vol,
        NA_real_
      )
    )
}

regressoes <- painel |>
  group_by(cnpj, fundo) |>
  group_modify(~ regressao_um_fundo(.x)) |>
  ungroup()

reg_modelo <- regressoes |>
  group_by(cnpj, fundo) |>
  summarise(
    r2_ajustado = first(na.omit(r2_ajustado), default = NA_real_),
    resid_vol_anual = first(na.omit(resid_vol_anual), default = NA_real_),
    alpha_anual = first(na.omit(alpha_anual), default = NA_real_),
    alpha_information_ratio = first(
      na.omit(alpha_information_ratio), default = NA_real_
    ),
    .groups = "drop"
  )

reg_coef <- regressoes |>
  select(cnpj, fundo, term, estimate, p_value) |>
  pivot_wider(
    names_from = term,
    values_from = c(estimate, p_value),
    names_glue = "{.value}_{term}"
  )

colunas_coef_esperadas <- c(
  "estimate_(Intercept)", "p_value_(Intercept)",
  "estimate_fator_credito_liq", "p_value_fator_credito_liq",
  "estimate_fator_iliquidez", "p_value_fator_iliquidez",
  "estimate_fator_juros", "p_value_fator_juros"
)
for (nm in setdiff(colunas_coef_esperadas, names(reg_coef))) {
  reg_coef[[nm]] <- NA_real_
}

reg_resumo <- reg_modelo |>
  left_join(reg_coef, by = c("cnpj", "fundo")) |>
  transmute(
    cnpj, fundo, r2_ajustado, resid_vol_anual,
    alpha_anual, alpha_information_ratio,
    alpha_diario = .data[["estimate_(Intercept)"]],
    p_valor_alpha = .data[["p_value_(Intercept)"]],
    beta_credito_liq = estimate_fator_credito_liq,
    p_beta_credito_liq = p_value_fator_credito_liq,
    beta_iliquidez = estimate_fator_iliquidez,
    p_beta_iliquidez = p_value_fator_iliquidez,
    beta_juros = estimate_fator_juros,
    p_beta_juros = p_value_fator_juros
  )

# ---- 10. Custo e eficiência ---------------------------------------------------
analise <- metricas |>
  left_join(reg_resumo, by = c("cnpj", "fundo")) |>
  mutate(
    # Aproximação: a cota já é líquida da taxa de administração. Somar a taxa
    # anual ao retorno/alfa líquido estima o valor antes desse custo, sem captar
    # taxa de performance, despesas extraordinárias ou custos de fundos investidos.
    excesso_bruto_aprox = excesso_cdi_anual + taxa_adm,
    alpha_bruto_aprox = alpha_anual + taxa_adm,

    captura_excesso_investidor = if_else(
      excesso_cdi_anual > 0 & excesso_bruto_aprox > 0,
      excesso_cdi_anual / excesso_bruto_aprox,
      NA_real_
    ),
    custo_sobre_excesso_bruto = if_else(
      excesso_bruto_aprox > 0, taxa_adm / excesso_bruto_aprox, NA_real_
    ),
    alpha_liquido_por_custo = if_else(
      taxa_adm > 0, alpha_anual / taxa_adm, NA_real_
    ),
    excesso_liquido_por_custo = if_else(
      taxa_adm > 0, excesso_cdi_anual / taxa_adm, NA_real_
    )
  )

# ---- 11. Overlap opcional -----------------------------------------------------
calcula_overlap <- function(carteiras, chave_ativo) {
  chave <- rlang::ensym(chave_ativo)

  ultima_posicao <- carteiras |>
    filter(!is.na(!!chave), !is.na(peso)) |>
    group_by(cnpj) |>
    filter(data == max(data, na.rm = TRUE)) |>
    ungroup() |>
    group_by(cnpj, !!chave) |>
    summarise(peso = sum(peso, na.rm = TRUE), .groups = "drop") |>
    group_by(cnpj) |>
    mutate(peso = peso / sum(peso, na.rm = TRUE)) |>
    ungroup()

  wide <- ultima_posicao |>
    pivot_wider(names_from = cnpj, values_from = peso, values_fill = 0)

  ids <- setdiff(names(wide), rlang::as_name(chave))
  if (length(ids) < 2) return(list(matriz = tibble(), pares = tibble()))

  mat_pesos <- as.matrix(wide[, ids, drop = FALSE])
  overlap <- outer(seq_along(ids), seq_along(ids), Vectorize(function(i, j) {
    sum(pmin(mat_pesos[, i], mat_pesos[, j]), na.rm = TRUE)
  }))
  dimnames(overlap) <- list(ids, ids)

  matriz <- as.data.frame(overlap) |>
    rownames_to_column("cnpj") |>
    as_tibble()

  pares <- as.data.frame(as.table(overlap), stringsAsFactors = FALSE) |>
    as_tibble() |>
    rename(cnpj_1 = Var1, cnpj_2 = Var2, overlap = Freq) |>
    mutate(cnpj_1 = as.character(cnpj_1), cnpj_2 = as.character(cnpj_2)) |>
    filter(cnpj_1 < cnpj_2) |>
    arrange(desc(overlap))

  list(matriz = matriz, pares = pares)
}

overlap_ativo <- list(matriz = tibble(), pares = tibble())
overlap_grupo <- list(matriz = tibble(), pares = tibble())

if (file.exists(ARQ_CARTEIRAS)) {
  carteiras <- readxl::read_excel(ARQ_CARTEIRAS, sheet = ABA_CARTEIRAS) |>
    janitor::clean_names() |>
    mutate(
      data = normaliza_data(data),
      cnpj = normaliza_cnpj(cnpj),
      peso = as.numeric(peso),
      peso = if_else(peso > 1.5, peso / 100, peso)
    ) |>
    filter(!is.na(data), !is.na(cnpj), is.finite(peso), peso >= 0)

  if (all(c("ativo_id", "peso") %in% names(carteiras))) {
    overlap_ativo <- calcula_overlap(carteiras, ativo_id)
  }
  if (all(c("grupo_economico", "peso") %in% names(carteiras))) {
    overlap_grupo <- calcula_overlap(carteiras, grupo_economico)
  }

  if (nrow(overlap_grupo$pares) > 0) {
    overlap_medio <- overlap_grupo$pares |>
      pivot_longer(c(cnpj_1, cnpj_2), values_to = "cnpj") |>
      group_by(cnpj) |>
      summarise(
        overlap_medio_grupo = mean(overlap, na.rm = TRUE),
        overlap_max_grupo = max(overlap, na.rm = TRUE),
        .groups = "drop"
      )

    analise <- analise |>
      left_join(overlap_medio, by = "cnpj")
  }
}

# ---- 12. Score novo -----------------------------------------------------------
ranking_novo <- analise |>
  mutate(
    # Custo e alfa: a taxa pesa bastante, mas não premia taxa baixa sem entrega.
    pct_alpha = pct_rank_seguro(alpha_anual, TRUE),
    pct_alpha_ir = pct_rank_seguro(alpha_information_ratio, TRUE),
    p_excesso_custo = pct_rank_seguro(excesso_liquido_por_custo, TRUE),
    p_taxa = pct_rank_seguro(taxa_adm, FALSE),
    p_captura = pct_rank_seguro(captura_excesso_investidor, TRUE),
    score_alpha_custo = media_disponivel(
      pct_alpha, pct_alpha_ir, p_excesso_custo, p_taxa, p_captura
    ),

    # Risco relativo ao CDI, não volatilidade absoluta da cota.
    p_te = pct_rank_seguro(tracking_error, FALSE),
    p_dd = pct_rank_seguro(abs(max_dd_rel_cdi), FALSE),
    p_es = pct_rank_seguro(abs(es_5_mensal), FALSE),
    p_pior_63d = pct_rank_seguro(abs(pior_63d_rel_cdi), FALSE),
    p_down_capture = pct_rank_seguro(down_capture_credito, FALSE),
    score_risco = media_disponivel(p_te, p_dd, p_es, p_pior_63d, p_down_capture),

    # Consistência e possível suavização de retornos.
    p_hit_mensal = pct_rank_seguro(hit_rate_mensal, TRUE),
    p_hit_12m = pct_rank_seguro(hit_rate_12m, TRUE),
    p_autocorr = pct_rank_seguro(abs(autocorrelacao_1d), FALSE),
    score_consistencia = media_disponivel(p_hit_mensal, p_hit_12m, p_autocorr),

    p_liquidez = pct_rank_seguro(liquidez_dias, FALSE),
    score_liquidez = p_liquidez,

    p_excesso = pct_rank_seguro(excesso_cdi_anual, TRUE),
    p_ir = pct_rank_seguro(information_ratio, TRUE),
    score_retorno = media_disponivel(p_excesso, p_ir),

    score_final_novo = media_ponderada_disponivel(
      cbind(
        score_alpha_custo, score_risco, score_consistencia,
        score_liquidez, score_retorno
      ),
      PESOS[c("alpha_custo", "risco", "consistencia", "liquidez", "retorno")]
    )
  ) |>
  arrange(desc(score_final_novo)) |>
  mutate(ranking_novo = row_number()) |>
  select(
    ranking_novo, grupo, fundo, gestor, cnpj,
    taxa_adm, liquidez_dias, risco_xp,
    excesso_cdi_anual, excesso_12m, excesso_24m_anual, excesso_36m_anual,
    alpha_anual, p_valor_alpha, pct_alpha, beta_credito_liq, beta_iliquidez, beta_juros,
    tracking_error, information_ratio, alpha_information_ratio,
    max_dd_rel_cdi, duracao_max_dd_dias, es_5_mensal,
    pior_21d_rel_cdi, pior_63d_rel_cdi,
    hit_rate_mensal, hit_rate_12m, autocorrelacao_1d,
    up_capture_credito, down_capture_credito,
    excesso_liquido_por_custo, alpha_liquido_por_custo,
    captura_excesso_investidor, custo_sobre_excesso_bruto,
    any_of(c("overlap_medio_grupo", "overlap_max_grupo")),
    score_alpha_custo, score_risco, score_consistencia,
    score_liquidez, score_retorno, score_final_novo
  )

# ---- 13. Exportação ------------------------------------------------------------
wb_out <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb_out, "Ranking_novo")
openxlsx::writeDataTable(wb_out, "Ranking_novo", ranking_novo)

openxlsx::addWorksheet(wb_out, "Metricas")
openxlsx::writeDataTable(wb_out, "Metricas", analise)

openxlsx::addWorksheet(wb_out, "Regressoes")
openxlsx::writeDataTable(wb_out, "Regressoes", regressoes)

if (nrow(overlap_ativo$pares) > 0) {
  openxlsx::addWorksheet(wb_out, "Overlap_pares_ativo")
  openxlsx::writeDataTable(wb_out, "Overlap_pares_ativo", overlap_ativo$pares)

  openxlsx::addWorksheet(wb_out, "Overlap_ativo")
  openxlsx::writeDataTable(wb_out, "Overlap_ativo", overlap_ativo$matriz)
}

if (nrow(overlap_grupo$pares) > 0) {
  openxlsx::addWorksheet(wb_out, "Overlap_pares_grupo")
  openxlsx::writeDataTable(wb_out, "Overlap_pares_grupo", overlap_grupo$pares)

  openxlsx::addWorksheet(wb_out, "Overlap_grupo")
  openxlsx::writeDataTable(wb_out, "Overlap_grupo", overlap_grupo$matriz)
}

# Formatação básica
cabecalho <- openxlsx::createStyle(
  fgFill = "#1F4E78", fontColour = "#FFFFFF", textDecoration = "bold",
  halign = "center", valign = "center"
)
percentual <- openxlsx::createStyle(numFmt = "0.00%")
numero_2d <- openxlsx::createStyle(numFmt = "0.00")

for (aba in names(wb_out)) {
  openxlsx::freezePane(wb_out, aba, firstRow = TRUE)
  openxlsx::addStyle(wb_out, aba, cabecalho, rows = 1, cols = 1:200, gridExpand = TRUE)
  openxlsx::setColWidths(wb_out, aba, cols = 1:200, widths = "auto")
}

# Formata percentuais na aba principal pelo nome das colunas.
cols_pct <- which(names(ranking_novo) %in% c(
  "taxa_adm", "excesso_cdi_anual", "excesso_12m", "excesso_24m_anual",
  "excesso_36m_anual", "alpha_anual", "tracking_error", "max_dd_rel_cdi",
  "es_5_mensal", "pior_21d_rel_cdi", "pior_63d_rel_cdi",
  "hit_rate_mensal", "hit_rate_12m", "captura_excesso_investidor",
  "custo_sobre_excesso_bruto", "overlap_medio_grupo", "overlap_max_grupo"
))
if (length(cols_pct) > 0) {
  openxlsx::addStyle(
    wb_out, "Ranking_novo", percentual,
    rows = 2:(nrow(ranking_novo) + 1), cols = cols_pct,
    gridExpand = TRUE, stack = TRUE
  )
}

openxlsx::saveWorkbook(wb_out, ARQ_SAIDA, overwrite = TRUE)

message("Pipeline concluído. Arquivo salvo em: ", normalizePath(ARQ_SAIDA))
