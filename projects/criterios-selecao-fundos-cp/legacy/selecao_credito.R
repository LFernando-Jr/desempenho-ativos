# =========================================================================
# Seleção quantitativa de fundos de crédito privado — v2
# -------------------------------------------------------------------------
# Mudanças vs v1:
#
# [dados] CDI coletado direto do SGS/BCB (série 12), com cache incremental.
#         gauge.xlsx passa a ser necessário só para IDA-DI / IDEX-CDI.
#
# [fix]   Sortino: downside deviation dividia pelo nº de obs. negativas
#           em vez do N total -> inflava o índice, e mais no fundo mais suave.
#         %gauge: era média de razões mensais -> vira razão dos acumulados.
#         Thresholds de cauda: eram bps DIÁRIOS de magnitude impossível
#           em high grade -> viram bps mensais.
#         excess_1M: era janela móvel de 21du lida no fim do mês; compor
#           isso gera overlap/buraco (meses têm 19-23 du) -> retorno
#           mensal-calendário vira objeto de primeira classe.
#         Guarda de histórico mínimo em todas as métricas comparativas.
#
# [novo]  Up/down capture e assimetria vs benchmark de crédito.
#         Beta condicional (abertura vs fechamento de spread) + alfa com t.
#         Ranking em janelas de estresse vs ranking de retorno acumulado.
#         Autocorrelação de 1ª ordem e vol dessuavizada (Geltner).
#
# NOTA: script não foi executado contra a API do BCB neste ambiente
#       (rede restrita). Rodar `get_cdi(forcar = TRUE)` isolado antes.
# =========================================================================


# Pacotes -----------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(PerformanceAnalytics)
library(zoo)
library(httr2)
library(jsonlite)
library(writexl)


# Setup -------------------------------------------------------------------

rm(list = ls())

invisible(suppressWarnings(
  Sys.setlocale(
    "LC_TIME",
    if (.Platform$OS.type == "windows") "Portuguese_Brazil.1252" else "pt_BR.UTF-8"
  )
))

PARAMS <- list(
  janelas_dias        = c(`1M` = 21, `12M` = 252, `36M` = 756),
  janela_hit_rate     = 36,          # meses, rolling
  janela_analise_anos = 3,
  cvar_p              = 0.95,
  
  min_meses_hist      = 24,          # histórico mínimo p/ entrar no ranking
  min_meses_neg       = 3,           # mínimo de meses negativos p/ down capture
  n_meses_estresse    = 6,           # piores meses do índice de crédito
  
  bench_caixa         = "CDI",       # benchmark de custo de oportunidade
  bench_credito       = "IDA-DI",    # benchmark de beta de crédito
  
  sgs_cdi             = 12,          # SGS: CDI, % a.d.
  data_inicio         = as.Date("2010-01-01"),
  cache_dir           = "cache",
  
  arq_fundos          = "sample_funds.xlsx",
  arq_gauge           = "gauge.xlsx",
  arq_saida           = "selecao_credito_resumo.xlsx"
)


# =========================================================================
# 0. COLETA DO CDI — SGS / BANCO CENTRAL -----------------------------------
# =========================================================================
# Série 12 = CDI em % ao dia (não anualizado). O índice é reconstituído por
# cumprod(1 + valor/100). Base arbitrária: só usamos variações.
#
# A API recusa janelas longas em série diária, então busca em blocos.
# Endpoint público, sem autenticação.

sgs_fetch <- function(codigo, inicio, fim) {
  url <- sprintf("https://api.bcb.gov.br/dados/serie/bcdata.sgs.%d/dados", codigo)
  
  txt <- request(url) |>
    req_url_query(
      formato     = "json",
      dataInicial = format(inicio, "%d/%m/%Y"),
      dataFinal   = format(fim,    "%d/%m/%Y")
    ) |>
    req_timeout(30) |>
    req_retry(max_tries = 4, backoff = \(i) 2^i) |>
    req_perform() |>
    resp_body_string()
  
  jsonlite::fromJSON(txt) |>
    as_tibble() |>
    transmute(
      date  = as.Date(data, format = "%d/%m/%Y"),
      valor = as.numeric(valor)
    )
}

sgs_get <- function(codigo, inicio, fim = Sys.Date(), anos_bloco = 9) {
  ini <- seq(from = inicio, to = fim, by = sprintf("%d years", anos_bloco))
  fin <- c(ini[-1] - 1, fim)
  
  map2(ini, fin, \(a, b) sgs_fetch(codigo, a, b)) |>
    list_rbind() |>
    filter(!is.na(date), !is.na(valor)) |>
    distinct(date, .keep_all = TRUE) |>
    arrange(date)
}

get_cdi <- function(inicio = PARAMS$data_inicio,
                    fim    = Sys.Date(),
                    forcar = FALSE) {
  
  dir.create(PARAMS$cache_dir, showWarnings = FALSE, recursive = TRUE)
  cache <- file.path(PARAMS$cache_dir, "cdi_sgs12.rds")
  
  if (!forcar && file.exists(cache)) {
    cdi <- readRDS(cache)
    # tolerância p/ fim de semana e feriado
    if (max(cdi$date) >= fim - 5) return(cdi)
    novo <- sgs_get(PARAMS$sgs_cdi, max(cdi$date) + 1, fim)
    cdi  <- bind_rows(cdi, novo) |>
      distinct(date, .keep_all = TRUE) |>
      arrange(date)
  } else {
    cdi <- sgs_get(PARAMS$sgs_cdi, inicio, fim)
  }
  
  saveRDS(cdi, cache)
  cdi
}

cdi_serie <- function(...) {
  get_cdi(...) |>
    transmute(
      name  = PARAMS$bench_caixa,
      date,
      value = cumprod(1 + valor / 100)
    )
}


# =========================================================================
# 1. CARGA -----------------------------------------------------------------
# =========================================================================

ler_xlsx_cotas <- function(arq) {
  readxl::read_excel(arq, sheet = 1) |>
    `colnames<-`(c("name", "Data", "value")) |>
    mutate(
      date = as.Date(Data, format = "%d/%m/%Y"),
      .before = "name",
      .keep = "unused"
    )
}

fund <- ler_xlsx_cotas(PARAMS$arq_fundos)

# Benchmarks: CDI pela API; o resto (IDA-DI, IDEX) continua no xlsx.
# O CDI eventualmente presente no xlsx é descartado — a API sempre vence.
gauge_xlsx <- if (file.exists(PARAMS$arq_gauge)) {
  ler_xlsx_cotas(PARAMS$arq_gauge) |> filter(name != PARAMS$bench_caixa)
} else {
  tibble(date = as.Date(character()), name = character(), value = numeric())
}

gauge_api <- tryCatch(
  cdi_serie(),
  error = function(e) {
    warning("Falha no SGS/BCB (", conditionMessage(e),
            "). Usando CDI do gauge.xlsx.", call. = FALSE)
    if (file.exists(PARAMS$arq_gauge)) {
      ler_xlsx_cotas(PARAMS$arq_gauge) |> filter(name == PARAMS$bench_caixa)
    } else {
      stop("Sem CDI: API indisponível e gauge.xlsx ausente.", call. = FALSE)
    }
  }
)

gauge      <- bind_rows(gauge_api, gauge_xlsx)
benchmarks <- unique(gauge$name)

message("Benchmarks carregados: ", paste(benchmarks, collapse = ", "))
message("CDI até ", format(max(gauge_api$date), "%d/%m/%Y"))

if (!PARAMS$bench_credito %in% benchmarks) {
  warning("Benchmark de crédito '", PARAMS$bench_credito, "' ausente. ",
          "Captura assimétrica, beta condicional e teste de estresse ",
          "não serão calculados.", call. = FALSE)
}


# =========================================================================
# 2. RETORNOS --------------------------------------------------------------
# =========================================================================

raw <- bind_rows(fund, gauge) |>
  filter(!is.na(date), !is.na(value)) |>
  distinct(name, date, .keep_all = TRUE) |>
  arrange(name, date)

ret_d <- raw |>
  group_by(name) |>
  mutate(ret_d = value / lag(value) - 1) |>
  ungroup() |>
  filter(!is.na(ret_d))

# Retorno mensal-calendário (objeto de primeira classe).
# Meses incompletos nas pontas são removidos: o primeiro mês de cada série
# não tem o retorno do 1º dia, e o mês corrente ainda está aberto.
ret_m <- ret_d |>
  mutate(mes = floor_date(date, "month")) |>
  group_by(name, mes) |>
  summarise(
    ret_m = prod(1 + ret_d) - 1,
    n_du  = n(),
    .groups = "drop"
  ) |>
  group_by(name) |>
  filter(mes > min(mes), mes < floor_date(Sys.Date(), "month")) |>
  ungroup()

# Janelas móveis diárias (mantidas para os gráficos de série temporal)
ret_rolling <- ret_d |>
  group_by(name) |>
  mutate(
    across(
      ret_d,
      list(
        acum_1M  = \(x) rollapplyr(1 + x, PARAMS$janelas_dias["1M"],  prod, fill = NA) - 1,
        acum_12M = \(x) rollapplyr(1 + x, PARAMS$janelas_dias["12M"], prod, fill = NA) - 1,
        acum_36M = \(x) rollapplyr(1 + x, PARAMS$janelas_dias["36M"], prod, fill = NA) - 1
      ),
      .names = "{.fn}"
    )
  ) |>
  ungroup()


# Painel diário fundo x benchmark (para caudas e distribuição) ------------
painel_d <- ret_rolling |>
  filter(!name %in% benchmarks) |>
  select(date, fund = name, fund_ret_d = ret_d,
         fund_1M = acum_1M, fund_12M = acum_12M, fund_36M = acum_36M) |>
  inner_join(
    ret_rolling |>
      filter(name %in% benchmarks) |>
      select(date, gauge = name, gauge_ret_d = ret_d,
             gauge_1M = acum_1M, gauge_12M = acum_12M, gauge_36M = acum_36M),
    by = "date", relationship = "many-to-many"
  ) |>
  mutate(
    excess_d   = (1 + fund_ret_d) / (1 + gauge_ret_d) - 1,
    excess_12M = (1 + fund_12M)   / (1 + gauge_12M)   - 1,
    excess_36M = (1 + fund_36M)   / (1 + gauge_36M)   - 1
  ) |>
  arrange(fund, gauge, date)

# Painel mensal fundo x benchmark (base de quase tudo daqui pra frente) ---
painel_m <- ret_m |>
  filter(!name %in% benchmarks) |>
  select(mes, fund = name, r_fund = ret_m) |>
  inner_join(
    ret_m |> filter(name %in% benchmarks) |> select(mes, gauge = name, r_gauge = ret_m),
    by = "mes", relationship = "many-to-many"
  ) |>
  mutate(excess_m = (1 + r_fund) / (1 + r_gauge) - 1) |>
  arrange(fund, gauge, mes)

# Universo elegível: histórico mínimo. Sem isso, fundo novo que não
# atravessou nenhum evento de crédito lidera todos os rankings.
elegiveis <- painel_m |>
  count(fund, gauge, name = "n_meses") |>
  filter(n_meses >= PARAMS$min_meses_hist)

painel_m <- painel_m |> semi_join(elegiveis, by = c("fund", "gauge"))

descartados <- painel_m |>
  distinct(fund) |>
  anti_join(ret_m |> distinct(fund = name) |> filter(!fund %in% benchmarks), by = "fund")

message("Fundos elegíveis (>= ", PARAMS$min_meses_hist, " meses): ",
        n_distinct(painel_m$fund))


# =========================================================================
# 3. HIT RATE MENSAL -------------------------------------------------------
# =========================================================================
# Diário é inútil em crédito: o acrual de carry faz o hit rate diário
# convergir para ~100% independente da qualidade do gestor.

hit_rate <- painel_m |>
  group_by(fund, gauge) |>
  arrange(mes, .by_group = TRUE) |>
  mutate(
    pos_m       = as.integer(excess_m > 0),
    hit_total   = cummean(pos_m),
    hit_rolling = rollapplyr(pos_m, PARAMS$janela_hit_rate, mean, partial = TRUE)
  ) |>
  ungroup() |>
  select(fund, gauge, mes, hit_total, hit_rolling)


# =========================================================================
# 4. CAPTURA ASSIMÉTRICA ---------------------------------------------------
# =========================================================================
# A métrica central. Contra o CDI é degenerada (nunca há mês negativo),
# então só faz sentido contra o benchmark de crédito.
#
# up_capture ~ down_capture -> índice com taxa.
# down_capture > up_capture -> assimetria ruim: entrega beta na queda e
#                              não acompanha na recuperação.

acum <- function(x) prod(1 + x) - 1

captura <- painel_m |>
  group_by(fund, gauge) |>
  summarise(
    n_meses     = n(),
    n_meses_neg = sum(r_gauge < 0),
    up_capture = {
      i <- r_gauge > 0
      if (sum(i) < PARAMS$min_meses_neg) NA_real_ else acum(r_fund[i]) / acum(r_gauge[i])
    },
    down_capture = {
      i <- r_gauge < 0
      if (sum(i) < PARAMS$min_meses_neg) NA_real_ else acum(r_fund[i]) / acum(r_gauge[i])
    },
    .groups = "drop"
  ) |>
  mutate(assimetria = up_capture - down_capture)


# =========================================================================
# 5. BETA CONDICIONAL E ALFA -----------------------------------------------
# =========================================================================
# r_fund = a + b_up * r_bench + b_extra * r_bench * 1{r_bench < 0}
# b_dn = b_up + b_extra. Alfa em base mensal, anualizado, com t-stat.

beta_condicional <- painel_m |>
  filter(gauge == PARAMS$bench_credito) |>
  group_by(fund) |>
  group_modify(~ {
    d <- .x |> mutate(neg = as.integer(r_gauge < 0))
    if (sum(d$neg) < 6 || sum(1 - d$neg) < 6) {
      return(tibble(alpha_m = NA_real_, alpha_t = NA_real_,
                    beta_up = NA_real_, beta_dn = NA_real_, r2 = NA_real_))
    }
    fit <- lm(r_fund ~ r_gauge + r_gauge:neg, data = d)
    cf  <- coef(fit)
    tibble(
      alpha_m = unname(cf[1]),
      alpha_t = summary(fit)$coefficients[1, 3],
      beta_up = unname(cf[2]),
      beta_dn = unname(cf[2] + coalesce(cf[3], 0)),
      r2      = summary(fit)$r.squared
    )
  }) |>
  ungroup() |>
  mutate(alpha_anual = (1 + alpha_m)^12 - 1)


# =========================================================================
# 6. JANELAS DE ESTRESSE ---------------------------------------------------
# =========================================================================
# Ranqueia os fundos usando SÓ os N piores meses do índice de crédito, e
# compara com o ranking por retorno acumulado. A divergência entre os dois
# rankings é o resultado — raramente coincidem, e o de estresse é o que
# tende a se repetir no próximo evento.

meses_estresse <- ret_m |>
  filter(name == PARAMS$bench_credito) |>
  slice_min(ret_m, n = PARAMS$n_meses_estresse, with_ties = FALSE) |>
  arrange(mes) |>
  pull(mes)

message("Meses de estresse: ",
        paste(format(meses_estresse, "%b/%y"), collapse = ", "))

estresse <- painel_m |>
  filter(gauge == PARAMS$bench_credito, mes %in% meses_estresse) |>
  group_by(fund) |>
  summarise(
    n_estresse      = n(),
    ret_estresse    = acum(r_fund),
    excess_estresse = acum(excess_m),
    pior_mes        = min(r_fund),
    .groups = "drop"
  ) |>
  # só ranqueia quem esteve presente na maioria dos episódios
  filter(n_estresse >= ceiling(PARAMS$n_meses_estresse / 2)) |>
  arrange(desc(ret_estresse)) |>
  mutate(rank_estresse = row_number())


# =========================================================================
# 7. ILIQUIDEZ: AUTOCORRELAÇÃO E VOL DESSUAVIZADA --------------------------
# =========================================================================
# rho1 > 0 em série de cota significa retorno parcialmente carregado para o
# período seguinte — assinatura de ativo que não negocia, marcado por curva.
# Não é diagnóstico de conduta do gestor: aparece igual em casa com política
# de MtM impecável carregando papel ilíquido. É medida de iliquidez do ativo.
#
# Geltner: r*_t = (r_t - rho * r_{t-1}) / (1 - rho)
# A vol dessuavizada é o denominador correto de qualquer razão risco-retorno.

rho1 <- function(r) {
  r <- r[!is.na(r)]
  if (length(r) < PARAMS$min_meses_hist) return(NA_real_)
  cor(r[-1], head(r, -1))
}

vol_dessuavizada <- function(r, freq = 12) {
  r <- r[!is.na(r)]
  if (length(r) < PARAMS$min_meses_hist) return(NA_real_)
  rho <- cor(r[-1], head(r, -1))
  if (is.na(rho) || rho <= 0 || rho >= 0.95) return(sd(r) * sqrt(freq))
  sd((r[-1] - rho * head(r, -1)) / (1 - rho)) * sqrt(freq)
}

iliquidez <- ret_m |>
  filter(!name %in% benchmarks) |>
  group_by(fund = name) |>
  arrange(mes, .by_group = TRUE) |>
  summarise(
    n_meses        = n(),
    rho1           = rho1(ret_m),
    vol_obs        = sd(ret_m) * sqrt(12),
    vol_unsmooth   = vol_dessuavizada(ret_m),
    .groups = "drop"
  ) |>
  mutate(fator_iliquidez = vol_unsmooth / vol_obs) |>
  filter(n_meses >= PARAMS$min_meses_hist)

# Razão vol mensal / vol diária escalada. Mesma informação por outra via:
# se > 1, a vol diária escalada por sqrt(21) subestima a vol verdadeira.
vol_freq <- ret_d |>
  filter(!name %in% benchmarks, date >= max(date) %m-% years(PARAMS$janela_analise_anos)) |>
  group_by(fund = name) |>
  summarise(vol_d_anual = sd(ret_d) * sqrt(252), .groups = "drop") |>
  left_join(
    ret_m |>
      filter(!name %in% benchmarks, mes >= max(mes) %m-% years(PARAMS$janela_analise_anos)) |>
      group_by(fund = name) |>
      summarise(vol_m_anual = sd(ret_m) * sqrt(12), .groups = "drop"),
    by = "fund"
  ) |>
  mutate(razao_vol_m_d = vol_m_anual / vol_d_anual)


# =========================================================================
# 8. RISCO: DRAWDOWN, RECOVERY, CVaR ---------------------------------------
# =========================================================================

dd_metrics <- ret_d |>
  filter(!name %in% benchmarks) |>
  group_by(fund = name) |>
  arrange(date, .by_group = TRUE) |>
  summarise(
    max_drawdown = as.numeric(maxDrawdown(ret_d)),
    avg_drawdown = as.numeric(AverageDrawdown(ret_d)),
    .groups = "drop"
  )

# Episódios: profundidade, duração e tempo até recuperar o HWM.
# Recovery NA = ainda submerso — informação, não erro.
dd_episodes <- ret_d |>
  filter(!name %in% benchmarks) |>
  group_by(fund = name) |>
  arrange(date, .by_group = TRUE) |>
  group_modify(~ {
    x   <- xts::xts(.x$ret_d, order.by = .x$date)
    tab <- try(table.Drawdowns(x, top = 5), silent = TRUE)
    if (inherits(tab, "try-error")) return(tibble())
    as_tibble(tab) |> janitor::clean_names()
  }) |>
  ungroup()

recovery <- dd_episodes |>
  group_by(fund) |>
  summarise(
    dd_mais_profundo   = min(depth, na.rm = TRUE),
    recovery_du        = recovery[which.min(depth)],
    ainda_submerso     = is.na(recovery[which.min(depth)]),
    .groups = "drop"
  )

es_metrics <- ret_d |>
  filter(!name %in% benchmarks,
         date >= max(date) %m-% years(PARAMS$janela_analise_anos)) |>
  group_by(fund = name) |>
  summarise(
    var_95 = -as.numeric(VaR(ret_d, p = PARAMS$cvar_p, method = "historical")),
    es_95  = -as.numeric(ES( ret_d, p = PARAMS$cvar_p, method = "historical")),
    .groups = "drop"
  )


# =========================================================================
# 9. FORMA DA DISTRIBUIÇÃO E CAUDAS ----------------------------------------
# =========================================================================
# Thresholds em bps MENSAIS. Em base diária os cortes da v1 (-25/-50/-100bps)
# retornavam zero para todo high grade — três colunas inúteis.
#
# Skew >= 0 em fundo de crédito é sinal amarelo: a distribuição natural da
# classe é carry positivo constante com eventos negativos raros.

forma <- painel_m |>
  filter(mes >= max(mes) %m-% years(PARAMS$janela_analise_anos)) |>
  group_by(fund, gauge) |>
  summarise(
    mediana_excess   = median(excess_m),
    skew_excess      = as.numeric(skewness(excess_m)),
    kurt_excess      = as.numeric(kurtosis(excess_m, method = "excess")),
    pior_excess_m    = min(excess_m),
    p_excess_pos     = mean(excess_m > 0),
    p_excess_lt_10bp = mean(excess_m < -0.0010),
    p_excess_lt_25bp = mean(excess_m < -0.0025),
    p_excess_lt_50bp = mean(excess_m < -0.0050),
    .groups = "drop"
  )


# =========================================================================
# 10. PERFORMANCE E SORTINO ------------------------------------------------
# =========================================================================

# Downside deviation com N cheio no denominador. A v1 dividia pelo número
# de observações negativas, o que inflava o Sortino — e inflava mais no
# fundo com menos meses negativos, isto é, no mais suavizado.
downside_dev <- function(excess, freq = 12) {
  excess <- excess[!is.na(excess)]
  sqrt(sum(pmin(excess, 0)^2) / length(excess)) * sqrt(freq)
}

performance <- painel_m |>
  filter(mes >= max(mes) %m-% years(PARAMS$janela_analise_anos)) |>
  group_by(fund, gauge) |>
  summarise(
    n_meses          = n(),
    ret_anual        = (1 + acum(r_fund))^(12 / n()) - 1,
    excess_anual     = (1 + acum(excess_m))^(12 / n()) - 1,
    # razão de acumulados, não média de razões
    pct_gauge        = acum(r_fund) / acum(r_gauge),
    tracking_error   = sd(excess_m) * sqrt(12),
    sortino          = ((1 + acum(excess_m))^(12 / n()) - 1) / downside_dev(excess_m),
    .groups = "drop"
  ) |>
  mutate(info_ratio = excess_anual / tracking_error)

# Sortino recalculado sobre a vol dessuavizada: corrige o fundo que parece
# eficiente só porque marca devagar.
performance <- performance |>
  left_join(iliquidez |> select(fund, fator_iliquidez), by = "fund") |>
  mutate(sortino_ajustado = sortino / coalesce(fator_iliquidez, 1))


# =========================================================================
# CONSOLIDAÇÃO -------------------------------------------------------------
# =========================================================================

ultimo_hit <- hit_rate |>
  group_by(fund, gauge) |>
  filter(mes == max(mes)) |>
  ungroup() |>
  select(fund, gauge, hit_total, hit_rolling)

resumo <- performance |>
  left_join(forma,            by = c("fund", "gauge")) |>
  left_join(captura |> select(-n_meses), by = c("fund", "gauge")) |>
  left_join(ultimo_hit,       by = c("fund", "gauge")) |>
  left_join(beta_condicional, by = "fund") |>
  left_join(iliquidez |> select(-n_meses), by = "fund") |>
  left_join(vol_freq,         by = "fund") |>
  left_join(dd_metrics,       by = "fund") |>
  left_join(recovery,         by = "fund") |>
  left_join(es_metrics,       by = "fund") |>
  left_join(estresse,         by = "fund") |>
  arrange(gauge, desc(excess_anual))

# Teste da divergência de rankings: quem lidera em retorno acumulado
# raramente lidera em estresse. A diferença é o que interessa.
divergencia <- resumo |>
  filter(gauge == PARAMS$bench_credito, !is.na(rank_estresse)) |>
  mutate(rank_retorno = rank(-excess_anual, ties.method = "first")) |>
  select(fund, rank_retorno, rank_estresse, excess_anual, ret_estresse,
         up_capture, down_capture, assimetria) |>
  mutate(delta_rank = rank_estresse - rank_retorno) |>
  arrange(desc(delta_rank))

write_xlsx(
  list(
    resumo      = resumo,
    divergencia = divergencia,
    captura     = captura,
    beta        = beta_condicional,
    estresse    = estresse,
    iliquidez   = iliquidez |> left_join(vol_freq, by = "fund"),
    drawdowns   = dd_episodes,
    hit_rate    = ultimo_hit
  ),
  PARAMS$arq_saida
)

message("Saída: ", PARAMS$arq_saida)


# =========================================================================
# GRÁFICOS -----------------------------------------------------------------
# =========================================================================

theme_capri <- theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    axis.line        = element_line(colour = "black"),
    legend.position  = "bottom",
    legend.title     = element_blank(),
    axis.title       = element_blank(),
    strip.background = element_blank()
  )

cap <- "Elaboração: Capri Family Office"

## G1: excesso 12M vs CDI --------------------------------------------------
painel_d |>
  filter(gauge == PARAMS$bench_caixa, !is.na(excess_12M)) |>
  ggplot(aes(date, excess_12M)) +
  geom_line() +
  geom_hline(yintercept = 0, linewidth = .25) +
  facet_wrap(~fund, scales = "free_y") +
  scale_y_continuous(labels = scales::percent) +
  theme_capri +
  labs(title = "Excesso de retorno 12M vs CDI", caption = cap)

## G2: hit rate mensal rolling --------------------------------------------
hit_rate |>
  filter(gauge == PARAMS$bench_credito) |>
  ggplot(aes(mes, hit_rolling)) +
  geom_line() +
  geom_hline(yintercept = .5, linetype = "dashed") +
  facet_wrap(~fund) +
  scale_y_continuous(labels = scales::percent) +
  theme_capri +
  labs(title = paste0("Hit rate mensal rolling ", PARAMS$janela_hit_rate,
                      "M vs ", PARAMS$bench_credito), caption = cap)

## G3: captura assimétrica — o gráfico principal ---------------------------
# Diagonal = simetria. O que se procura está acima dela.
captura |>
  filter(gauge == PARAMS$bench_credito, !is.na(down_capture)) |>
  ggplot(aes(down_capture, up_capture, label = fund)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_point(size = 3, alpha = .8) +
  ggrepel::geom_text_repel(size = 3, max.overlaps = 20) +
  theme_capri +
  labs(
    title    = paste0("Captura assimétrica vs ", PARAMS$bench_credito),
    subtitle = "Acima da diagonal: captura mais na alta do que na baixa",
    x = "Down capture", y = "Up capture", caption = cap
  ) +
  theme(axis.title = element_text())

## G4: divergência de rankings --------------------------------------------
divergencia |>
  ggplot(aes(rank_retorno, rank_estresse, label = fund)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_point(size = 3, alpha = .8) +
  ggrepel::geom_text_repel(size = 3, max.overlaps = 20) +
  scale_x_reverse() + scale_y_reverse() +
  theme_capri +
  labs(
    title    = "Ranking por retorno acumulado x ranking em estresse",
    subtitle = paste0("Piores ", PARAMS$n_meses_estresse, " meses do ",
                      PARAMS$bench_credito),
    x = "Rank retorno", y = "Rank estresse", caption = cap
  ) +
  theme(axis.title = element_text())

## G5: iliquidez ----------------------------------------------------------
iliquidez |>
  ggplot(aes(rho1, fator_iliquidez, label = fund)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(size = 3, alpha = .8) +
  ggrepel::geom_text_repel(size = 3, max.overlaps = 20) +
  theme_capri +
  labs(
    title    = "Autocorrelação de 1ª ordem x fator de dessuavização",
    subtitle = "Iliquidez do ativo carregado, não conduta do gestor",
    x = "rho(1) mensal", y = "vol dessuavizada / vol observada", caption = cap
  ) +
  theme(axis.title = element_text())

## G6: underwater ---------------------------------------------------------
ret_d |>
  filter(!name %in% benchmarks) |>
  group_by(name) |>
  arrange(date, .by_group = TRUE) |>
  mutate(dd = cumprod(1 + ret_d) / cummax(cumprod(1 + ret_d)) - 1) |>
  ungroup() |>
  ggplot(aes(date, dd)) +
  geom_area(alpha = .6) +
  facet_wrap(~name, scales = "free_x") +
  scale_y_continuous(labels = scales::percent) +
  theme_capri +
  labs(title = "Drawdown (underwater)", caption = cap)