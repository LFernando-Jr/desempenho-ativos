# ETAPA 3 — MÉTRICAS E CORRELAÇÕES

# Limpa os objetos da sessão para evitar dependências de execuções anteriores.
rm(list = ls())

# ------------------------------------------------------------
# Setup
# ------------------------------------------------------------

# Caminhos dos insumos e das saídas desta etapa.
path_intermediate = "projects/criterios-selecao-fundos-cp/data/intermediate"
path_retornos_historico = file.path(path_intermediate, "fundos_retornos_historico.rds")
path_retornos_score = file.path(path_intermediate, "fundos_retornos_score_36m.rds")
path_universo = file.path(path_intermediate, "universo_elegibilidade_36m.rds")

# Quantidade esperada de meses na janela comum do score.
JANELA_SCORE_MESES = 36L

# Quantidade mínima de observações para validar um mês.
MIN_OBS_MES = 15L

paths_necessarios = c(path_retornos_historico, path_retornos_score, path_universo)
paths_ausentes = paths_necessarios[!file.exists(paths_necessarios)]

if (length(paths_ausentes) > 0) {
  stop(
    "Arquivos ausentes na Etapa 3: ",
    paste(paths_ausentes, collapse = ", "),
    ". Execute primeiro a Etapa 2."
  )
}

fundos_retornos_historico = read_rds(file = path_retornos_historico)
fundos_retornos_score = read_rds(file = path_retornos_score)
universo_elegibilidade = read_rds(file = path_universo)

# ------------------------------------------------------------
# Helpers
# ------------------------------------------------------------

# Compõe retornos diários e produz uma observação por fundo e mês.
agrega_mensal = function(base_retornos) {
  base_retornos %>%
    mutate(mes = floor_date(data, unit = "month")) %>%
    group_by(
      nome_xlsx,
      nome_curto,
      nome_plot,
      nome_quantum,
      taxa_adm_aa,
      mes
    ) %>%
    summarise(
      primeira_data = min(data),
      ultima_data = max(data),
      n_obs = n(),
      ret_fundo_m = prod(1 + ret_liq, na.rm = TRUE) - 1,
      ret_cdi_m = prod(1 + ret_cdi, na.rm = TRUE) - 1,
      ret_ida_di_m = if_else(
        all(is.na(ret_ida_di)),
        NA_real_,
        prod(1 + ret_ida_di, na.rm = TRUE) - 1
      ),
      ret_ida_liq_di_m = if_else(
        all(is.na(ret_ida_liq_di)),
        NA_real_,
        prod(1 + ret_ida_liq_di, na.rm = TRUE) - 1
      ),
      ret_irfm_1_m = if_else(
        all(is.na(ret_irfm_1)),
        NA_real_,
        prod(1 + ret_irfm_1, na.rm = TRUE) - 1
      ),
      .groups = "drop"
    ) %>%
    mutate(
      mes_completo = day(primeira_data) <= 7 &
        day(ultima_data) >= 24 &
        n_obs >= MIN_OBS_MES,
      excesso_cdi_m = (1 + ret_fundo_m) / (1 + ret_cdi_m) - 1
    ) %>%
    filter(mes_completo, is.finite(excesso_cdi_m)) %>%
    arrange(nome_plot, mes)
}

# Calcula o drawdown máximo do patrimônio relativo ao CDI e sua recuperação.
calcula_drawdown = function(retornos) {
  patrimonio_relativo = cumprod(1 + retornos)
  pico = cummax(patrimonio_relativo)
  drawdown = patrimonio_relativo / pico - 1
  indice_fundo = which.min(drawdown)
  nivel_pico_anterior = pico[indice_fundo]
  indice_recuperacao = which(
    seq_along(patrimonio_relativo) > indice_fundo &
      patrimonio_relativo >= nivel_pico_anterior
  )

  meses_recuperacao = if (length(indice_recuperacao) == 0) {
    NA_integer_
  } else {
    min(indice_recuperacao) - indice_fundo
  }

  tibble(
    max_drawdown_excesso = drawdown[indice_fundo],
    meses_para_recuperar = meses_recuperacao
  )
}

# Calcula autocorrelação de primeira ordem somente quando há variação suficiente.
autocor_lag1 = function(x) {
  valores = x[is.finite(x)]

  if (length(valores) < 4 || sd(valores) == 0) {
    return(NA_real_)
  }

  as.numeric(
    acf(
      x = valores,
      lag.max = 1,
      plot = FALSE,
      na.action = na.pass
    )$acf[2]
  )
}

# Calcula correlação somente quando existem pares e variação suficientes.
cor_segura = function(x, y) {
  completos = complete.cases(x, y)
  x_validos = x[completos]
  y_validos = y[completos]

  if (
    length(x_validos) < 6 ||
      sd(x_validos) == 0 ||
      sd(y_validos) == 0
  ) {
    return(NA_real_)
  }

  cor(x = x_validos, y = y_validos, method = "pearson")
}

# ------------------------------------------------------------
# Bases mensais
# ------------------------------------------------------------

fundos_mensais_historico = agrega_mensal(
  base_retornos = fundos_retornos_historico
)

fundos_mensais_score = agrega_mensal(
  base_retornos = fundos_retornos_score
)

checagem_meses = fundos_mensais_score %>%
  count(nome_plot, name = "n_meses") %>%
  filter(n_meses != JANELA_SCORE_MESES)

if (nrow(checagem_meses) > 0) {
  print(checagem_meses)
  stop("A base mensal do score não possui exatamente 36 meses para todos os fundos.")
}

message("[03] Todos os fundos do score possuem exatamente 36 meses completos.")

write_rds(
  x = fundos_mensais_historico,
  file = file.path(path_intermediate, "fundos_mensais_historico.rds")
)

write_rds(
  x = fundos_mensais_score,
  file = file.path(path_intermediate, "fundos_mensais_score_36m.rds")
)

# ------------------------------------------------------------
# Consistência e risco
# ------------------------------------------------------------

fundos_mensais_score = fundos_mensais_score %>%
  group_by(nome_xlsx, nome_plot, nome_quantum, taxa_adm_aa) %>%
  arrange(mes, .by_group = TRUE) %>%
  mutate(
    excesso_6m = slide_dbl(
      .x = excesso_cdi_m,
      .f = ~ prod(1 + .x) - 1,
      .before = 5,
      .complete = TRUE
    ),
    excesso_12m = slide_dbl(
      .x = excesso_cdi_m,
      .f = ~ prod(1 + .x) - 1,
      .before = 11,
      .complete = TRUE
    )
  ) %>%
  ungroup()

metricas_score = fundos_mensais_score %>%
  group_by(nome_xlsx, nome_plot, nome_quantum, taxa_adm_aa) %>%
  group_modify(
    .f = ~ {
      base_fundo = .x %>% arrange(mes)
      excesso = base_fundo$excesso_cdi_m
      excesso_6m = base_fundo$excesso_6m
      excesso_12m = base_fundo$excesso_12m
      n_meses = length(excesso)
      tres_piores = sort(excesso, na.last = NA)[seq_len(min(3, n_meses))]
      drawdown = calcula_drawdown(retornos = excesso)

      tibble(
        inicio_serie_mensal_score = min(base_fundo$mes),
        fim_serie_mensal_score = max(base_fundo$mes),
        n_meses_score = n_meses,
        excesso_cdi_aa = prod(1 + excesso)^(12 / n_meses) - 1,
        hit_rate_mensal = mean(excesso > 0),
        hit_rate_6m = mean(excesso_6m > 0, na.rm = TRUE),
        hit_rate_12m = mean(excesso_12m > 0, na.rm = TRUE),
        volatilidade_excesso_aa = sd(excesso) * sqrt(12),
        pior_mes = min(excesso),
        media_tres_piores_meses = mean(tres_piores),
        autocorrelacao_lag1 = autocor_lag1(x = excesso),
        max_drawdown_excesso = drawdown$max_drawdown_excesso,
        meses_para_recuperar = drawdown$meses_para_recuperar
      )
    }
  ) %>%
  ungroup()

# Preserva a persistência de 36 meses no histórico completo apenas como diagnóstico.
metricas_historicas_36m = fundos_mensais_historico %>%
  semi_join(metricas_score, by = "nome_plot") %>%
  group_by(nome_xlsx, nome_plot, nome_quantum, taxa_adm_aa) %>%
  arrange(mes, .by_group = TRUE) %>%
  mutate(
    excesso_36m_historico = slide_dbl(
      .x = excesso_cdi_m,
      .f = ~ prod(1 + .x) - 1,
      .before = JANELA_SCORE_MESES - 1L,
      .complete = TRUE
    )
  ) %>%
  summarise(
    inicio_historico_mensal = min(mes),
    fim_historico_mensal = max(mes),
    n_meses_historico = n(),
    n_janelas_36m_historico = sum(!is.na(excesso_36m_historico)),
    hit_rate_36m_historico = if_else(
      n_janelas_36m_historico > 0,
      mean(excesso_36m_historico > 0, na.rm = TRUE),
      NA_real_
    ),
    .groups = "drop"
  )

# ------------------------------------------------------------
# Correlações
# ------------------------------------------------------------

base_cor_wide = fundos_mensais_score %>%
  select(mes, nome_plot, excesso_cdi_m) %>%
  pivot_wider(
    names_from = nome_plot,
    values_from = excesso_cdi_m
  ) %>%
  arrange(mes)

matriz_excessos = base_cor_wide %>%
  select(-mes) %>%
  as.matrix()

matriz_cor = cor(
  x = matriz_excessos,
  use = "pairwise.complete.obs",
  method = "pearson"
)

matriz_obs = crossprod(!is.na(matriz_excessos))

if (anyNA(matriz_cor)) {
  warning("Há correlações indefinidas na matriz de excessos mensais.")
}

resumo_correlacao = map_dfr(
  .x = seq_len(nrow(matriz_cor)),
  .f = function(i) {
    nome_fundo = rownames(matriz_cor)[i]
    correlacoes = matriz_cor[i, ]
    observacoes = matriz_obs[i, ]
    manter = names(correlacoes) != nome_fundo & is.finite(correlacoes)
    correlacoes_validas = correlacoes[manter]
    observacoes_validas = observacoes[manter]

    if (length(correlacoes_validas) == 0) {
      return(
        tibble(
          nome_plot = nome_fundo,
          correlacao_media_pares = NA_real_,
          correlacao_mediana_pares = NA_real_,
          correlacao_maxima = NA_real_,
          fundo_mais_correlacionado = NA_character_,
          meses_em_comum_com_par_mais_proximo = NA_integer_
        )
      )
    }

    indice_max = which.max(correlacoes_validas)

    tibble(
      nome_plot = nome_fundo,
      correlacao_media_pares = mean(correlacoes_validas),
      correlacao_mediana_pares = median(correlacoes_validas),
      correlacao_maxima = correlacoes_validas[indice_max],
      fundo_mais_correlacionado = names(correlacoes_validas)[indice_max],
      meses_em_comum_com_par_mais_proximo = as.integer(
        observacoes_validas[indice_max]
      )
    )
  }
)

write_rds(
  x = matriz_cor,
  file = file.path(path_intermediate, "matriz_correlacao_excessos_36m.rds")
)

write_rds(
  x = matriz_obs,
  file = file.path(path_intermediate, "matriz_observacoes_correlacao_36m.rds")
)

write_excel_csv2(
  x = matriz_cor %>% as.data.frame() %>% rownames_to_column("nome_plot"),
  file = file.path(path_intermediate, "matriz_correlacao_excessos_mensais_36m.csv")
)

# ------------------------------------------------------------
# Afinidade com benchmarks
# ------------------------------------------------------------

base_bench_long = fundos_mensais_score %>%
  select(
    nome_xlsx,
    nome_plot,
    nome_quantum,
    taxa_adm_aa,
    mes,
    ret_fundo_m,
    ret_cdi_m,
    ret_ida_di_m,
    ret_ida_liq_di_m,
    ret_irfm_1_m,
    excesso_cdi_m
  ) %>%
  pivot_longer(
    cols = c(ret_ida_di_m, ret_ida_liq_di_m, ret_irfm_1_m),
    names_to = "benchmark",
    values_to = "ret_benchmark_m"
  ) %>%
  mutate(
    benchmark = recode(
      benchmark,
      "ret_ida_di_m" = "IDA-DI",
      "ret_ida_liq_di_m" = "IDA LIQ-DI",
      "ret_irfm_1_m" = "IRF-M 1"
    ),
    excesso_benchmark_cdi_m = (1 + ret_benchmark_m) / (1 + ret_cdi_m) - 1,
    retorno_relativo_benchmark_m = (1 + ret_fundo_m) /
      (1 + ret_benchmark_m) - 1
  ) %>%
  filter(
    is.finite(excesso_benchmark_cdi_m),
    is.finite(retorno_relativo_benchmark_m)
  )

afinidade_benchmarks = base_bench_long %>%
  group_by(nome_xlsx, nome_plot, nome_quantum, taxa_adm_aa, benchmark) %>%
  summarise(
    n_meses = n(),
    correlacao = cor_segura(
      x = excesso_cdi_m,
      y = excesso_benchmark_cdi_m
    ),
    tracking_error_aa = sd(retorno_relativo_benchmark_m) * sqrt(12),
    excesso_benchmark_aa = prod(1 + retorno_relativo_benchmark_m)^(12 / n_meses) - 1,
    hit_rate_mensal = mean(retorno_relativo_benchmark_m > 0),
    .groups = "drop"
  )

resumo_benchmark_proximo = afinidade_benchmarks %>%
  filter(is.finite(tracking_error_aa), is.finite(correlacao)) %>%
  group_by(nome_xlsx, nome_plot, nome_quantum, taxa_adm_aa) %>%
  summarise(
    benchmark_menor_tracking_error = benchmark[which.min(tracking_error_aa)],
    menor_tracking_error_aa = min(tracking_error_aa),
    benchmark_maior_correlacao = benchmark[which.max(correlacao)],
    maior_correlacao = max(correlacao),
    .groups = "drop"
  )

write_rds(
  x = afinidade_benchmarks,
  file = file.path(path_intermediate, "afinidade_benchmarks_36m.rds")
)

write_excel_csv2(
  x = afinidade_benchmarks,
  file = file.path(path_intermediate, "afinidade_benchmarks_36m.csv")
)

# ------------------------------------------------------------
# Base consolidada
# ------------------------------------------------------------

status_historico = fundos_retornos_historico %>%
  group_by(nome_xlsx, nome_plot, nome_quantum, taxa_adm_aa) %>%
  summarise(
    inicio_serie_diaria = min(data),
    fim_serie_diaria = max(data),
    n_retornos_diarios = n(),
    .groups = "drop"
  )

metricas_todos_fundos = universo_elegibilidade %>%
  left_join(
    status_historico,
    by = c("nome_xlsx", "nome_plot", "nome_quantum", "taxa_adm_aa"),
    relationship = "one-to-one"
  ) %>%
  left_join(
    metricas_score,
    by = c("nome_xlsx", "nome_plot", "nome_quantum", "taxa_adm_aa"),
    relationship = "one-to-one"
  ) %>%
  left_join(
    metricas_historicas_36m,
    by = c("nome_xlsx", "nome_plot", "nome_quantum", "taxa_adm_aa"),
    relationship = "one-to-one"
  ) %>%
  left_join(
    resumo_correlacao,
    by = "nome_plot",
    relationship = "one-to-one"
  ) %>%
  left_join(
    resumo_benchmark_proximo,
    by = c("nome_xlsx", "nome_plot", "nome_quantum", "taxa_adm_aa"),
    relationship = "one-to-one"
  ) %>%
  mutate(
    elegivel_ranking = elegivel_score_36m &
      n_meses_score == JANELA_SCORE_MESES &
      is.finite(excesso_cdi_aa) &
      is.finite(hit_rate_mensal) &
      is.finite(hit_rate_6m) &
      is.finite(hit_rate_12m) &
      is.finite(max_drawdown_excesso)
  ) %>%
  arrange(desc(elegivel_ranking), desc(excesso_cdi_aa), nome_plot)

write_rds(
  x = metricas_todos_fundos,
  file = file.path(path_intermediate, "metricas_todos_fundos_36m.rds")
)

write_excel_csv2(
  x = metricas_todos_fundos,
  file = file.path(path_intermediate, "metricas_todos_fundos_36m.csv")
)

message("[03] Métricas de 36 meses, histórico e correlações concluídos.")
