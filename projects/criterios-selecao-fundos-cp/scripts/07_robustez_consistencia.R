# Diagnóstico isolado: não modifica score, ranking ou outputs canônicos.
# Execute depois das etapas 2 a 5, a partir da raiz desempenho-ativos.
rm(list = ls())

path_projeto = "projects/criterios-selecao-fundos-cp"
path_intermediate = file.path(path_projeto, "data", "intermediate")
path_diagnostico = file.path(path_projeto, "robustez")
dir.create(path_diagnostico, recursive = TRUE, showWarnings = FALSE)

oficial = read_rds(file.path(path_intermediate, "priorizacao_qualitativa_36m.rds"))
historico = read_rds(file.path(path_intermediate, "fundos_mensais_historico.rds"))
mes_final = max(oficial$fim_serie_mensal_score)

# Usa os 47 fundos oficiais como universo de partida. A calibração robusta
# (mediana e MAD) permanece a da janela oficial de 36m, evitando confundir
# mudança da janela com mudança da escala ou da composição da amostra.
nota_na_escala_oficial = function(x, referencia) {
  centro = median(referencia)
  escala = mad(referencia, center = centro, constant = 1.4826)
  if (!is.finite(escala) || escala <= .Machine$double.eps) {
    escala = IQR(referencia) / 1.349
  }
  if (!is.finite(escala) || escala <= .Machine$double.eps) {
    escala = sd(referencia)
  }
  if (!is.finite(escala) || escala <= .Machine$double.eps) {
    return(rep(50, length(x)))
  }
  z = pmin(pmax((x - centro) / escala, -4), 4)
  100 / (1 + exp(-z))
}

calcula_horizonte = function(n_meses) {
  mes_inicio = mes_final %m-% months(n_meses - 1L)
  base = historico %>%
    semi_join(oficial %>% distinct(nome_plot), by = "nome_plot") %>%
    filter(mes >= mes_inicio, mes <= mes_final, mes_completo,
      is.finite(excesso_cdi_m)) %>%
    group_by(nome_plot) %>%
    filter(n_distinct(mes) == n_meses,
      min(mes) == mes_inicio, max(mes) == mes_final) %>%
    arrange(mes, .by_group = TRUE) %>%
    mutate(
      excesso_6m = slider::slide_dbl(excesso_cdi_m,
        ~ prod(1 + .x) - 1, .before = 5, .complete = TRUE),
      excesso_12m = slider::slide_dbl(excesso_cdi_m,
        ~ prod(1 + .x) - 1, .before = 11, .complete = TRUE)
    ) %>% ungroup()

  metr = base %>% group_by(nome_plot) %>% summarise(
    n_meses_observados = n(),
    n_janelas_6m = sum(is.finite(excesso_6m)),
    n_janelas_12m = sum(is.finite(excesso_12m)),
    hit_mensal_longo = mean(excesso_cdi_m > 0),
    hit_6m_longo = mean(excesso_6m > 0, na.rm = TRUE),
    hit_12m_longo = mean(excesso_12m > 0, na.rm = TRUE),
    .groups = "drop")
  if (anyDuplicated(metr$nome_plot) ||
      any(metr$n_meses_observados != n_meses) ||
      any(metr$n_janelas_6m != n_meses - 5L) ||
      any(metr$n_janelas_12m != n_meses - 11L)) {
    stop("Séries incompletas ou janelas inesperadas no diagnóstico.")
  }

  comparacao = oficial %>% inner_join(metr, by = "nome_plot") %>% mutate(
    nota_hit_mensal_longo = nota_na_escala_oficial(
      hit_mensal_longo, oficial$hit_rate_mensal),
    nota_hit_6m_longo = nota_na_escala_oficial(
      hit_6m_longo, oficial$hit_rate_6m),
    nota_hit_12m_longo = nota_na_escala_oficial(
      hit_12m_longo, oficial$hit_rate_12m),
    nota_consistencia_longa = 0.20 * nota_hit_mensal_longo +
      0.20 * nota_hit_6m_longo + 0.60 * nota_hit_12m_longo,
    score_teste = nota_final + 0.25 *
      (nota_consistencia_longa - nota_consistencia),
    nota_minima_teste = pmin(nota_retorno, nota_consistencia_longa,
      nota_risco, nota_custo),
    red_flag = !is.na(red_flags_absolutos),
    aprovado_teste = !red_flag & score_teste >= 58 & nota_minima_teste >= 30,
    fronteira_teste = !red_flag & !aprovado_teste & score_teste >= 52,
    status_teste = case_when(
      red_flag ~ "Red flag absoluto",
      aprovado_teste ~ "Aprovado com margem",
      fronteira_teste ~ "Zona cinzenta",
      TRUE ~ "Não aprovado"
    ),
    horizonte_meses = n_meses,
    mes_inicio = mes_inicio,
    mes_fim = mes_final
  )
  comparacao = comparacao %>%
    arrange(desc(nota_final), nome_plot) %>%
    mutate(rank_oficial_subset = row_number()) %>%
    arrange(desc(score_teste), nome_plot) %>%
    mutate(rank_teste_subset = row_number(),
      delta_rank_subset = rank_teste_subset - rank_oficial_subset) %>%
    arrange(rank_oficial_subset) %>%
    transmute(horizonte_meses, mes_inicio, mes_fim, nome_plot,
      ranking_geral, rank_oficial_subset, rank_teste_subset,
      delta_rank_subset, hit_rate_mensal, hit_mensal_longo,
      hit_rate_6m, hit_6m_longo, hit_rate_12m, hit_12m_longo,
      n_meses_observados, n_janelas_6m, n_janelas_12m,
      nota_consistencia, nota_consistencia_longa, nota_final, score_teste,
      status_quantitativo, status_teste, aprovado_quantitativo,
      aprovado_teste, zona_fronteira, fronteira_teste)
  comparacao
}

detalhe = purrr::map_dfr(c(48L, 60L, 72L), calcula_horizonte)
resumo = detalhe %>% group_by(horizonte_meses) %>% summarise(
  fundos_com_historico = n(),
  fundos_excluidos = nrow(oficial) - n(),
  janelas_6m = first(n_janelas_6m),
  janelas_12m = first(n_janelas_12m),
  maior_delta_rank_subset = max(abs(delta_rank_subset)),
  mudancas_de_aprovacao = sum((aprovado_quantitativo %in% TRUE) != aprovado_teste),
  mudancas_de_fronteira = sum(zona_fronteira != fronteira_teste),
  .groups = "drop")

readr::write_excel_csv2(detalhe,
  file.path(path_diagnostico, "comparacao_consistencia_janelas_longas.csv"))
readr::write_excel_csv2(resumo,
  file.path(path_diagnostico, "resumo_consistencia_janelas_longas.csv"))
print(resumo)
