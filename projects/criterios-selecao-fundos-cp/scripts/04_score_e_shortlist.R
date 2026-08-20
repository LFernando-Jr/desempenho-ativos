# ETAPA 4 — SCORE E SHORTLIST
# Execute preferencialmente por 00_run_all.R.

# ------------------------------------------------------------

# Percentil com tratamento explícito para vetores pequenos.
score_maior_melhor <- function(x) {
  resultado <- rep(NA_real_, length(x))
  validos <- is.finite(x)
  
  if (sum(validos) == 1) {
    resultado[validos] <- 0.5
  }
  
  if (sum(validos) > 1) {
    resultado[validos] <-
      dplyr::percent_rank(x[validos])
  }
  
  resultado
}

score_menor_melhor <- function(x) {
  score_maior_melhor(-x)
}

ranking_base <- metricas_todos_fundos |>
  filter(elegivel_ranking) |>
  mutate(
    # Retorno: 20% do score final.
    score_retorno =
      score_maior_melhor(excesso_cdi_aa),
    
    # Consistência: 25% do score final.
    score_consistencia =
      0.40 * score_maior_melhor(hit_rate_mensal) +
      0.20 * score_maior_melhor(hit_rate_6m) +
      0.40 * score_maior_melhor(hit_rate_36m),
    
    # Risco: 20% do score final.
    # Drawdowns e meses ruins menos negativos recebem nota maior.
    score_risco =
      0.40 * score_maior_melhor(max_drawdown_excesso) +
      0.30 * score_maior_melhor(media_tres_piores_meses) +
      0.30 * score_menor_melhor(volatilidade_excesso_aa),
    
    # Custo: 25% do score final.
    eficiencia_custo =
      excesso_cdi_aa /
      pmax(taxa_adm_aa, 0.0001),
    
    score_taxa =
      score_menor_melhor(taxa_adm_aa),
    
    score_eficiencia =
      score_maior_melhor(eficiencia_custo),
    
    score_custo =
      0.60 * score_taxa +
      0.40 * score_eficiencia,
    
    # Diferenciação: 10% do score final.
    # Menor correlação com os pares recebe maior nota.
    score_diferenciacao =
      0.60 * score_menor_melhor(correlacao_media_pares) +
      0.40 * score_menor_melhor(correlacao_maxima),
    
    score_preliminar =
      0.20 * score_retorno +
      0.25 * score_consistencia +
      0.20 * score_risco +
      0.25 * score_custo +
      0.10 * score_diferenciacao
  )

# Limites relativos usados apenas como alertas de cauda.
limite_drawdown <- quantile(
  ranking_base$max_drawdown_excesso,
  probs = 0.10,
  na.rm = TRUE
)

limite_cauda <- quantile(
  ranking_base$media_tres_piores_meses,
  probs = 0.10,
  na.rm = TRUE
)

monta_red_flags <- function(
    excesso,
    hit_36m,
    drawdown,
    cauda
) {
  flags <- character()
  
  if (is.finite(excesso) && excesso <= 0) {
    flags <- c(
      flags,
      "Excesso anualizado não positivo"
    )
  }
  
  if (is.finite(hit_36m) && hit_36m < 0.50) {
    flags <- c(
      flags,
      "Menos de 50% das janelas de 36m acima do CDI"
    )
  }
  
  if (
    is.finite(drawdown) &&
    drawdown <= limite_drawdown
  ) {
    flags <- c(
      flags,
      "Drawdown entre os 10% piores"
    )
  }
  
  if (
    is.finite(cauda) &&
    cauda <= limite_cauda
  ) {
    flags <- c(
      flags,
      "Cauda entre os 10% piores"
    )
  }
  
  if (length(flags) == 0) {
    return(NA_character_)
  }
  
  paste(flags, collapse = "; ")
}

ranking_fundos <- ranking_base |>
  mutate(
    red_flags = pmap_chr(
      list(
        excesso_cdi_aa,
        hit_rate_36m,
        max_drawdown_excesso,
        media_tres_piores_meses
      ),
      monta_red_flags
    )
  ) |>
  arrange(
    desc(score_preliminar)
  ) |>
  mutate(
    ranking_geral =
      row_number(),
    
    quartil_score =
      ntile(
        desc(score_preliminar),
        4
      ),
    
    classificacao = case_when(
      quartil_score == 1 ~ "Q1 - Destaque",
      quartil_score == 2 ~ "Q2 - Aprovado",
      quartil_score == 3 ~ "Q3 - Observação",
      quartil_score == 4 ~ "Q4 - Descartado"
    ),
    
    elegivel_shortlist =
      quartil_score <= 2 &
      is.na(red_flags)
  ) |>
  group_by(cluster) |>
  arrange(
    desc(score_preliminar),
    .by_group = TRUE
  ) |>
  mutate(
    ranking_no_cluster =
      row_number(),
    
    ranking_elegivel_no_cluster =
      if_else(
        elegivel_shortlist,
        cumsum(elegivel_shortlist),
        NA_integer_
      ),
    
    shortlist =
      elegivel_shortlist &
      ranking_elegivel_no_cluster <=
      MAX_FUNDOS_POR_CLUSTER
  ) |>
  ungroup() |>
  mutate(
    status_triagem = case_when(
      shortlist ~ "Shortlist",
      !is.na(red_flags) ~ "Reprovado por red flag",
      quartil_score >= 3 ~ classificacao,
      elegivel_shortlist & !shortlist ~
        "Aprovado, fora do limite por cluster",
      TRUE ~ classificacao
    ),
    
    # Quartis próprios de cada bloco.
    quartil_retorno =
      ntile(desc(score_retorno), 4),
    
    quartil_consistencia =
      ntile(desc(score_consistencia), 4),
    
    quartil_risco =
      ntile(desc(score_risco), 4),
    
    quartil_custo =
      ntile(desc(score_custo), 4),
    
    quartil_diferenciacao =
      ntile(desc(score_diferenciacao), 4)
  ) |>
  arrange(ranking_geral)

write_excel_csv2(
  ranking_fundos,
  "data/intermediate/ranking_fundos_etapa2_36m.csv"
)

write_rds(
  ranking_fundos,
  "data/intermediate/ranking_fundos_etapa2_36m.rds"
)

# ------------------------------------------------------------
# 7.1. Visualização do ranking final
# ------------------------------------------------------------
