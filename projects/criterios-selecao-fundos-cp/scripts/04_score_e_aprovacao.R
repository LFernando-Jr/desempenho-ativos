# ETAPA 4 — SCORE E APROVAÇÃO QUANTITATIVA

# Limpa os objetos da sessão para evitar dependências de execuções anteriores.
rm(list = ls())

# ------------------------------------------------------------
# Setup
# ------------------------------------------------------------

# Caminhos dos insumos e das saídas desta etapa.
path_intermediate = "projects/criterios-selecao-fundos-cp/data/intermediate"
path_metricas = file.path(path_intermediate, "metricas_todos_fundos_36m.rds")

if (!file.exists(path_metricas)) {
  stop("Arquivo ausente na Etapa 4: ", path_metricas, ". Execute primeiro a Etapa 3.")
}

metricas_todos_fundos = read_rds(file = path_metricas)

# Pesos editáveis dos pilares do score de qualidade individual.
cfg_pesos_score = c(
  retorno = 0.30,
  consistencia = 0.25,
  risco = 0.20,
  custo = 0.25
)

# Pesos internos do pilar de consistência.
cfg_pesos_consistencia = c(
  mensal = 0.40,
  seis_meses = 0.20,
  doze_meses = 0.40
)

# Pesos internos do pilar de risco.
cfg_pesos_risco = c(
  drawdown = 0.40,
  cauda = 0.30,
  volatilidade = 0.30
)

# Pesos internos do pilar de custo.
cfg_pesos_custo = c(
  taxa = 0.60,
  razao_excesso_taxa = 0.40
)

# Limite operacional aplicado ao z-score antes da transformação logística.
LIMITE_Z_ROBUSTO = 4

# Valida os vetores de pesos antes de calcular qualquer nota.
valida_pesos = function(pesos, nome_bloco) {
  if (abs(sum(pesos) - 1) > 1e-10) {
    stop("Os pesos de ", nome_bloco, " não somam 100%.")
  }
}

valida_pesos(pesos = cfg_pesos_score, nome_bloco = "score")
valida_pesos(pesos = cfg_pesos_consistencia, nome_bloco = "consistência")
valida_pesos(pesos = cfg_pesos_risco, nome_bloco = "risco")
valida_pesos(pesos = cfg_pesos_custo, nome_bloco = "custo")

message("[04] Pesos validados: todos os blocos somam 100%.")

# ------------------------------------------------------------
# Helpers
# ------------------------------------------------------------

# Converte uma métrica em nota contínua de 0 a 100 preservando distâncias robustas.
nota_robusta = function(
  x,
  maior_melhor,
  nome_metrica,
  limite_z = LIMITE_Z_ROBUSTO
) {
  resultado = rep(NA_real_, length(x))
  validos = is.finite(x)
  valores = x[validos]

  if (length(valores) < 3) {
    warning("Poucos valores válidos para calcular a nota de ", nome_metrica, ".")
    return(resultado)
  }

  if (n_distinct(valores) == 1) {
    resultado[validos] = 50
    message("[04] ", nome_metrica, ": todos os valores são iguais; nota 50.")
    return(resultado)
  }

  centro = median(x = valores, na.rm = TRUE)
  escala = mad(
    x = valores,
    center = centro,
    constant = 1.4826,
    na.rm = TRUE
  )

  if (!is.finite(escala) || escala <= .Machine$double.eps) {
    escala = IQR(x = valores, na.rm = TRUE) / 1.349
    warning("MAD nulo em ", nome_metrica, "; utilizada escala baseada no IQR.")
  }

  if (!is.finite(escala) || escala <= .Machine$double.eps) {
    escala = sd(x = valores, na.rm = TRUE)
    warning("IQR nulo em ", nome_metrica, "; utilizado desvio-padrão como fallback.")
  }

  if (!is.finite(escala) || escala <= .Machine$double.eps) {
    resultado[validos] = 50
    warning("Não foi possível diferenciar ", nome_metrica, "; atribuída nota 50.")
    return(resultado)
  }

  z = (valores - centro) / escala

  if (!maior_melhor) {
    z = -z
  }

  z = pmin(pmax(z, -limite_z), limite_z)
  resultado[validos] = 100 / (1 + exp(-z))
  resultado
}

# Consolida red flags absolutos sem transformar alertas relativos em reprovação.
monta_red_flags_absolutos = function(excesso_cdi_aa) {
  if (is.finite(excesso_cdi_aa) && excesso_cdi_aa <= 0) {
    return("Excesso anualizado sobre o CDI não positivo")
  }

  NA_character_
}

# Consolida alertas relativos que exigem revisão, mas não reprovam automaticamente.
monta_alertas_relativos = function(
  drawdown,
  cauda,
  limite_drawdown,
  limite_cauda
) {
  alertas = character()

  if (is.finite(drawdown) && drawdown <= limite_drawdown) {
    alertas = c(alertas, "Drawdown entre os 10% piores")
  }

  if (is.finite(cauda) && cauda <= limite_cauda) {
    alertas = c(alertas, "Cauda entre os 10% piores")
  }

  if (length(alertas) == 0) {
    return(NA_character_)
  }

  paste(alertas, collapse = "; ")
}

# ------------------------------------------------------------
# Universo do score
# ------------------------------------------------------------

metricas_score = metricas_todos_fundos %>%
  filter(elegivel_ranking) %>%
  select(
    nome_xlsx,
    nome_curto,
    nome_plot,
    nome_quantum,
    taxa_adm_aa,
    excesso_cdi_aa,
    hit_rate_mensal,
    hit_rate_6m,
    hit_rate_12m,
    volatilidade_excesso_aa,
    max_drawdown_excesso,
    media_tres_piores_meses,
    pior_mes,
    autocorrelacao_lag1,
    meses_para_recuperar,
    inicio_serie_mensal_score,
    fim_serie_mensal_score,
    n_meses_score,
    hit_rate_36m_historico,
    n_janelas_36m_historico,
    benchmark_menor_tracking_error,
    menor_tracking_error_aa
  )

colunas_indevidas_score = intersect(
  names(metricas_score),
  c(
    "cluster",
    "correlacao_media_pares",
    "correlacao_maxima",
    "fundo_mais_correlacionado"
  )
)

if (length(colunas_indevidas_score) > 0) {
  stop(
    "O score contém colunas de correlação ou clusters: ",
    paste(colunas_indevidas_score, collapse = ", "),
    "."
  )
}

message("[04] Score independente de correlações e clusters.")

taxas_invalidas = metricas_score %>%
  filter(!is.finite(taxa_adm_aa) | taxa_adm_aa <= 0)

if (nrow(taxas_invalidas) > 0) {
  print(taxas_invalidas %>% select(nome_plot, taxa_adm_aa))
  stop(
    "Há taxas ausentes, zero ou negativas. ",
    "Revise o cadastro antes de calcular a razão excesso/taxa."
  )
}

log_taxas = log(metricas_score$taxa_adm_aa)
centro_log_taxas = median(log_taxas)
escala_log_taxas = mad(
  x = log_taxas,
  center = centro_log_taxas,
  constant = 1.4826
)

if (!is.finite(escala_log_taxas) || escala_log_taxas <= .Machine$double.eps) {
  escala_log_taxas = IQR(log_taxas) / 1.349
}

z_log_taxas = if (
  is.finite(escala_log_taxas) &&
    escala_log_taxas > .Machine$double.eps
) {
  (log_taxas - centro_log_taxas) / escala_log_taxas
} else {
  rep(0, length(log_taxas))
}

diagnostico_taxas = metricas_score %>%
  transmute(
    nome_plot,
    taxa_adm_aa,
    z_robusto_log_taxa = z_log_taxas,
    taxa_requer_revisao = z_robusto_log_taxa < -LIMITE_Z_ROBUSTO
  ) %>%
  arrange(taxa_adm_aa)

write_excel_csv2(
  x = diagnostico_taxas,
  file = file.path(path_intermediate, "diagnostico_taxas_36m.csv")
)

taxas_muito_pequenas = diagnostico_taxas %>%
  filter(taxa_requer_revisao)

if (nrow(taxas_muito_pequenas) > 0) {
  print(taxas_muito_pequenas)
  stop(
    "Há taxa(s) positiva(s) extremamente pequenas em relação ao universo. ",
    "Confirme o cadastro antes de calcular a razão excesso/taxa."
  )
}

message(
  "[04] Menor taxa válida do universo: ",
  percent(min(metricas_score$taxa_adm_aa), accuracy = 0.01, decimal.mark = ","),
  "."
)

# ------------------------------------------------------------
# Notas
# ------------------------------------------------------------

ranking_base = metricas_score %>%
  mutate(
    nota_retorno = nota_robusta(
      x = excesso_cdi_aa,
      maior_melhor = TRUE,
      nome_metrica = "excesso CDI"
    ),
    nota_hit_mensal = nota_robusta(
      x = hit_rate_mensal,
      maior_melhor = TRUE,
      nome_metrica = "hit rate mensal"
    ),
    nota_hit_6m = nota_robusta(
      x = hit_rate_6m,
      maior_melhor = TRUE,
      nome_metrica = "hit rate 6 meses"
    ),
    nota_hit_12m = nota_robusta(
      x = hit_rate_12m,
      maior_melhor = TRUE,
      nome_metrica = "hit rate 12 meses"
    ),
    nota_consistencia =
      cfg_pesos_consistencia["mensal"] * nota_hit_mensal +
      cfg_pesos_consistencia["seis_meses"] * nota_hit_6m +
      cfg_pesos_consistencia["doze_meses"] * nota_hit_12m,
    nota_drawdown = nota_robusta(
      x = max_drawdown_excesso,
      maior_melhor = TRUE,
      nome_metrica = "drawdown"
    ),
    nota_cauda = nota_robusta(
      x = media_tres_piores_meses,
      maior_melhor = TRUE,
      nome_metrica = "média dos três piores meses"
    ),
    nota_volatilidade = nota_robusta(
      x = volatilidade_excesso_aa,
      maior_melhor = FALSE,
      nome_metrica = "volatilidade do excesso"
    ),
    nota_risco =
      cfg_pesos_risco["drawdown"] * nota_drawdown +
      cfg_pesos_risco["cauda"] * nota_cauda +
      cfg_pesos_risco["volatilidade"] * nota_volatilidade,
    razao_excesso_taxa = excesso_cdi_aa / taxa_adm_aa,
    nota_taxa = nota_robusta(
      x = taxa_adm_aa,
      maior_melhor = FALSE,
      nome_metrica = "taxa de administração"
    ),
    nota_razao_excesso_taxa = nota_robusta(
      x = razao_excesso_taxa,
      maior_melhor = TRUE,
      nome_metrica = "razão excesso/taxa"
    ),
    nota_custo =
      cfg_pesos_custo["taxa"] * nota_taxa +
      cfg_pesos_custo["razao_excesso_taxa"] * nota_razao_excesso_taxa,
    nota_final =
      cfg_pesos_score["retorno"] * nota_retorno +
      cfg_pesos_score["consistencia"] * nota_consistencia +
      cfg_pesos_score["risco"] * nota_risco +
      cfg_pesos_score["custo"] * nota_custo
  )

limite_drawdown_relativo = quantile(
  x = ranking_base$max_drawdown_excesso,
  probs = 0.10,
  na.rm = TRUE
)

limite_cauda_relativo = quantile(
  x = ranking_base$media_tres_piores_meses,
  probs = 0.10,
  na.rm = TRUE
)

ranking_fundos = ranking_base %>%
  mutate(
    red_flags_absolutos = map_chr(
      .x = excesso_cdi_aa,
      .f = monta_red_flags_absolutos
    ),
    alertas_relativos = map2_chr(
      .x = max_drawdown_excesso,
      .y = media_tres_piores_meses,
      .f = ~ monta_alertas_relativos(
        drawdown = .x,
        cauda = .y,
        limite_drawdown = limite_drawdown_relativo,
        limite_cauda = limite_cauda_relativo
      )
    )
  ) %>%
  arrange(desc(nota_final), nome_plot) %>%
  mutate(
    ranking_geral = row_number(),
    quartil_score = ntile(desc(nota_final), 4),
    classificacao_descritiva = paste0("Q", quartil_score),
    aprovado_quantitativo = case_when(
      !is.na(red_flags_absolutos) ~ FALSE,
      TRUE ~ NA
    ),
    zona_fronteira = NA,
    status_quantitativo = case_when(
      !is.na(red_flags_absolutos) ~ "Reprovado por red flag absoluto",
      TRUE ~ "Pendente de calibração da aprovação"
    ),
    quartil_retorno = ntile(desc(nota_retorno), 4),
    quartil_consistencia = ntile(desc(nota_consistencia), 4),
    quartil_risco = ntile(desc(nota_risco), 4),
    quartil_custo = ntile(desc(nota_custo), 4)
  )

correlacao_retorno_eficiencia = cor(
  x = ranking_fundos$nota_retorno,
  y = ranking_fundos$nota_razao_excesso_taxa,
  use = "complete.obs",
  method = "pearson"
)

message(
  "[04] Correlação entre nota de retorno e nota da razão excesso/taxa: ",
  number(correlacao_retorno_eficiencia, accuracy = 0.01, decimal.mark = ","),
  "."
)

diagnostico_score = tibble(
  metrica = c(
    "correlacao_nota_retorno_eficiencia",
    "limite_drawdown_relativo",
    "limite_cauda_relativo"
  ),
  valor = c(
    correlacao_retorno_eficiencia,
    limite_drawdown_relativo,
    limite_cauda_relativo
  )
)

write_rds(
  x = ranking_fundos,
  file = file.path(path_intermediate, "ranking_fundos_36m.rds")
)

write_excel_csv2(
  x = ranking_fundos,
  file = file.path(path_intermediate, "ranking_fundos_36m.csv")
)

write_excel_csv2(
  x = diagnostico_score,
  file = file.path(path_intermediate, "diagnostico_score_36m.csv")
)

message("[04] Score de qualidade concluído; aprovação permanece pendente de calibração.")
