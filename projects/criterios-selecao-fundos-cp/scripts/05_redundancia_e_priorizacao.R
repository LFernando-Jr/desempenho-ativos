# ETAPA 5 — REDUNDÂNCIA E PRIORIZAÇÃO

# Limpa os objetos da sessão para evitar dependências de execuções anteriores.
rm(list = ls())

# ------------------------------------------------------------
# Setup
# ------------------------------------------------------------

# Caminhos dos insumos, da configuração e das saídas desta etapa.
path_intermediate = "projects/criterios-selecao-fundos-cp/data/intermediate"
path_ranking = file.path(path_intermediate, "ranking_fundos_36m.rds")
path_matriz_cor = file.path(path_intermediate, "matriz_correlacao_excessos_36m.rds")
path_mensais_historico = file.path(path_intermediate, "fundos_mensais_historico.rds")
path_mensais_score = file.path(path_intermediate, "fundos_mensais_score_36m.rds")
path_carteira_atual = "projects/criterios-selecao-fundos-cp/data/config/fundos_carteira_atual.csv"

# Valores candidatos usados apenas para mostrar a sensibilidade dos alertas.
cfg_limiares_redundancia = c(0.75, 0.80, 0.85, 0.90)

# Quantidades de clusters avaliadas sem escolher uma solução definitiva.
cfg_k_clusters = 3:7

# Defasagens usadas para verificar estabilidade em outras janelas de 36 meses.
cfg_defasagens_janela = c(0L, 3L, 6L)

paths_necessarios = c(
  path_ranking,
  path_matriz_cor,
  path_mensais_historico,
  path_mensais_score
)

paths_ausentes = paths_necessarios[!file.exists(paths_necessarios)]

if (length(paths_ausentes) > 0) {
  stop(
    "Arquivos ausentes na Etapa 5: ",
    paste(paths_ausentes, collapse = ", "),
    ". Execute primeiro as Etapas 3 e 4."
  )
}

ranking_fundos = read_rds(file = path_ranking)
matriz_cor = read_rds(file = path_matriz_cor)
fundos_mensais_historico = read_rds(file = path_mensais_historico)
fundos_mensais_score = read_rds(file = path_mensais_score)

# ------------------------------------------------------------
# Helpers
# ------------------------------------------------------------

# Calcula o adjusted Rand index para comparar duas soluções de clusters.
adjusted_rand_index = function(grupos_a, grupos_b) {
  nomes_comuns = intersect(names(grupos_a), names(grupos_b))

  if (length(nomes_comuns) < 3) {
    return(NA_real_)
  }

  tabela = table(grupos_a[nomes_comuns], grupos_b[nomes_comuns])
  combina_dois = function(x) x * (x - 1) / 2
  soma_celulas = sum(combina_dois(tabela))
  soma_linhas = sum(combina_dois(rowSums(tabela)))
  soma_colunas = sum(combina_dois(colSums(tabela)))
  total_pares = combina_dois(sum(tabela))

  if (total_pares == 0) {
    return(NA_real_)
  }

  esperado = soma_linhas * soma_colunas / total_pares
  denominador = 0.5 * (soma_linhas + soma_colunas) - esperado

  if (denominador == 0) {
    return(1)
  }

  (soma_celulas - esperado) / denominador
}

# Forma clusters em uma janela histórica comum para testar estabilidade temporal.
calcula_grupos_janela = function(
  base_mensal,
  nomes_fundos,
  mes_fim,
  k,
  janela_meses = 36L
) {
  mes_inicio = mes_fim %m-% months(janela_meses - 1L)

  base_janela = base_mensal %>%
    filter(
      nome_plot %in% nomes_fundos,
      mes >= mes_inicio,
      mes <= mes_fim
    ) %>%
    group_by(nome_plot) %>%
    filter(n_distinct(mes) == janela_meses) %>%
    ungroup()

  nomes_completos = base_janela %>%
    distinct(nome_plot) %>%
    pull(nome_plot)

  if (length(nomes_completos) < max(k + 1L, 4L)) {
    return(NULL)
  }

  matriz_janela = base_janela %>%
    filter(nome_plot %in% nomes_completos) %>%
    select(mes, nome_plot, excesso_cdi_m) %>%
    pivot_wider(
      names_from = nome_plot,
      values_from = excesso_cdi_m
    ) %>%
    select(-mes) %>%
    as.matrix()

  matriz_cor_janela = cor(
    x = matriz_janela,
    use = "pairwise.complete.obs",
    method = "pearson"
  )

  matriz_cor_janela[is.na(matriz_cor_janela)] = 0
  diag(matriz_cor_janela) = 1

  arvore = hclust(
    d = as.dist(1 - matriz_cor_janela),
    method = "average"
  )

  cutree(tree = arvore, k = k)
}

# ------------------------------------------------------------
# Universo da análise de redundância
# ------------------------------------------------------------

# Enquanto a aprovação não está calibrada, usa fundos sem red flag absoluto.
fundos_candidatos = ranking_fundos %>%
  filter(is.na(red_flags_absolutos)) %>%
  pull(nome_plot)

carteira_atual = if (file.exists(path_carteira_atual)) {
  read_csv2(
    file = path_carteira_atual,
    show_col_types = FALSE
  ) %>%
    filter(!is.na(nome_plot), nome_plot != "") %>%
    distinct(nome_plot)
} else {
  tibble(nome_plot = character())
}

fundos_carteira_disponiveis = intersect(
  carteira_atual$nome_plot,
  colnames(matriz_cor)
)

fundos_carteira_ausentes = setdiff(
  carteira_atual$nome_plot,
  colnames(matriz_cor)
)

if (length(fundos_carteira_ausentes) > 0) {
  warning(
    "Fundos da carteira sem série compatível: ",
    paste(fundos_carteira_ausentes, collapse = ", "),
    "."
  )
}

if (nrow(carteira_atual) == 0) {
  message(
    "[05] Carteira atual não preenchida; complete fundos_carteira_atual.csv ",
    "para ativar a comparação com posições existentes."
  )
}

nomes_analise = union(
  intersect(fundos_candidatos, colnames(matriz_cor)),
  fundos_carteira_disponiveis
)

if (length(nomes_analise) < 3) {
  stop("São necessários ao menos três fundos para analisar redundância.")
}

matriz_cor_analise = matriz_cor[nomes_analise, nomes_analise, drop = FALSE]

# ------------------------------------------------------------
# Pares e sensibilidade dos limiares
# ------------------------------------------------------------

pares_redundancia = as.data.frame(
  as.table(matriz_cor_analise),
  stringsAsFactors = FALSE
) %>%
  as_tibble() %>%
  rename(
    fundo_a = Var1,
    fundo_b = Var2,
    correlacao = Freq
  ) %>%
  filter(fundo_a < fundo_b, is.finite(correlacao)) %>%
  mutate(
    fundo_a_carteira = fundo_a %in% fundos_carteira_disponiveis,
    fundo_b_carteira = fundo_b %in% fundos_carteira_disponiveis,
    envolve_carteira_atual = fundo_a_carteira | fundo_b_carteira,
    tipo_par = case_when(
      fundo_a_carteira & fundo_b_carteira ~ "Entre posições atuais",
      envolve_carteira_atual ~ "Candidato versus posição atual",
      TRUE ~ "Entre candidatos"
    )
  ) %>%
  arrange(desc(correlacao))

sensibilidade_limiares = map_dfr(
  .x = cfg_limiares_redundancia,
  .f = ~ tibble(
    limiar = .x,
    n_pares = sum(pares_redundancia$correlacao >= .x),
    n_pares_com_carteira = sum(
      pares_redundancia$correlacao >= .x &
        pares_redundancia$envolve_carteira_atual
    )
  )
)

distribuicao_correlacoes = tibble(
  estatistica = c("mínimo", "p25", "mediana", "p75", "p90", "p95", "máximo"),
  valor = c(
    min(pares_redundancia$correlacao),
    quantile(pares_redundancia$correlacao, probs = 0.25),
    median(pares_redundancia$correlacao),
    quantile(pares_redundancia$correlacao, probs = 0.75),
    quantile(pares_redundancia$correlacao, probs = 0.90),
    quantile(pares_redundancia$correlacao, probs = 0.95),
    max(pares_redundancia$correlacao)
  )
)

resumo_redundancia = map_dfr(
  .x = intersect(fundos_candidatos, nomes_analise),
  .f = function(nome_fundo) {
    pares_fundo = pares_redundancia %>%
      filter(fundo_a == nome_fundo | fundo_b == nome_fundo) %>%
      mutate(
        fundo_par = if_else(fundo_a == nome_fundo, fundo_b, fundo_a),
        par_na_carteira = fundo_par %in% fundos_carteira_disponiveis
      )

    par_maximo = pares_fundo %>%
      slice_max(correlacao, n = 1, with_ties = FALSE)

    par_carteira = pares_fundo %>%
      filter(par_na_carteira) %>%
      slice_max(correlacao, n = 1, with_ties = FALSE)

    fundo_maximo = if (nrow(par_maximo) == 0) NA_character_ else par_maximo$fundo_par[1]
    correlacao_maxima = if (nrow(par_maximo) == 0) NA_real_ else par_maximo$correlacao[1]
    fundo_maximo_carteira = if (nrow(par_carteira) == 0) {
      NA_character_
    } else {
      par_carteira$fundo_par[1]
    }
    correlacao_maxima_carteira = if (nrow(par_carteira) == 0) {
      NA_real_
    } else {
      par_carteira$correlacao[1]
    }

    tibble(
      nome_plot = nome_fundo,
      fundo_mais_correlacionado = fundo_maximo,
      correlacao_maxima = correlacao_maxima,
      fundo_carteira_mais_correlacionado = fundo_maximo_carteira,
      correlacao_maxima_carteira = correlacao_maxima_carteira
    )
  }
)

# ------------------------------------------------------------
# Diagnóstico de clusters
# ------------------------------------------------------------

matriz_cor_cluster = matriz_cor_analise
matriz_cor_cluster[is.na(matriz_cor_cluster)] = 0
diag(matriz_cor_cluster) = 1

distancia_cor = as.dist(1 - matriz_cor_cluster)
cluster_hierarquico = hclust(
  d = distancia_cor,
  method = "average"
)

k_validos = cfg_k_clusters[
  cfg_k_clusters >= 2 & cfg_k_clusters < length(nomes_analise)
]

membros_clusters = map_dfr(
  .x = k_validos,
  .f = function(k) {
    grupos = cutree(tree = cluster_hierarquico, k = k)
    tibble(
      k = k,
      nome_plot = names(grupos),
      cluster = as.integer(grupos)
    )
  }
)

diagnostico_clusters = map_dfr(
  .x = k_validos,
  .f = function(k) {
    grupos = cutree(tree = cluster_hierarquico, k = k)
    silhueta = silhouette(x = grupos, dist = distancia_cor)
    tamanhos = table(grupos)

    pares_k = pares_redundancia %>%
      mutate(
        cluster_a = grupos[fundo_a],
        cluster_b = grupos[fundo_b],
        mesmo_cluster = cluster_a == cluster_b
      )

    tibble(
      k = k,
      silhueta_media = mean(silhueta[, "sil_width"]),
      menor_cluster = min(tamanhos),
      maior_cluster = max(tamanhos),
      correlacao_media_intracluster = mean(
        pares_k$correlacao[pares_k$mesmo_cluster],
        na.rm = TRUE
      ),
      correlacao_media_intercluster = mean(
        pares_k$correlacao[!pares_k$mesmo_cluster],
        na.rm = TRUE
      )
    )
  }
)

mes_fim_atual = max(fundos_mensais_score$mes)

estabilidade_clusters = crossing(
  k = k_validos,
  defasagem_meses = cfg_defasagens_janela
) %>%
  mutate(
    mes_fim = mes_fim_atual %m-% months(defasagem_meses),
    grupos = map2(
      .x = k,
      .y = mes_fim,
      .f = ~ calcula_grupos_janela(
        base_mensal = fundos_mensais_historico,
        nomes_fundos = nomes_analise,
        mes_fim = .y,
        k = .x
      )
    )
  ) %>%
  group_by(k) %>%
  mutate(
    grupos_referencia = list(grupos[[which.min(defasagem_meses)]]),
    adjusted_rand = map2_dbl(
      .x = grupos_referencia,
      .y = grupos,
      .f = ~ {
        if (is.null(.x) || is.null(.y)) {
          return(NA_real_)
        }

        adjusted_rand_index(grupos_a = .x, grupos_b = .y)
      }
    ),
    n_fundos_janela = map_int(
      .x = grupos,
      .f = ~ if (is.null(.x)) 0L else length(.x)
    )
  ) %>%
  ungroup() %>%
  select(k, defasagem_meses, mes_fim, n_fundos_janela, adjusted_rand)

# ------------------------------------------------------------
# Priorização qualitativa
# ------------------------------------------------------------

priorizacao_qualitativa = ranking_fundos %>%
  left_join(
    resumo_redundancia,
    by = "nome_plot",
    relationship = "one-to-one",
    suffix = c("", "_redundancia")
  ) %>%
  mutate(
    prioridade_analise_qualitativa = case_when(
      !is.na(red_flags_absolutos) ~ NA_integer_,
      TRUE ~ rank(-nota_final, ties.method = "first")
    ),
    status_priorizacao = case_when(
      !is.na(red_flags_absolutos) ~ "Fora da priorização por red flag absoluto",
      TRUE ~ "Prioridade provisória; aprovação ainda não calibrada"
    )
  ) %>%
  arrange(prioridade_analise_qualitativa, ranking_geral)

write_rds(
  x = priorizacao_qualitativa,
  file = file.path(path_intermediate, "priorizacao_qualitativa_36m.rds")
)

write_rds(
  x = cluster_hierarquico,
  file = file.path(path_intermediate, "dendrograma_diagnostico_36m.rds")
)

write_excel_csv2(
  x = pares_redundancia,
  file = file.path(path_intermediate, "pares_redundancia_36m.csv")
)

write_excel_csv2(
  x = sensibilidade_limiares,
  file = file.path(path_intermediate, "sensibilidade_limiares_redundancia_36m.csv")
)

write_excel_csv2(
  x = distribuicao_correlacoes,
  file = file.path(path_intermediate, "distribuicao_correlacoes_36m.csv")
)

write_excel_csv2(
  x = diagnostico_clusters,
  file = file.path(path_intermediate, "diagnostico_clusters_k_3_7.csv")
)

write_excel_csv2(
  x = membros_clusters,
  file = file.path(path_intermediate, "membros_clusters_k_3_7.csv")
)

write_excel_csv2(
  x = estabilidade_clusters,
  file = file.path(path_intermediate, "estabilidade_clusters_janelas.csv")
)

message("[05] Redundância e diagnósticos de clusters concluídos sem corte automático.")
