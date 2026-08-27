# Setup -------------------------------------------------------------------

rm(list = ls())

# Coleta -----------------------------------------------------------------

# parte da refatoração como já discutimos

arquivo_entrada = paste0(
  "projects/criterios-selecao-fundos-cp/data/intermediate/",
  "fundos_retornos_etapa1_36m.rds"
)

MIN_OBS_MES = 15L
MIN_MESES_COR = 36L
MIN_MESES_METRICA = 36L
JANELA_CONSISTENCIA_MESES = 36L
N_CLUSTERS = 5L
MIN_MESES_RANKING = 36L
MAX_FUNDOS_POR_CLUSTER = 3L

# Gráfico de retorno acumulado relativo ao benchmark.
# Opções: "IDA-DI" ou "IDA LIQ-DI".
BENCHMARK_ACUMULADO = "IDA-DI"
JANELA_ACUMULADA_MESES = 36L

if (!file.exists(arquivo_entrada)) {
  stop(
    "Arquivo não encontrado: ",
    arquivo_entrada,
    ". Execute primeiro o script da Etapa 1."
  )
}

fundos_retornos = read_rds(arquivo_entrada) %>%
  mutate(
    nome_plot = as.character(nome_plot)
  )

colunas_necessarias = c(
  "nome_xlsx",
  "nome_plot",
  "nome_quantum",
  "taxa_adm_aa",
  "revisar",
  "data",
  "ret_liq",
  "ret_cdi",
  "ret_ida_di",
  "ret_ida_liq_di",
  "ret_irfm_1"
)

colunas_ausentes = setdiff(
  colunas_necessarias,
  names(fundos_retornos)
)

if (length(colunas_ausentes) > 0) {
  stop(
    "Colunas ausentes na base da Etapa 1: ",
    paste(colunas_ausentes, collapse = ", ")
  )
}

# ------------------------------------------------------------
# 1. Universo elegível
# ------------------------------------------------------------
# A Etapa 1 já restringiu o universo aos fundos com
# 36 meses completos. Aqui validamos novamente:
# - ausência de pendência no de-para;
# - série chegando à data máxima comum.

data_fim = max(
  fundos_retornos$data,
  na.rm = TRUE
)

fundos_elegiveis = fundos_retornos %>%
  group_by(
    nome_xlsx,
    nome_plot,
    nome_quantum,
    taxa_adm_aa
  ) %>%
  summarise(
    ultima_data = max(data),
    revisar = any(revisar),
    .groups = "drop"
  ) %>%
  filter(
    !revisar,
    ultima_data == data_fim
  )

cat("Data final comum:", format(data_fim, "%d/%m/%Y"), "\n")
cat("Fundos elegíveis:", nrow(fundos_elegiveis), "\n")

# ------------------------------------------------------------
# 2. Agregação mensal
# ------------------------------------------------------------
# Os retornos são compostos geometricamente dentro de cada mês.
# Meses parciais são removidos para não contaminar correlações,
# hit rates e métricas de risco.

fundos_mensais = fundos_retornos %>%
  semi_join(
    fundos_elegiveis,
    by = c(
      "nome_xlsx",
      "nome_plot",
      "nome_quantum",
      "taxa_adm_aa"
    )
  ) %>%
  mutate(
    mes = floor_date(data, unit = "month")
  ) %>%
  group_by(
    nome_xlsx,
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

    excesso_cdi_m = (1 + ret_fundo_m) /
      (1 + ret_cdi_m) -
      1,

    ida_di_xs_cdi_m = (1 + ret_ida_di_m) /
      (1 + ret_cdi_m) -
      1,

    ida_liq_di_xs_cdi_m = (1 + ret_ida_liq_di_m) /
      (1 + ret_cdi_m) -
      1,

    irfm_1_xs_cdi_m = (1 + ret_irfm_1_m) /
      (1 + ret_cdi_m) -
      1
  ) %>%
  filter(
    mes_completo,
    !is.na(excesso_cdi_m)
  ) %>%
  arrange(nome_plot, mes)

cat(
  "Observações mensais completas:",
  nrow(fundos_mensais),
  "\n"
)

write_rds(
  fundos_mensais,
  "projects/criterios-selecao-fundos-cp/data/intermediate/fundos_mensais_etapa2_36m.rds"
)

write_excel_csv2(
  fundos_mensais,
  "projects/criterios-selecao-fundos-cp/data/intermediate/fundos_mensais_etapa2_36m.csv"
)

# ------------------------------------------------------------
# 3. Correlação dos excessos mensais sobre o CDI
# ------------------------------------------------------------

fundos_para_correlacao = fundos_mensais %>%
  group_by(
    nome_plot,
    nome_quantum
  ) %>%
  summarise(
    n_meses = n(),
    desvio = sd(excesso_cdi_m, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(
    n_meses >= MIN_MESES_COR,
    is.finite(desvio),
    desvio > 0
  )

base_cor_wide = fundos_mensais %>%
  semi_join(
    fundos_para_correlacao,
    by = c("nome_plot", "nome_quantum")
  ) %>%
  select(
    mes,
    nome_plot,
    excesso_cdi_m
  ) %>%
  pivot_wider(
    names_from = nome_plot,
    values_from = excesso_cdi_m
  ) %>%
  arrange(mes)

matriz_excessos = base_cor_wide %>%
  select(-mes) %>%
  as.matrix()

matriz_cor = cor(
  matriz_excessos,
  use = "pairwise.complete.obs",
  method = "pearson"
)

# Quantidade de meses em comum para cada par.
matriz_obs = crossprod(
  !is.na(matriz_excessos)
)

# Correlações indefinidas não são usadas no clustering.
# Com o filtro mínimo e data final comum, isso não deveria ocorrer.
if (anyNA(matriz_cor)) {
  warning(
    "Há correlações indefinidas. Para o clustering, ",
    "elas serão tratadas como correlação zero."
  )
}

matriz_cor_cluster = matriz_cor
matriz_cor_cluster[is.na(matriz_cor_cluster)] = 0
diag(matriz_cor_cluster) = 1

distancia_cor = as.dist(
  1 - matriz_cor_cluster
)

cluster_hierarquico = hclust(
  distancia_cor,
  method = "average"
)

ordem_cluster = colnames(matriz_cor_cluster)[
  cluster_hierarquico$order
]

grupos_cluster = cutree(
  cluster_hierarquico,
  k = min(
    N_CLUSTERS,
    ncol(matriz_cor_cluster)
  )
)

base_clusters = tibble(
  nome_plot = names(grupos_cluster),
  cluster = as.integer(grupos_cluster)
) %>%
  left_join(
    fundos_elegiveis %>%
      select(
        nome_xlsx,
        nome_plot,
        nome_quantum,
        taxa_adm_aa
      ),
    by = "nome_plot",
    relationship = "one-to-one"
  ) %>%
  arrange(cluster, nome_plot)

write_excel_csv2(
  base_clusters,
  "projects/criterios-selecao-fundos-cp/data/intermediate/clusters_fundos_36m.csv"
)

# ------------------------------------------------------------
# 3.1. Resumo de correlação de cada fundo
# ------------------------------------------------------------

resumo_correlacao = map_dfr(
  seq_len(nrow(matriz_cor)),
  function(i) {
    nome_fundo = rownames(matriz_cor)[i]

    correlacoes = matriz_cor[i, ]
    observacoes = matriz_obs[i, ]

    manter = names(correlacoes) != nome_fundo

    correlacoes = correlacoes[manter]
    observacoes = observacoes[manter]

    validas = is.finite(correlacoes)

    correlacoes_validas = correlacoes[validas]
    observacoes_validas = observacoes[validas]

    indice_max = which.max(correlacoes_validas)

    tibble(
      nome_plot = nome_fundo,

      correlacao_media_pares = mean(correlacoes_validas),

      correlacao_mediana_pares = median(correlacoes_validas),

      correlacao_maxima = correlacoes_validas[indice_max],

      fundo_mais_correlacionado = names(correlacoes_validas)[indice_max],

      meses_em_comum_com_par_mais_proximo = observacoes_validas[indice_max]
    )
  }
) %>%
  left_join(
    base_clusters %>%
      select(nome_plot, cluster),
    by = "nome_plot",
    relationship = "one-to-one"
  ) %>%
  arrange(desc(correlacao_media_pares))

write_excel_csv2(
  resumo_correlacao,
  "projects/criterios-selecao-fundos-cp/data/intermediate/resumo_correlacao_fundos_36m.csv"
)

# ------------------------------------------------------------
# 3.2. Heatmap da correlação
# ------------------------------------------------------------

base_cor_long = as.data.frame(
  as.table(matriz_cor)
) %>%
  as_tibble() %>%
  rename(
    fundo_linha = Var1,
    fundo_coluna = Var2,
    correlacao = Freq
  ) %>%
  mutate(
    fundo_linha = factor(
      fundo_linha,
      levels = rev(ordem_cluster)
    ),

    fundo_coluna = factor(
      fundo_coluna,
      levels = ordem_cluster
    )
  )

grafico_correlacao = ggplot(
  base_cor_long,
  aes(
    x = fundo_coluna,
    y = fundo_linha,
    fill = correlacao
  )
) +
  geom_tile() +
  scale_fill_gradient2(
    low = "#B2182B",
    mid = "white",
    high = "#2166AC",
    midpoint = 0,
    limits = c(-1, 1),
    oob = squish,
    labels = number_format(
      accuracy = 0.1,
      decimal.mark = ","
    ),
    name = "Correlação"
  ) +
  labs(
    title = "Correlação dos excessos mensais sobre o CDI",

    subtitle = paste0(
      "Fundos com pelo menos ",
      MIN_MESES_COR,
      " meses completos | Ordem definida por clustering"
    ),

    x = NULL,
    y = NULL,

    caption = paste(
      "A correlação representa similaridade comportamental,",
      "não necessariamente sobreposição de ativos."
    )
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid = element_blank(),

    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      vjust = 0.5,
      size = 5.5
    ),

    axis.text.y = element_text(
      size = 5.5
    ),

    plot.title = element_text(
      face = "bold"
    ),

    plot.caption = element_text(
      hjust = 0
    )
  )

print(grafico_correlacao)

ggsave(
  filename = "projects/criterios-selecao-fundos-cp/output/figures/heatmap_correlacao_excessos_mensais_36m.png",

  plot = grafico_correlacao,

  width = 15,

  height = 13,

  dpi = 300
)

# Exporta a matriz em formato tabular.
write_excel_csv2(
  matriz_cor %>%
    as.data.frame() %>%
    rownames_to_column("nome_plot"),
  "projects/criterios-selecao-fundos-cp/data/intermediate/matriz_correlacao_excessos_mensais_36m.csv"
)

# ------------------------------------------------------------
# 3.3. Dendrograma
# ------------------------------------------------------------

png(
  filename = "projects/criterios-selecao-fundos-cp/output/figures/dendrograma_fundos_excessos_mensais_36m.png",

  width = 2600,

  height = 1500,

  res = 200
)

par(
  mar = c(16, 5, 4, 2)
)

plot(
  cluster_hierarquico,
  labels = cluster_hierarquico$labels,
  hang = -1,
  cex = 0.55,
  main = "Clustering dos excessos mensais sobre o CDI",
  sub = paste0(
    "Método average | ",
    MIN_MESES_COR,
    " meses mínimos"
  ),
  xlab = "",
  ylab = "Distância: 1 - correlação"
)

rect.hclust(
  cluster_hierarquico,
  k = min(
    N_CLUSTERS,
    ncol(matriz_cor_cluster)
  )
)

dev.off()

# ------------------------------------------------------------
# 4. Consistência e risco relativo ao CDI
# ------------------------------------------------------------

fundos_mensais_rolling = fundos_mensais %>%
  group_by(
    nome_xlsx,
    nome_plot,
    nome_quantum,
    taxa_adm_aa
  ) %>%
  arrange(mes, .by_group = TRUE) %>%
  mutate(
    excesso_6m = slide_dbl(
      excesso_cdi_m,
      ~ prod(1 + .x) - 1,
      .before = 5,
      .complete = TRUE
    ),

    excesso_36m = slide_dbl(
      excesso_cdi_m,
      ~ prod(1 + .x) - 1,
      .before = JANELA_CONSISTENCIA_MESES - 1L,
      .complete = TRUE
    )
  ) %>%
  ungroup()

calcula_drawdown = function(retornos) {
  patrimonio_relativo = cumprod(1 + retornos)
  pico = cummax(patrimonio_relativo)
  drawdown = patrimonio_relativo / pico - 1

  indice_fundo = which.min(drawdown)
  max_drawdown = drawdown[indice_fundo]

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
    max_drawdown_excesso = max_drawdown,

    meses_para_recuperar = meses_recuperacao
  )
}

autocor_lag1 = function(x) {
  x = x[is.finite(x)]

  if (length(x) < 4 || sd(x) == 0) {
    return(NA_real_)
  }

  as.numeric(
    acf(
      x,
      lag.max = 1,
      plot = FALSE,
      na.action = na.pass
    )$acf[2]
  )
}

metricas_consistencia = fundos_mensais_rolling %>%
  group_by(
    nome_xlsx,
    nome_plot,
    nome_quantum,
    taxa_adm_aa
  ) %>%
  filter(
    n() >= MIN_MESES_METRICA
  ) %>%
  group_modify(
    ~ {
      base_fundo = .x %>%
        arrange(mes)

      excesso = base_fundo$excesso_cdi_m
      excesso_6m = base_fundo$excesso_6m
      excesso_36m = base_fundo$excesso_36m

      n_meses = length(excesso)

      tres_piores = sort(
        excesso,
        na.last = NA
      )[
        seq_len(
          min(3, sum(is.finite(excesso)))
        )
      ]

      drawdown = calcula_drawdown(excesso)

      tibble(
        primeira_data = min(base_fundo$mes),

        ultima_data = max(base_fundo$mes),

        n_meses = n_meses,

        excesso_cdi_aa = prod(1 + excesso)^(12 / n_meses) - 1,

        hit_rate_mensal = mean(excesso > 0),

        hit_rate_6m = mean(
          excesso_6m > 0,
          na.rm = TRUE
        ),

        n_janelas_36m = sum(
          !is.na(excesso_36m)
        ),

        hit_rate_36m = mean(
          excesso_36m > 0,
          na.rm = TRUE
        ),

        volatilidade_excesso_aa = sd(excesso) * sqrt(12),

        pior_mes = min(excesso),

        media_tres_piores_meses = mean(tres_piores),

        autocorrelacao_lag1 = autocor_lag1(excesso),

        max_drawdown_excesso = drawdown$max_drawdown_excesso,

        meses_para_recuperar = drawdown$meses_para_recuperar
      )
    }
  ) %>%
  ungroup() %>%
  left_join(
    base_clusters %>%
      select(nome_plot, cluster),
    by = "nome_plot",
    relationship = "many-to-one"
  ) %>%
  arrange(desc(excesso_cdi_aa))

write_excel_csv2(
  metricas_consistencia,
  "projects/criterios-selecao-fundos-cp/data/intermediate/metricas_consistencia_risco_36m.csv"
)

# ------------------------------------------------------------
# 4.1. Dispersão: retorno versus consistência
# ------------------------------------------------------------

destaques_consistencia = bind_rows(
  metricas_consistencia %>%
    slice_max(
      excesso_cdi_aa,
      n = 5,
      with_ties = FALSE
    ),

  metricas_consistencia %>%
    slice_min(
      excesso_cdi_aa,
      n = 5,
      with_ties = FALSE
    ),

  metricas_consistencia %>%
    slice_max(
      hit_rate_36m,
      n = 3,
      with_ties = FALSE
    )
) %>%
  distinct(nome_plot, .keep_all = TRUE)

mediana_excesso = median(
  metricas_consistencia$excesso_cdi_aa,
  na.rm = TRUE
)

mediana_hit = median(
  metricas_consistencia$hit_rate_36m,
  na.rm = TRUE
)

grafico_consistencia = ggplot(
  metricas_consistencia,
  aes(
    x = excesso_cdi_aa,
    y = hit_rate_36m,
    size = taxa_adm_aa
  )
) +
  geom_vline(
    xintercept = mediana_excesso,
    linetype = "dashed",
    linewidth = 0.5
  ) +
  geom_hline(
    yintercept = mediana_hit,
    linetype = "dashed",
    linewidth = 0.5
  ) +
  geom_point(
    alpha = 0.75
  ) +
  geom_text_repel(
    data = destaques_consistencia,
    aes(
      label = str_wrap(
        nome_plot,
        width = 24
      )
    ),
    size = 3,
    max.overlaps = Inf,
    box.padding = 0.4,
    point.padding = 0.3,
    show.legend = FALSE
  ) +
  scale_x_continuous(
    labels = percent_format(
      accuracy = 0.1,
      decimal.mark = ","
    )
  ) +
  scale_y_continuous(
    labels = percent_format(
      accuracy = 1,
      decimal.mark = ","
    ),
    limits = c(0, 1)
  ) +
  scale_size_continuous(
    labels = percent_format(
      accuracy = 0.1,
      decimal.mark = ","
    ),
    name = "Taxa de\nadministração"
  ) +
  labs(
    title = "Excesso sobre o CDI versus consistência",

    subtitle = paste(
      "Eixo vertical: proporção das janelas móveis",
      "de 36 meses acima do CDI"
    ),

    x = "Excesso anualizado sobre o CDI",

    y = "Hit rate das janelas de 36 meses",

    caption = paste(
      "Linhas tracejadas representam as medianas da amostra.",
      "A coluna n_janelas_36m informa quantas janelas completas",
      "foram usadas para cada fundo."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),

    plot.title = element_text(
      face = "bold"
    ),

    plot.caption = element_text(
      hjust = 0
    )
  )

print(grafico_consistencia)

ggsave(
  filename = "projects/criterios-selecao-fundos-cp/output/figures/grafico_consistencia_vs_excesso_36m.png",

  plot = grafico_consistencia,

  width = 11,

  height = 7,

  dpi = 300
)

# ------------------------------------------------------------
# 5. Afinidade com benchmarks
# ------------------------------------------------------------
# Comparações mensais:
# - correlação entre o excesso do fundo e o excesso do índice
#   sobre o CDI;
# - tracking error do retorno relativo fundo/índice;
# - excesso anualizado sobre o índice;
# - hit rate mensal contra o índice.

base_bench_long = fundos_mensais %>%
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
    cols = c(
      ret_ida_di_m,
      ret_ida_liq_di_m,
      ret_irfm_1_m
    ),
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

    excesso_benchmark_cdi_m = (1 + ret_benchmark_m) /
      (1 + ret_cdi_m) -
      1,

    retorno_relativo_benchmark_m = (1 + ret_fundo_m) /
      (1 + ret_benchmark_m) -
      1
  ) %>%
  filter(
    !is.na(excesso_benchmark_cdi_m),
    !is.na(retorno_relativo_benchmark_m)
  )

cor_segura = function(x, y) {
  completos = complete.cases(x, y)

  x = x[completos]
  y = y[completos]

  if (
    length(x) < 6 ||
      sd(x) == 0 ||
      sd(y) == 0
  ) {
    return(NA_real_)
  }

  cor(x, y)
}

afinidade_benchmarks = base_bench_long %>%
  group_by(
    nome_xlsx,
    nome_plot,
    nome_quantum,
    taxa_adm_aa,
    benchmark
  ) %>%
  summarise(
    n_meses = n(),

    correlacao = cor_segura(
      excesso_cdi_m,
      excesso_benchmark_cdi_m
    ),

    tracking_error_aa = sd(
      retorno_relativo_benchmark_m
    ) *
      sqrt(12),

    excesso_benchmark_aa = prod(
      1 + retorno_relativo_benchmark_m
    )^(12 / n_meses) -
      1,

    hit_rate_mensal = mean(
      retorno_relativo_benchmark_m > 0
    ),

    .groups = "drop"
  ) %>%
  filter(
    n_meses >= MIN_MESES_METRICA
  )

resumo_benchmark_proximo = afinidade_benchmarks %>%
  group_by(
    nome_xlsx,
    nome_plot,
    nome_quantum,
    taxa_adm_aa
  ) %>%
  summarise(
    benchmark_menor_tracking_error = benchmark[which.min(tracking_error_aa)],

    menor_tracking_error_aa = min(tracking_error_aa),

    benchmark_maior_correlacao = benchmark[which.max(correlacao)],

    maior_correlacao = max(correlacao, na.rm = TRUE),

    .groups = "drop"
  ) %>%
  arrange(menor_tracking_error_aa)

write_excel_csv2(
  afinidade_benchmarks,
  "projects/criterios-selecao-fundos-cp/data/intermediate/afinidade_benchmarks_36m.csv"
)

write_excel_csv2(
  resumo_benchmark_proximo,
  "projects/criterios-selecao-fundos-cp/data/intermediate/resumo_benchmark_proximo_36m.csv"
)

# ------------------------------------------------------------
# 5.1. Heatmap de afinidade com benchmarks
# ------------------------------------------------------------

ordem_afinidade = c(
  ordem_cluster,
  setdiff(
    unique(afinidade_benchmarks$nome_plot),
    ordem_cluster
  )
)

base_afinidade_plot = afinidade_benchmarks %>%
  mutate(
    nome_plot = factor(
      nome_plot,
      levels = rev(ordem_afinidade)
    ),

    benchmark = factor(
      benchmark,
      levels = c(
        "IDA LIQ-DI",
        "IDA-DI",
        "IRF-M 1"
      )
    ),

    rotulo = paste0(
      "\u03c1 ",
      number(
        correlacao,
        accuracy = 0.01,
        decimal.mark = ","
      ),
      "\nTE ",
      percent(
        tracking_error_aa,
        accuracy = 0.1,
        decimal.mark = ","
      )
    )
  )

grafico_afinidade = ggplot(
  base_afinidade_plot,
  aes(
    x = benchmark,
    y = nome_plot,
    fill = correlacao
  )
) +
  geom_tile(
    linewidth = 0.4,
    color = "white"
  ) +
  geom_text(
    aes(label = rotulo),
    size = 2.4
  ) +
  scale_fill_gradient2(
    low = "#B2182B",
    mid = "white",
    high = "#2166AC",
    midpoint = 0,
    limits = c(-1, 1),
    oob = squish,
    labels = number_format(
      accuracy = 0.1,
      decimal.mark = ","
    ),
    name = "Correlação"
  ) +
  scale_y_discrete(
    labels = function(x) {
      str_wrap(x, width = 30)
    }
  ) +
  labs(
    title = "Afinidade dos fundos com benchmarks",

    subtitle = "Correlação dos excessos sobre CDI e tracking error anualizado",

    x = NULL,
    y = NULL,

    caption = paste(
      "\u03c1: correlação mensal;",
      "TE: volatilidade anualizada do retorno relativo fundo/índice."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),

    axis.text.x = element_text(
      face = "bold"
    ),

    axis.text.y = element_text(
      size = 7
    ),

    plot.title = element_text(
      face = "bold"
    ),

    plot.caption = element_text(
      hjust = 0
    )
  )

print(grafico_afinidade)

altura_afinidade = max(
  10,
  n_distinct(afinidade_benchmarks$nome_plot) * 0.30
)

ggsave(
  filename = "projects/criterios-selecao-fundos-cp/output/figures/heatmap_afinidade_benchmarks_36m.png",

  plot = grafico_afinidade,

  width = 9,

  height = altura_afinidade,

  dpi = 300
)

# ------------------------------------------------------------
# 6. Base consolidada com todas as métricas
# ------------------------------------------------------------

# Universo elegível recebido da Etapa 1, já restrito aos fundos
# com 36 meses completos. Mantemos os status para auditoria.
universo_status = fundos_retornos %>%
  group_by(
    nome_xlsx,
    nome_plot,
    nome_quantum,
    taxa_adm_aa
  ) %>%
  summarise(
    inicio_serie_diaria = min(data),
    fim_serie_diaria = max(data),
    n_retornos_diarios = n(),
    revisar = any(revisar),
    .groups = "drop"
  ) %>%
  mutate(
    serie_atualizada = fim_serie_diaria == data_fim,

    elegivel_etapa2 = !revisar & serie_atualizada,

    motivo_inelegibilidade_etapa2 = case_when(
      revisar ~ "De-para pendente de revisão",
      !serie_atualizada ~ "Série não chega à data final comum",
      TRUE ~ NA_character_
    )
  )

# Métricas de afinidade em formato largo: uma coluna por
# benchmark e por indicador.
afinidade_wide = afinidade_benchmarks %>%
  mutate(
    benchmark_chave = recode(
      benchmark,
      "IDA-DI" = "ida_di",
      "IDA LIQ-DI" = "ida_liq_di",
      "IRF-M 1" = "irfm_1"
    )
  ) %>%
  select(
    nome_xlsx,
    nome_plot,
    nome_quantum,
    taxa_adm_aa,
    benchmark_chave,
    n_meses,
    correlacao,
    tracking_error_aa,
    excesso_benchmark_aa,
    hit_rate_mensal
  ) %>%
  pivot_wider(
    names_from = benchmark_chave,
    values_from = c(
      n_meses,
      correlacao,
      tracking_error_aa,
      excesso_benchmark_aa,
      hit_rate_mensal
    ),
    names_glue = "{.value}_{benchmark_chave}"
  )

metricas_todos_fundos = universo_status %>%
  left_join(
    metricas_consistencia %>%
      rename(
        inicio_serie_mensal = primeira_data,
        fim_serie_mensal = ultima_data,
        n_meses_completos = n_meses
      ),
    by = c(
      "nome_xlsx",
      "nome_plot",
      "nome_quantum",
      "taxa_adm_aa"
    ),
    relationship = "one-to-one"
  ) %>%
  left_join(
    resumo_correlacao %>%
      select(
        -cluster
      ),
    by = "nome_plot",
    relationship = "one-to-one"
  ) %>%
  left_join(
    resumo_benchmark_proximo,
    by = c(
      "nome_xlsx",
      "nome_plot",
      "nome_quantum",
      "taxa_adm_aa"
    ),
    relationship = "one-to-one"
  ) %>%
  left_join(
    afinidade_wide,
    by = c(
      "nome_xlsx",
      "nome_plot",
      "nome_quantum",
      "taxa_adm_aa"
    ),
    relationship = "one-to-one"
  ) %>%
  mutate(
    elegivel_ranking = elegivel_etapa2 &
      !is.na(n_meses_completos) &
      n_meses_completos >= MIN_MESES_RANKING &
      n_janelas_36m >= 1 &
      !is.na(excesso_cdi_aa) &
      !is.na(hit_rate_36m) &
      !is.na(max_drawdown_excesso) &
      !is.na(correlacao_media_pares) &
      !is.na(correlacao_maxima),

    motivo_inelegibilidade_ranking = case_when(
      !elegivel_etapa2 ~ motivo_inelegibilidade_etapa2,

      is.na(n_meses_completos) ~
        "Sem meses completos suficientes para cálculo",

      n_meses_completos < MIN_MESES_RANKING ~
        paste0(
          "Histórico inferior a ",
          MIN_MESES_RANKING,
          " meses completos"
        ),

      is.na(n_janelas_36m) | n_janelas_36m < 1 ~
        "Sem janela completa de 36 meses",

      is.na(correlacao_media_pares) |
        is.na(correlacao_maxima) ~
        "Sem histórico suficiente para correlação e clustering",

      TRUE ~ NA_character_
    )
  ) %>%
  arrange(
    desc(elegivel_ranking),
    desc(excesso_cdi_aa),
    nome_plot
  )

write_excel_csv2(
  metricas_todos_fundos,
  "projects/criterios-selecao-fundos-cp/data/intermediate/metricas_todos_fundos_36m.csv"
)

write_rds(
  metricas_todos_fundos,
  "projects/criterios-selecao-fundos-cp/data/intermediate/metricas_todos_fundos_36m.rds"
)

# ------------------------------------------------------------
# 7. Score preliminar, quartis e shortlist
