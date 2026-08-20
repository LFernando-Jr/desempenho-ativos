# ============================================================
# FUNDOS HIGH GRADE — ETAPA 2
# Base mensal, correlação e clustering, consistência,
# drawdown relativo ao CDI e afinidade com benchmarks.
#
# Pré-requisito:
# - executar antes o script da Etapa 1;
# - ter no diretório o arquivo fundos_retornos_etapa1.rds.
# ============================================================

library(tidyverse)
library(lubridate)
library(slider)
library(scales)
library(ggrepel)
library(openxlsx)

# ------------------------------------------------------------
# 0. Parâmetros e caminhos
# ------------------------------------------------------------

setwd("C:\\Users\\nandd\\Downloads\\selecao_credito")

arquivo_entrada <- "fundos_retornos_etapa1.rds"

MIN_OBS_MES       <- 15L
MIN_MESES_COR     <- 24L
MIN_MESES_METRICA <- 12L
N_CLUSTERS             <- 5L
MIN_MESES_RANKING       <- 24L
MAX_FUNDOS_POR_CLUSTER  <- 3L

if (!file.exists(arquivo_entrada)) {
  stop(
    "Arquivo não encontrado: ", arquivo_entrada,
    ". Execute primeiro o script da Etapa 1."
  )
}

fundos_retornos <- read_rds(arquivo_entrada) |>
  mutate(
    nome_plot = as.character(nome_plot)
  )

colunas_necessarias <- c(
  "nome_xlsx", "nome_plot", "nome_quantum", "taxa_adm_aa",
  "revisar", "data", "ret_liq", "ret_cdi",
  "ret_ida_di", "ret_ida_liq_di", "ret_irfm_1"
)

colunas_ausentes <- setdiff(
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
# Mantém fundos:
# - sem pendência de revisão;
# - cuja série chega à data máxima da base.

data_fim <- max(
  fundos_retornos$data,
  na.rm = TRUE
)

fundos_elegiveis <- fundos_retornos |>
  group_by(
    nome_xlsx,
    nome_plot,
    nome_quantum,
    taxa_adm_aa
  ) |>
  summarise(
    ultima_data = max(data),
    revisar = any(revisar),
    .groups = "drop"
  ) |>
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

fundos_mensais <- fundos_retornos |>
  semi_join(
    fundos_elegiveis,
    by = c(
      "nome_xlsx",
      "nome_plot",
      "nome_quantum",
      "taxa_adm_aa"
    )
  ) |>
  mutate(
    mes = floor_date(data, unit = "month")
  ) |>
  group_by(
    nome_xlsx,
    nome_plot,
    nome_quantum,
    taxa_adm_aa,
    mes
  ) |>
  summarise(
    primeira_data = min(data),
    ultima_data = max(data),
    n_obs = n(),

    ret_fundo_m =
      prod(1 + ret_liq, na.rm = TRUE) - 1,

    ret_cdi_m =
      prod(1 + ret_cdi, na.rm = TRUE) - 1,

    ret_ida_di_m =
      if_else(
        all(is.na(ret_ida_di)),
        NA_real_,
        prod(1 + ret_ida_di, na.rm = TRUE) - 1
      ),

    ret_ida_liq_di_m =
      if_else(
        all(is.na(ret_ida_liq_di)),
        NA_real_,
        prod(1 + ret_ida_liq_di, na.rm = TRUE) - 1
      ),

    ret_irfm_1_m =
      if_else(
        all(is.na(ret_irfm_1)),
        NA_real_,
        prod(1 + ret_irfm_1, na.rm = TRUE) - 1
      ),

    .groups = "drop"
  ) |>
  mutate(
    mes_completo =
      day(primeira_data) <= 7 &
      day(ultima_data) >= 24 &
      n_obs >= MIN_OBS_MES,

    excesso_cdi_m =
      (1 + ret_fundo_m) /
      (1 + ret_cdi_m) - 1,

    ida_di_xs_cdi_m =
      (1 + ret_ida_di_m) /
      (1 + ret_cdi_m) - 1,

    ida_liq_di_xs_cdi_m =
      (1 + ret_ida_liq_di_m) /
      (1 + ret_cdi_m) - 1,

    irfm_1_xs_cdi_m =
      (1 + ret_irfm_1_m) /
      (1 + ret_cdi_m) - 1
  ) |>
  filter(
    mes_completo,
    !is.na(excesso_cdi_m)
  ) |>
  arrange(nome_plot, mes)

cat(
  "Observações mensais completas:",
  nrow(fundos_mensais),
  "\n"
)

write_rds(
  fundos_mensais,
  "fundos_mensais_etapa2.rds"
)

write_excel_csv2(
  fundos_mensais,
  "fundos_mensais_etapa2.csv"
)

# ------------------------------------------------------------
# 3. Correlação dos excessos mensais sobre o CDI
# ------------------------------------------------------------

fundos_para_correlacao <- fundos_mensais |>
  group_by(
    nome_plot,
    nome_quantum
  ) |>
  summarise(
    n_meses = n(),
    desvio = sd(excesso_cdi_m, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(
    n_meses >= MIN_MESES_COR,
    is.finite(desvio),
    desvio > 0
  )

base_cor_wide <- fundos_mensais |>
  semi_join(
    fundos_para_correlacao,
    by = c("nome_plot", "nome_quantum")
  ) |>
  select(
    mes,
    nome_plot,
    excesso_cdi_m
  ) |>
  pivot_wider(
    names_from = nome_plot,
    values_from = excesso_cdi_m
  ) |>
  arrange(mes)

matriz_excessos <- base_cor_wide |>
  select(-mes) |>
  as.matrix()

matriz_cor <- cor(
  matriz_excessos,
  use = "pairwise.complete.obs",
  method = "pearson"
)

# Quantidade de meses em comum para cada par.
matriz_obs <- crossprod(
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

matriz_cor_cluster <- matriz_cor
matriz_cor_cluster[is.na(matriz_cor_cluster)] <- 0
diag(matriz_cor_cluster) <- 1

distancia_cor <- as.dist(
  1 - matriz_cor_cluster
)

cluster_hierarquico <- hclust(
  distancia_cor,
  method = "average"
)

ordem_cluster <- colnames(matriz_cor_cluster)[
  cluster_hierarquico$order
]

grupos_cluster <- cutree(
  cluster_hierarquico,
  k = min(
    N_CLUSTERS,
    ncol(matriz_cor_cluster)
  )
)

base_clusters <- tibble(
  nome_plot = names(grupos_cluster),
  cluster = as.integer(grupos_cluster)
) |>
  left_join(
    fundos_elegiveis |>
      select(
        nome_xlsx,
        nome_plot,
        nome_quantum,
        taxa_adm_aa
      ),
    by = "nome_plot",
    relationship = "one-to-one"
  ) |>
  arrange(cluster, nome_plot)

write_excel_csv2(
  base_clusters,
  "clusters_fundos.csv"
)

# ------------------------------------------------------------
# 3.1. Resumo de correlação de cada fundo
# ------------------------------------------------------------

resumo_correlacao <- map_dfr(
  seq_len(nrow(matriz_cor)),
  function(i) {
    nome_fundo <- rownames(matriz_cor)[i]

    correlacoes <- matriz_cor[i, ]
    observacoes <- matriz_obs[i, ]

    manter <- names(correlacoes) != nome_fundo

    correlacoes <- correlacoes[manter]
    observacoes <- observacoes[manter]

    validas <- is.finite(correlacoes)

    correlacoes_validas <- correlacoes[validas]
    observacoes_validas <- observacoes[validas]

    indice_max <- which.max(correlacoes_validas)

    tibble(
      nome_plot = nome_fundo,

      correlacao_media_pares =
        mean(correlacoes_validas),

      correlacao_mediana_pares =
        median(correlacoes_validas),

      correlacao_maxima =
        correlacoes_validas[indice_max],

      fundo_mais_correlacionado =
        names(correlacoes_validas)[indice_max],

      meses_em_comum_com_par_mais_proximo =
        observacoes_validas[indice_max]
    )
  }
) |>
  left_join(
    base_clusters |>
      select(nome_plot, cluster),
    by = "nome_plot",
    relationship = "one-to-one"
  ) |>
  arrange(desc(correlacao_media_pares))

write_excel_csv2(
  resumo_correlacao,
  "resumo_correlacao_fundos.csv"
)

# ------------------------------------------------------------
# 3.2. Heatmap da correlação
# ------------------------------------------------------------

base_cor_long <- as.data.frame(
  as.table(matriz_cor)
) |>
  as_tibble() |>
  rename(
    fundo_linha = Var1,
    fundo_coluna = Var2,
    correlacao = Freq
  ) |>
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

grafico_correlacao <- ggplot(
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
    title =
      "Correlação dos excessos mensais sobre o CDI",

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
  filename =
    "heatmap_correlacao_excessos_mensais.png",

  plot =
    grafico_correlacao,

  width =
    15,

  height =
    13,

  dpi =
    300
)

# Exporta a matriz em formato tabular.
write_excel_csv2(
  matriz_cor |>
    as.data.frame() |>
    rownames_to_column("nome_plot"),
  "matriz_correlacao_excessos_mensais.csv"
)

# ------------------------------------------------------------
# 3.3. Dendrograma
# ------------------------------------------------------------

png(
  filename =
    "dendrograma_fundos_excessos_mensais.png",

  width =
    2600,

  height =
    1500,

  res =
    200
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

fundos_mensais_rolling <- fundos_mensais |>
  group_by(
    nome_xlsx,
    nome_plot,
    nome_quantum,
    taxa_adm_aa
  ) |>
  arrange(mes, .by_group = TRUE) |>
  mutate(
    excesso_6m =
      slide_dbl(
        excesso_cdi_m,
        ~ prod(1 + .x) - 1,
        .before = 5,
        .complete = TRUE
      ),

    excesso_12m =
      slide_dbl(
        excesso_cdi_m,
        ~ prod(1 + .x) - 1,
        .before = 11,
        .complete = TRUE
      )
  ) |>
  ungroup()

calcula_drawdown <- function(retornos) {
  patrimonio_relativo <- cumprod(1 + retornos)
  pico <- cummax(patrimonio_relativo)
  drawdown <- patrimonio_relativo / pico - 1

  indice_fundo <- which.min(drawdown)
  max_drawdown <- drawdown[indice_fundo]

  nivel_pico_anterior <- pico[indice_fundo]

  indice_recuperacao <- which(
    seq_along(patrimonio_relativo) > indice_fundo &
      patrimonio_relativo >= nivel_pico_anterior
  )

  meses_recuperacao <- if (
    length(indice_recuperacao) == 0
  ) {
    NA_integer_
  } else {
    min(indice_recuperacao) - indice_fundo
  }

  tibble(
    max_drawdown_excesso =
      max_drawdown,

    meses_para_recuperar =
      meses_recuperacao
  )
}

autocor_lag1 <- function(x) {
  x <- x[is.finite(x)]

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

metricas_consistencia <- fundos_mensais_rolling |>
  group_by(
    nome_xlsx,
    nome_plot,
    nome_quantum,
    taxa_adm_aa
  ) |>
  filter(
    n() >= MIN_MESES_METRICA
  ) |>
  group_modify(
    ~ {
      base_fundo <- .x |>
        arrange(mes)

      excesso <- base_fundo$excesso_cdi_m
      excesso_6m <- base_fundo$excesso_6m
      excesso_12m <- base_fundo$excesso_12m

      n_meses <- length(excesso)

      tres_piores <- sort(
        excesso,
        na.last = NA
      )[
        seq_len(
          min(3, sum(is.finite(excesso)))
        )
      ]

      drawdown <- calcula_drawdown(excesso)

      tibble(
        primeira_data =
          min(base_fundo$mes),

        ultima_data =
          max(base_fundo$mes),

        n_meses =
          n_meses,

        excesso_cdi_aa =
          prod(1 + excesso)^(12 / n_meses) - 1,

        hit_rate_mensal =
          mean(excesso > 0),

        hit_rate_6m =
          mean(
            excesso_6m > 0,
            na.rm = TRUE
          ),

        hit_rate_12m =
          mean(
            excesso_12m > 0,
            na.rm = TRUE
          ),

        volatilidade_excesso_aa =
          sd(excesso) * sqrt(12),

        pior_mes =
          min(excesso),

        media_tres_piores_meses =
          mean(tres_piores),

        autocorrelacao_lag1 =
          autocor_lag1(excesso),

        max_drawdown_excesso =
          drawdown$max_drawdown_excesso,

        meses_para_recuperar =
          drawdown$meses_para_recuperar
      )
    }
  ) |>
  ungroup() |>
  left_join(
    base_clusters |>
      select(nome_plot, cluster),
    by = "nome_plot",
    relationship = "many-to-one"
  ) |>
  arrange(desc(excesso_cdi_aa))

write_excel_csv2(
  metricas_consistencia,
  "metricas_consistencia_risco.csv"
)

# ------------------------------------------------------------
# 4.1. Dispersão: retorno versus consistência
# ------------------------------------------------------------

destaques_consistencia <- bind_rows(
  metricas_consistencia |>
    slice_max(
      excesso_cdi_aa,
      n = 5,
      with_ties = FALSE
    ),

  metricas_consistencia |>
    slice_min(
      excesso_cdi_aa,
      n = 5,
      with_ties = FALSE
    ),

  metricas_consistencia |>
    slice_max(
      hit_rate_12m,
      n = 3,
      with_ties = FALSE
    )
) |>
  distinct(nome_plot, .keep_all = TRUE)

mediana_excesso <- median(
  metricas_consistencia$excesso_cdi_aa,
  na.rm = TRUE
)

mediana_hit <- median(
  metricas_consistencia$hit_rate_12m,
  na.rm = TRUE
)

grafico_consistencia <- ggplot(
  metricas_consistencia,
  aes(
    x = excesso_cdi_aa,
    y = hit_rate_12m,
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
    title =
      "Excesso sobre o CDI versus consistência",

    subtitle = paste(
      "Eixo vertical: proporção das janelas móveis",
      "de 12 meses acima do CDI"
    ),

    x =
      "Excesso anualizado sobre o CDI",

    y =
      "Hit rate das janelas de 12 meses",

    caption =
      "Linhas tracejadas representam as medianas da amostra."
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
  filename =
    "grafico_consistencia_vs_excesso.png",

  plot =
    grafico_consistencia,

  width =
    11,

  height =
    7,

  dpi =
    300
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

base_bench_long <- fundos_mensais |>
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
  ) |>
  pivot_longer(
    cols = c(
      ret_ida_di_m,
      ret_ida_liq_di_m,
      ret_irfm_1_m
    ),
    names_to = "benchmark",
    values_to = "ret_benchmark_m"
  ) |>
  mutate(
    benchmark = recode(
      benchmark,
      "ret_ida_di_m" = "IDA-DI",
      "ret_ida_liq_di_m" = "IDA LIQ-DI",
      "ret_irfm_1_m" = "IRF-M 1"
    ),

    excesso_benchmark_cdi_m =
      (1 + ret_benchmark_m) /
      (1 + ret_cdi_m) - 1,

    retorno_relativo_benchmark_m =
      (1 + ret_fundo_m) /
      (1 + ret_benchmark_m) - 1
  ) |>
  filter(
    !is.na(excesso_benchmark_cdi_m),
    !is.na(retorno_relativo_benchmark_m)
  )

cor_segura <- function(x, y) {
  completos <- complete.cases(x, y)

  x <- x[completos]
  y <- y[completos]

  if (
    length(x) < 6 ||
    sd(x) == 0 ||
    sd(y) == 0
  ) {
    return(NA_real_)
  }

  cor(x, y)
}

afinidade_benchmarks <- base_bench_long |>
  group_by(
    nome_xlsx,
    nome_plot,
    nome_quantum,
    taxa_adm_aa,
    benchmark
  ) |>
  summarise(
    n_meses = n(),

    correlacao =
      cor_segura(
        excesso_cdi_m,
        excesso_benchmark_cdi_m
      ),

    tracking_error_aa =
      sd(
        retorno_relativo_benchmark_m
      ) * sqrt(12),

    excesso_benchmark_aa =
      prod(
        1 + retorno_relativo_benchmark_m
      )^(12 / n_meses) - 1,

    hit_rate_mensal =
      mean(
        retorno_relativo_benchmark_m > 0
      ),

    .groups = "drop"
  ) |>
  filter(
    n_meses >= MIN_MESES_METRICA
  )

resumo_benchmark_proximo <- afinidade_benchmarks |>
  group_by(
    nome_xlsx,
    nome_plot,
    nome_quantum,
    taxa_adm_aa
  ) |>
  summarise(
    benchmark_menor_tracking_error =
      benchmark[which.min(tracking_error_aa)],

    menor_tracking_error_aa =
      min(tracking_error_aa),

    benchmark_maior_correlacao =
      benchmark[which.max(correlacao)],

    maior_correlacao =
      max(correlacao, na.rm = TRUE),

    .groups = "drop"
  ) |>
  arrange(menor_tracking_error_aa)

write_excel_csv2(
  afinidade_benchmarks,
  "afinidade_benchmarks.csv"
)

write_excel_csv2(
  resumo_benchmark_proximo,
  "resumo_benchmark_proximo.csv"
)

# ------------------------------------------------------------
# 5.1. Heatmap de afinidade com benchmarks
# ------------------------------------------------------------

ordem_afinidade <- c(
  ordem_cluster,
  setdiff(
    unique(afinidade_benchmarks$nome_plot),
    ordem_cluster
  )
)

base_afinidade_plot <- afinidade_benchmarks |>
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

grafico_afinidade <- ggplot(
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
    title =
      "Afinidade dos fundos com benchmarks",

    subtitle =
      "Correlação dos excessos sobre CDI e tracking error anualizado",

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

altura_afinidade <- max(
  10,
  n_distinct(afinidade_benchmarks$nome_plot) * 0.30
)

ggsave(
  filename =
    "heatmap_afinidade_benchmarks.png",

  plot =
    grafico_afinidade,

  width =
    9,

  height =
    altura_afinidade,

  dpi =
    300
)

# ------------------------------------------------------------
# 6. Base consolidada da Etapa 2
# ------------------------------------------------------------
# ------------------------------------------------------------
# 6. Base consolidada com todas as métricas
# ------------------------------------------------------------

# Universo completo da Etapa 1, inclusive fundos que não entraram
# nas métricas mensais por série defasada ou revisão pendente.
universo_status <- fundos_retornos |>
  group_by(
    nome_xlsx,
    nome_plot,
    nome_quantum,
    taxa_adm_aa
  ) |>
  summarise(
    inicio_serie_diaria = min(data),
    fim_serie_diaria = max(data),
    n_retornos_diarios = n(),
    revisar = any(revisar),
    .groups = "drop"
  ) |>
  mutate(
    serie_atualizada =
      fim_serie_diaria == data_fim,

    elegivel_etapa2 =
      !revisar & serie_atualizada,

    motivo_inelegibilidade_etapa2 = case_when(
      revisar ~ "De-para pendente de revisão",
      !serie_atualizada ~ "Série não chega à data final comum",
      TRUE ~ NA_character_
    )
  )

# Métricas de afinidade em formato largo: uma coluna por
# benchmark e por indicador.
afinidade_wide <- afinidade_benchmarks |>
  mutate(
    benchmark_chave = recode(
      benchmark,
      "IDA-DI" = "ida_di",
      "IDA LIQ-DI" = "ida_liq_di",
      "IRF-M 1" = "irfm_1"
    )
  ) |>
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
  ) |>
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

metricas_todos_fundos <- universo_status |>
  left_join(
    metricas_consistencia |>
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
  ) |>
  left_join(
    resumo_correlacao |>
      select(
        -cluster
      ),
    by = "nome_plot",
    relationship = "one-to-one"
  ) |>
  left_join(
    resumo_benchmark_proximo,
    by = c(
      "nome_xlsx",
      "nome_plot",
      "nome_quantum",
      "taxa_adm_aa"
    ),
    relationship = "one-to-one"
  ) |>
  left_join(
    afinidade_wide,
    by = c(
      "nome_xlsx",
      "nome_plot",
      "nome_quantum",
      "taxa_adm_aa"
    ),
    relationship = "one-to-one"
  ) |>
  mutate(
    elegivel_ranking =
      elegivel_etapa2 &
      !is.na(n_meses_completos) &
      n_meses_completos >= MIN_MESES_RANKING &
      !is.na(excesso_cdi_aa) &
      !is.na(hit_rate_12m) &
      !is.na(max_drawdown_excesso) &
      !is.na(correlacao_media_pares),

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

      is.na(correlacao_media_pares) ~
        "Sem histórico suficiente para correlação e clustering",

      TRUE ~ NA_character_
    )
  ) |>
  arrange(
    desc(elegivel_ranking),
    desc(excesso_cdi_aa),
    nome_plot
  )

write_excel_csv2(
  metricas_todos_fundos,
  "metricas_todos_fundos.csv"
)

write_rds(
  metricas_todos_fundos,
  "metricas_todos_fundos.rds"
)

# ------------------------------------------------------------
# 7. Score preliminar, quartis e shortlist
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
      0.40 * score_maior_melhor(hit_rate_12m),

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
    hit_12m,
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

  if (is.finite(hit_12m) && hit_12m < 0.50) {
    flags <- c(
      flags,
      "Menos de 50% das janelas de 12m acima do CDI"
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
        hit_rate_12m,
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
    )
  ) |>
  arrange(ranking_geral)

write_excel_csv2(
  ranking_fundos,
  "ranking_fundos_etapa2.csv"
)

write_rds(
  ranking_fundos,
  "ranking_fundos_etapa2.rds"
)

# ------------------------------------------------------------
# 7.1. Visualização do ranking
# ------------------------------------------------------------

grafico_ranking <- ranking_fundos |>
  mutate(
    nome_plot = fct_reorder(
      nome_plot,
      score_preliminar
    )
  ) |>
  ggplot(
    aes(
      x = score_preliminar,
      y = nome_plot,
      fill = classificacao
    )
  ) +
  geom_col() +
  scale_x_continuous(
    labels = percent_format(
      accuracy = 1,
      decimal.mark = ","
    )
  ) +
  labs(
    title =
      "Score preliminar dos fundos high grade",

    subtitle = paste(
      "Retorno 20% | Consistência 25% | Risco 20% |",
      "Custo 25% | Diferenciação 10%"
    ),

    x =
      "Nota final",

    y =
      NULL,

    fill =
      NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor =
      element_blank(),

    plot.title =
      element_text(face = "bold"),

    axis.text.y =
      element_text(size = 7)
  )

print(grafico_ranking)

altura_ranking <- max(
  9,
  nrow(ranking_fundos) * 0.28
)

ggsave(
  filename =
    "grafico_ranking_fundos.png",

  plot =
    grafico_ranking,

  width =
    11,

  height =
    altura_ranking,

  dpi =
    300
)

# ------------------------------------------------------------
# 8. Workbook XLSX
# ------------------------------------------------------------

arquivo_xlsx_saida <-
  "analise_high_grade_etapa2.xlsx"

# 8.1. Tabela com todos os fundos e todas as métricas.
todos_fundos_xlsx <- metricas_todos_fundos |>
  transmute(
    Fundo = nome_plot,
    `Nome oficial` = nome_xlsx,
    `Nome Quantum` = nome_quantum,
    `Taxa de administração a.a.` = taxa_adm_aa,
    `Revisar de-para` = revisar,
    `Série atualizada` = serie_atualizada,
    `Elegível Etapa 2` = elegivel_etapa2,
    `Motivo inelegibilidade Etapa 2` =
      motivo_inelegibilidade_etapa2,
    `Elegível ranking` = elegivel_ranking,
    `Motivo inelegibilidade ranking` =
      motivo_inelegibilidade_ranking,
    `Início série diária` = inicio_serie_diaria,
    `Fim série diária` = fim_serie_diaria,
    `Retornos diários` = n_retornos_diarios,
    `Início série mensal` = inicio_serie_mensal,
    `Fim série mensal` = fim_serie_mensal,
    `Meses completos` = n_meses_completos,
    Cluster = cluster,
    `Excesso CDI a.a.` = excesso_cdi_aa,
    `Hit rate mensal` = hit_rate_mensal,
    `Hit rate 6 meses` = hit_rate_6m,
    `Hit rate 12 meses` = hit_rate_12m,
    `Volatilidade do excesso a.a.` =
      volatilidade_excesso_aa,
    `Pior mês relativo ao CDI` = pior_mes,
    `Média dos 3 piores meses` =
      media_tres_piores_meses,
    `Autocorrelação lag 1` =
      autocorrelacao_lag1,
    `Drawdown máximo do excesso` =
      max_drawdown_excesso,
    `Meses para recuperar` =
      meses_para_recuperar,
    `Correlação média com pares` =
      correlacao_media_pares,
    `Correlação mediana com pares` =
      correlacao_mediana_pares,
    `Correlação máxima com par` =
      correlacao_maxima,
    `Fundo mais correlacionado` =
      fundo_mais_correlacionado,
    `Meses em comum com par mais próximo` =
      meses_em_comum_com_par_mais_proximo,
    `Benchmark de menor tracking error` =
      benchmark_menor_tracking_error,
    `Menor tracking error a.a.` =
      menor_tracking_error_aa,
    `Benchmark de maior correlação` =
      benchmark_maior_correlacao,
    `Maior correlação com benchmark` =
      maior_correlacao,
    `Meses IDA LIQ-DI` =
      n_meses_ida_liq_di,
    `Correlação IDA LIQ-DI` =
      correlacao_ida_liq_di,
    `Tracking error IDA LIQ-DI a.a.` =
      tracking_error_aa_ida_liq_di,
    `Excesso vs. IDA LIQ-DI a.a.` =
      excesso_benchmark_aa_ida_liq_di,
    `Hit rate mensal vs. IDA LIQ-DI` =
      hit_rate_mensal_ida_liq_di,
    `Meses IDA-DI` =
      n_meses_ida_di,
    `Correlação IDA-DI` =
      correlacao_ida_di,
    `Tracking error IDA-DI a.a.` =
      tracking_error_aa_ida_di,
    `Excesso vs. IDA-DI a.a.` =
      excesso_benchmark_aa_ida_di,
    `Hit rate mensal vs. IDA-DI` =
      hit_rate_mensal_ida_di,
    `Meses IRF-M 1` =
      n_meses_irfm_1,
    `Correlação IRF-M 1` =
      correlacao_irfm_1,
    `Tracking error IRF-M 1 a.a.` =
      tracking_error_aa_irfm_1,
    `Excesso vs. IRF-M 1 a.a.` =
      excesso_benchmark_aa_irfm_1,
    `Hit rate mensal vs. IRF-M 1` =
      hit_rate_mensal_irfm_1
  )

# 8.2. Ranking com nota, quartil e abertura dos blocos.
ranking_xlsx <- ranking_fundos |>
  transmute(
    Ranking = ranking_geral,
    Fundo = nome_plot,
    Cluster = cluster,
    `Ranking no cluster` = ranking_no_cluster,
    `Nota final` = 100 * score_preliminar,
    Quartil = quartil_score,
    Classificação = classificacao,
    Shortlist = if_else(shortlist, "SIM", "NÃO"),
    `Status da triagem` = status_triagem,
    `Red flags` = red_flags,
    `Nota retorno` = 100 * score_retorno,
    `Nota consistência` = 100 * score_consistencia,
    `Nota risco` = 100 * score_risco,
    `Nota custo` = 100 * score_custo,
    `Nota diferenciação` = 100 * score_diferenciacao,
    `Nota taxa` = 100 * score_taxa,
    `Nota eficiência` = 100 * score_eficiencia,
    `Excesso CDI a.a.` = excesso_cdi_aa,
    `Hit rate mensal` = hit_rate_mensal,
    `Hit rate 6 meses` = hit_rate_6m,
    `Hit rate 12 meses` = hit_rate_12m,
    `Drawdown máximo do excesso` =
      max_drawdown_excesso,
    `Média dos 3 piores meses` =
      media_tres_piores_meses,
    `Volatilidade do excesso a.a.` =
      volatilidade_excesso_aa,
    `Taxa de administração a.a.` =
      taxa_adm_aa,
    `Eficiência do custo` =
      eficiencia_custo,
    `Correlação média com pares` =
      correlacao_media_pares,
    `Correlação máxima com par` =
      correlacao_maxima,
    `Fundo mais correlacionado` =
      fundo_mais_correlacionado,
    `Benchmark de menor tracking error` =
      benchmark_menor_tracking_error,
    `Menor tracking error a.a.` =
      menor_tracking_error_aa
  )

wb <- createWorkbook(
  creator = "Análise de fundos high grade"
)

addWorksheet(
  wb,
  "Todos os Fundos",
  gridLines = FALSE
)

addWorksheet(
  wb,
  "Ranking",
  gridLines = FALSE
)

# Estilos.
estilo_titulo <- createStyle(
  fontSize = 14,
  fontColour = "#FFFFFF",
  fgFill = "#1F4E78",
  textDecoration = "bold",
  halign = "center",
  valign = "center"
)

estilo_subtitulo <- createStyle(
  fontSize = 10,
  fontColour = "#1F1F1F",
  fgFill = "#D9EAF7",
  textDecoration = "bold",
  halign = "center",
  valign = "center",
  wrapText = TRUE
)

estilo_percentual <- createStyle(
  numFmt = "0.00%"
)

estilo_numero <- createStyle(
  numFmt = "0.00"
)

estilo_nota <- createStyle(
  numFmt = "0.0"
)

estilo_data <- createStyle(
  numFmt = "dd/mm/yyyy"
)

estilo_inteiro <- createStyle(
  numFmt = "0"
)

estilo_wrap <- createStyle(
  wrapText = TRUE,
  valign = "top"
)

# Função para aplicar estilo por nome de coluna.
aplica_estilo_colunas <- function(
    wb,
    aba,
    dados,
    linha_cabecalho,
    colunas,
    estilo
) {
  indices <- which(
    names(dados) %in% colunas
  )

  if (
    length(indices) > 0 &&
    nrow(dados) > 0
  ) {
    addStyle(
      wb,
      sheet = aba,
      style = estilo,
      rows =
        (linha_cabecalho + 1):
        (linha_cabecalho + nrow(dados)),
      cols = indices,
      gridExpand = TRUE,
      stack = TRUE
    )
  }
}

# ------------------------------------------------------------
# 8.3. Aba Todos os Fundos
# ------------------------------------------------------------

writeDataTable(
  wb,
  sheet = "Todos os Fundos",
  x = todos_fundos_xlsx,
  startRow = 1,
  startCol = 1,
  tableStyle = "TableStyleMedium2",
  withFilter = TRUE
)

freezePane(
  wb,
  sheet = "Todos os Fundos",
  firstActiveRow = 2,
  firstActiveCol = 2
)

setColWidths(
  wb,
  sheet = "Todos os Fundos",
  cols = 1:ncol(todos_fundos_xlsx),
  widths = 14
)

setColWidths(
  wb,
  sheet = "Todos os Fundos",
  cols = c(1, 2, 3),
  widths = c(30, 45, 45)
)

colunas_texto_todos <- c(
  "Motivo inelegibilidade Etapa 2",
  "Motivo inelegibilidade ranking",
  "Fundo mais correlacionado",
  "Benchmark de menor tracking error",
  "Benchmark de maior correlação"
)

setColWidths(
  wb,
  sheet = "Todos os Fundos",
  cols = which(
    names(todos_fundos_xlsx) %in%
      colunas_texto_todos
  ),
  widths = 28
)

addStyle(
  wb,
  sheet = "Todos os Fundos",
  style = estilo_wrap,
  rows = 1:(nrow(todos_fundos_xlsx) + 1),
  cols = 1:ncol(todos_fundos_xlsx),
  gridExpand = TRUE,
  stack = TRUE
)

percentuais_todos <- c(
  "Taxa de administração a.a.",
  "Excesso CDI a.a.",
  "Hit rate mensal",
  "Hit rate 6 meses",
  "Hit rate 12 meses",
  "Volatilidade do excesso a.a.",
  "Pior mês relativo ao CDI",
  "Média dos 3 piores meses",
  "Drawdown máximo do excesso",
  "Menor tracking error a.a.",
  "Tracking error IDA LIQ-DI a.a.",
  "Excesso vs. IDA LIQ-DI a.a.",
  "Hit rate mensal vs. IDA LIQ-DI",
  "Tracking error IDA-DI a.a.",
  "Excesso vs. IDA-DI a.a.",
  "Hit rate mensal vs. IDA-DI",
  "Tracking error IRF-M 1 a.a.",
  "Excesso vs. IRF-M 1 a.a.",
  "Hit rate mensal vs. IRF-M 1"
)

aplica_estilo_colunas(
  wb,
  "Todos os Fundos",
  todos_fundos_xlsx,
  1,
  percentuais_todos,
  estilo_percentual
)

aplica_estilo_colunas(
  wb,
  "Todos os Fundos",
  todos_fundos_xlsx,
  1,
  c(
    "Início série diária",
    "Fim série diária",
    "Início série mensal",
    "Fim série mensal"
  ),
  estilo_data
)

aplica_estilo_colunas(
  wb,
  "Todos os Fundos",
  todos_fundos_xlsx,
  1,
  c(
    "Retornos diários",
    "Meses completos",
    "Cluster",
    "Meses para recuperar",
    "Meses em comum com par mais próximo",
    "Meses IDA LIQ-DI",
    "Meses IDA-DI",
    "Meses IRF-M 1"
  ),
  estilo_inteiro
)

# ------------------------------------------------------------
# 8.4. Aba Ranking
# ------------------------------------------------------------

mergeCells(
  wb,
  sheet = "Ranking",
  cols = 1:10,
  rows = 1
)

writeData(
  wb,
  sheet = "Ranking",
  x = "Ranking preliminar — fundos de crédito high grade",
  startRow = 1,
  startCol = 1
)

addStyle(
  wb,
  sheet = "Ranking",
  style = estilo_titulo,
  rows = 1,
  cols = 1:10,
  gridExpand = TRUE
)

writeData(
  wb,
  sheet = "Ranking",
  x = data.frame(
    Retorno = "20%",
    Consistência = "25%",
    Risco = "20%",
    Custo = "25%",
    Diferenciação = "10%",
    `Máx. por cluster` =
      MAX_FUNDOS_POR_CLUSTER
  ),
  startRow = 2,
  startCol = 1,
  colNames = TRUE
)

addStyle(
  wb,
  sheet = "Ranking",
  style = estilo_subtitulo,
  rows = 2:3,
  cols = 1:6,
  gridExpand = TRUE,
  stack = TRUE
)

writeDataTable(
  wb,
  sheet = "Ranking",
  x = ranking_xlsx,
  startRow = 5,
  startCol = 1,
  tableStyle = "TableStyleMedium2",
  withFilter = TRUE
)

freezePane(
  wb,
  sheet = "Ranking",
  firstActiveRow = 6,
  firstActiveCol = 3
)

setColWidths(
  wb,
  sheet = "Ranking",
  cols = 1:ncol(ranking_xlsx),
  widths = 14
)

setColWidths(
  wb,
  sheet = "Ranking",
  cols = 2,
  widths = 32
)

setColWidths(
  wb,
  sheet = "Ranking",
  cols = which(
    names(ranking_xlsx) %in%
      c(
        "Status da triagem",
        "Red flags",
        "Fundo mais correlacionado",
        "Benchmark de menor tracking error"
      )
  ),
  widths = 28
)

addStyle(
  wb,
  sheet = "Ranking",
  style = estilo_wrap,
  rows = 5:(nrow(ranking_xlsx) + 5),
  cols = 1:ncol(ranking_xlsx),
  gridExpand = TRUE,
  stack = TRUE
)

aplica_estilo_colunas(
  wb,
  "Ranking",
  ranking_xlsx,
  5,
  c(
    "Nota final",
    "Nota retorno",
    "Nota consistência",
    "Nota risco",
    "Nota custo",
    "Nota diferenciação",
    "Nota taxa",
    "Nota eficiência",
    "Eficiência do custo"
  ),
  estilo_nota
)

aplica_estilo_colunas(
  wb,
  "Ranking",
  ranking_xlsx,
  5,
  c(
    "Excesso CDI a.a.",
    "Hit rate mensal",
    "Hit rate 6 meses",
    "Hit rate 12 meses",
    "Drawdown máximo do excesso",
    "Média dos 3 piores meses",
    "Volatilidade do excesso a.a.",
    "Taxa de administração a.a.",
    "Menor tracking error a.a."
  ),
  estilo_percentual
)

aplica_estilo_colunas(
  wb,
  "Ranking",
  ranking_xlsx,
  5,
  c(
    "Ranking",
    "Cluster",
    "Ranking no cluster",
    "Quartil"
  ),
  estilo_inteiro
)

# Escala de cores na nota final.
col_nota_final <- which(
  names(ranking_xlsx) == "Nota final"
)

if (
  length(col_nota_final) == 1 &&
  nrow(ranking_xlsx) > 0
) {
  conditionalFormatting(
    wb,
    sheet = "Ranking",
    cols = col_nota_final,
    rows = 6:(nrow(ranking_xlsx) + 5),
    type = "colourScale",
    style = c(
      "#F8696B",
      "#FFEB84",
      "#63BE7B"
    )
  )
}

saveWorkbook(
  wb,
  file = arquivo_xlsx_saida,
  overwrite = TRUE
)

# ------------------------------------------------------------
# 9. Bases consolidadas
# ------------------------------------------------------------

resumo_etapa2 <- metricas_todos_fundos |>
  arrange(
    desc(elegivel_ranking),
    desc(excesso_cdi_aa)
  )

write_excel_csv2(
  resumo_etapa2,
  "resumo_etapa2.csv"
)

write_rds(
  resumo_etapa2,
  "resumo_etapa2.rds"
)

# ------------------------------------------------------------
# 10. Encerramento
# ------------------------------------------------------------

cat("\nEtapa 2 concluída. Arquivos gerados:\n")
cat("- fundos_mensais_etapa2.rds\n")
cat("- fundos_mensais_etapa2.csv\n")
cat("- matriz_correlacao_excessos_mensais.csv\n")
cat("- heatmap_correlacao_excessos_mensais.png\n")
cat("- dendrograma_fundos_excessos_mensais.png\n")
cat("- clusters_fundos.csv\n")
cat("- resumo_correlacao_fundos.csv\n")
cat("- metricas_consistencia_risco.csv\n")
cat("- grafico_consistencia_vs_excesso.png\n")
cat("- afinidade_benchmarks.csv\n")
cat("- resumo_benchmark_proximo.csv\n")
cat("- heatmap_afinidade_benchmarks.png\n")
cat("- metricas_todos_fundos.csv\n")
cat("- metricas_todos_fundos.rds\n")
cat("- ranking_fundos_etapa2.csv\n")
cat("- ranking_fundos_etapa2.rds\n")
cat("- grafico_ranking_fundos.png\n")
cat("- analise_high_grade_etapa2.xlsx\n")
cat("- resumo_etapa2.csv\n")
cat("- resumo_etapa2.rds\n")
