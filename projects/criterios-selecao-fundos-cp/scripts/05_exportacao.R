# ETAPA 5 — GRÁFICOS E EXPORTAÇÃO
# Execute preferencialmente por 00_run_all.R.


cores_quartis <- c(
  "Q1 - Destaque" = "#F8766D",
  "Q2 - Aprovado" = "#7CAE00",
  "Q3 - Observação" = "#00BFC4",
  "Q4 - Descartado" = "#C77CFF"
)

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
  scale_fill_manual(
    values = cores_quartis,
    drop = FALSE
  ) +
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
      "Retorno 20% | Consistência de 36 meses 25% |",
      "Risco 20% | Custo 25% | Diferenciação 10%"
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
    "output/figures/grafico_ranking_fundos_36m.png",
  
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
# 7.2. Ranking individual de cada bloco do score
# ------------------------------------------------------------
# Cada bloco recebe seus próprios quartis.
# Assim, Q1 em custo significa o primeiro quartil do score de custo,
# e não necessariamente Q1 no ranking final.

base_scores_long <- ranking_fundos |>
  select(
    nome_plot,
    score_retorno,
    score_consistencia,
    score_risco,
    score_custo,
    score_diferenciacao
  ) |>
  pivot_longer(
    cols = starts_with("score_"),
    names_to = "bloco",
    values_to = "nota"
  ) |>
  group_by(bloco) |>
  mutate(
    quartil_bloco =
      ntile(desc(nota), 4),
    
    classificacao_bloco = case_when(
      quartil_bloco == 1 ~ "Q1 - Destaque",
      quartil_bloco == 2 ~ "Q2 - Aprovado",
      quartil_bloco == 3 ~ "Q3 - Observação",
      quartil_bloco == 4 ~ "Q4 - Descartado"
    )
  ) |>
  ungroup()

config_blocos <- tribble(
  ~bloco_id,               ~titulo,                    ~subtitulo,                                              ~arquivo,
  "score_retorno",         "Score de retorno",         "Excesso anualizado sobre o CDI",                       "output/figures/grafico_score_retorno_36m.png",
  "score_consistencia",    "Score de consistência",    "Hit rates mensal e em janelas móveis de 6 e 36 meses", "output/figures/grafico_score_consistencia_36m.png",
  "score_risco",           "Score de risco",           "Drawdown, cauda negativa e volatilidade do excesso",   "output/figures/grafico_score_risco_36m.png",
  "score_custo",           "Score de custo",           "Taxa de administração e eficiência do custo",          "output/figures/grafico_score_custo_36m.png",
  "score_diferenciacao",   "Score de diferenciação",   "Correlação média e máxima com os demais fundos",        "output/figures/grafico_score_diferenciacao_36m.png"
)

gera_grafico_bloco <- function(
    bloco_id,
    titulo,
    subtitulo,
    arquivo
) {
  dados_bloco <- base_scores_long |>
    filter(.data$bloco == bloco_id) |>
    mutate(
      nome_plot =
        fct_reorder(nome_plot, nota),
      
      classificacao_bloco =
        factor(
          classificacao_bloco,
          levels = names(cores_quartis)
        )
    )
  
  grafico <- ggplot(
    dados_bloco,
    aes(
      x = nota,
      y = nome_plot,
      fill = classificacao_bloco
    )
  ) +
    geom_col() +
    scale_fill_manual(
      values = cores_quartis,
      drop = FALSE
    ) +
    scale_x_continuous(
      labels = percent_format(
        accuracy = 1,
        decimal.mark = ","
      ),
      limits = c(0, 1)
    ) +
    labs(
      title = titulo,
      subtitle = subtitulo,
      x = "Nota do bloco",
      y = NULL,
      fill = NULL
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
  
  print(grafico)
  
  ggsave(
    filename = arquivo,
    plot = grafico,
    width = 11,
    height = altura_ranking,
    dpi = 300
  )
}

pwalk(
  config_blocos,
  gera_grafico_bloco
)

# Base tabular dos quartis por bloco.
quartis_blocos <- base_scores_long |>
  select(
    nome_plot,
    bloco,
    nota,
    quartil_bloco,
    classificacao_bloco
  ) |>
  arrange(
    bloco,
    desc(nota)
  )

write_excel_csv2(
  quartis_blocos,
  "data/intermediate/quartis_por_bloco_36m.csv"
)

# ------------------------------------------------------------
# 7.3. Retorno acumulado relativo ao IDA
# ------------------------------------------------------------
# O gráfico mostra o excesso acumulado:
# (1 + retorno do fundo) / (1 + retorno do IDA) - 1.
# A linha zero representa desempenho igual ao benchmark.

benchmark_config <- switch(
  BENCHMARK_ACUMULADO,
  
  "IDA-DI" = list(
    coluna = "ret_ida_di_m",
    rotulo = "IDA-DI",
    slug = "ida_di"
  ),
  
  "IDA LIQ-DI" = list(
    coluna = "ret_ida_liq_di_m",
    rotulo = "IDA LIQ-DI",
    slug = "ida_liq_di"
  ),
  
  stop(
    "BENCHMARK_ACUMULADO deve ser ",
    "'IDA-DI' ou 'IDA LIQ-DI'."
  )
)

data_fim_mensal <- floor_date(
  data_fim,
  unit = "month"
)

data_inicio_acumulado <- data_fim_mensal %m-%
  months(JANELA_ACUMULADA_MESES - 1L)

base_retorno_acumulado <- fundos_mensais |>
  inner_join(
    ranking_fundos |>
      select(
        nome_plot,
        quartil_score,
        classificacao,
        shortlist
      ),
    by = "nome_plot",
    relationship = "many-to-one"
  ) |>
  mutate(
    ret_benchmark_m =
      .data[[benchmark_config$coluna]]
  ) |>
  filter(
    mes >= data_inicio_acumulado,
    mes <= data_fim_mensal,
    !is.na(ret_benchmark_m)
  ) |>
  group_by(
    nome_plot,
    quartil_score,
    classificacao,
    shortlist
  ) |>
  filter(
    n_distinct(mes) ==
      JANELA_ACUMULADA_MESES,
    
    min(mes) ==
      data_inicio_acumulado,
    
    max(mes) ==
      data_fim_mensal
  ) |>
  arrange(mes, .by_group = TRUE) |>
  mutate(
    retorno_relativo_m =
      (1 + ret_fundo_m) /
      (1 + ret_benchmark_m) - 1,
    
    retorno_relativo_acumulado =
      cumprod(
        1 + retorno_relativo_m
      ) - 1,
    
    destaque_linha =
      if_else(
        shortlist,
        "Shortlist",
        "Demais fundos"
      )
  ) |>
  ungroup()

rotulos_retorno_acumulado <- base_retorno_acumulado |>
  group_by(
    nome_plot,
    classificacao,
    destaque_linha
  ) |>
  slice_max(
    mes,
    n = 1,
    with_ties = FALSE
  ) |>
  ungroup()

cores_linhas_acumulado <- c(
  "Shortlist" = "#1F77B4",
  "Demais fundos" = "#B7B7B7"
)

grafico_retorno_acumulado <- ggplot(
  base_retorno_acumulado,
  aes(
    x = mes,
    y = retorno_relativo_acumulado,
    group = nome_plot,
    color = destaque_linha
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 0.5
  ) +
  geom_line(
    linewidth = 0.65,
    alpha = 0.80
  ) +
  geom_text_repel(
    data = rotulos_retorno_acumulado,
    aes(
      label = str_trunc(
        nome_plot,
        width = 24
      )
    ),
    size = 2.5,
    hjust = 0,
    direction = "y",
    nudge_x = 35,
    segment.size = 0.25,
    min.segment.length = 0,
    max.overlaps = Inf,
    show.legend = FALSE
  ) +
  facet_wrap(
    ~ classificacao,
    ncol = 2,
    scales = "free_y"
  ) +
  scale_color_manual(
    values = cores_linhas_acumulado,
    name = NULL
  ) +
  scale_x_date(
    date_breaks = "6 months",
    date_labels = "%b/%y",
    expand = expansion(
      mult = c(0.01, 0.24)
    )
  ) +
  scale_y_continuous(
    labels = percent_format(
      accuracy = 0.1,
      decimal.mark = ","
    )
  ) +
  coord_cartesian(
    clip = "off"
  ) +
  labs(
    title = paste0(
      "Retorno acumulado relativo ao ",
      benchmark_config$rotulo
    ),
    
    subtitle = paste0(
      "Últimos ",
      JANELA_ACUMULADA_MESES,
      " meses completos | Positivo indica desempenho acima do benchmark"
    ),
    
    x = NULL,
    
    y = paste0(
      "Retorno acumulado relativo ao ",
      benchmark_config$rotulo
    ),
    
    caption = paste(
      "Painéis definidos pelo quartil do score final.",
      "Fundos da shortlist aparecem destacados."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor =
      element_blank(),
    
    plot.title =
      element_text(face = "bold"),
    
    plot.caption =
      element_text(hjust = 0),
    
    strip.text =
      element_text(face = "bold"),
    
    plot.margin =
      margin(
        t = 10,
        r = 150,
        b = 10,
        l = 10
      )
  )

print(grafico_retorno_acumulado)

arquivo_grafico_acumulado <- file.path(
  "output",
  "figures",
  paste0(
    "grafico_retorno_acumulado_vs_",
    benchmark_config$slug,
    "_",
    JANELA_ACUMULADA_MESES,
    "m.png"
  )
)

ggsave(
  filename =
    arquivo_grafico_acumulado,
  
  plot =
    grafico_retorno_acumulado,
  
  width =
    16,
  
  height =
    11,
  
  dpi =
    300
)

write_excel_csv2(
  base_retorno_acumulado,
  "data/intermediate/base_retorno_acumulado_vs_ida_36m.csv"
)

# ------------------------------------------------------------
# 8. Workbook XLSX
# ------------------------------------------------------------

arquivo_xlsx_saida <-
  "output/reports/analise_high_grade_etapa2_36m.xlsx"

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
    `Janelas completas de 36 meses` = n_janelas_36m,
    `Hit rate 36 meses` = hit_rate_36m,
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
    `Quartil retorno` = quartil_retorno,
    `Nota consistência` = 100 * score_consistencia,
    `Quartil consistência` = quartil_consistencia,
    `Nota risco` = 100 * score_risco,
    `Quartil risco` = quartil_risco,
    `Nota custo` = 100 * score_custo,
    `Quartil custo` = quartil_custo,
    `Nota diferenciação` = 100 * score_diferenciacao,
    `Quartil diferenciação` = quartil_diferenciacao,
    `Nota taxa` = 100 * score_taxa,
    `Nota eficiência` = 100 * score_eficiencia,
    `Excesso CDI a.a.` = excesso_cdi_aa,
    `Hit rate mensal` = hit_rate_mensal,
    `Hit rate 6 meses` = hit_rate_6m,
    `Janelas completas de 36 meses` = n_janelas_36m,
    `Hit rate 36 meses` = hit_rate_36m,
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
  "Hit rate 36 meses",
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
    "Janelas completas de 36 meses",
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
    "Hit rate 36 meses",
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
    "Janelas completas de 36 meses",
    "Quartil",
    "Quartil retorno",
    "Quartil consistência",
    "Quartil risco",
    "Quartil custo",
    "Quartil diferenciação"
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
  "data/intermediate/resumo_etapa2_36m.csv"
)

write_rds(
  resumo_etapa2,
  "data/intermediate/resumo_etapa2_36m.rds"
)

# ------------------------------------------------------------
# 10. Encerramento
# ------------------------------------------------------------

cat("\nEtapa 2 concluída. Arquivos gerados:\n")
cat("- fundos_mensais_etapa2_36m.rds\n")
cat("- fundos_mensais_etapa2_36m.csv\n")
cat("- matriz_correlacao_excessos_mensais_36m.csv\n")
cat("- heatmap_correlacao_excessos_mensais_36m.png\n")
cat("- dendrograma_fundos_excessos_mensais_36m.png\n")
cat("- clusters_fundos_36m.csv\n")
cat("- resumo_correlacao_fundos_36m.csv\n")
cat("- metricas_consistencia_risco_36m.csv\n")
cat("- grafico_consistencia_vs_excesso_36m.png\n")
cat("- afinidade_benchmarks_36m.csv\n")
cat("- resumo_benchmark_proximo_36m.csv\n")
cat("- heatmap_afinidade_benchmarks_36m.png\n")
cat("- metricas_todos_fundos_36m.csv\n")
cat("- metricas_todos_fundos_36m.rds\n")
cat("- ranking_fundos_etapa2_36m.csv\n")
cat("- ranking_fundos_etapa2_36m.rds\n")
cat("- grafico_ranking_fundos_36m.png\n")
cat("- grafico_score_retorno_36m.png\n")
cat("- grafico_score_consistencia_36m.png\n")
cat("- grafico_score_risco_36m.png\n")
cat("- grafico_score_custo_36m.png\n")
cat("- grafico_score_diferenciacao_36m.png\n")
cat("- quartis_por_bloco_36m.csv\n")
cat("- base_retorno_acumulado_vs_ida_36m.csv\n")
cat("- ", arquivo_grafico_acumulado, "\n", sep = "")
cat("- analise_high_grade_etapa2_36m.xlsx\n")
cat("- resumo_etapa2_36m.csv\n")
cat("- resumo_etapa2_36m.rds\n")
