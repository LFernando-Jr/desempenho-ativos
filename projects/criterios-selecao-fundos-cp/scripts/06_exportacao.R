# ETAPA 6 — EXPORTAÇÃO

# Limpa os objetos da sessão para evitar dependências de execuções anteriores.
rm(list = ls())

# ------------------------------------------------------------
# Setup
# ------------------------------------------------------------

# Caminhos dos insumos e das saídas desta etapa.
path_intermediate = "projects/criterios-selecao-fundos-cp/data/intermediate"
path_figures = "projects/criterios-selecao-fundos-cp/output/figures"
path_reports = "projects/criterios-selecao-fundos-cp/output/reports"
path_relatorio = file.path(path_reports, "analise_high_grade_etapa2_36m.xlsx")

dir.create(path = path_figures, recursive = TRUE, showWarnings = FALSE)
dir.create(path = path_reports, recursive = TRUE, showWarnings = FALSE)

figuras_canonicas = c(
  "heatmap_correlacao_excessos_mensais_36m.png",
  "dendrograma_fundos_excessos_mensais_36m.png",
  "grafico_ranking_fundos_36m.png",
  "grafico_score_retorno_36m.png",
  "grafico_score_consistencia_36m.png",
  "grafico_score_risco_36m.png",
  "grafico_score_custo_36m.png",
  "heatmap_afinidade_benchmarks_36m.png",
  "grafico_excesso_cdi_quartis_36m.png",
  "grafico_excesso_cdi_fundos_aprovados_36m.png",
  "grafico_drawdown_excesso_cdi_aprovados_36m.png"
)

figuras_obsoletas = setdiff(
  list.files(path = path_figures, pattern = "[.]png$"),
  figuras_canonicas
)

if (length(figuras_obsoletas) > 0) {
  warning(
    "Há figuras antigas fora do conjunto canônico: ",
    paste(figuras_obsoletas, collapse = " | "),
    ". Preserve-as em legacy ou remova-as antes de publicar os outputs finais."
  )
}

paths_necessarios = c(
  file.path(path_intermediate, "metricas_todos_fundos_36m.rds"),
  file.path(path_intermediate, "priorizacao_qualitativa_36m.rds"),
  file.path(path_intermediate, "matriz_correlacao_excessos_36m.rds"),
  file.path(path_intermediate, "dendrograma_diagnostico_36m.rds"),
  file.path(path_intermediate, "afinidade_benchmarks_36m.rds"),
  file.path(path_intermediate, "fundos_mensais_historico.rds"),
  file.path(path_intermediate, "fundos_mensais_score_36m.rds"),
  file.path(path_intermediate, "diagnostico_clusters_k_3_7.csv"),
  file.path(path_intermediate, "estabilidade_clusters_janelas.csv"),
  file.path(path_intermediate, "pares_redundancia_36m.csv"),
  file.path(path_intermediate, "sensibilidade_limiares_redundancia_36m.csv"),
  file.path(path_intermediate, "distribuicao_correlacoes_36m.csv"),
  file.path(path_intermediate, "membros_clusters_k_3_7.csv"),
  file.path(path_intermediate, "diagnostico_score_36m.csv"),
  file.path(path_intermediate, "diagnostico_taxas_36m.csv")
)

paths_ausentes = paths_necessarios[!file.exists(paths_necessarios)]

if (length(paths_ausentes) > 0) {
  stop(
    "Arquivos ausentes na Etapa 6: ",
    paste(paths_ausentes, collapse = ", "),
    ". Execute primeiro as Etapas 3 a 5."
  )
}

metricas_todos_fundos = read_rds(
  file = file.path(path_intermediate, "metricas_todos_fundos_36m.rds")
)

priorizacao_qualitativa = read_rds(
  file = file.path(path_intermediate, "priorizacao_qualitativa_36m.rds")
)

matriz_cor = read_rds(
  file = file.path(path_intermediate, "matriz_correlacao_excessos_36m.rds")
)

cluster_hierarquico = read_rds(
  file = file.path(path_intermediate, "dendrograma_diagnostico_36m.rds")
)

afinidade_benchmarks = read_rds(
  file = file.path(path_intermediate, "afinidade_benchmarks_36m.rds")
)

fundos_mensais_historico = read_rds(
  file = file.path(path_intermediate, "fundos_mensais_historico.rds")
)

fundos_mensais_score = read_rds(
  file = file.path(path_intermediate, "fundos_mensais_score_36m.rds")
)

diagnostico_clusters = read_csv2(
  file = file.path(path_intermediate, "diagnostico_clusters_k_3_7.csv"),
  show_col_types = FALSE
)

estabilidade_clusters = read_csv2(
  file = file.path(path_intermediate, "estabilidade_clusters_janelas.csv"),
  show_col_types = FALSE
)

pares_redundancia = read_csv2(
  file = file.path(path_intermediate, "pares_redundancia_36m.csv"),
  show_col_types = FALSE
)

sensibilidade_limiares = read_csv2(
  file = file.path(
    path_intermediate,
    "sensibilidade_limiares_redundancia_36m.csv"
  ),
  show_col_types = FALSE
)

distribuicao_correlacoes = read_csv2(
  file = file.path(path_intermediate, "distribuicao_correlacoes_36m.csv"),
  show_col_types = FALSE
)

membros_clusters = read_csv2(
  file = file.path(path_intermediate, "membros_clusters_k_3_7.csv"),
  show_col_types = FALSE
)

diagnostico_score = read_csv2(
  file = file.path(path_intermediate, "diagnostico_score_36m.csv"),
  show_col_types = FALSE
)

diagnostico_taxas = read_csv2(
  file = file.path(path_intermediate, "diagnostico_taxas_36m.csv"),
  show_col_types = FALSE
)

# ------------------------------------------------------------
# Correlações
# ------------------------------------------------------------

ordem_dendrograma = cluster_hierarquico$labels[cluster_hierarquico$order]

base_cor_long = as.data.frame(
  as.table(matriz_cor),
  stringsAsFactors = FALSE
) %>%
  as_tibble() %>%
  rename(
    fundo_linha = Var1,
    fundo_coluna = Var2,
    correlacao = Freq
  ) %>%
  mutate(
    fundo_linha = factor(fundo_linha, levels = rev(ordem_dendrograma)),
    fundo_coluna = factor(fundo_coluna, levels = ordem_dendrograma)
  )

grafico_correlacao = ggplot(
  data = base_cor_long,
  mapping = aes(x = fundo_coluna, y = fundo_linha, fill = correlacao)
) +
  geom_tile() +
  scale_fill_gradient2(
    low = "#B2182B",
    mid = "white",
    high = "#2166AC",
    midpoint = 0,
    limits = c(-1, 1),
    oob = squish,
    name = "Correlação"
  ) +
  labs(
    title = "Correlação dos excessos mensais sobre o CDI",
    subtitle = "Janela comum de 36 meses; ordem definida pelo dendrograma",
    x = NULL,
    y = NULL,
    caption = paste(
      "Similaridade comportamental não comprova sobreposição de carteira.",
      "Clusters ainda não são usados como corte decisório."
    )
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 5.5),
    axis.text.y = element_text(size = 5.5),
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(hjust = 0)
  )

ggsave(
  filename = file.path(
    path_figures,
    "heatmap_correlacao_excessos_mensais_36m.png"
  ),
  plot = grafico_correlacao,
  width = 15,
  height = 13,
  dpi = 300
)

png(
  filename = file.path(
    path_figures,
    "dendrograma_fundos_excessos_mensais_36m.png"
  ),
  width = 2600,
  height = 1500,
  res = 200
)

par(mar = c(16, 5, 4, 2))

plot(
  x = cluster_hierarquico,
  labels = cluster_hierarquico$labels,
  hang = -1,
  cex = 0.55,
  main = "Dendrograma dos excessos mensais sobre o CDI",
  sub = "Método average; nenhum número de clusters foi imposto",
  xlab = "",
  ylab = "Distância: 1 - correlação"
)

dev.off()

# ------------------------------------------------------------
# Score
# ------------------------------------------------------------

cores_quartis = c(
  "Q1" = "#1B7837",
  "Q2" = "#5AAE61",
  "Q3" = "#FDB863",
  "Q4" = "#D73027"
)

base_ranking_plot = priorizacao_qualitativa %>%
  mutate(
    nome_plot = fct_reorder(nome_plot, nota_final),
    quartil = paste0("Q", quartil_score)
  )

grafico_ranking = ggplot(
  data = base_ranking_plot,
  mapping = aes(x = nome_plot, y = nota_final, fill = quartil)
) +
  geom_col(width = 0.75) +
  coord_flip() +
  scale_fill_manual(values = cores_quartis) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(
    title = "Score de qualidade individual",
    subtitle = "Aprovado com margem a partir de 58; zona cinzenta entre 52 e abaixo de 58",
    x = NULL,
    y = "Nota",
    fill = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold")
  )

ggsave(
  filename = file.path(path_figures, "grafico_ranking_fundos_36m.png"),
  plot = grafico_ranking,
  width = 12,
  height = max(9, nrow(base_ranking_plot) * 0.25),
  dpi = 300
)

# Cada pilar ordena os fundos pela sua própria nota, da maior para a menor.

pilares_score = c(
  retorno = "nota_retorno",
  consistencia = "nota_consistencia",
  risco = "nota_risco",
  custo = "nota_custo"
)

titulos_pilares = c(
  retorno = "Retorno",
  consistencia = "Consistência",
  risco = "Risco",
  custo = "Custo"
)

for (pilar in names(pilares_score)) {
  coluna_nota = pilares_score[[pilar]]
  titulo_pilar = titulos_pilares[[pilar]]
  base_pilar_plot = priorizacao_qualitativa %>%
    arrange(desc(.data[[coluna_nota]]), nome_plot) %>%
    mutate(nome_plot = factor(nome_plot, levels = rev(nome_plot)))

  grafico_pilar = ggplot(
    data = base_pilar_plot,
    mapping = aes(x = nome_plot, y = .data[[coluna_nota]])
  ) +
    geom_col(fill = "#1F77B4", width = 0.75) +
    coord_flip() +
    scale_y_continuous(limits = c(0, 100)) +
    labs(
      title = paste("Score de", titulo_pilar),
      subtitle = "Fundos em ordem decrescente da nota do pilar",
      x = NULL,
      y = "Nota"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      panel.grid.major.y = element_blank(),
      plot.title = element_text(face = "bold")
    )

  ggsave(
    filename = file.path(
      path_figures,
      paste0("grafico_score_", pilar, "_36m.png")
    ),
    plot = grafico_pilar,
    width = 12,
    height = max(9, nrow(base_pilar_plot) * 0.25),
    dpi = 300,
    bg = "white"
  )
}

# ------------------------------------------------------------
# Afinidade com benchmarks
# ------------------------------------------------------------

base_afinidade_plot = afinidade_benchmarks %>%
  mutate(
    nome_plot = factor(nome_plot, levels = rev(ordem_dendrograma)),
    benchmark = factor(
      benchmark,
      levels = c("IDA LIQ-DI", "IDA-DI", "IRF-M 1")
    ),
    rotulo = paste0(
      "ρ ",
      number(correlacao, accuracy = 0.01, decimal.mark = ","),
      "\nTE ",
      percent(tracking_error_aa, accuracy = 0.1, decimal.mark = ",")
    )
  )

grafico_afinidade = ggplot(
  data = base_afinidade_plot,
  mapping = aes(x = benchmark, y = nome_plot, fill = correlacao)
) +
  geom_tile(linewidth = 0.4, color = "white") +
  geom_text(mapping = aes(label = rotulo), size = 2.4) +
  scale_fill_gradient2(
    low = "#B2182B",
    mid = "white",
    high = "#2166AC",
    midpoint = 0,
    limits = c(-1, 1),
    oob = squish,
    name = "Correlação"
  ) +
  labs(
    title = "Afinidade dos fundos com benchmarks",
    subtitle = "Correlação dos excessos sobre CDI e tracking error anualizado",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(face = "bold"),
    axis.text.y = element_text(size = 7),
    plot.title = element_text(face = "bold")
  )

ggsave(
  filename = file.path(path_figures, "heatmap_afinidade_benchmarks_36m.png"),
  plot = grafico_afinidade,
  width = 9,
  height = max(10, n_distinct(afinidade_benchmarks$nome_plot) * 0.30),
  dpi = 300
)

# ------------------------------------------------------------
# Trajetórias na janela comum de 36 meses
# ------------------------------------------------------------

# Mantém o histórico completo na planilha, embora os gráficos usem a janela comum.
base_historico_plot = fundos_mensais_historico %>%
  inner_join(
    priorizacao_qualitativa %>% select(nome_plot, quartil_score),
    by = "nome_plot", relationship = "many-to-one"
  ) %>%
  group_by(nome_plot, quartil_score) %>%
  arrange(mes, .by_group = TRUE) %>%
  mutate(excesso_cdi_acumulado = cumprod(1 + excesso_cdi_m) - 1) %>%
  ungroup() %>%
  mutate(quartil = paste0("Q", quartil_score))

mes_inicio_plot = min(fundos_mensais_score$mes) - 1

base_trajetorias_plot = fundos_mensais_score %>%
  inner_join(
    priorizacao_qualitativa %>%
      select(nome_plot, quartil_score, aprovado_quantitativo),
    by = "nome_plot",
    relationship = "many-to-one"
  ) %>%
  group_by(nome_plot) %>%
  arrange(mes, .by_group = TRUE) %>%
  mutate(excesso_cdi_acumulado = cumprod(1 + excesso_cdi_m) - 1) %>%
  ungroup() %>%
  select(nome_plot, mes, quartil_score, aprovado_quantitativo,
         excesso_cdi_acumulado) %>%
  bind_rows(
    priorizacao_qualitativa %>%
      transmute(
        nome_plot,
        mes = mes_inicio_plot,
        quartil_score,
        aprovado_quantitativo,
        excesso_cdi_acumulado = 0
      )
  ) %>%
  arrange(nome_plot, mes) %>%
  group_by(nome_plot) %>%
  mutate(drawdown_excesso =
           (1 + excesso_cdi_acumulado) /
           cummax(1 + excesso_cdi_acumulado) - 1) %>%
  ungroup() %>%
  mutate(quartil = paste0("Q", quartil_score))

base_quartis_plot = base_trajetorias_plot %>%
  group_by(mes, quartil) %>%
  summarise(
    mediana = median(excesso_cdi_acumulado),
    p25 = quantile(excesso_cdi_acumulado, 0.25),
    p75 = quantile(excesso_cdi_acumulado, 0.75),
    .groups = "drop"
  )

grafico_quartis = ggplot(base_quartis_plot, aes(x = mes, color = quartil)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
  geom_ribbon(aes(ymin = p25, ymax = p75, fill = quartil),
              alpha = 0.12, color = NA) +
  geom_line(aes(y = mediana), linewidth = 1) +
  scale_color_manual(values = cores_quartis) +
  scale_fill_manual(values = cores_quartis, guide = "none") +
  scale_y_continuous(labels = percent_format(accuracy = 0.1,
                                            decimal.mark = ",")) +
  labs(
    title = "Excesso acumulado sobre o CDI por quartil",
    subtitle = "36 meses comuns; linha = mediana dos fundos; faixa = intervalo interquartil",
    x = NULL,
    y = "Excesso acumulado",
    color = "Quartil do score"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

ggsave(
  filename = file.path(path_figures, "grafico_excesso_cdi_quartis_36m.png"),
  plot = grafico_quartis,
  width = 12, height = 7, dpi = 300, bg = "white"
)

base_aprovados_plot = base_trajetorias_plot %>%
  filter(aprovado_quantitativo)

rotulos_aprovados = base_aprovados_plot %>%
  group_by(nome_plot) %>%
  slice_max(mes, n = 1, with_ties = FALSE) %>%
  ungroup()

grafico_aprovados = ggplot(
  base_aprovados_plot,
  aes(x = mes, y = excesso_cdi_acumulado, group = nome_plot, color = nome_plot)
) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
  geom_line(linewidth = 0.8) +
  ggrepel::geom_text_repel(
    data = rotulos_aprovados,
    aes(label = nome_plot),
    direction = "y", hjust = 0, nudge_x = 30,
    segment.color = "grey70", max.overlaps = Inf, size = 3
  ) +
  scale_x_date(expand = expansion(mult = c(0.02, 0.30))) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1,
                                            decimal.mark = ",")) +
  guides(color = "none") +
  labs(
    title = "Excesso acumulado sobre o CDI — aprovados quantitativamente",
    subtitle = "Mesma janela de 36 meses; fundos da zona cinzenta não exibidos",
    x = NULL, y = "Excesso acumulado"
  ) +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(face = "bold"))

ggsave(
  filename = file.path(path_figures,
                       "grafico_excesso_cdi_fundos_aprovados_36m.png"),
  plot = grafico_aprovados,
  width = 14, height = 8, dpi = 300, bg = "white"
)

grafico_drawdown = ggplot(
  base_aprovados_plot,
  aes(x = mes, y = drawdown_excesso)
) +
  geom_hline(yintercept = 0, color = "grey70") +
  geom_area(fill = "#B35850", alpha = 0.7) +
  facet_wrap(vars(nome_plot), ncol = 3) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1,
                                            decimal.mark = ",")) +
  labs(
    title = "Drawdown do excesso sobre o CDI — aprovados quantitativamente",
    subtitle = "36 meses comuns; queda em relação ao pico anterior de excesso acumulado",
    x = NULL, y = "Drawdown do excesso"
  ) +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(face = "bold"))

ggsave(
  filename = file.path(path_figures,
                       "grafico_drawdown_excesso_cdi_aprovados_36m.png"),
  plot = grafico_drawdown,
  width = 14, height = 9, dpi = 300, bg = "white"
)

# ------------------------------------------------------------
# Workbook
# ------------------------------------------------------------

ranking_xlsx = priorizacao_qualitativa %>%
  select(
    ranking_geral,
    nome_plot,
    nota_final,
    quartil_score,
    classificacao_descritiva,
    status_quantitativo,
    aprovado_quantitativo,
    zona_fronteira,
    nota_minima_pilar,
    pilar_abaixo_minimo,
    red_flags_absolutos,
    alertas_relativos,
    nota_retorno,
    nota_consistencia,
    nota_risco,
    nota_custo,
    nota_taxa,
    nota_razao_excesso_taxa,
    excesso_cdi_aa,
    hit_rate_mensal,
    hit_rate_6m,
    hit_rate_12m,
    volatilidade_excesso_aa,
    max_drawdown_excesso,
    taxa_adm_aa,
    razao_excesso_taxa,
    prioridade_analise_qualitativa,
    criterio_priorizacao,
    status_priorizacao,
    status_qualitativo,
    observacao_qualitativa,
    fundo_mais_correlacionado,
    correlacao_maxima,
    nivel_redundancia_maxima,
    fundo_carteira_mais_correlacionado,
    correlacao_maxima_carteira,
    nivel_redundancia_carteira
  )

metodologia_xlsx = tibble(
  item = c(
    "Janela do score",
    "Histórico completo",
    "Conversão das métricas",
    "Pesos dos pilares",
    "Consistência",
    "Custo",
    "Diferenciação",
    "Aprovação",
    "Zona cinzenta",
    "Piso por pilar",
    "Avaliação qualitativa",
    "Redundância",
    "Faixas de redundância",
    "Clusters"
  ),
  decisao = c(
    "36 meses completos e comuns a todos os fundos",
    "Preservado apenas para diagnósticos e visualizações",
    "Z-score robusto com MAD padrão, limite [-4,4] e logística 0-100",
    "Retorno 30%; consistência 25%; risco 20%; custo 25%",
    "Hit rates mensal 40%, 6 meses 20% e 12 meses 40%",
    "Taxa 60% e razão excesso líquido/taxa 40%",
    "Não integra o score de qualidade",
    "Aprovado com margem: nota >= 58, sem red flag e sem pilar abaixo de 30",
    "Nota de 52 a abaixo de 58, ou nota >= 58 com algum pilar abaixo de 30",
    "Nota mínima de 30 em retorno, consistência, risco e custo; falha direciona à zona cinzenta",
    "Decisão manual: aprovado; aprovado com condição; pendente de informações; reprovado",
    "Diagnóstico posterior ao score; sem exclusão automática",
    "Atenção >= 0,80; elevada >= 0,85; muito elevada >= 0,90",
    "k de 3 a 7 em diagnóstico; nenhum k definitivo"
  )
)

matriz_cor_xlsx = matriz_cor %>%
  as.data.frame() %>%
  rownames_to_column("Fundo")

wb = createWorkbook(creator = "Análise de fundos high grade")

abas = c(
  "Ranking",
  "Todos os Fundos",
  "Pares Redundância",
  "Sensib. Redundância",
  "Distrib. Correlações",
  "Diagnóstico Clusters",
  "Membros Clusters",
  "Estab. Clusters",
  "Diagnóstico Score",
  "Diagnóstico Taxas",
  "Afinidade Benchmarks",
  "Matriz Correlação",
  "Histórico Mensal",
  "Metodologia"
)

walk(.x = abas, .f = ~ addWorksheet(wb = wb, sheetName = .x, gridLines = FALSE))

dados_abas = list(
  "Ranking" = ranking_xlsx,
  "Todos os Fundos" = metricas_todos_fundos,
  "Pares Redundância" = pares_redundancia,
  "Sensib. Redundância" = sensibilidade_limiares,
  "Distrib. Correlações" = distribuicao_correlacoes,
  "Diagnóstico Clusters" = diagnostico_clusters,
  "Membros Clusters" = membros_clusters,
  "Estab. Clusters" = estabilidade_clusters,
  "Diagnóstico Score" = diagnostico_score,
  "Diagnóstico Taxas" = diagnostico_taxas,
  "Afinidade Benchmarks" = afinidade_benchmarks,
  "Matriz Correlação" = matriz_cor_xlsx,
  "Histórico Mensal" = base_historico_plot,
  "Metodologia" = metodologia_xlsx
)

estilo_nota = createStyle(numFmt = "0.0")
estilo_percentual = createStyle(numFmt = "0.00%")
estilo_decimal = createStyle(numFmt = "0.00")
estilo_data = createStyle(numFmt = "mmm/yyyy")

iwalk(
  .x = dados_abas,
  .f = function(dados, aba) {
    writeDataTable(
      wb = wb,
      sheet = aba,
      x = dados,
      startRow = 1,
      startCol = 1,
      tableStyle = "TableStyleMedium2",
      withFilter = TRUE
    )

    freezePane(
      wb = wb,
      sheet = aba,
      firstActiveRow = 2,
      firstActiveCol = 2
    )

    setColWidths(
      wb = wb,
      sheet = aba,
      cols = seq_len(ncol(dados)),
      widths = "auto"
    )

    if (nrow(dados) > 0) {
      linhas_dados = seq.int(from = 2, to = nrow(dados) + 1)
      nomes_colunas = names(dados)

      colunas_notas = which(str_detect(nomes_colunas, "^(nota_|score_)"))
      colunas_percentuais = which(
        str_detect(
          nomes_colunas,
          "(^ret_|^excesso_|hit_rate|volatilidade|drawdown|cauda|taxa_adm|tracking_error)"
        ) &
          nomes_colunas != "razao_excesso_taxa"
      )
      colunas_decimais = which(
        str_detect(
          nomes_colunas,
          "(correlacao|silhouette|adjusted_rand|z_robusto|razao_excesso_taxa|^valor$)"
        )
      )
      colunas_datas = which(
        str_detect(nomes_colunas, "(^mes$|^mes_fim$|data)")
      )

      if (length(colunas_percentuais) > 0) {
        addStyle(
          wb = wb,
          sheet = aba,
          style = estilo_percentual,
          rows = linhas_dados,
          cols = colunas_percentuais,
          gridExpand = TRUE,
          stack = TRUE
        )
      }

      if (length(colunas_decimais) > 0) {
        addStyle(
          wb = wb,
          sheet = aba,
          style = estilo_decimal,
          rows = linhas_dados,
          cols = colunas_decimais,
          gridExpand = TRUE,
          stack = TRUE
        )
      }

      if (length(colunas_notas) > 0) {
        addStyle(
          wb = wb,
          sheet = aba,
          style = estilo_nota,
          rows = linhas_dados,
          cols = colunas_notas,
          gridExpand = TRUE,
          stack = TRUE
        )
      }

      if (length(colunas_datas) > 0) {
        addStyle(
          wb = wb,
          sheet = aba,
          style = estilo_data,
          rows = linhas_dados,
          cols = colunas_datas,
          gridExpand = TRUE,
          stack = TRUE
        )
      }
    }
  }
)

saveWorkbook(
  wb = wb,
  file = path_relatorio,
  overwrite = TRUE
)

message("[06] Gráficos e workbook final exportados.")
message("[06] Relatório: ", path_relatorio)
