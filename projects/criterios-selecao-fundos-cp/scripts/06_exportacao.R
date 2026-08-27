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

# Remove relações vazias criadas pelo openxlsx quando a planilha não contém
# desenhos incorporados. O Excel ignora essas referências, mas outros leitores
# podem interpretar o arquivo como inválido.
corrige_relacionamentos_xlsx = function(path) {
  path_temp = tempfile(pattern = "xlsx_relacionamentos_")
  path_xlsx_temp = tempfile(fileext = ".xlsx")

  dir.create(path = path_temp, recursive = TRUE, showWarnings = FALSE)

  on.exit(
    expr = {
      unlink(x = path_temp, recursive = TRUE, force = TRUE)
      unlink(x = path_xlsx_temp, force = TRUE)
    },
    add = TRUE
  )

  unzip(zipfile = path, exdir = path_temp)

  path_drawings = file.path(path_temp, "xl", "drawings")
  paths_rels = list.files(
    path = file.path(path_temp, "xl", "worksheets", "_rels"),
    pattern = "[.]rels$",
    full.names = TRUE
  )

  if (!dir.exists(path_drawings) && length(paths_rels) > 0) {
    walk(
      .x = paths_rels,
      .f = function(path_rels) {
        xml_rels = readLines(
          con = path_rels,
          warn = FALSE,
          encoding = "UTF-8"
        ) %>%
          paste(collapse = "") %>%
          str_remove_all(
            pattern = paste0(
              "<Relationship[^>]+Type=\"",
              "[^\"]+/(drawing|vmlDrawing)\"",
              "[^>]*/>"
            )
          )

        writeLines(
          text = xml_rels,
          con = path_rels,
          useBytes = TRUE
        )
      }
    )
  }

  arquivos_xlsx = list.files(
    path = path_temp,
    recursive = TRUE,
    all.files = TRUE,
    no.. = TRUE
  )

  zipr = getExportedValue(ns = "zip", name = "zipr")

  zipr(
    zipfile = path_xlsx_temp,
    files = arquivos_xlsx,
    recurse = TRUE,
    include_directories = FALSE,
    root = path_temp
  )

  substituiu_arquivo = file.copy(
    from = path_xlsx_temp,
    to = path,
    overwrite = TRUE
  )

  if (!substituiu_arquivo) {
    stop("Não foi possível substituir o workbook após a validação estrutural.")
  }

  invisible(path)
}

dir.create(path = path_figures, recursive = TRUE, showWarnings = FALSE)
dir.create(path = path_reports, recursive = TRUE, showWarnings = FALSE)

figuras_canonicas = c(
  "heatmap_correlacao_excessos_mensais_36m.png",
  "dendrograma_fundos_excessos_mensais_36m.png",
  "grafico_ranking_fundos_36m.png",
  "grafico_pilares_score_36m.png",
  "heatmap_afinidade_benchmarks_36m.png",
  "grafico_historico_completo_excesso_cdi.png"
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
  file = file.path(path_intermediate, "sensibilidade_limiares_redundancia_36m.csv"),
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
  filename = file.path(path_figures, "heatmap_correlacao_excessos_mensais_36m.png"),
  plot = grafico_correlacao,
  width = 15,
  height = 13,
  dpi = 300
)

png(
  filename = file.path(path_figures, "dendrograma_fundos_excessos_mensais_36m.png"),
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
    subtitle = "Quartis são descritivos; aprovação quantitativa ainda será calibrada",
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

base_blocos_plot = priorizacao_qualitativa %>%
  select(
    nome_plot,
    nota_retorno,
    nota_consistencia,
    nota_risco,
    nota_custo
  ) %>%
  pivot_longer(
    cols = starts_with("nota_"),
    names_to = "bloco",
    values_to = "nota"
  ) %>%
  mutate(
    bloco = recode(
      bloco,
      "nota_retorno" = "Retorno",
      "nota_consistencia" = "Consistência",
      "nota_risco" = "Risco",
      "nota_custo" = "Custo"
    )
  )

grafico_blocos = ggplot(
  data = base_blocos_plot,
  mapping = aes(x = reorder(nome_plot, nota), y = nota)
) +
  geom_col(fill = "#1F77B4", width = 0.75) +
  coord_flip() +
  facet_wrap(facets = vars(bloco), ncol = 2) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(
    title = "Abertura dos pilares do score",
    x = NULL,
    y = "Nota"
  ) +
  theme_minimal(base_size = 9) +
  theme(
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold")
  )

ggsave(
  filename = file.path(path_figures, "grafico_pilares_score_36m.png"),
  plot = grafico_blocos,
  width = 16,
  height = max(12, nrow(priorizacao_qualitativa) * 0.35),
  dpi = 300
)

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
# Histórico completo
# ------------------------------------------------------------

base_historico_plot = fundos_mensais_historico %>%
  inner_join(
    priorizacao_qualitativa %>%
      select(nome_plot, quartil_score),
    by = "nome_plot",
    relationship = "many-to-one"
  ) %>%
  group_by(nome_plot, quartil_score) %>%
  arrange(mes, .by_group = TRUE) %>%
  mutate(excesso_cdi_acumulado = cumprod(1 + excesso_cdi_m) - 1) %>%
  ungroup() %>%
  mutate(quartil = paste0("Q", quartil_score))

grafico_historico = ggplot(
  data = base_historico_plot,
  mapping = aes(
    x = mes,
    y = excesso_cdi_acumulado,
    group = nome_plot,
    color = quartil
  )
) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
  geom_line(linewidth = 0.55, alpha = 0.60) +
  facet_wrap(facets = vars(quartil), ncol = 2, scales = "free_y") +
  scale_color_manual(values = cores_quartis) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1, decimal.mark = ",")) +
  labs(
    title = "Histórico completo do excesso acumulado sobre o CDI",
    subtitle = "O histórico completo é diagnóstico; somente os 36 meses comuns entram no score",
    x = NULL,
    y = "Excesso acumulado",
    color = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

ggsave(
  filename = file.path(path_figures, "grafico_historico_completo_excesso_cdi.png"),
  plot = grafico_historico,
  width = 15,
  height = 10,
  dpi = 300
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
    fundo_mais_correlacionado,
    correlacao_maxima,
    fundo_carteira_mais_correlacionado,
    correlacao_maxima_carteira
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
    "Redundância",
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
    "Pendente de calibração; quartis são apenas descritivos",
    "Diagnóstico posterior ao score; sem exclusão automática",
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
        ) & nomes_colunas != "razao_excesso_taxa"
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

corrige_relacionamentos_xlsx(path = path_relatorio)

message("[06] Workbook validado sem relações internas pendentes.")
message("[06] Gráficos e workbook final exportados.")
message("[06] Relatório: ", path_relatorio)
