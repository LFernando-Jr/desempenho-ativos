# Apresentação auditável; não altera métricas nem a régua do score.
cria_ranking_workbook = function(base, abertura) {
  pesos = c(Retorno = 0.30, Consistência = 0.25, Risco = 0.20, Custo = 0.25)
  componentes = abertura %>%
    mutate(contribuicao_score = contribuicao_pilar * unname(pesos[pilar]))
  verificacao = componentes %>%
    group_by(nome_plot) %>% summarise(soma = sum(contribuicao_score), .groups = "drop") %>%
    left_join(select(base, nome_plot, nota_final), by = "nome_plot")
  if (any(!is.finite(verificacao$soma)) ||
      any(abs(verificacao$soma - verificacao$nota_final) > 1e-8)) {
    stop("Contribuições das métricas não reconciliam com o score final.")
  }
  resultado = base %>% arrange(ranking_geral) %>%
    transmute(ranking_geral, nome_plot, nota_final, quartil_score = paste0("Q", quartil_score),
      nota_retorno, excesso_cdi_aa, pp_retorno = nota_retorno * 0.30,
      nota_consistencia, hit_rate_mensal, pp_hit_mensal = nota_hit_mensal * 0.20 * 0.25,
      hit_rate_6m, pp_hit_6m = nota_hit_6m * 0.20 * 0.25,
      hit_rate_12m, pp_hit_12m = nota_hit_12m * 0.60 * 0.25,
      pp_consistencia = nota_consistencia * 0.25,
      nota_risco, max_drawdown_excesso, pp_drawdown = nota_drawdown * 0.40 * 0.20,
      media_tres_piores_meses, pp_cauda = nota_cauda * 0.30 * 0.20,
      volatilidade_excesso_aa, pp_volatilidade = nota_volatilidade * 0.30 * 0.20,
      pp_risco = nota_risco * 0.20,
      nota_custo, taxa_adm_aa, pp_taxa = nota_taxa * 0.60 * 0.25,
      razao_excesso_taxa, pp_razao = nota_razao_excesso_taxa * 0.40 * 0.25,
      pp_custo = nota_custo * 0.25, status_quantitativo)
  soma_metricas = rowSums(select(resultado, pp_retorno, pp_hit_mensal, pp_hit_6m,
    pp_hit_12m, pp_drawdown, pp_cauda, pp_volatilidade, pp_taxa, pp_razao))
  if (any(abs(soma_metricas - resultado$nota_final) > 1e-8)) {
    stop("Abertura do Ranking diverge da metodologia vigente.")
  }
  resultado
}

titulos_ranking_workbook = function() {
  c("Posição", "Fundo", "Score", "Quartil", "Nota retorno",
    "Excesso CDI a.a.", "Retorno (pp)", "Nota consistência", "Hit mensal",
    "Mensal (pp)", "Hit 6m", "6m (pp)", "Hit 12m", "12m (pp)",
    "Consistência (pp)", "Nota risco", "Drawdown excesso", "Drawdown (pp)",
    "3 piores meses", "Cauda (pp)", "Vol. excesso a.a.", "Volatilidade (pp)",
    "Risco (pp)", "Nota custo", "Taxa adm. a.a.", "Taxa (pp)",
    "Excesso / taxa", "Razão (pp)", "Custo (pp)", "Situação quantitativa")
}

escreve_ranking_workbook = function(wb, dados) {
  titulos = titulos_ranking_workbook()
  azul = createStyle(fgFill = "#17365D", fontColour = "#FFFFFF", textDecoration = "bold", halign = "center")
  cabecalho = createStyle(fgFill = "#DCE6F1", fontColour = "#17365D", textDecoration = "bold", wrapText = TRUE, valign = "center")
  writeData(wb, "Ranking", "Análise high grade | Ranking", startRow = 1)
  addStyle(wb, "Ranking", createStyle(fontSize = 18, fontColour = "#17365D", textDecoration = "bold"), rows = 1, cols = 1)
  writeData(wb, "Ranking", "Retorno e risco: 36 meses comuns | consistência: 72 meses comuns | contribuições em pontos do score final", startRow = 2)
  writeData(wb, "Ranking", "Nota × peso no pilar × peso do pilar. A soma das 9 métricas = score; os totais dos pilares não devem ser somados novamente.", startRow = 3)
  grupos = list(c(1, 4), c(5, 7), c(8, 15), c(16, 23), c(24, 29), c(30, 30))
  rotulos = c("Ranking geral", "Retorno | 30%", "Consistência | 25%", "Risco | 20%", "Custo | 25%", "Decisão")
  for (i in seq_along(grupos)) {
    colunas = seq.int(grupos[[i]][1], grupos[[i]][2])
    if (length(colunas) > 1) mergeCells(wb, "Ranking", cols = colunas, rows = 6)
    writeData(wb, "Ranking", rotulos[i], startRow = 6, startCol = colunas[1])
    addStyle(wb, "Ranking", azul, rows = 6, cols = colunas, gridExpand = TRUE)
  }
  exibir = dados
  names(exibir) = titulos
  writeDataTable(wb, "Ranking", exibir, startRow = 7, tableStyle = "TableStyleMedium2", headerStyle = cabecalho)
  freezePane(wb, "Ranking", firstActiveRow = 8, firstActiveCol = 3)
  setColWidths(wb, "Ranking", cols = seq_len(ncol(dados)), widths = 17)
  setColWidths(wb, "Ranking", cols = c(1, 2, 3, 4, 30), widths = c(9, 34, 11, 11, 28))
  setRowHeights(wb, "Ranking", rows = 7, heights = 42)
  linhas = seq.int(8, nrow(dados) + 7)
  addStyle(wb, "Ranking", createStyle(numFmt = "0.00"), rows = linhas, cols = 3:29, gridExpand = TRUE, stack = TRUE)
  percentuais = which(names(dados) %in% c("excesso_cdi_aa", "hit_rate_mensal", "hit_rate_6m", "hit_rate_12m", "max_drawdown_excesso", "media_tres_piores_meses", "volatilidade_excesso_aa", "taxa_adm_aa"))
  addStyle(wb, "Ranking", createStyle(numFmt = "0.00%"), rows = linhas, cols = percentuais, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, "Ranking", createStyle(fgFill = "#EAF0F7", fontColour = "#17365D", textDecoration = "bold"), rows = linhas, cols = c(3, 5, 8, 16, 24), gridExpand = TRUE, stack = TRUE)
  invisible(titulos)
}
