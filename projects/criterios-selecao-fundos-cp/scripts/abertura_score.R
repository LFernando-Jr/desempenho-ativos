# Memória auditável dos componentes do score, sem recalcular a classificação.
# O z apresentado é o valor orientado e truncado usado na nota logística.

cria_abertura_score = function(priorizacao_qualitativa) {
  componentes = tribble(
    ~pilar, ~metrica, ~campo_valor, ~campo_nota, ~campo_pilar, ~campo_quartil, ~peso, ~unidade, ~origem, ~sentido, ~calculo,
    "Retorno", "Excesso anualizado sobre CDI", "excesso_cdi_aa", "nota_retorno", "nota_retorno", "quartil_retorno", 1.00, "Percentual", "Cotas ajustadas e CDI", "Maior é melhor", "Produto dos excessos mensais relativos, elevado a 12/36, menos 1",
    "Consistência", "Hit rate mensal", "hit_rate_mensal", "nota_hit_mensal", "nota_consistencia", "quartil_consistencia", 0.40, "Percentual", "Excessos mensais", "Maior é melhor", "Meses com excesso positivo divididos por 36",
    "Consistência", "Hit rate 6 meses", "hit_rate_6m", "nota_hit_6m", "nota_consistencia", "quartil_consistencia", 0.20, "Percentual", "Janelas de excessos mensais", "Maior é melhor", "Janelas móveis de 6 meses com excesso composto positivo / janelas válidas",
    "Consistência", "Hit rate 12 meses", "hit_rate_12m", "nota_hit_12m", "nota_consistencia", "quartil_consistencia", 0.40, "Percentual", "Janelas de excessos mensais", "Maior é melhor", "Janelas móveis de 12 meses com excesso composto positivo / janelas válidas",
    "Risco", "Drawdown máximo do excesso", "max_drawdown_excesso", "nota_drawdown", "nota_risco", "quartil_risco", 0.40, "Percentual", "Índice acumulado de excesso", "Maior é melhor", "Maior queda do índice acumulado de excesso desde um pico anterior",
    "Risco", "Média dos 3 piores meses", "media_tres_piores_meses", "nota_cauda", "nota_risco", "quartil_risco", 0.30, "Percentual", "Excessos mensais", "Maior é melhor", "Média dos três menores excessos mensais na janela comum",
    "Risco", "Volatilidade do excesso", "volatilidade_excesso_aa", "nota_volatilidade", "nota_risco", "quartil_risco", 0.30, "Percentual", "Excessos mensais", "Menor é melhor", "Desvio-padrão dos excessos mensais multiplicado por raiz de 12",
    "Custo", "Taxa de administração", "taxa_adm_aa", "nota_taxa", "nota_custo", "quartil_custo", 0.60, "Percentual", "Cadastro de fundos", "Menor é melhor", "Taxa anual de administração informada no cadastro",
    "Custo", "Excesso dividido pela taxa", "razao_excesso_taxa", "nota_razao_excesso_taxa", "nota_custo", "quartil_custo", 0.40, "Razão", "Excesso anualizado e cadastro", "Maior é melhor", "Excesso anualizado sobre CDI / taxa anual de administração"
  )

  abertura = map_dfr(seq_len(nrow(componentes)), function(i) {
    item = componentes[i, ]
    priorizacao_qualitativa %>%
      transmute(
        ranking_geral,
        nome_plot,
        pilar = item$pilar,
        metrica = item$metrica,
        origem_dado = item$origem,
        valor_metrica = .data[[item$campo_valor]],
        unidade_metrica = item$unidade,
        z_usado_na_nota = qlogis(.data[[item$campo_nota]] / 100),
        nota_metrica = .data[[item$campo_nota]],
        peso_no_pilar = item$peso,
        contribuicao_pilar = peso_no_pilar * nota_metrica,
        nota_pilar = .data[[item$campo_pilar]],
        quartil_pilar = paste0("Q", .data[[item$campo_quartil]]),
        sentido_melhor = item$sentido,
        calculo_metrica = item$calculo
      )
  }) %>%
    mutate(pilar = factor(pilar, levels = c("Retorno", "Consistência", "Risco", "Custo"))) %>%
    arrange(ranking_geral, pilar) %>%
    mutate(pilar = as.character(pilar))

  conciliacao = abertura %>%
    group_by(ranking_geral, nome_plot, pilar) %>%
    summarise(
      soma_contribuicoes = sum(contribuicao_pilar),
      nota_pilar = first(nota_pilar),
      .groups = "drop"
    )

  if (any(abs(conciliacao$soma_contribuicoes - conciliacao$nota_pilar) > 1e-8)) {
    stop("A abertura dos componentes não reconcilia com uma nota de pilar.")
  }

  notas_recalculadas = 100 / (1 + exp(-abertura$z_usado_na_nota))
  if (any(!is.finite(abertura$z_usado_na_nota)) ||
      any(abs(notas_recalculadas - abertura$nota_metrica) > 1e-8)) {
    stop("O z-score da abertura não reconcilia com a nota da métrica.")
  }

  abertura
}
