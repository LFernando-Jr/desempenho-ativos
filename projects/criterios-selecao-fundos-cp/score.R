# ------------------------------------------------------------
# 7. Score preliminar e redução do universo — janela padrão 36m
# ------------------------------------------------------------

# Função: quanto maior o valor, melhor.
score_maior_melhor <- function(x) {
  percent_rank(x)
}

# Função: quanto menor o valor, melhor.
score_menor_melhor <- function(x) {
  percent_rank(-x)
}

shortlist_score <- resumo_etapa2 |>
  filter(
    n_meses >= 36,
    n_janelas_36m >= 1,
    !is.na(excesso_cdi_aa),
    !is.na(hit_rate_36m),
    !is.na(max_drawdown_excesso),
    !is.na(correlacao_media_pares),
    !is.na(correlacao_maxima)
  ) |>
  mutate(
    # -----------------------
    # Retorno
    # -----------------------
    score_retorno =
      score_maior_melhor(excesso_cdi_aa),
    
    # -----------------------
    # Consistência
    # -----------------------
    # A janela estrutural principal passa a ser 36 meses.
    score_consistencia =
      0.40 * score_maior_melhor(hit_rate_mensal) +
      0.20 * score_maior_melhor(hit_rate_6m) +
      0.40 * score_maior_melhor(hit_rate_36m),
    
    # -----------------------
    # Risco
    # -----------------------
    score_risco =
      0.40 * score_maior_melhor(max_drawdown_excesso) +
      0.30 * score_maior_melhor(media_tres_piores_meses) +
      0.30 * score_menor_melhor(volatilidade_excesso_aa),
    
    # -----------------------
    # Custo
    # -----------------------
    # Menor taxa é melhor.
    score_taxa =
      score_menor_melhor(taxa_adm_aa),
    
    # Excesso líquido anualizado entregue por unidade
    # de taxa de administração anual.
    eficiencia_custo =
      excesso_cdi_aa /
      pmax(taxa_adm_aa, 0.0001),
    
    score_eficiencia =
      score_maior_melhor(eficiencia_custo),
    
    score_custo =
      0.50 * score_taxa +
      0.50 * score_eficiencia,
    
    # -----------------------
    # Diferenciação
    # -----------------------
    # Menor correlação média e máxima é melhor.
    score_diferenciacao =
      0.50 * score_menor_melhor(correlacao_media_pares) +
      0.50 * score_menor_melhor(correlacao_maxima),
    
    # -----------------------
    # Score final
    # -----------------------
    score_preliminar =
      0.20 * score_retorno +
      0.25 * score_consistencia +
      0.20 * score_risco +
      0.25 * score_custo +
      0.10 * score_diferenciacao
  ) |>
  mutate(
    # Q1 = melhores fundos.
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
    )
  ) |>
  arrange(
    desc(score_preliminar)
  )

# ------------------------------------------------------------
# 7.1. Red flags e elegibilidade para shortlist
# ------------------------------------------------------------

limite_drawdown <- quantile(
  shortlist_score$max_drawdown_excesso,
  probs = 0.10,
  na.rm = TRUE
)

limite_cauda <- quantile(
  shortlist_score$media_tres_piores_meses,
  probs = 0.10,
  na.rm = TRUE
)

shortlist_score <- shortlist_score |>
  mutate(
    red_flag = case_when(
      hit_rate_36m < 0.50 ~
        "Baixa consistência em janelas de 36 meses",
      
      max_drawdown_excesso <= limite_drawdown ~
        "Drawdown extremo",
      
      media_tres_piores_meses <= limite_cauda ~
        "Cauda negativa",
      
      TRUE ~
        NA_character_
    ),
    
    elegivel_shortlist =
      quartil_score <= 2 &
      is.na(red_flag)
  )

# ------------------------------------------------------------
# 7.2. Seleção dos melhores fundos dentro de cada cluster
# ------------------------------------------------------------

shortlist_clusters <- shortlist_score |>
  filter(
    elegivel_shortlist
  ) |>
  group_by(
    cluster
  ) |>
  slice_max(
    score_preliminar,
    n = 3,
    with_ties = FALSE
  ) |>
  ungroup() |>
  arrange(
    cluster,
    desc(score_preliminar)
  )

# ------------------------------------------------------------
# 7.3. Gráfico do score final
# ------------------------------------------------------------

grafico_shortlist <- shortlist_score |>
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
      "Retorno (20%), consistência de 36 meses (25%),",
      "risco (20%), custo (25%) e diferenciação (10%)"
    ),
    
    x =
      "Score",
    
    y =
      NULL,
    
    fill =
      NULL
  ) +
  theme_minimal(
    base_size = 11
  ) +
  theme(
    panel.grid.minor =
      element_blank(),
    
    plot.title =
      element_text(face = "bold"),
    
    axis.text.y =
      element_text(size = 7)
  )

print(grafico_shortlist)

write_excel_csv2(
  shortlist_score,
  "score_preliminar_fundos_36m.csv"
)

write_excel_csv2(
  shortlist_clusters,
  "shortlist_clusters_36m.csv"
)

ggsave(
  filename =
    "grafico_score_preliminar_36m.png",
  
  plot =
    grafico_shortlist,
  
  width =
    11,
  
  height =
    max(
      9,
      nrow(shortlist_score) * 0.28
    ),
  
  dpi =
    300
)
