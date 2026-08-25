# ETAPA 2 — RETORNOS E ELEGIBILIDADE
# Execute preferencialmente por 00_run_all.R.

# ------------------------------------------------------------

benchs_wide = benchs_raw %>%
  pivot_wider(
    names_from = benchmark,
    values_from = indice
  ) %>%
  arrange(data) %>%
  mutate(
    du_id = row_number()
  )

benchs_anterior = benchs_wide %>%
  rename(data_anterior = data) %>%
  rename_with(
    ~ paste0(.x, "_anterior"),
    -data_anterior
  )

# ------------------------------------------------------------
# 5. Retornos e excessos
# ------------------------------------------------------------

fundos_retornos = fundos_raw %>%
  left_join(
    de_para %>%
      select(
        nome_quantum,
        nome_xlsx,
        nome_curto,
        nome_plot,
        taxa_adm_aa,
        criterio_match,
        similaridade,
        revisar
      ),
    by = "nome_quantum",
    relationship = "many-to-one"
  ) %>%
  filter(!is.na(nome_xlsx)) %>%
  group_by(nome_quantum) %>%
  arrange(data, .by_group = TRUE) %>%
  mutate(
    data_anterior = lag(data),
    cota_anterior = lag(cota),

    # Retorno observado na cota.
    ret_liq = cota / cota_anterior - 1
  ) %>%
  ungroup() %>%
  left_join(
    benchs_wide,
    by = "data",
    relationship = "many-to-one"
  ) %>%
  left_join(
    benchs_anterior,
    by = "data_anterior",
    relationship = "many-to-one"
  ) %>%
  mutate(
    # Dias úteis transcorridos entre as duas cotas.
    n_du = du_id - du_id_anterior,

    # Fator da taxa de administração no intervalo.
    fator_taxa_intervalo = (1 + taxa_adm_aa)^(n_du / 252),

    # Retorno aproximado antes da taxa de administração.
    # Recompõe apenas a taxa informada no XLSX.
    ret_pre_taxa_adm_aprox = (1 + ret_liq) *
      fator_taxa_intervalo -
      1,

    # Retornos dos benchmarks no mesmo intervalo.
    ret_cdi = cdi / cdi_anterior - 1,

    ret_ida_di = ida_di / ida_di_anterior - 1,

    ret_ida_liq_di = ida_liq_di / ida_liq_di_anterior - 1,

    ret_irfm_1 = irfm_1 / irfm_1_anterior - 1,

    # Excessos geométricos.
    excesso_cdi_liq = (1 + ret_liq) /
      (1 + ret_cdi) -
      1,

    excesso_cdi_pre_taxa = (1 + ret_pre_taxa_adm_aprox) /
      (1 + ret_cdi) -
      1,

    excesso_ida_di_liq = (1 + ret_liq) /
      (1 + ret_ida_di) -
      1,

    excesso_ida_liq_di_liq = (1 + ret_liq) /
      (1 + ret_ida_liq_di) -
      1,

    excesso_irfm_1_liq = (1 + ret_liq) /
      (1 + ret_irfm_1) -
      1
  ) %>%
  filter(!is.na(ret_liq))

# ------------------------------------------------------------
# 6. Checagens
# ------------------------------------------------------------

fundos_mapeados = fundos_raw %>%
  semi_join(
    de_para,
    by = "nome_quantum"
  )

observacoes_esperadas =
  nrow(fundos_mapeados) -
  n_distinct(fundos_mapeados$nome_quantum)

checagem = fundos_retornos %>%
  summarise(
    fundos = n_distinct(nome_quantum),

    observacoes = n(),

    observacoes_esperadas = observacoes_esperadas,

    sem_taxa = sum(is.na(taxa_adm_aa)),

    sem_cdi = sum(is.na(ret_cdi)),

    intervalos_nao_diarios = sum(n_du != 1, na.rm = TRUE),

    maior_intervalo_du = max(n_du, na.rm = TRUE)
  )

print(checagem)

duplicacoes_finais = fundos_retornos %>%
  count(nome_quantum, data) %>%
  filter(n > 1)

if (nrow(duplicacoes_finais) > 0) {
  print(duplicacoes_finais)

  stop(
    "A base final contém mais de uma observação ",
    "por fundo e data."
  )
}

data_maxima = max(
  fundos_raw$data,
  na.rm = TRUE
)

fundos_defasados = fundos_raw %>%
  group_by(nome_quantum) %>%
  summarise(
    ultima_data = max(data),
    .groups = "drop"
  ) %>%
  filter(ultima_data < data_maxima)

cat("\nFundos cuja série termina antes da data máxima:\n")
print(fundos_defasados)

# ------------------------------------------------------------
# 7. Saídas da base
# ------------------------------------------------------------
# ------------------------------------------------------------
# 7. Elegibilidade: 36 meses completos
# ------------------------------------------------------------
# A partir deste ponto, a análise exclui fundos que não:
# - tenham histórico praticamente integral de 36 meses;
# - cheguem à data final comum da base;
# - possuam quantidade mínima de observações na janela;
# - estejam livres de pendência no de-para.

data_fim = max(
  fundos_retornos$data[
    !is.na(fundos_retornos$ret_cdi)
  ],
  na.rm = TRUE
)

data_inicio_padrao = data_fim %m-%
  months(JANELA_PADRAO_MESES)

fundos_elegiveis_36m = fundos_retornos %>%
  filter(
    data > data_inicio_padrao,
    data <= data_fim
  ) %>%
  group_by(
    nome_xlsx,
    nome_curto,
    nome_plot,
    nome_quantum,
    taxa_adm_aa
  ) %>%
  summarise(
    primeira_data_janela = min(data),

    ultima_data_janela = max(data),

    n_obs_janela = n(),

    revisar = any(revisar),

    .groups = "drop"
  ) %>%
  mutate(
    cobertura_36m = !revisar &
      primeira_data_janela <= data_inicio_padrao + days(10) &
      ultima_data_janela == data_fim &
      n_obs_janela >= JANELA_PADRAO_MESES * 18
  )

fundos_excluidos_36m = fundos_elegiveis_36m %>%
  filter(!cobertura_36m) %>%
  mutate(
    motivo_exclusao = case_when(
      revisar ~
        "De-para pendente de revisão",

      ultima_data_janela < data_fim ~
        "Série não chega à data final comum",

      primeira_data_janela > data_inicio_padrao + days(10) ~
        "Histórico inferior a 36 meses",

      n_obs_janela < JANELA_PADRAO_MESES * 18 ~
        "Quantidade insuficiente de observações",

      TRUE ~
        "Cobertura insuficiente da janela"
    )
  )

fundos_elegiveis_36m = fundos_elegiveis_36m %>%
  filter(cobertura_36m)

cat(
  "\nFundos elegíveis com 36 meses completos:",
  nrow(fundos_elegiveis_36m),
  "de",
  n_distinct(fundos_retornos$nome_quantum),
  "\n"
)

cat("\nFundos excluídos por histórico insuficiente:\n")
print(
  fundos_excluidos_36m %>%
    select(
      nome_plot,
      primeira_data_janela,
      ultima_data_janela,
      n_obs_janela,
      motivo_exclusao
    )
)

write_excel_csv2(
  fundos_excluidos_36m,
  "projects/criterios-selecao-fundos-cp/data/intermediate/fundos_excluidos_janela_36m.csv"
)

# Exclui do restante da análise os fundos sem 36 meses completos.
fundos_retornos = fundos_retornos %>%
  semi_join(
    fundos_elegiveis_36m,
    by = c(
      "nome_xlsx",
      "nome_curto",
      "nome_plot",
      "nome_quantum",
      "taxa_adm_aa"
    )
  )

# ------------------------------------------------------------
# 8. Saídas da base elegível
# ------------------------------------------------------------

write_rds(
  fundos_retornos,
  "projects/criterios-selecao-fundos-cp/data/intermediate/fundos_retornos_etapa1_36m.rds"
)

write_excel_csv2(
  fundos_retornos %>%
    select(
      nome_xlsx,
      nome_curto,
      nome_plot,
      nome_quantum,
      criterio_match,
      similaridade,
      revisar,
      data,
      cota,
      taxa_adm_aa,
      n_du,
      ret_liq,
      ret_pre_taxa_adm_aprox,
      ret_cdi,
      excesso_cdi_liq,
      excesso_cdi_pre_taxa,
      ret_ida_di,
      excesso_ida_di_liq,
      ret_ida_liq_di,
      excesso_ida_liq_di_liq,
      ret_irfm_1,
      excesso_irfm_1_liq
  ),
  "projects/criterios-selecao-fundos-cp/data/intermediate/fundos_retornos_etapa1_36m.csv"
)

# ------------------------------------------------------------
# 9. Visualização 1:
#    janelas de excesso anualizado sobre o CDI
