# ETAPA 2 — RETORNOS E ELEGIBILIDADE

# Limpa os objetos da sessão para evitar dependências de execuções anteriores.
rm(list = ls())

# ------------------------------------------------------------
# Setup
# ------------------------------------------------------------

# Caminhos dos insumos e das saídas desta etapa.
path_intermediate = "projects/criterios-selecao-fundos-cp/data/intermediate"
path_fundos_raw = file.path(path_intermediate, "fundos_raw.rds")
path_benchs_raw = file.path(path_intermediate, "benchs_raw.rds")
path_de_para = file.path(path_intermediate, "de_para_fundos.rds")

# Janela comum utilizada nas métricas e no score.
JANELA_SCORE_MESES = 36L

paths_necessarios = c(path_fundos_raw, path_benchs_raw, path_de_para)
paths_ausentes = paths_necessarios[!file.exists(paths_necessarios)]

if (length(paths_ausentes) > 0) {
  stop(
    "Arquivos ausentes na Etapa 2: ",
    paste(paths_ausentes, collapse = ", "),
    ". Execute primeiro a Etapa 1."
  )
}

fundos_raw = read_rds(file = path_fundos_raw)
benchs_raw = read_rds(file = path_benchs_raw)
de_para = read_rds(file = path_de_para)

# ------------------------------------------------------------
# Retornos
# ------------------------------------------------------------

benchs_wide = benchs_raw %>%
  pivot_wider(
    names_from = benchmark,
    values_from = indice
  ) %>%
  arrange(data) %>%
  mutate(du_id = row_number())

benchs_anterior = benchs_wide %>%
  rename(data_anterior = data) %>%
  rename_with(
    .fn = ~ paste0(.x, "_anterior"),
    .cols = -data_anterior
  )

# O calendário do CDI define os limites efetivos de cada mês.
# Um mês só é considerado encerrado quando já existe observação no mês seguinte.
calendario_cdi = benchs_raw %>%
  filter(benchmark == "cdi") %>%
  arrange(data) %>%
  transmute(
    data,
    mes = floor_date(data, unit = "month"),
    data_anterior_cdi = lag(data),
    ret_cdi_calendario = indice / lag(indice) - 1
  ) %>%
  filter(is.finite(ret_cdi_calendario))

calendario_mensal_cdi = calendario_cdi %>%
  group_by(mes) %>%
  summarise(
    data_inicio_intervalo_esperada = data_anterior_cdi[which.min(data)],
    primeira_data_cdi = min(data),
    ultima_data_cdi = max(data),
    n_datas_cdi = n(),
    .groups = "drop"
  )

fundos_retornos_historico = fundos_raw %>%
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
    n_du = du_id - du_id_anterior,
    ret_cdi = cdi / cdi_anterior - 1,
    ret_ida_di = ida_di / ida_di_anterior - 1,
    ret_ida_liq_di = ida_liq_di / ida_liq_di_anterior - 1,
    ret_irfm_1 = irfm_1 / irfm_1_anterior - 1,
    excesso_cdi_liq = (1 + ret_liq) / (1 + ret_cdi) - 1,
    excesso_ida_di_liq = (1 + ret_liq) / (1 + ret_ida_di) - 1,
    excesso_ida_liq_di_liq = (1 + ret_liq) /
      (1 + ret_ida_liq_di) - 1,
    excesso_irfm_1_liq = (1 + ret_liq) / (1 + ret_irfm_1) - 1
  ) %>%
  filter(!is.na(ret_liq))

# ------------------------------------------------------------
# Checks
# ------------------------------------------------------------

# Confere se cada série perdeu somente a primeira observação ao calcular retornos.
fundos_mapeados = fundos_raw %>%
  semi_join(de_para, by = "nome_quantum")

observacoes_esperadas = nrow(fundos_mapeados) -
  n_distinct(fundos_mapeados$nome_quantum)

if (nrow(fundos_retornos_historico) != observacoes_esperadas) {
  stop(
    "A base de retornos contém ",
    nrow(fundos_retornos_historico),
    " observações; eram esperadas ",
    observacoes_esperadas,
    "."
  )
}

message("[02] Quantidade de retornos reconciliada com o histórico de cotas.")

# Identifica fundos sem taxa antes da formação do score.
fundos_sem_taxa = fundos_retornos_historico %>%
  filter(is.na(taxa_adm_aa)) %>%
  distinct(nome_plot)

if (nrow(fundos_sem_taxa) > 0) {
  warning("Há ", nrow(fundos_sem_taxa), " fundo(s) sem taxa de administração.")
} else {
  message("[02] Todos os fundos mapeados possuem taxa de administração.")
}

# Confere a disponibilidade do CDI para todos os retornos utilizados.
observacoes_sem_cdi = sum(is.na(fundos_retornos_historico$ret_cdi))

if (observacoes_sem_cdi > 0) {
  warning("Há ", observacoes_sem_cdi, " retorno(s) sem CDI correspondente.")
} else {
  message("[02] Todos os retornos possuem CDI correspondente.")
}

# Garante uma única observação por fundo e data.
duplicacoes_finais = fundos_retornos_historico %>%
  count(nome_quantum, data, name = "n") %>%
  filter(n > 1)

if (nrow(duplicacoes_finais) > 0) {
  print(duplicacoes_finais)
  stop("A base de retornos contém duplicatas por fundo e data.")
}

message("[02] Base de retornos sem duplicatas por fundo e data.")

# Mostra intervalos que atravessam mais de um dia útil da base de benchmarks.
intervalos_nao_diarios = fundos_retornos_historico %>%
  filter(!is.na(n_du), n_du != 1)

if (nrow(intervalos_nao_diarios) > 0) {
  warning(
    "Há ",
    nrow(intervalos_nao_diarios),
    " intervalo(s) com mais de um dia útil entre cotas; maior intervalo: ",
    max(intervalos_nao_diarios$n_du, na.rm = TRUE),
    "."
  )
} else {
  message("[02] Todos os retornos respeitam intervalos diários na base de benchmarks.")
}

# Lista séries que terminam antes da data máxima disponível.
data_maxima = max(fundos_raw$data, na.rm = TRUE)

fundos_defasados = fundos_raw %>%
  group_by(nome_quantum) %>%
  summarise(
    ultima_data = max(data),
    .groups = "drop"
  ) %>%
  filter(ultima_data < data_maxima)

if (nrow(fundos_defasados) > 0) {
  warning("Há ", nrow(fundos_defasados), " série(s) defasada(s).")
  print(fundos_defasados)
} else {
  message("[02] Todas as séries chegam à data máxima da base.")
}

# ------------------------------------------------------------
# Janela comum de 36 meses
# ------------------------------------------------------------

mes_maximo_cdi = max(calendario_mensal_cdi$mes)

meses_cdi_fechados = calendario_mensal_cdi %>%
  filter(mes < mes_maximo_cdi)

if (nrow(meses_cdi_fechados) < JANELA_SCORE_MESES) {
  stop(
    "O calendário do CDI não contém ",
    JANELA_SCORE_MESES,
    " meses encerrados."
  )
}

mes_fim_score = max(meses_cdi_fechados$mes)

mes_inicio_score = mes_fim_score %m-% months(JANELA_SCORE_MESES - 1L)

calendario_score = meses_cdi_fechados %>%
  filter(mes >= mes_inicio_score, mes <= mes_fim_score)

if (nrow(calendario_score) != JANELA_SCORE_MESES) {
  stop("O calendário do CDI possui lacunas na janela comum de 36 meses.")
}

data_fim_score = calendario_score %>%
  filter(mes == mes_fim_score) %>%
  pull(ultima_data_cdi)

cobertura_mensal = fundos_retornos_historico %>%
  mutate(mes = floor_date(data, unit = "month")) %>%
  filter(mes >= mes_inicio_score, mes <= mes_fim_score) %>%
  group_by(
    nome_xlsx,
    nome_curto,
    nome_plot,
    nome_quantum,
    taxa_adm_aa,
    revisar,
    mes
  ) %>%
  summarise(
    primeira_data = min(data),
    data_inicio_intervalo = data_anterior[which.min(data)],
    ultima_data = max(data),
    n_obs = n(),
    n_cdi_validos = sum(is.finite(ret_cdi)),
    n_intervalos_invalidos = sum(!is.finite(n_du) | n_du < 1),
    .groups = "drop"
  ) %>%
  left_join(
    calendario_score,
    by = "mes",
    relationship = "many-to-one"
  ) %>%
  mutate(
    mes_completo = coalesce(
      data_inicio_intervalo == data_inicio_intervalo_esperada &
        ultima_data == ultima_data_cdi &
        n_cdi_validos == n_obs &
        n_intervalos_invalidos == 0,
      FALSE
    )
  )

resumo_cobertura = cobertura_mensal %>%
  group_by(
    nome_xlsx,
    nome_curto,
    nome_plot,
    nome_quantum,
    taxa_adm_aa
  ) %>%
  summarise(
    revisar = any(revisar),
    primeiro_mes = min(mes),
    ultimo_mes = max(mes),
    n_meses = n_distinct(mes),
    n_meses_completos = sum(mes_completo),
    .groups = "drop"
  )

universo_elegibilidade = de_para %>%
  select(
    nome_xlsx,
    nome_curto,
    nome_plot,
    nome_quantum,
    taxa_adm_aa,
    revisar
  ) %>%
  left_join(
    resumo_cobertura,
    by = c(
      "nome_xlsx",
      "nome_curto",
      "nome_plot",
      "nome_quantum",
      "taxa_adm_aa",
      "revisar"
    ),
    relationship = "one-to-one"
  ) %>%
  mutate(
    n_meses = coalesce(n_meses, 0L),
    n_meses_completos = coalesce(n_meses_completos, 0L),
    data_inicio_score = mes_inicio_score,
    data_fim_score = data_fim_score,
    elegivel_score_36m = !revisar &
      !is.na(primeiro_mes) &
      primeiro_mes == mes_inicio_score &
      ultimo_mes == mes_fim_score &
      n_meses == JANELA_SCORE_MESES &
      n_meses_completos == JANELA_SCORE_MESES,
    motivo_inelegibilidade = case_when(
      revisar ~ "De-para pendente de revisão",
      is.na(primeiro_mes) ~ "Sem observações na janela comum",
      primeiro_mes > mes_inicio_score ~ "Histórico inferior a 36 meses",
      ultimo_mes < mes_fim_score ~ "Série não chega ao mês final comum",
      n_meses < JANELA_SCORE_MESES ~ "Meses ausentes na janela comum",
      n_meses_completos < JANELA_SCORE_MESES ~ "Há meses incompletos na janela comum",
      TRUE ~ NA_character_
    )
  )

fundos_retornos_score = fundos_retornos_historico %>%
  semi_join(
    universo_elegibilidade %>% filter(elegivel_score_36m),
    by = c(
      "nome_xlsx",
      "nome_curto",
      "nome_plot",
      "nome_quantum",
      "taxa_adm_aa"
    )
  ) %>%
  mutate(
    mes = floor_date(data, unit = "month"),
    em_janela_score = mes >= mes_inicio_score & mes <= mes_fim_score
  ) %>%
  filter(em_janela_score)

observacoes_score_sem_cdi = fundos_retornos_score %>%
  filter(!is.finite(ret_cdi))

if (nrow(observacoes_score_sem_cdi) > 0) {
  print(
    observacoes_score_sem_cdi %>%
      select(nome_plot, data, ret_liq, ret_cdi)
  )
  stop("A janela do score contém retornos sem CDI válido.")
}

if (n_distinct(fundos_retornos_score$nome_plot) == 0) {
  stop("Nenhum fundo possui 36 meses completos na janela comum do score.")
}

message(
  "[02] Janela do score: ",
  format(mes_inicio_score, "%m/%Y"),
  " a ",
  format(mes_fim_score, "%m/%Y"),
  "."
)

message(
  "[02] Fundos elegíveis: ",
  n_distinct(fundos_retornos_score$nome_plot),
  " de ",
  nrow(universo_elegibilidade),
  "."
)

# ------------------------------------------------------------
# Saídas
# ------------------------------------------------------------

write_rds(
  x = fundos_retornos_historico,
  file = file.path(path_intermediate, "fundos_retornos_historico.rds")
)

write_rds(
  x = fundos_retornos_score,
  file = file.path(path_intermediate, "fundos_retornos_score_36m.rds")
)

write_rds(
  x = universo_elegibilidade,
  file = file.path(path_intermediate, "universo_elegibilidade_36m.rds")
)

write_rds(
  x = calendario_mensal_cdi,
  file = file.path(path_intermediate, "calendario_mensal_cdi.rds")
)

write_excel_csv2(
  x = universo_elegibilidade,
  file = file.path(path_intermediate, "universo_elegibilidade_36m.csv")
)

write_excel_csv2(
  x = universo_elegibilidade %>% filter(!elegivel_score_36m),
  file = file.path(path_intermediate, "fundos_excluidos_janela_36m.csv")
)

message("[02] Retornos, histórico e elegibilidade concluídos.")
