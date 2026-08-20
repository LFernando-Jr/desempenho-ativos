# ============================================================
# FUNDOS HIGH GRADE — ETAPA 1
# Importação, de-para validado, retornos, excessos,
# nomes limpos e visualizações iniciais.
# ============================================================

library(tidyverse)
library(readxl)
library(lubridate)
library(janitor)
library(stringi)
library(scales)
library(ggrepel)

# ------------------------------------------------------------
# 0. Caminhos
# ------------------------------------------------------------

setwd("C:\\Users\\nandd\\Downloads\\selecao_credito")

arquivo_xlsx   <- "analise_quantitativa_fundos_high_grade.xlsx"
arquivo_fundos <- "funds_hist.csv"
arquivo_benchs <- "benchs_hist.csv"

# Janela estrutural padrão da análise.
JANELA_PADRAO_MESES <- 36L

locale_quantum <- locale(
  decimal_mark = ",",
  grouping_mark = ".",
  encoding = "Windows-1252"
)

# ------------------------------------------------------------
# 1. Importação
# ------------------------------------------------------------

fundos_raw <- read_delim(
  arquivo_fundos,
  delim = ";",
  locale = locale_quantum,
  show_col_types = FALSE,
  trim_ws = TRUE
) |>
  clean_names() |>
  transmute(
    nome_quantum = nome_do_ativo,
    data = dmy(data),
    cota = as.numeric(cota_ajustados)
  ) |>
  filter(
    !is.na(nome_quantum),
    !is.na(data),
    !is.na(cota)
  ) |>
  arrange(nome_quantum, data)

benchs_raw <- read_delim(
  arquivo_benchs,
  delim = ";",
  locale = locale_quantum,
  show_col_types = FALSE,
  trim_ws = TRUE
) |>
  clean_names() |>
  transmute(
    benchmark = recode(
      nome_do_ativo,
      "CDI" = "cdi",
      "IDA-DI" = "ida_di",
      "IDA LIQ-DI" = "ida_liq_di",
      "IRF-M 1" = "irfm_1"
    ),
    data = dmy(data),
    indice = as.numeric(numero_indice_ajustados)
  ) |>
  filter(
    benchmark %in% c("cdi", "ida_di", "ida_liq_di", "irfm_1"),
    !is.na(data),
    !is.na(indice)
  ) |>
  arrange(benchmark, data)

taxas_xlsx <- read_excel(
  arquivo_xlsx,
  sheet = "Base Tratada"
) |>
  clean_names() |>
  transmute(
    nome_xlsx = fundo,
    taxa_adm_aa = as.numeric(taxa_de_administracao)
  ) |>
  filter(!is.na(nome_xlsx))

cat("Fundos no histórico:", n_distinct(fundos_raw$nome_quantum), "\n")
cat("Fundos no XLSX:", n_distinct(taxas_xlsx$nome_xlsx), "\n")
cat("Benchmarks:", paste(unique(benchs_raw$benchmark), collapse = ", "), "\n")

# Travas contra duplicações na matéria-prima.
if (anyDuplicated(fundos_raw[c("nome_quantum", "data")]) > 0) {
  stop("Há mais de uma cota para o mesmo fundo e data em funds_hist.csv.")
}

if (anyDuplicated(benchs_raw[c("benchmark", "data")]) > 0) {
  stop("Há mais de um nível para o mesmo benchmark e data em benchs_hist.csv.")
}

# ------------------------------------------------------------
# 2. Funções para nomes
# ------------------------------------------------------------

# 2.1. Normalização usada somente para sugerir o de-para.
termos_genericos <- c(
  "RESP", "LIMITADA", "RL", "FI", "FIF", "FIC", "CIC",
  "FUNDO", "FUNDOS", "DE", "DO", "DA", "DOS", "DAS", "EM",
  "RENDA", "FIXA", "RF", "FIRF", "CREDITO", "PRIVADO",
  "CP", "LP", "MULTIMERCADO", "MULTI", "MM", "FIM",
  "CLASSE", "CLASSES", "INVESTIMENTO", "INVEST",
  "SUSTENTAVEL", "IS", "CI", "CIRF", "COTAS", "COTA",
  "LONGO", "PRAZO", "E"
)

normaliza_nome <- function(x) {
  x |>
    stri_trans_general("Latin-ASCII") |>
    str_to_upper() |>
    str_replace_all("[^A-Z0-9]+", " ") |>
    str_squish() |>
    str_split(" ") |>
    map_chr(
      ~ paste(.x[!.x %in% termos_genericos], collapse = " ")
    )
}

# 2.2. Limpeza usada somente para exibição nos gráficos.
# Preserva marca, estratégia, Advisory, Plus, High Grade,
# Yield, Feeder, números e outros termos distintivos.
termos_exibicao <- c(
  "RESP", "LIMITADA", "RL",
  "FI", "FIF", "FIC", "CIC", "FIRF", "FIM", "CIRF",
  "FUNDO", "FUNDOS",
  "DE", "DO", "DA", "DOS", "DAS", "EM",
  "RENDA", "FIXA", "RF",
  "CP", "LP",
  "MULTIMERCADO", "MULTI", "MM",
  "CLASSE", "CLASSES",
  "INVESTIMENTO", "INVESTIMENTOS",
  "COTAS", "COTA",
  "LONGO", "PRAZO",
  "IS"
)

limpa_nome_exibicao <- function(x) {
  x_limpo <- x |>
    str_replace_all("[()/,;_-]+", " ") |>
    str_squish()
  
  str_split(x_limpo, "\\s+") |>
    map_chr(function(tokens) {
      tokens_comparacao <- tokens |>
        stri_trans_general("Latin-ASCII") |>
        str_to_upper()
      
      tokens_mantidos <- tokens[
        !tokens_comparacao %in% termos_exibicao
      ]
      
      paste(tokens_mantidos, collapse = " ") |>
        str_squish()
    })
}

# ------------------------------------------------------------
# 3. De-para dos nomes
# ------------------------------------------------------------
# A similaridade serve apenas para sugerir correspondências.
# Novus e JGP são exceções validadas manualmente.

nomes_xlsx <- taxas_xlsx |>
  mutate(chave_xlsx = normaliza_nome(nome_xlsx))

nomes_quantum <- fundos_raw |>
  distinct(nome_quantum) |>
  mutate(chave_quantum = normaliza_nome(nome_quantum))

matriz_distancia <- adist(
  nomes_xlsx$chave_xlsx,
  nomes_quantum$chave_quantum,
  ignore.case = TRUE
)

indice_melhor <- max.col(
  -matriz_distancia,
  ties.method = "first"
)

de_para <- nomes_xlsx |>
  mutate(
    nome_quantum =
      nomes_quantum$nome_quantum[indice_melhor]
  )

# Localiza um único nome da Quantum a partir de um padrão.
encontra_nome_quantum <- function(padrao) {
  candidatos <- nomes_quantum |>
    filter(
      str_detect(
        nome_quantum,
        regex(padrao, ignore_case = TRUE)
      )
    ) |>
    pull(nome_quantum)
  
  if (length(candidatos) != 1) {
    stop(
      "O padrão '", padrao,
      "' encontrou ", length(candidatos),
      " nomes na Quantum: ",
      paste(candidatos, collapse = " | ")
    )
  }
  
  candidatos
}

nome_novus_quantum <- encontra_nome_quantum(
  "NOVUS.*HIGH GRADE"
)

nome_jgp_corporate_quantum <- encontra_nome_quantum(
  "JGP.*CORPORATE.*FEEDER III"
)

# Correções validadas:
# 1. Novus foi associado incorretamente à Tríade pelo algoritmo.
# 2. O JGP Feeder II foi pesquisado por CNPJ e aparece como
#    Feeder III no nome exportado pela Quantum.
de_para <- de_para |>
  mutate(
    nome_quantum = case_when(
      str_detect(
        nome_xlsx,
        regex("^Novus Crédito", ignore_case = TRUE)
      ) ~ nome_novus_quantum,
      
      str_detect(
        nome_xlsx,
        regex("JGP Corporate.*FEEDER II", ignore_case = TRUE)
      ) ~ nome_jgp_corporate_quantum,
      
      TRUE ~ nome_quantum
    ),
    
    criterio_match = case_when(
      str_detect(
        nome_xlsx,
        regex("^Novus Crédito", ignore_case = TRUE)
      ) ~ "Validado manualmente",
      
      str_detect(
        nome_xlsx,
        regex("JGP Corporate.*FEEDER II", ignore_case = TRUE)
      ) ~ "CNPJ validado manualmente",
      
      TRUE ~ "Similaridade de nome"
    ),
    
    chave_quantum = normaliza_nome(nome_quantum),
    
    distancia = map2_int(
      chave_xlsx,
      chave_quantum,
      ~ as.integer(adist(.x, .y)[1])
    ),
    
    similaridade =
      1 - distancia /
      pmax(
        nchar(chave_xlsx),
        nchar(chave_quantum),
        1
      ),
    
    revisar = case_when(
      criterio_match %in% c(
        "Validado manualmente",
        "CNPJ validado manualmente"
      ) ~ FALSE,
      
      similaridade < 0.85 ~ TRUE,
      
      TRUE ~ FALSE
    ),
    
    nome_curto = limpa_nome_exibicao(nome_xlsx)
  ) |>
  group_by(nome_curto) |>
  mutate(
    # Se dois fundos virarem o mesmo nome curto, mantém o nome
    # oficial completo para não misturar produtos distintos.
    nome_plot = if_else(
      n() > 1,
      nome_xlsx,
      nome_curto
    )
  ) |>
  ungroup() |>
  select(
    nome_xlsx,
    nome_curto,
    nome_plot,
    nome_quantum,
    taxa_adm_aa,
    criterio_match,
    similaridade,
    revisar
  )

# ------------------------------------------------------------
# 3.1. Validação do de-para
# ------------------------------------------------------------

duplicados_de_para <- de_para |>
  count(nome_quantum, name = "n") |>
  filter(n > 1)

if (nrow(duplicados_de_para) > 0) {
  print(
    de_para |>
      semi_join(
        duplicados_de_para,
        by = "nome_quantum"
      ) |>
      arrange(nome_quantum)
  )
  
  stop(
    "O de-para associou mais de um fundo do XLSX ",
    "ao mesmo fundo da Quantum."
  )
}

cat(
  "Pares que merecem revisão manual:",
  sum(de_para$revisar),
  "\n"
)

print(
  de_para |>
    filter(revisar)
)

cat("\nColisões de nomes curtos:\n")

print(
  de_para |>
    count(nome_curto) |>
    filter(n > 1)
)

cat("\nFundos da Quantum sem correspondência:\n")

print(
  nomes_quantum |>
    anti_join(de_para, by = "nome_quantum")
)

cat("\nFundos do XLSX sem correspondência:\n")

print(
  taxas_xlsx |>
    anti_join(de_para, by = "nome_xlsx")
)

write_excel_csv2(
  de_para,
  "de_para_fundos_revisao.csv"
)

# ------------------------------------------------------------
# 4. Benchmarks em formato largo
# ------------------------------------------------------------

benchs_wide <- benchs_raw |>
  pivot_wider(
    names_from = benchmark,
    values_from = indice
  ) |>
  arrange(data) |>
  mutate(
    du_id = row_number()
  )

benchs_anterior <- benchs_wide |>
  rename(data_anterior = data) |>
  rename_with(
    ~ paste0(.x, "_anterior"),
    -data_anterior
  )

# ------------------------------------------------------------
# 5. Retornos e excessos
# ------------------------------------------------------------

fundos_retornos <- fundos_raw |>
  left_join(
    de_para |>
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
  ) |>
  filter(!is.na(nome_xlsx)) |>
  group_by(nome_quantum) |>
  arrange(data, .by_group = TRUE) |>
  mutate(
    data_anterior = lag(data),
    cota_anterior = lag(cota),
    
    # Retorno observado na cota.
    ret_liq =
      cota / cota_anterior - 1
  ) |>
  ungroup() |>
  left_join(
    benchs_wide,
    by = "data",
    relationship = "many-to-one"
  ) |>
  left_join(
    benchs_anterior,
    by = "data_anterior",
    relationship = "many-to-one"
  ) |>
  mutate(
    # Dias úteis transcorridos entre as duas cotas.
    n_du =
      du_id - du_id_anterior,
    
    # Fator da taxa de administração no intervalo.
    fator_taxa_intervalo =
      (1 + taxa_adm_aa)^(n_du / 252),
    
    # Retorno aproximado antes da taxa de administração.
    # Recompõe apenas a taxa informada no XLSX.
    ret_pre_taxa_adm_aprox =
      (1 + ret_liq) *
      fator_taxa_intervalo - 1,
    
    # Retornos dos benchmarks no mesmo intervalo.
    ret_cdi =
      cdi / cdi_anterior - 1,
    
    ret_ida_di =
      ida_di / ida_di_anterior - 1,
    
    ret_ida_liq_di =
      ida_liq_di / ida_liq_di_anterior - 1,
    
    ret_irfm_1 =
      irfm_1 / irfm_1_anterior - 1,
    
    # Excessos geométricos.
    excesso_cdi_liq =
      (1 + ret_liq) /
      (1 + ret_cdi) - 1,
    
    excesso_cdi_pre_taxa =
      (1 + ret_pre_taxa_adm_aprox) /
      (1 + ret_cdi) - 1,
    
    excesso_ida_di_liq =
      (1 + ret_liq) /
      (1 + ret_ida_di) - 1,
    
    excesso_ida_liq_di_liq =
      (1 + ret_liq) /
      (1 + ret_ida_liq_di) - 1,
    
    excesso_irfm_1_liq =
      (1 + ret_liq) /
      (1 + ret_irfm_1) - 1
  ) |>
  filter(!is.na(ret_liq))

# ------------------------------------------------------------
# 6. Checagens
# ------------------------------------------------------------

fundos_mapeados <- fundos_raw |>
  semi_join(
    de_para,
    by = "nome_quantum"
  )

observacoes_esperadas <-
  nrow(fundos_mapeados) -
  n_distinct(fundos_mapeados$nome_quantum)

checagem <- fundos_retornos |>
  summarise(
    fundos =
      n_distinct(nome_quantum),
    
    observacoes =
      n(),
    
    observacoes_esperadas =
      observacoes_esperadas,
    
    sem_taxa =
      sum(is.na(taxa_adm_aa)),
    
    sem_cdi =
      sum(is.na(ret_cdi)),
    
    intervalos_nao_diarios =
      sum(n_du != 1, na.rm = TRUE),
    
    maior_intervalo_du =
      max(n_du, na.rm = TRUE)
  )

print(checagem)

duplicacoes_finais <- fundos_retornos |>
  count(nome_quantum, data) |>
  filter(n > 1)

if (nrow(duplicacoes_finais) > 0) {
  print(duplicacoes_finais)
  
  stop(
    "A base final contém mais de uma observação ",
    "por fundo e data."
  )
}

data_maxima <- max(
  fundos_raw$data,
  na.rm = TRUE
)

fundos_defasados <- fundos_raw |>
  group_by(nome_quantum) |>
  summarise(
    ultima_data = max(data),
    .groups = "drop"
  ) |>
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

data_fim <- max(
  fundos_retornos$data[
    !is.na(fundos_retornos$ret_cdi)
  ],
  na.rm = TRUE
)

data_inicio_padrao <- data_fim %m-%
  months(JANELA_PADRAO_MESES)

fundos_elegiveis_36m <- fundos_retornos |>
  filter(
    data > data_inicio_padrao,
    data <= data_fim
  ) |>
  group_by(
    nome_xlsx,
    nome_curto,
    nome_plot,
    nome_quantum,
    taxa_adm_aa
  ) |>
  summarise(
    primeira_data_janela =
      min(data),
    
    ultima_data_janela =
      max(data),
    
    n_obs_janela =
      n(),
    
    revisar =
      any(revisar),
    
    .groups =
      "drop"
  ) |>
  mutate(
    cobertura_36m =
      !revisar &
      primeira_data_janela <=
      data_inicio_padrao + days(10) &
      ultima_data_janela ==
      data_fim &
      n_obs_janela >=
      JANELA_PADRAO_MESES * 18
  )

fundos_excluidos_36m <- fundos_elegiveis_36m |>
  filter(!cobertura_36m) |>
  mutate(
    motivo_exclusao = case_when(
      revisar ~
        "De-para pendente de revisão",
      
      ultima_data_janela < data_fim ~
        "Série não chega à data final comum",
      
      primeira_data_janela >
        data_inicio_padrao + days(10) ~
        "Histórico inferior a 36 meses",
      
      n_obs_janela <
        JANELA_PADRAO_MESES * 18 ~
        "Quantidade insuficiente de observações",
      
      TRUE ~
        "Cobertura insuficiente da janela"
    )
  )

fundos_elegiveis_36m <- fundos_elegiveis_36m |>
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
  fundos_excluidos_36m |>
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
  "fundos_excluidos_janela_36m.csv"
)

# Exclui do restante da análise os fundos sem 36 meses completos.
fundos_retornos <- fundos_retornos |>
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
  "fundos_retornos_etapa1_36m.rds"
)

write_excel_csv2(
  fundos_retornos |>
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
  "fundos_retornos_etapa1_36m.csv"
)

# ------------------------------------------------------------
# 9. Visualização 1:
#    janelas de excesso anualizado sobre o CDI
# ------------------------------------------------------------
# O universo já está restrito aos fundos com 36 meses completos.
# As janelas de 6, 12 e 24 meses são mantidas como diagnóstico
# complementar, enquanto 36 meses é a referência estrutural.

horizontes <- tribble(
  ~janela,    ~meses,
  "6 meses",       6L,
  "12 meses",     12L,
  "24 meses",     24L,
  "36 meses",     36L
)

calcula_janela <- function(janela, meses) {
  data_inicio_janela <- data_fim %m-%
    months(meses)
  
  fundos_retornos |>
    filter(
      data > data_inicio_janela,
      data <= data_fim,
      !is.na(ret_liq),
      !is.na(ret_cdi)
    ) |>
    group_by(
      nome_xlsx,
      nome_plot,
      nome_quantum,
      taxa_adm_aa
    ) |>
    summarise(
      primeira_data =
        min(data),
      
      ultima_data =
        max(data),
      
      n_obs =
        n(),
      
      ret_fundo_acum =
        prod(1 + ret_liq) - 1,
      
      ret_cdi_acum =
        prod(1 + ret_cdi) - 1,
      
      .groups =
        "drop"
    ) |>
    mutate(
      janela =
        janela,
      
      meses =
        meses,
      
      cobertura_ok =
        primeira_data <=
        data_inicio_janela + days(10) &
        ultima_data ==
        data_fim &
        n_obs >=
        meses * 18,
      
      ret_fundo_aa =
        (1 + ret_fundo_acum)^(12 / meses) - 1,
      
      ret_cdi_aa =
        (1 + ret_cdi_acum)^(12 / meses) - 1,
      
      excesso_cdi_aa =
        (
          (1 + ret_fundo_acum) /
            (1 + ret_cdi_acum)
        )^(12 / meses) - 1,
      
      percentual_cdi =
        ret_fundo_aa /
        ret_cdi_aa
    ) |>
    filter(cobertura_ok)
}

base_janelas <- pmap_dfr(
  horizontes,
  calcula_janela
)

# Ordena pelo excesso de 36 meses.
ordem_36m <- base_janelas |>
  filter(janela == "36 meses") |>
  arrange(excesso_cdi_aa) |>
  pull(nome_plot)

fundos_sem_36m <- setdiff(
  unique(base_janelas$nome_plot),
  ordem_36m
)

ordem_fundos <- c(
  fundos_sem_36m,
  ordem_36m
)

base_janelas <- base_janelas |>
  mutate(
    janela = factor(
      janela,
      levels = horizontes$janela
    ),
    
    nome_plot = factor(
      nome_plot,
      levels = ordem_fundos
    )
  )

# Limites robustos evitam que um único outlier comprima
# excessivamente as diferenças entre os demais fundos.
limite_cor <- max(
  abs(
    quantile(
      base_janelas$excesso_cdi_aa,
      probs = c(0.05, 0.95),
      na.rm = TRUE
    )
  )
)

grafico_janelas <- ggplot(
  base_janelas,
  aes(
    x = janela,
    y = nome_plot,
    fill = excesso_cdi_aa
  )
) +
  geom_tile(
    linewidth = 0.4,
    color = "white"
  ) +
  geom_text(
    aes(
      label = percent(
        excesso_cdi_aa,
        accuracy = 0.1,
        decimal.mark = ","
      )
    ),
    size = 2.7
  ) +
  scale_fill_gradient2(
    low = "#B2182B",
    mid = "white",
    high = "#2166AC",
    midpoint = 0,
    limits = c(-limite_cor, limite_cor),
    oob = squish,
    labels = percent_format(
      accuracy = 0.1,
      decimal.mark = ","
    ),
    name = "Excesso\na.a."
  ) +
  scale_y_discrete(
    labels = function(x) {
      str_wrap(x, width = 30)
    }
  ) +
  labs(
    title =
      "Excesso anualizado sobre o CDI por janela",
    
    subtitle = paste0(
      "Universo com 36 meses completos | Janelas encerradas em ",
      format(data_fim, "%d/%m/%Y")
    ),
    
    x = NULL,
    y = NULL,
    
    caption = paste(
      "Os fundos são ordenados pelo excesso anualizado de 36 meses.",
      "As janelas menores são apresentadas como diagnóstico complementar."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid =
      element_blank(),
    
    axis.text.x =
      element_text(face = "bold"),
    
    axis.text.y =
      element_text(size = 7.5),
    
    plot.title =
      element_text(face = "bold"),
    
    plot.caption =
      element_text(hjust = 0),
    
    legend.position =
      "right"
  )

print(grafico_janelas)

altura_heatmap <- max(
  10,
  n_distinct(base_janelas$nome_plot) * 0.29
)

ggsave(
  filename =
    "heatmap_janelas_excesso_cdi_36m.png",
  
  plot =
    grafico_janelas,
  
  width =
    10,
  
  height =
    altura_heatmap,
  
  dpi =
    300
)

write_excel_csv2(
  base_janelas |>
    mutate(
      nome_plot =
        as.character(nome_plot),
      
      janela =
        as.character(janela)
    ) |>
    arrange(
      nome_plot,
      meses
    ),
  "base_janelas_retorno_36m.csv"
)

# ------------------------------------------------------------
# 10. Visualização 2:
#     taxa versus excesso bruto estimado em 36 meses
# ------------------------------------------------------------

base_visual_36m <- fundos_retornos |>
  filter(
    data > data_inicio_padrao,
    data <= data_fim
  ) |>
  group_by(
    nome_xlsx,
    nome_plot,
    nome_quantum,
    taxa_adm_aa
  ) |>
  summarise(
    primeira_data =
      min(data),
    
    ultima_data =
      max(data),
    
    n_obs =
      n(),
    
    ret_liq_36m =
      prod(1 + ret_liq, na.rm = TRUE) - 1,
    
    ret_pre_taxa_36m =
      prod(
        1 + ret_pre_taxa_adm_aprox,
        na.rm = TRUE
      ) - 1,
    
    ret_cdi_36m =
      prod(1 + ret_cdi, na.rm = TRUE) - 1,
    
    taxa_efetiva_36m =
      prod(
        fator_taxa_intervalo,
        na.rm = TRUE
      ) - 1,
    
    .groups =
      "drop"
  ) |>
  mutate(
    excesso_cdi_liq_36m =
      (1 + ret_liq_36m) /
      (1 + ret_cdi_36m) - 1,
    
    excesso_cdi_pre_taxa_36m =
      (1 + ret_pre_taxa_36m) /
      (1 + ret_cdi_36m) - 1
  ) |>
  filter(
    primeira_data <=
      data_inicio_padrao + days(10),
    
    ultima_data ==
      data_fim,
    
    n_obs >=
      JANELA_PADRAO_MESES * 18,
    
    !is.na(excesso_cdi_pre_taxa_36m)
  )

# Rotula somente os cinco melhores e os cinco piores
# excessos líquidos da janela de 36 meses.
destaques <- bind_rows(
  base_visual_36m |>
    slice_max(
      excesso_cdi_liq_36m,
      n = 5,
      with_ties = FALSE
    ),
  
  base_visual_36m |>
    slice_min(
      excesso_cdi_liq_36m,
      n = 5,
      with_ties = FALSE
    )
) |>
  distinct(nome_plot, .keep_all = TRUE)

grafico_custo <- ggplot(
  base_visual_36m,
  aes(
    x = taxa_efetiva_36m,
    y = excesso_cdi_pre_taxa_36m
  )
) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    linewidth = 0.7
  ) +
  geom_point(
    size = 2.8,
    alpha = 0.75
  ) +
  geom_text_repel(
    data = destaques,
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
      accuracy = 0.1,
      decimal.mark = ","
    )
  ) +
  labs(
    title =
      "Geração bruta estimada versus taxa de administração",
    
    subtitle = paste0(
      "Janela de 36 meses até ",
      format(data_fim, "%d/%m/%Y"),
      " | Linha tracejada: excesso bruto igual ao custo"
    ),
    
    x =
      "Taxa de administração efetiva em 36 meses",
    
    y =
      "Excesso bruto estimado sobre o CDI em 36 meses",
    
    caption = paste(
      "Retorno bruto estimado pela reposição da taxa de administração.",
      "Não recompõe outras despesas ou custos indiretos."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor =
      element_blank(),
    
    plot.title =
      element_text(face = "bold"),
    
    plot.caption =
      element_text(hjust = 0)
  )

print(grafico_custo)

ggsave(
  filename =
    "grafico_taxa_vs_excesso_bruto_36m.png",
  
  plot =
    grafico_custo,
  
  width =
    11,
  
  height =
    7,
  
  dpi =
    300
)

write_excel_csv2(
  base_visual_36m |>
    arrange(
      desc(excesso_cdi_liq_36m)
    ),
  "base_visualizacao_36m.csv"
)

# ------------------------------------------------------------
# 11. Encerramento
# ------------------------------------------------------------

cat("\nEtapa 1 concluída. Arquivos gerados:\n")
cat("- de_para_fundos_revisao.csv\n")
cat("- fundos_excluidos_janela_36m.csv\n")
cat("- fundos_retornos_etapa1_36m.rds\n")
cat("- fundos_retornos_etapa1_36m.csv\n")
cat("- base_janelas_retorno_36m.csv\n")
cat("- heatmap_janelas_excesso_cdi_36m.png\n")
cat("- base_visualizacao_36m.csv\n")
cat("- grafico_taxa_vs_excesso_bruto_36m.png\n")

