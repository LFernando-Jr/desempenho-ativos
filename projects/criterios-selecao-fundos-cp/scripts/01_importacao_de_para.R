# Setup -------------------------------------------------------------------

rm(list = ls())

arquivo_xlsx = paste0(
  "projects/criterios-selecao-fundos-cp/data/input/",
  "analise_quantitativa_fundos_high_grade.xlsx"
)
arquivo_fundos = paste0(
  "projects/criterios-selecao-fundos-cp/data/input/",
  "funds_hist.csv"
)

arquivo_benchs = paste0(
  "projects/criterios-selecao-fundos-cp/data/input/",
  "benchs_hist.csv"
)

JANELA_PADRAO_MESES = 36L

locale_quantum = locale(
  decimal_mark = ",",
  grouping_mark = ".",
  encoding = "Windows-1252"
)

# Coleta -----------------------------------------------------------------

fundos_raw = read_delim(
  file = arquivo_fundos,
  delim = ";",
  locale = locale_quantum,
  show_col_types = FALSE,
  trim_ws = TRUE
) %>%
  clean_names() %>%
  transmute(
    nome_quantum = nome_do_ativo,
    data = dmy(data),
    cota = as.numeric(cota_ajustados)
  ) %>%
  filter(
    !is.na(nome_quantum),
    !is.na(data),
    !is.na(cota)
  ) %>%
  arrange(nome_quantum, data)

benchs_raw = read_delim(
  arquivo_benchs,
  delim = ";",
  locale = locale_quantum,
  show_col_types = FALSE,
  trim_ws = TRUE
) %>%
  clean_names() %>%
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
  ) %>%
  filter(
    benchmark %in% c("cdi", "ida_di", "ida_liq_di", "irfm_1"),
    !is.na(data),
    !is.na(indice)
  ) %>%
  arrange(benchmark, data)

taxas_xlsx = read_excel(
  arquivo_xlsx,
  sheet = "Base Tratada"
) %>%
  clean_names() %>%
  transmute(
    nome_xlsx = fundo,
    taxa_adm_aa = as.numeric(taxa_de_administracao)
  ) %>%
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

# Tratamento -------------------------------------------------------------

## helpers ---------------------------------------------------------------

termos_genericos = c(
  "RESP",
  "LIMITADA",
  "RL",
  "FI",
  "FIF",
  "FIC",
  "CIC",
  "FUNDO",
  "FUNDOS",
  "DE",
  "DO",
  "DA",
  "DOS",
  "DAS",
  "EM",
  "RENDA",
  "FIXA",
  "RF",
  "FIRF",
  "CREDITO",
  "PRIVADO",
  "CP",
  "LP",
  "MULTIMERCADO",
  "MULTI",
  "MM",
  "FIM",
  "CLASSE",
  "CLASSES",
  "INVESTIMENTO",
  "INVEST",
  "SUSTENTAVEL",
  "IS",
  "CI",
  "CIRF",
  "COTAS",
  "COTA",
  "LONGO",
  "PRAZO",
  "E"
)


normaliza_nome = function(x) {
  x %>%
    stri_trans_general("Latin-ASCII") %>%
    str_to_upper() %>%
    str_replace_all("[^A-Z0-9]+", " ") %>%
    str_squish() %>%
    str_split(" ") %>%
    map_chr(
      ~ paste(.x[!.x %in% termos_genericos], collapse = " ")
    )
}

# limpeza usada somente para exibição nos gráficos
termos_exibicao = c(
  "RESP",
  "LIMITADA",
  "RL",
  "FI",
  "FIF",
  "FIC",
  "CIC",
  "FIRF",
  "FIM",
  "CIRF",
  "FUNDO",
  "FUNDOS",
  "DE",
  "DO",
  "DA",
  "DOS",
  "DAS",
  "EM",
  "RENDA",
  "FIXA",
  "RF",
  "CP",
  "LP",
  "MULTIMERCADO",
  "MULTI",
  "MM",
  "CLASSE",
  "CLASSES",
  "INVESTIMENTO",
  "INVESTIMENTOS",
  "COTAS",
  "COTA",
  "LONGO",
  "PRAZO",
  "IS"
)

limpa_nome_exibicao = function(x) {
  x_limpo = x %>%
    str_replace_all("[()/,;_-]+", " ") %>%
    str_squish()

  str_split(x_limpo, "\\s+") %>%
    map_chr(function(tokens) {
      tokens_comparacao = tokens %>%
        stri_trans_general("Latin-ASCII") %>%
        str_to_upper()

      tokens_mantidos = tokens[
        !tokens_comparacao %in% termos_exibicao
      ]

      paste(tokens_mantidos, collapse = " ") %>%
        str_squish()
    })
}

## execução --------------------------------------------------------------

nomes_xlsx = taxas_xlsx %>%
  mutate(chave_xlsx = normaliza_nome(nome_xlsx))

nomes_quantum = fundos_raw %>%
  distinct(nome_quantum) %>%
  mutate(chave_quantum = normaliza_nome(nome_quantum))

matriz_distancia = adist(
  nomes_xlsx$chave_xlsx,
  nomes_quantum$chave_quantum,
  ignore.case = TRUE
)

indice_melhor = max.col(
  -matriz_distancia,
  ties.method = "first"
)

de_para = nomes_xlsx %>%
  mutate(
    nome_quantum = nomes_quantum$nome_quantum[indice_melhor]
  )

encontra_nome_quantum = function(padrao) {
  candidatos = nomes_quantum %>%
    filter(
      str_detect(
        nome_quantum,
        regex(padrao, ignore_case = TRUE)
      )
    ) %>%
    pull(nome_quantum)

  if (length(candidatos) != 1) {
    stop(
      "O padrão '",
      padrao,
      "' encontrou ",
      length(candidatos),
      " nomes na Quantum: ",
      paste(candidatos, collapse = " | ")
    )
  }

  candidatos
}

nome_novus_quantum = encontra_nome_quantum(
  "NOVUS.*HIGH GRADE"
)

nome_jgp_corporate_quantum = encontra_nome_quantum(
  "JGP.*CORPORATE.*FEEDER III"
)

# correções validadas:
# 1. Novus foi associado incorretamente à Tríade pelo algoritmo
# 2. O JGP Feeder II foi pesquisado por CNPJ e aparece como
#    Feeder III no nome exportado pela Quantum

de_para = de_para %>%
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

    similaridade = 1 -
      distancia /
        pmax(
          nchar(chave_xlsx),
          nchar(chave_quantum),
          1
        ),

    revisar = case_when(
      criterio_match %in%
        c(
          "Validado manualmente",
          "CNPJ validado manualmente"
        ) ~ FALSE,

      similaridade < 0.85 ~ TRUE,

      TRUE ~ FALSE
    ),

    nome_curto = limpa_nome_exibicao(nome_xlsx)
  ) %>%
  group_by(nome_curto) %>%
  mutate(
    # se dois fundos virarem o mesmo nome curto, mantém o nome
    # oficial completo para não misturar produtos distintos.
    nome_plot = if_else(
      n() > 1,
      nome_xlsx,
      nome_curto
    )
  ) %>%
  ungroup() %>%
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

## validação -------------------------------------------------------------

duplicados_de_para = de_para %>%
  count(nome_quantum, name = "n") %>%
  filter(n > 1)

if (nrow(duplicados_de_para) > 0) {
  print(
    de_para %>%
      semi_join(
        duplicados_de_para,
        by = "nome_quantum"
      ) %>%
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
  de_para %>%
    filter(revisar)
)

cat("\nColisões de nomes curtos:\n")

print(
  de_para %>%
    count(nome_curto) %>%
    filter(n > 1)
)

cat("\nFundos da Quantum sem correspondência:\n")

print(
  nomes_quantum %>%
    anti_join(de_para, by = "nome_quantum")
)

cat("\nFundos do XLSX sem correspondência:\n")

print(
  taxas_xlsx %>%
    anti_join(de_para, by = "nome_xlsx")
)

write_excel_csv2(
  de_para,
  "projects/criterios-selecao-fundos-cp/data/config/de_para_fundos_revisao.csv"
)

message("[01] Importação e de-para concluídos.")
