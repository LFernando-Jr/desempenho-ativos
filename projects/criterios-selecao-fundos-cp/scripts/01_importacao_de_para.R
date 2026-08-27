# ETAPA 1 — IMPORTAÇÃO E DE-PARA

# Limpa os objetos da sessão para evitar dependências de execuções anteriores.
rm(list = ls())

# ------------------------------------------------------------
# 0. Caminhos
# ------------------------------------------------------------

# Caminhos dos insumos e das saídas desta etapa.
path_xlsx = "projects/criterios-selecao-fundos-cp/data/input/analise_quantitativa_fundos_high_grade.xlsx"
path_fundos = "projects/criterios-selecao-fundos-cp/data/input/funds_hist.csv"
path_benchs = "projects/criterios-selecao-fundos-cp/data/input/benchs_hist.csv"
path_intermediate = "projects/criterios-selecao-fundos-cp/data/intermediate"
path_de_para_manual = "projects/criterios-selecao-fundos-cp/data/config/de_para_fundos_revisao.csv"
path_de_para_sugerido = file.path(
  path_intermediate,
  "de_para_fundos_sugerido.csv"
)

dir.create(
  path = path_intermediate,
  recursive = TRUE,
  showWarnings = FALSE
)

# Define a leitura dos arquivos exportados pela Quantum.
locale_quantum = locale(
  decimal_mark = ",",
  grouping_mark = ".",
  encoding = "Windows-1252"
)

# ------------------------------------------------------------
# 1. Importação
# ------------------------------------------------------------

fundos_raw = read_delim(
  file = path_fundos,
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
  file = path_benchs,
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

cadastro_fundos = read_excel(
  path = path_xlsx,
  sheet = "Base Tratada"
) %>%
  clean_names() %>%
  transmute(
    nome_xlsx = fundo,
    taxa_adm_aa = as.numeric(taxa_de_administracao)
  ) %>%
  filter(!is.na(nome_xlsx))

message("[01] Fundos no histórico: ", n_distinct(fundos_raw$nome_quantum))
message("[01] Fundos no cadastro: ", n_distinct(cadastro_fundos$nome_xlsx))
message("[01] Benchmarks: ", paste(unique(benchs_raw$benchmark), collapse = ", "))

# Travas contra duplicações na matéria-prima.
if (anyDuplicated(fundos_raw[c("nome_quantum", "data")]) > 0) {
  stop("Há mais de uma cota para o mesmo fundo e data em funds_hist.csv.")
}

message("[01] Cotas sem duplicatas por fundo e data.")

if (anyDuplicated(benchs_raw[c("benchmark", "data")]) > 0) {
  stop("Há mais de um nível para o mesmo benchmark e data em benchs_hist.csv.")
}

message("[01] Benchmarks sem duplicatas por série e data.")

# ------------------------------------------------------------
# 2. Funções para nomes
# ------------------------------------------------------------

# Termos removidos apenas para comparar os nomes do cadastro com os da Quantum.
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

# Normaliza os nomes para sugerir o de-para, sem alterar os nomes exibidos.
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

# Termos removidos somente dos nomes exibidos nos gráficos.
# Preserva marca, estratégia, Advisory, Plus, High Grade,
# Yield, Feeder, números e outros termos distintivos.
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

# Simplifica os nomes de exibição sem eliminar termos distintivos dos produtos.
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

# ------------------------------------------------------------
# 3. De-para dos nomes
# ------------------------------------------------------------
# A similaridade serve apenas para sugerir correspondências.
# Novus e JGP são exceções validadas manualmente.

nomes_xlsx = cadastro_fundos %>%
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

# Localiza um único nome da Quantum a partir de um padrão.
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

# Correções validadas:
# 1. Novus foi associado incorretamente à Tríade pelo algoritmo.
# 2. O JGP Feeder II foi pesquisado por CNPJ e aparece como
#    Feeder III no nome exportado pela Quantum.
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
    # Se dois fundos virarem o mesmo nome curto, mantém o nome
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

# Aplica somente as correspondências explicitamente preenchidas na configuração.
# O arquivo manual nunca é sobrescrito pelo pipeline.
if (file.exists(path_de_para_manual)) {
  de_para_manual = read_csv2(
    file = path_de_para_manual,
    show_col_types = FALSE
  ) %>%
    clean_names()

  colunas_manuais_necessarias = c("nome_xlsx", "nome_quantum")
  colunas_manuais_ausentes = setdiff(
    colunas_manuais_necessarias,
    names(de_para_manual)
  )

  if (length(colunas_manuais_ausentes) > 0) {
    stop(
      "O de-para manual não contém as colunas: ",
      paste(colunas_manuais_ausentes, collapse = ", "),
      "."
    )
  }

  de_para_manual = de_para_manual %>%
    transmute(
      nome_xlsx,
      nome_quantum_manual = nome_quantum
    ) %>%
    filter(
      !is.na(nome_xlsx),
      nome_xlsx != "",
      !is.na(nome_quantum_manual),
      nome_quantum_manual != ""
    )

  if (anyDuplicated(de_para_manual$nome_xlsx) > 0) {
    stop("O de-para manual contém mais de uma linha para o mesmo nome_xlsx.")
  }

  nomes_xlsx_invalidos = setdiff(
    de_para_manual$nome_xlsx,
    cadastro_fundos$nome_xlsx
  )

  nomes_quantum_invalidos = setdiff(
    de_para_manual$nome_quantum_manual,
    nomes_quantum$nome_quantum
  )

  if (length(nomes_xlsx_invalidos) > 0) {
    stop(
      "O de-para manual contém fundos ausentes do cadastro: ",
      paste(nomes_xlsx_invalidos, collapse = " | "),
      "."
    )
  }

  if (length(nomes_quantum_invalidos) > 0) {
    stop(
      "O de-para manual contém nomes ausentes da Quantum: ",
      paste(nomes_quantum_invalidos, collapse = " | "),
      "."
    )
  }

  de_para = de_para %>%
    left_join(
      de_para_manual,
      by = "nome_xlsx",
      relationship = "one-to-one"
    ) %>%
    mutate(
      correspondencia_manual = !is.na(nome_quantum_manual),
      nome_quantum = coalesce(nome_quantum_manual, nome_quantum),
      chave_xlsx_manual = normaliza_nome(nome_xlsx),
      chave_quantum_manual = normaliza_nome(nome_quantum),
      distancia_manual = map2_int(
        chave_xlsx_manual,
        chave_quantum_manual,
        ~ as.integer(adist(.x, .y)[1])
      ),
      criterio_match = if_else(
        correspondencia_manual,
        "Validado no arquivo de configuração",
        criterio_match
      ),
      similaridade = if_else(
        correspondencia_manual,
        1 -
          distancia_manual /
            pmax(
              nchar(chave_xlsx_manual),
              nchar(chave_quantum_manual),
              1
            ),
        similaridade
      ),
      revisar = if_else(
        correspondencia_manual,
        FALSE,
        revisar
      )
    ) %>%
    select(
      -nome_quantum_manual,
      -correspondencia_manual,
      -chave_xlsx_manual,
      -chave_quantum_manual,
      -distancia_manual
    )

  message(
    "[01] Correspondências manuais aplicadas: ",
    nrow(de_para_manual),
    "."
  )
} else {
  message(
    "[01] De-para manual ausente; utilizadas apenas sugestões e exceções ",
    "codificadas."
  )
}

# ------------------------------------------------------------
# 3.1. Validação do de-para
# ------------------------------------------------------------

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

message("[01] De-para sem duplicidade de fundos da Quantum.")

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
  cadastro_fundos %>%
    anti_join(de_para, by = "nome_xlsx")
)

write_excel_csv2(
  x = de_para,
  file = path_de_para_sugerido
)

write_rds(
  x = fundos_raw,
  file = file.path(path_intermediate, "fundos_raw.rds")
)

write_rds(
  x = benchs_raw,
  file = file.path(path_intermediate, "benchs_raw.rds")
)

write_rds(
  x = cadastro_fundos,
  file = file.path(path_intermediate, "cadastro_fundos.rds")
)

write_rds(
  x = de_para,
  file = file.path(path_intermediate, "de_para_fundos.rds")
)

message("[01] Importação e de-para concluídos.")
