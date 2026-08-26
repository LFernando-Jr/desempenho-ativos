# SETUP INICIAL — CRITÉRIOS DE SELEÇÃO DE FUNDOS
# Execute apenas ao configurar o projeto em uma nova máquina.

if (!dir.exists("projects/criterios-selecao-fundos-cp")) {
  stop(
    "Abra a pasta raiz 'desempenho-ativos' no Positron antes de executar o setup."
  )
}

pacotes_necessarios = c(
  "conflicted",
  "tidyverse",
  "magrittr",
  "readxl",
  "openxlsx",
  "stringi",
  "slider",
  "scales",
  "cluster",
  "janitor",
  "lubridate",
  "zoo",
  "ggrepel"
)

pacotes_ausentes = pacotes_necessarios[
  !vapply(
    X = pacotes_necessarios,
    FUN = requireNamespace,
    FUN.VALUE = logical(1),
    quietly = TRUE
  )
]

if (length(pacotes_ausentes) > 0) {
  stop(
    "Pacotes ausentes: ",
    paste(pacotes_ausentes, collapse = ", "),
    ". Execute renv::restore() antes de rodar o pipeline."
  )
}

arquivos_entrada = c(
  "projects/criterios-selecao-fundos-cp/data/input/analise_quantitativa_fundos_high_grade.xlsx",
  "projects/criterios-selecao-fundos-cp/data/input/funds_hist.csv",
  "projects/criterios-selecao-fundos-cp/data/input/benchs_hist.csv"
)

arquivos_ausentes = arquivos_entrada[!file.exists(arquivos_entrada)]

if (length(arquivos_ausentes) > 0) {
  stop(
    "Arquivos de entrada ausentes: ",
    paste(arquivos_ausentes, collapse = ", ")
  )
}

message("Setup validado. O pipeline está pronto para execução.")
