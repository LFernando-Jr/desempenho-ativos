# Pipeline canônico — critérios de seleção de fundos de crédito privado

if (!dir.exists("projects/criterios-selecao-fundos-cp")) {
  stop(
    "Abra a pasta raiz 'desempenho-ativos' no Positron antes de rodar este pipeline."
  )
}

dir.create(
  file.path("projects", "criterios-selecao-fundos-cp", "data", "intermediate"),
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  file.path("projects", "criterios-selecao-fundos-cp", "output", "figures"),
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  file.path("projects", "criterios-selecao-fundos-cp", "output", "reports"),
  recursive = TRUE,
  showWarnings = FALSE
)

source(
  file = "projects/criterios-selecao-fundos-cp/scripts/01_importacao_de_para.R",
  encoding = "UTF-8"
)

source(
  file = "projects/criterios-selecao-fundos-cp/scripts/02_retornos_e_elegibilidade.R",
  encoding = "UTF-8"
)

source(
  file = "projects/criterios-selecao-fundos-cp/scripts/03_metricas_e_correlacoes.R",
  encoding = "UTF-8"
)

source(
  file = "projects/criterios-selecao-fundos-cp/scripts/04_score_e_aprovacao.R",
  encoding = "UTF-8"
)

source(
  file = "projects/criterios-selecao-fundos-cp/scripts/05_redundancia_e_priorizacao.R",
  encoding = "UTF-8"
)

source(
  file = "projects/criterios-selecao-fundos-cp/scripts/06_exportacao.R",
  encoding = "UTF-8"
)

message("\nPipeline concluído.")
