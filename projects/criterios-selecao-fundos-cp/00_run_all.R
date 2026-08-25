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

etapas = c(
  "projects/criterios-selecao-fundos-cp/scripts/01_importacao_de_para.R",
  "projects/criterios-selecao-fundos-cp/scripts/02_retornos_e_elegibilidade.R",
  "projects/criterios-selecao-fundos-cp/scripts/03_metricas_e_clusters.R",
  "projects/criterios-selecao-fundos-cp/scripts/04_score_e_shortlist.R",
  "projects/criterios-selecao-fundos-cp/scripts/05_exportacao.R"
)

for (etapa in etapas) {
  message("\nExecutando: ", etapa)
  source(etapa, encoding = "UTF-8")
}

message("\nPipeline concluído.")
