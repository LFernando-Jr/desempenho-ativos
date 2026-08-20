# Pipeline canônico — critérios de seleção de fundos de crédito privado

arquivo_atual = tryCatch(
  normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = TRUE),
  error = function(e) normalizePath("00_run_all.R", winslash = "/", mustWork = TRUE)
)
raiz_projeto = dirname(arquivo_atual)
diretorio_anterior = getwd()
on.exit(setwd(diretorio_anterior), add = TRUE)
setwd(raiz_projeto)

dir.create(file.path("data", "intermediate"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path("output", "figures"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path("output", "reports"), recursive = TRUE, showWarnings = FALSE)

etapas = c(
  "scripts/01_importacao_de_para.R",
  "scripts/02_retornos_e_elegibilidade.R",
  "scripts/03_metricas_e_clusters.R",
  "scripts/04_score_e_shortlist.R",
  "scripts/05_exportacao.R"
)

for (etapa in etapas) {
  message("\nExecutando: ", etapa)
  source(etapa, encoding = "UTF-8")
}

message("\nPipeline concluído.")
