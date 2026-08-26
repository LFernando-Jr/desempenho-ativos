source("renv/activate.R")

# Lista de pacotes a serem carregados -------------------------------------

pacotes = c(
  "conflicted",
  "tidyverse",
  "magrittr",
  "readxl",
  "openxlsx",
  "stringi",
  "slider",
  "scales",
  "cluster",
  "quantmod",
  "janitor",
  "bizdays",
  "tidyquant",
  "PerformanceAnalytics",
  "lubridate",
  "zoo",
  "ggrepel"
)

lapply(pacotes, library, character.only = TRUE)

rm(pacotes)

# Conflitos ---------------------------------------------------------------

conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("view", "tibble")
conflict_prefer("set_names", "magrittr")
conflict_prefer("last", "dplyr")
conflict_prefer("first", "dplyr")

# Seja bem-vindo! ---------------------------------------------------------

message(paste(
  "Conflitos resolvidos.",
  "Ambiente pronto.",
  "Bem-vindo ao projeto 'desempenho-ativos'!",
  sep = " "
))
