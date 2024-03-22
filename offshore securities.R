
# Pacotes -----------------------------------------------------------------

library(tidyverse)
library(Quandl)

# Setup -------------------------------------------------------------------

rm(list = ls())

Sys.setenv("LANGUAGE" = "Pt")
Sys.setlocale("LC_ALL", "Portuguese")

Quandl.api_key('Kn6L-n4knnqdN_j8FpAu')

# Coleta de dados ---------------------------------------------------------

getSymbols(c("CBU7.L"), src = 'yahoo', return.class = "data.frame")

CBU7.L
