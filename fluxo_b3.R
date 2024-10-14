
# Pacotes -----------------------------------------------------------------

library(tidyverse)

# Setup -------------------------------------------------------------------

rm(list = ls())

Sys.setenv("LANGUAGE" = "Pt")
Sys.setlocale("LC_ALL", "Portuguese")

# Coleta de dados ---------------------------------------------------------

data = read.csv2(paste0(getwd(), "/Dados/fluxo-estrangeiro.csv"),
                  sep = ",", 
                  header = TRUE) %>% 
  as_tibble() %>% 
  mutate(Data = as.Date(Data, format = "%d/%m/%Y"),
         across(where(is.character), ~ . %>%
                  str_replace_all(" mi", "") %>%
                  str_replace_all("\\.", "") %>%
                  str_replace_all(",", ".") %>%
                  as.numeric())) %>% 
  `colnames<-`(c("date",
                 "Estrangeiro",
                 "Institucional",
                 "Pessoa Física",
                 "Insituição Financeira",
                 "Outros"))

# Tratamento de dados -----------------------------------------------------

data %<>% 
  arrange(date) %>% 
  pivot_longer(-date) %>% 
  mutate(acc_y   = if_else(date >= "2024-01-01", cumsum(value), NA_real_),
         acc_12m = cumsum(value))

# Visualização de dados ---------------------------------------------------

data %>% 
  ggplot() +
  geom_line(aes(x = date, y = acc_12m)) +
  facet_wrap(~name, scales = "free", nrow = 5)
