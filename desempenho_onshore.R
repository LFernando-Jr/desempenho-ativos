
# Pacotes -----------------------------------------------------------------

library(tidyverse)
library(magrittr)

# Setup -------------------------------------------------------------------

rm(list = ls())

Sys.setenv("LANGUAGE" = "Pt")
Sys.setlocale("LC_ALL", "Portuguese")

# Coleta de dados ---------------------------------------------------------

data = readxl::read_excel(paste0(getwd(), "/dados/index.xlsx"), 
                          sheet = 1) %>%
  mutate(date  = as.Date(Data, format = "%d/%m/%Y"),
         name  = `Nome do Ativo`,
         value = `Número Índice`,
        .keep  = "none")

# Estrutura
glimpse(data)

# Tratamento de dados -----------------------------------------------------

data %<>% 
  group_by(name) %>% 
  mutate(name = case_when(name == "IDA-IPCA Infraestrutura"~"IDA-Infra", 
                          TRUE ~ name),
         var = (value/lag(value, 1) - 1) * 100,
         acumulado_12_meses = (zoo::rollapply(1 + var/100, 
                                              width = 252, 
                                              FUN = prod, 
                                              align = 'right', 
                                              fill = NA) - 1)*100) %>%
  group_by(name, year(date), month(date)) %>%
  mutate(acumulado_mes = round((cumprod(1 + var/100) - 1) * 100, 2)) %>%
  group_by(name, year(date)) %>%
  mutate(acumulado_ano = round((cumprod(1 + var/100) - 1) * 100, 2)) %>% 
  ungroup() %>% 
  select(-c(3,4,6,7)) %>% 
  pivot_longer(cols = -c(1:2),
               names_to = "retorno")

lst_dt = data %>% 
  arrange(desc(date)) %>% 
  group_by(name, retorno) %>%
  slice(1) %>%
  ungroup() 

# Visualização de dados ---------------------------------------------------

## Linhas -----------------------------------------------------------------

retorno = c("acumulado_12_meses",
            "acumulado_ano",
            "acumulado_mes")

for (i in retorno) {
  
  g = data %>% 
    filter(., case_when(
      i == "acumulado_12_meses" ~ date >= last(data$date) - 360,
      i == "acumulado_ano" ~ date >= floor_date(Sys.Date(), "year"),
      i == "acumulado_mes" ~ date >= floor_date(Sys.Date(), "month")),
      retorno == i,
      !name %in% c("Idex-Infra Geral JGP",
                   "Idex-CDI Geral JGP")) %>% 
    mutate(name = factor(name, 
                         levels = arrange(filter(lst_dt, retorno == i), 
                                          desc(value)
                         )$name)) %>%
    ggplot() +
    aes(date, value, colour = name, linetype = name) +
    geom_line(linewidth = .75) +
    theme_bw() + theme(panel.grid.minor = element_blank(), 
                       axis.line        = element_line(colour = "black"),
                       legend.title     = element_blank(), 
                       axis.title       = element_blank(), 
                       strip.background = element_blank()) + 
    scale_x_date(expand = c(0,0), 
                 date_labels = case_when(
                   i == "acumulado_12_meses" ~ "%b-%y",
                   i == "acumulado_ano" ~ "%b-%y",
                   i == "acumulado_mes" ~ "%d"), 
                 breaks = case_when(
                   i == "acumulado_12_meses" ~ "1 month",
                   i == "acumulado_ano" ~ "1 month",
                   i == "acumulado_mes" ~ "1 day")) +
    scale_colour_manual(
      values = c(
        "Ibovespa" = "#2F47AD",
        "CDI"      = "black",
        "IHFA"     = "#E47632",
        "IDA-DI"   = "#FFBB78",
        "IMA-B"    = "#D62728",
        "IMA-B 5"  = "#31AFE0",
        "IRF-M"    = "#9467BD",
        "SMLL"     = "#E377C2",
        "IFIX"     = "#7F5A58",
        "Dólar"    = "#AEC7E8"
        ),
      labels = c(
        "Ibovespa" = paste0(
          "Ibovespa: ", 
          round(lst_dt$value[which(lst_dt$name == "Ibovespa" & 
                                     lst_dt$retorno == i)],2), "%"),
        "CDI" = paste0(
          "CDI: ", 
          round(lst_dt$value[which(lst_dt$name == "CDI" & 
                                     lst_dt$retorno == i)],2), "%"),
        "IDA-DI" = paste0(
          "IDA-DI: ", 
          round(lst_dt$value[which(lst_dt$name == "IDA-DI" & 
                                     lst_dt$retorno == i)],2), "%"),
        "IHFA" = paste0(
          "IHFA: ", 
          round(lst_dt$value[which(lst_dt$name == "IHFA" & 
                                     lst_dt$retorno == i)],2), "%"),
        "IFIX" = paste0(
          "IFIX: ", 
          round(lst_dt$value[which(lst_dt$name == "IFIX" & 
                                     lst_dt$retorno == i)],2), "%"),
        "SMLL" = paste0(
          "SMLL: ", 
          round(lst_dt$value[which(lst_dt$name == "SMLL" & 
                                     lst_dt$retorno == i)],2), "%"),
        "IMA-B" = paste0(
          "IMA-B: ", 
          round(lst_dt$value[which(lst_dt$name == "IMA-B" & 
                                     lst_dt$retorno == i)],2), "%"),
        "IMA-B 5" = paste0(
          "IMA-B 5: ",
          round(lst_dt$value[which(lst_dt$name == "IMA-B 5" & 
                                     lst_dt$retorno == i)],2), "%"),
        "IRF-M" = paste0(
          "IRF-M: ", 
          round(lst_dt$value[which(lst_dt$name == "IRF-M" & 
                                     lst_dt$retorno == i)],2), "%"),
        "Dólar" = paste0(
          "Dólar: ", 
          round(lst_dt$value[which(lst_dt$name == "Dólar" & 
                                     lst_dt$retorno == i)],2), "%")
      )
    ) +
    scale_linetype_manual(values = c("CDI"                  = "longdash", 
                                     "Ibovespa"             = "solid",
                                     "IDA-DI"               = "solid",
                                     "Idex-Infra Geral JGP" = "solid",
                                     "Idex-CDI Geral JGP"   = "solid",
                                     "IHFA"                 = "solid",
                                     "IMA-B"                = "solid",
                                     "IMA-B 5"              = "solid",
                                     "IRF-M"                = "solid",
                                     "Dólar"                = "solid",
                                     "IFIX"                 = "solid",
                                     "SMLL"                 = "solid")) +
    guides(linetype = "none") +
    labs(title = NULL,
         subtitle = case_when(
           i == "acumulado_12_meses" ~ "Retorno acumulado em 12 meses",
           i == "acumulado_ano" ~ "Retorno acumulado no ano",
           i == "acumulado_mes" ~ "Retorno acumulado no mês"),
         caption = paste0("Capri FO com dados da Quantum Axis até ", 
                          as.Date(lst_dt$date, format = "%dd-%mm-yy%")))
  
  print(g)
  
  ggsave(paste0(i, ".png"), 
         width = 4800, 
         # width = 15,
         height = 2160, 
         # height = 8.661, 
         units = "px", 
         # units = "in", 
         dpi = 576, 
         # dpi = 800,
         path = paste0(getwd(), "/gráficos/onshore"))
}
  
## Barras -----------------------------------------------------------------

for (i in retorno) {
  
  g = lst_dt %>%
    filter(retorno == i,
           !name %in% c("Idex-CDI Geral JGP",
                        "Idex-Infra Geral JGP")) %>%
    ggplot() +
    aes(x = reorder(name, value), 
        y = value, fill = value > 0) +
    geom_bar(stat = "identity") +
    coord_flip(
      ylim = c(min(lst_dt$value[which(lst_dt$retorno == i)]) 
               - ifelse(i == "acumulado_mes", 2, 5),
               max(lst_dt$value[which(lst_dt$retorno == i)]) 
               + ifelse(i == "acumulado_mes", 2, 5))
    ) +
    geom_text(
      aes(label = paste0(round(value, 2), "%")),
      hjust = ifelse(lst_dt$value[which(lst_dt$retorno == i &
                                          !lst_dt$name %in% c(
                                            "Idex-CDI Geral JGP",
                                            "Idex-Infra Geral JGP"
                                            ))] > 0,
                     -0.1, 1.1)) +
    scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "red")) + 
    theme_bw() + 
    theme(legend.position = "none", 
          panel.border = element_blank(), 
          axis.line.x.bottom = element_line(color = "black"), 
          axis.line.y.left =  element_line(color = "black")) + 
    labs(subtitle = case_when( 
      i == "acumulado_12_meses" ~ "Retorno acumulado em 12 meses", 
      i == "acumulado_ano" ~ "Retorno acumulado no ano", 
      i == "acumulado_mes" ~ "Retorno acumulado no mês"), 
      x = NULL, 
      y = "Retorno (%)",
      caption = paste0("Capri FO com dados da Quantum Axis até ", 
                       as.Date(lst_dt$date, format = "%dd-%mm-yy%")))
  
  print(g)
  
  ggsave(paste0(i, "_barras.png"), 
         width = 4800, 
         # width = 15,
         height = 2160, 
         # height = 8.661,
         units = "px",
         # units = "in",
         dpi = 576, 
         # dpi = 800,
         path = paste0(getwd(), "/gráficos/onshore"))
}
