
# Carregando pacotes ------------------------------------------------------

library(tidyverse)
library(magrittr)

# Setup -------------------------------------------------------------------

rm(list = ls())

Sys.setenv("LANGUAGE" = "Pt")
Sys.setlocale("LC_ALL", "Portuguese")

# Coleta de dados ---------------------------------------------------------

funds = readxl::read_excel(paste0(getwd(), "/dados/fundos.xlsx"), 
                           sheet = 1) %>%
  mutate(date  = as.Date(Data, format = "%d/%m/%Y"),
         name  = `Nome do Ativo`,
         value = Cota,
        .keep  = "none") %>% 
  rbind(., readxl::read_excel(paste0(getwd(), "/dados/index.xlsx"), 
                              sheet = 1) %>% 
          mutate(date  = as.Date(Data, format = "%d/%m/%Y"),
                 name  = `Nome do Ativo`,
                 value = `Número Índice`,
                .keep  = "none") %>% 
          filter(name == "IHFA"))

funds %<>% filter(!name == "OCCAM RETORNO ABSOLUTO ADVISORY FIC MULTIMERCADO")

cdi = readxl::read_excel(paste0(getwd(), "/dados/index.xlsx"), 
                         sheet = 1) %>%
  `colnames<-`(c("name",
                 "date",
                 "cdi")) %>%
  mutate(date = as.Date(date, format = "%d/%m/%Y"))  %>%
  filter(name == "CDI") %>%
  select(date, cdi)

df = inner_join(funds, cdi)

# Estrutura
glimpse(df)

# Tratamento de dados -----------------------------------------------------

data = df %>%
  #fundos
  arrange(name, date) %>%
  group_by(name) %>% 
  mutate(var = (value/lag(value, 1) - 1) * 100,
         acumulado_36_meses = (zoo::rollapply(1 + var/100, 
                                              width = 756, 
                                              FUN   = prod, 
                                              align = 'right', 
                                              fill  = NA) - 1)*100) %>%
  group_by(name, year(date), month(date)) %>%
  mutate(acumulado_mes = round((cumprod(1 + var/100) - 1) * 100, 
                               2)) %>%
  group_by(name, year(date)) %>%
  mutate(acumulado_ano = round((cumprod(1 + var/100) - 1) * 100, 
                               2)) %>% 
  ungroup() %>% 
  #cdi
  arrange(name, date) %>%
  group_by(name) %>% 
  mutate(var_cdi = (cdi/lag(cdi, 1) - 1) * 100,
         acumulado_36_meses_cdi = (zoo::rollapply(1 + var_cdi/100, 
                                                  width = 756,
                                                  FUN = prod, 
                                                  align = 'right',
                                                  fill = NA) - 1)*100) %>%
  group_by(name, year(date), month(date)) %>%
  mutate(acumulado_mes_cdi = round((cumprod(1 + var_cdi/100) - 1) * 100, 
                                   2)) %>%
  group_by(name, year(date)) %>%
  mutate(acumulado_ano_cdi = round((cumprod(1 + var_cdi/100) - 1) * 100,
                                   2)) %>%
  ungroup() %>% 
  #excesso
  mutate(
    excess_var_36_meses = (
      (1 + acumulado_36_meses/100)/(1 + acumulado_36_meses_cdi/100) - 1
    )*100
  ) %>%
  mutate(
    excess_var_ano = (
      (1 + acumulado_ano/100)/(1 + acumulado_ano_cdi/100) - 1
    )*100
  ) %>%
  mutate(
    excess_var_mes = (
      (1 + acumulado_mes/100)/(1 + acumulado_mes_cdi/100) - 1
    )*100
  ) %>% 
  select(c(1:2,15:17)) %>% 
  pivot_longer(cols = -c(1:2),
               names_to = "retorno")

lst_dt = data %>% 
  arrange(desc(date)) %>% 
  group_by(name, retorno) %>%
  slice(1) %>%
  ungroup()

# Visualização de dados ---------------------------------------------------

## Linhas -----------------------------------------------------------------

retorno = c("excess_var_36_meses",
            "excess_var_ano",
            "excess_var_mes")

for (i in retorno) {
  
  g = data %>% 
    filter(., case_when(
      i == "excess_var_36_meses" ~ date >= last(data$date) - 360,
      i == "excess_var_ano" ~ date >= floor_date(Sys.Date(), "year"),
      i == "excess_var_mes" ~ date >= floor_date(Sys.Date(), "month")),
      retorno == i) %>% 
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
                   i == "excess_var_36_meses" ~ "%b-%y",
                   i == "excess_var_ano" ~ "%b-%y",
                   i == "excess_var_mes" ~ "%d"), 
                 breaks = case_when(
                   i == "excess_var_36_meses" ~ "1 month",
                   i == "excess_var_ano" ~ "1 month",
                   i == "excess_var_mes" ~ "1 day")) +
    scale_colour_manual(
      values = c(
        "ABSOLUTE VERTEX FIC MULTIMERCADO"                 = "#2F47AD",
        "IHFA"                                             = "black",
        "JGP STRATEGY FIC MULTIMERCADO"                    = "#8C977D",
        "KINEA ATLAS II RESP LIMITADA FIF MULTIMERCADO"    = "#31AFE0",
        "SPX NIMITZ FEEDER FIC MULTIMERCADO"               = "#E47632",
        "KAPITALO ZETA FIC MULTIMERCADO"                   = "#AD4728",
        "OCCAM RETORNO ABSOLUTO ADVISORY FIC MULTIMERCADO" = "#3BA58B",
        "LEGACY CAPITAL ADVISORY FIC MULTIMERCADO"         = "#D4A83F",
        "VERDE AM X60 ADVISORY FIC MULTIMERCADO"           = "#2f5a3d"
      ),
      labels = c(
        "ABSOLUTE VERTEX FIC MULTIMERCADO" = paste0(
          "Absolute: ", 
          round(lst_dt$value[which(
            lst_dt$name == "ABSOLUTE VERTEX FIC MULTIMERCADO" & 
              lst_dt$retorno == i)],2), "%"),
        "IHFA" = paste0(
          "IHFA: ", 
          round(lst_dt$value[which(
            lst_dt$name == "IHFA" & 
              lst_dt$retorno == i)],2), "%"),
        "JGP STRATEGY FIC MULTIMERCADO" = paste0(
          "JGP: ", 
          round(lst_dt$value[which(
            lst_dt$name == "JGP STRATEGY FIC MULTIMERCADO" & 
              lst_dt$retorno == i)],2), "%"),
        "KINEA ATLAS II RESP LIMITADA FIF MULTIMERCADO" = paste0(
          "Kinea: ", 
          round(lst_dt$value[which(
            lst_dt$name == "KINEA ATLAS II RESP LIMITADA FIF MULTIMERCADO" & 
              lst_dt$retorno == i)],2), "%"),
        "SPX NIMITZ FEEDER FIC MULTIMERCADO" = paste0(
          "SPX: ", 
          round(lst_dt$value[which(
            lst_dt$name == "SPX NIMITZ FEEDER FIC MULTIMERCADO" & 
              lst_dt$retorno == i)],2), "%"),
        "KAPITALO ZETA FIC MULTIMERCADO" = paste0(
          "Kapitalo: ", 
          round(lst_dt$value[which(
            lst_dt$name == "KAPITALO ZETA FIC MULTIMERCADO" & 
              lst_dt$retorno == i)],2), "%"),
        "OCCAM RETORNO ABSOLUTO ADVISORY FIC MULTIMERCADO" = paste0(
          "Occam: ", 
          round(lst_dt$value[which(
            lst_dt$name == "OCCAM RETORNO ABSOLUTO ADVISORY FIC MULTIMERCADO" & 
              lst_dt$retorno == i)],2), "%"),
        "LEGACY CAPITAL ADVISORY FIC MULTIMERCADO" = paste0(
          "Legacy: ", 
          round(lst_dt$value[which(
            lst_dt$name == "LEGACY CAPITAL ADVISORY FIC MULTIMERCADO" & 
              lst_dt$retorno == i)],2), "%"),
        "VERDE AM X60 ADVISORY FIC MULTIMERCADO" = paste0(
          "Verde: ", 
          round(lst_dt$value[which(
            lst_dt$name == "VERDE AM X60 ADVISORY FIC MULTIMERCADO" & 
              lst_dt$retorno == i)],2), "%"))) +
    scale_linetype_manual(values = c(
      "ABSOLUTE VERTEX FIC MULTIMERCADO"                 = "solid",
      "IHFA"                                             = "longdash",
      "JGP STRATEGY FIC MULTIMERCADO"                    = "solid",
      "KINEA ATLAS II RESP LIMITADA FIF MULTIMERCADO"    = "solid",
      "SPX NIMITZ FEEDER FIC MULTIMERCADO"               = "solid",
      "KAPITALO ZETA FIC MULTIMERCADO"                   = "solid",
      "OCCAM RETORNO ABSOLUTO ADVISORY FIC MULTIMERCADO" = "solid",
      "LEGACY CAPITAL ADVISORY FIC MULTIMERCADO"         = "solid",
      "VERDE AM X60 ADVISORY FIC MULTIMERCADO"           = "solid"
    )) +
    guides(linetype = "none") +
    labs(title = NULL,
         subtitle = case_when(
           i == "excess_var_36_meses" ~ "Excesso de retorno trianual acumulado",
           i == "excess_var_ano" ~ "Excesso retorno acumulado no ano",
           i == "excess_var_mes" ~ "Excesso retorno acumulado no mês"),
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
         path = paste0(getwd(), "/gráficos/fundos"))
  
}

## Barras -----------------------------------------------------------------

for (i in retorno) {
  
  g = lst_dt %>%
    filter(retorno == i) %>% 
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
      hjust = ifelse(lst_dt$value[which(lst_dt$retorno == i)] > 0, 
                     -0.1, 1.1)) +
    scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "red")) + 
    scale_x_discrete(label = c(
      "ABSOLUTE VERTEX FIC MULTIMERCADO"                 = "Absolute",
      "IHFA"                                             = "IHFA",
      "JGP STRATEGY FIC MULTIMERCADO"                    = "JGP",
      "KINEA ATLAS II RESP LIMITADA FIF MULTIMERCADO"    = "Kinea",
      "SPX NIMITZ FEEDER FIC MULTIMERCADO"               = "SPX",
      "KAPITALO ZETA FIC MULTIMERCADO"                   = "Kapitalo",
      "OCCAM RETORNO ABSOLUTO ADVISORY FIC MULTIMERCADO" = "Occam",
      "LEGACY CAPITAL ADVISORY FIC MULTIMERCADO"         = "Legacy",
      "VERDE AM X60 ADVISORY FIC MULTIMERCADO"           = "Verde"
    )) +
    theme_bw() + 
    theme(legend.position = "none", 
          panel.border = element_blank(), 
          axis.line.x.bottom = element_line(color = "black"), 
          axis.line.y.left =  element_line(color = "black")) + 
    labs(subtitle = case_when( 
      i == "excess_var_36_meses" ~ "Excesso de retorno trianual acumulado",
      i == "excess_var_ano" ~ "Excesso retorno acumulado no ano",
      i == "excess_var_mes" ~ "Excesso retorno acumulado no mês"),
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
         path = paste0(getwd(), "/gráficos/fundos"))
  
}
