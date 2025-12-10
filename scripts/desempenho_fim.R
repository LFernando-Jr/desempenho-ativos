
# Setup -------------------------------------------------------------------

rm(list = ls())

# Coleta de dados ---------------------------------------------------------

fim = read_excel("dados/desempenho_fim.xlsx", sheet = 1) %>%
  mutate(date  = as.Date(Data, format = "%d/%m/%Y"),
         name  = `Nome do Ativo`,
         value = Cota,
        .keep  = "none") %>% 
  rbind(., read_excel("dados/desempenho_onshore.xlsx", sheet = 1) %>% 
          mutate(date  = as.Date(Data, format = "%d/%m/%Y"),
                 name  = `Nome do Ativo`,
                 value = `Número Índice`,
                .keep  = "none") %>% 
          dplyr::filter(name == "IHFA")) %>% 
  mutate(name = case_when(
    name == "ABSOLUTE VERTEX FIF CIC MULTIMERCADO" ~ "Vertex",
    name == "JGP STRATEGY FIC MULTIMERCADO" ~ "Strategy",
    name == "KAPITALO ZETA FIC MULTIMERCADO" ~ "Zeta",                                     
    name == "KINEA ATLAS II RESP LIMITADA FIF MULTIMERCADO" ~ "Atlas II",
    name == "LEGACY CAPITAL ADVISORY FIC MULTIMERCADO" ~ "Legacy",
    name == paste0("OCCAM RETORNO ABSOLUTO ADVISORY ",
                   "RESP LIMITADA FIF CIC MULTIMERCADO") ~ "Retorno Absoluto",
    name == "SPX NIMITZ FEEDER FIC MULTIMERCADO" ~ "Nimitz",                                 
    name == "VERDE AM X60 ADVISORY FIC MULTIMERCADO" ~ "Verde",
    name == "IHFA" ~ "IHFA"
    ))

fim$name %>% unique()

fim %<>% dplyr::filter(name != "Retorno Absoluto")

cdi = read_excel("dados/desempenho_onshore.xlsx", sheet = 1) %>%
  mutate(date  = as.Date(Data, format = "%d/%m/%Y"),
         name  = `Nome do Ativo`,
         cdi   = `Número Índice`,
        .keep  = "none") %>%
  dplyr::filter(name == "CDI") %>%
  select(date, cdi)

data_raw = inner_join(fim, cdi)

glimpse(data_raw)

# Tratamento de dados -----------------------------------------------------

data = data_raw %>%
  #fundos
  arrange(name, date) %>%
  group_by(name) %>% 
  mutate(var = (value/dplyr::lag(value, 1) - 1),
         acumulado_36_meses = (zoo::rollapply(1 + var, 
                                              width = 756, 
                                              FUN   = prod, 
                                              align = 'right', 
                                              fill  = NA) - 1)) %>%
  group_by(name, year(date), month(date)) %>%
  mutate(acumulado_mes = cumprod(1 + var) - 1) %>%
  group_by(name, year(date)) %>%
  mutate(acumulado_ano = cumprod(1 + var) - 1) %>% 
  ungroup() %>% 
  #cdi
  arrange(name, date) %>%
  group_by(name) %>%
  mutate(var_cdi = (cdi/dplyr::lag(cdi, 1) - 1),
         acumulado_36_meses_cdi = (zoo::rollapply(1 + var_cdi, 
                                                  width = 756,
                                                  FUN = prod, 
                                                  align = 'right',
                                                  fill = NA) - 1)) %>%
  group_by(name, year(date), month(date)) %>%
  mutate(acumulado_mes_cdi = cumprod(1 + var_cdi) - 1) %>%
  group_by(name, year(date)) %>%
  mutate(acumulado_ano_cdi = cumprod(1 + var_cdi) - 1) %>%
  ungroup() %>% 
  #excesso
  mutate(excess_var_36_meses = (
    ((1 + acumulado_36_meses)/(1 + acumulado_36_meses_cdi))^(252/756) - 1
    )) %>%
  mutate(excess_var_ano = (
    (1 + acumulado_ano)/(1 + acumulado_ano_cdi) - 1
    )) %>%
  mutate(excess_var_mes = (
    (1 + acumulado_mes)/(1 + acumulado_mes_cdi) - 1
    )) %>% 
  select(c(1:2,15:17)) %>% 
  pivot_longer(cols = -c(1:2),
               names_to = "retorno") %>% 
  mutate(value = round(value*100,2))

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
    dplyr::filter(., case_when(
      i == "excess_var_36_meses" ~ date >= Sys.Date()-365,
      i == "excess_var_ano" ~ date >= floor_date(Sys.Date(), "year"),
      i == "excess_var_mes" ~ date >= floor_date(Sys.Date(), "month")),
      retorno == i) %>% 
    mutate(name = factor(name, 
                         levels = arrange(dplyr::filter(lst_dt, retorno == i), 
                                          desc(value)
                         )$name)) %>%
    ggplot() +
    geom_hline(yintercept = 0) +
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
        "Vertex"           = "#2F47AD",
        "IHFA"             = "black",
        "Strategy"         = "#8C977D",
        "Atlas II"         = "#31AFE0",
        "Nimitz"           = "#E47632",
        "Zeta"             = "#AD4728",
        "Retorno Absoluto" = "#3BA58B",
        "Legacy"           = "#D4A83F",
        "Verde"            = "#2f5a3d"
      ),
      labels = c("Vertex" = paste0("Absolute: ", round(lst_dt$value[which(lst_dt$name == "Vertex" & lst_dt$retorno == i)],2), "%"),
                 "IHFA" = paste0("IHFA: ", round(lst_dt$value[which(lst_dt$name == "IHFA" & lst_dt$retorno == i)],2), "%"),
                 "Strategy" = paste0("JGP: ", round(lst_dt$value[which(lst_dt$name == "Strategy" & lst_dt$retorno == i)],2), "%"),
                 "Atlas II" = paste0("Kinea: ", round(lst_dt$value[which(lst_dt$name == "Atlas II" & lst_dt$retorno == i)],2), "%"),
                 "Nimitz" = paste0("SPX: ", round(lst_dt$value[which(lst_dt$name == "Nimitz" & lst_dt$retorno == i)],2), "%"),
                 "Zeta" = paste0("Kapitalo: ", round(lst_dt$value[which(lst_dt$name == "Zeta" &lst_dt$retorno == i)],2), "%"),
                 "Retorno Absoluto" = paste0("Occam: ", round(lst_dt$value[which(lst_dt$name == "Retorno Absoluto" & lst_dt$retorno == i)],2), "%"),
                 "Legacy" = paste0("Legacy: ", round(lst_dt$value[which(lst_dt$name == "Legacy" & lst_dt$retorno == i)],2), "%"),
                 "Verde" = paste0("Verde: ", round(lst_dt$value[which(lst_dt$name == "Verde" & lst_dt$retorno == i)],2), "%"))) +
    scale_linetype_manual(values = c("Vertex"           = "solid",
                                     "IHFA"             = "longdash",
                                     "Strategy"         = "solid",
                                     "Atlas II"         = "solid",
                                     "Nimitz"           = "solid",
                                     "Zeta"             = "solid",
                                     "Retorno Absoluto" = "solid",
                                     "Legacy"           = "solid",
                                     "Verde"            = "solid")) +
    guides(linetype = "none") +
    labs(title = NULL,
         subtitle = case_when(
           i == "excess_var_36_meses" ~ "Excesso de retorno trianual ao ano",
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
         path = paste0(getwd(), "/saídas/fim"))
  
  }

## Barras -----------------------------------------------------------------

for (i in retorno) {
  
  g = lst_dt %>%
    dplyr::filter(retorno == i) %>% 
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
      i == "excess_var_36_meses" ~ "Excesso de retorno trianual ao ano",
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
         path = paste0(getwd(), "/saídas/fim"))
  
}
