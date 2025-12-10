
# Setup -------------------------------------------------------------------

rm(list = ls())

# Coleta de dados ---------------------------------------------------------

data = readxl::read_excel(paste0(getwd(), "/dados/index.xlsx"), 
                          sheet = 1) %>%
  mutate(date  = as.Date(Data, format = "%d/%m/%Y"),
         name  = `Nome do Ativo`,
         value = `Número Índice`,
        .keep  = "none")

glimpse(data)

# Tratamento de dados -----------------------------------------------------

data %<>% 
  filter(date >= "2024-03-19",
         date <= "2024-12-31") %>% 
  group_by(name) %>% 
  mutate(name = case_when(name == "IDA-IPCA Infraestrutura"~"IDA-Infra", 
                          TRUE ~ name),
         var = (value/lag(value, 1) - 1) * 100,
         acumulado_12_meses = (zoo::rollapply(1 + var/100, 
                                              width = 252, 
                                              FUN = prod, 
                                              align = 'right', 
                                              fill = NA) - 1)*100) %>%
  summarise(acc = (prod(1 + var/100, na.rm = TRUE) - 1)*100)

data %<>% rbind(., c("Carteira", 8.57)) %>% 
  mutate(value = as.numeric(acc))


lst_dt = data %>% 
  arrange(desc(date)) %>% 
  group_by(name, retorno) %>%
  slice(1) %>%
  ungroup() 

# Visualização de dados ---------------------------------------------------

## Barras -----------------------------------------------------------------

lst_dt %>%
  filter(!name %in% c("Idex-CDI Geral JGP",
                      "Idex-Infra Geral JGP")) %>%
  ggplot() +
  aes(x = reorder(name, value), 
      y = value, fill = name) +
  geom_bar(stat = "identity") +
  coord_flip(ylim = c(-25, 30)
             #  c(min(lst_dt$value[which(lst_dt$retorno == i)]) 
             # - ifelse(i == "acumulado_mes", 2, 5),
             # max(lst_dt$value[which(lst_dt$retorno == i)]) 
             # + ifelse(i == "acumulado_mes", 2, 5))
  ) +
  geom_text(
    aes(label = paste0(round(value, 2), "%")),
    hjust = ifelse(lst_dt$value[which(!lst_dt$name %in% c(
      "Idex-CDI Geral JGP",
      "Idex-Infra Geral JGP"
    ))] > 0,
    -0.1, 1.1)
  ) +
  # scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "red", "Carteira" = "black")) + 
  scale_fill_manual(values = c("Carteira" = "steelblue",
                               "Dólar" = "steelblue",
                               "IDA-DI" = "steelblue", 
                               "CDI" = "steelblue",
                               "IHFA" = "steelblue", 
                               "IMA-B 5" = "steelblue", 
                               "IRF-M" = "steelblue", 
                               "IDA-Infra" = "red",
                               "IMA-B" = "red", 
                               "IFIX" = "red",
                               "Ibovespa" = "red",
                               "SMLL" = "red")) + 
  theme_bw() + 
  theme(legend.position = "none", 
        panel.grid = element_blank(),
        panel.border = element_blank(), 
        axis.line.x.bottom = element_line(color = "black"),
        axis.line.y.left =  element_line(color = "black")
        ) + 
  labs(subtitle = "Retorno acumulado entre 19/03 e 31/12",
       x = NULL, 
       y = "Retorno (%)",
       caption = paste0("Capri FO com dados da Quantum Axis"))

ggsave("acc_carteira_barras.png", 
       width = 4800, 
       # width = 15,
       height = 2160, 
       # height = 8.661,
       units = "px",
       # units = "in",
       dpi = 576, 
       # dpi = 800,
       path = paste0("C:\\Users\\LuizFernandoLopes\\Downloads"))
