
data = read_excel("dados/desempenho_onshore.xlsx", sheet = 1) %>%
  mutate(date  = as.Date(Data, format = "%d/%m/%Y"),
         name  = `Nome do Ativo`,
         value = `Número Índice`,
         .keep  = "none")

glimpse(data)

# Tratamento de dados -----------------------------------------------------

data %<>% 
  group_by(name) %>% 
  mutate(name = case_when(name == "IDA-IPCA Infraestrutura"~"IDA-Infra", 
                          TRUE ~ name),
         var = value/dplyr::lag(value, 1) - 1,
         acumulado_36_meses = (zoo::rollapply(1 + var, 
                                              width = 756, 
                                              FUN = prod, 
                                              align = 'right', 
                                              fill = NA)^(252/756) - 1)) %>% 
  ungroup() %>% 
  dplyr::filter(name %in% c("CDI", "IHFA")) %>% 
  dplyr::select(-c(3,4)) %>% 
  pivot_wider(id_cols = date,
              names_from = name,
              values_from = acumulado_36_meses) %>% 
  drop_na() %>% 
  mutate(`Excesso de retorno` = ((1 + IHFA)/(1 + CDI) - 1))

data %>% 
  dplyr::filter(date >= "2019-01-01") %>% 
  ggplot() + 
  aes(x = date, y = `Excesso de retorno`) + 
  geom_line(linewidth = .75, colour = "#cb6451") +
  theme_minimal() + 
  theme(legend.title     = element_blank(), 
        strip.background = element_blank()) + 
  scale_x_date(date_labels = "%Y", 
               breaks = "12 months") +
  scale_y_continuous(n.breaks = 10,
                     labels = scales::percent) +
  scale_colour_manual(values = "#2F47AD") +
  labs(title = "Retorno do IHFA acumulado em janelas de 36 meses",
       x = NULL,
       y = NULL,
       subtitle = "CDI+ anualizado",
       caption = "Capri FO com dados da Quantum Axis")

ggsave("ihfa.png", 
       width = 2016, 
       height = 1142.4*1.5, 
       units = "px",
       dpi = 576/1.5, 
       path = "C:\\Users\\LuizFernandoLopes\\Downloads\\")

