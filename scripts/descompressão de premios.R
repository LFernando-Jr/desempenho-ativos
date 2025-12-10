

data = read_excel("data/desempenho_onshore.xlsx", sheet = 1) %>%
  mutate(date  = as.Date(Data, format = "%d/%m/%Y"),
         name  = `Nome do Ativo`,
         value = `Número Índice`,
         .keep  = "none")

data %>% 
  # mutate(line = value[which(date == "2025-09-01")],
  #        .by = name) %>% 
  dplyr::filter(date >= as.Date("2025-09-01"),
                name == "IDA-DI") %>% 
  ggplot() + 
  aes(x = date, y = value) +
  geom_line() + 
  # geom_hline(aes(yintercept = line)) +
  theme_bw() +
  facet_wrap(~name, scales = "free_y")
