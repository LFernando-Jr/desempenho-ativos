
data = read_excel("C:\\Users\\LuizFernandoLopes\\OneDrive - Capri Family Office\\BZ ERP.xlsx", sheet = 1) %>%
  mutate(date  = as.Date(Date, format = "%d/%m/%Y"),
         erp  = ERP,
         .keep  = "none")

glimpse(data)

# Tratamento de dados -----------------------------------------------------

data %>% 
  dplyr::filter(date >= "2019-01-01") %>% 
  ggplot() + 
  aes(x = date, y = erp) + 
  geom_line(linewidth = .75, colour = "#cb6451") +
  theme_minimal() + 
  theme(legend.title     = element_blank(), 
        strip.background = element_blank()) + 
  scale_x_date(date_labels = "%Y", 
               breaks = "12 months") +
  scale_y_continuous(n.breaks = 10,
                     labels = scales::percent) +
  scale_colour_manual(values = "#2F47AD") +
  labs(title = "Equity Risk Premium",
       x = NULL,
       y = NULL,
       subtitle = "B120+ anualizado",
       caption = "Capri FO com dados da Bloomberg e do Tesouro Nacional")

ggsave("imab.png", 
       width = 2016, 
       height = 1142.4*1.5, 
       units = "px",
       dpi = 576/1.5, 
       path = "C:\\Users\\LuizFernandoLopes\\Downloads\\")

