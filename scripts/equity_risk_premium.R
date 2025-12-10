
library(tsibble)

data = read_excel("C:\\Users\\LuizFernandoLopes\\Downloads\\BZ ERP.xlsx",
                  sheet = 1) %>% 
  set_names(c("date", "earnings_yield")) %>% 
  mutate(date = date %>% yearmonth(),
         earnings_yield = 1/earnings_yield * 100)

data %>% 
  ggplot() +
  aes(x = as.Date(date), y = earnings_yield) +
  geom_line()

load(paste0("C:/Users/LuizFernandoLopes/OneDrive/Documentos",
            "/Research/acm/dados/results_ntnb.RData"))

results_ntnb %<>% 
  dplyr::filter(maturity %in% c(120)) %>% 
  dplyr::select(date, Fitted) %>% 
  mutate(date = date %>% yearmonth(),
         risk_free = Fitted*100,
         .keep = "none")

inner_join(data, results_ntnb) %>% 
  mutate(value = ((1 + earnings_yield/100)/(1 + risk_free/100) - 1)*100) %>%
  ggplot() + 
  aes(x = date, y = earnings_yield) + 
  geom_line(linewidth = .75) + 
  geom_vline(xintercept = as.Date("2025-01-01")) +
  theme_bw() + 
  scale_x_index()
  theme(panel.grid.minor = element_blank(), 
        legend.position  = "bottom", 
        legend.title     = element_blank(),
        axis.line        = element_line(colour = "black")) +
  guides(fill   = guide_legend(ncol = 1),
         colour = guide_legend(ncol = 3)) +
  labs(title = "Earnings Yield",
       x = "Mês",
       y = NULL,
       subtitle = "Ibovespa",
       caption = "Fonte: Elaboração do autor com dados do BBG e do TN")
  
