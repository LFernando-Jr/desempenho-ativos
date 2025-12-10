
# Pacotes -----------------------------------------------------------------

library(tidyverse)

# Setup -------------------------------------------------------------------

rm(list = ls())

trimmed_sd <- function(x, lower = 0, upper = 1) {
  # calcula os quantis de aparo
  qs <- quantile(x, probs = c(lower, upper), na.rm = TRUE)
  # filtra e calcula o sd
  sd(x[x >= qs[1] & x <= qs[2]], na.rm = TRUE)
}

# Coleta de dados ---------------------------------------------------------

data = read_excel(path = "data/idex_cdi_geral_datafile.xlsx")

low_rated = read_excel(path = "data/idex_cdi_low_rated_datafile.xlsx")

# Tratamento e Visualização de dados --------------------------------------

hist = data %>%
  dplyr::filter(Data == max(Data)) %>% 
  mutate(date = Data %>% as.Date(), 
         spread = `Peso no índice (%)`*`Spread de compra (%)`*100 %>% round(2),
         .keep = "none")
  
data %>%
  ggplot() +
  aes(x = Data, y = `Spread de compra (%)`) +
  geom_boxplot()

hist %>% ggplot() +
  geom_histogram(aes(x = spread), bins = 30, fill = "white", colour = "black") +
  geom_density(aes(x = spread), colour = "red", linewidth = .75) +
  geom_vline(xintercept = median(hist$spread)) + 
  theme_bw() + theme(panel.grid.minor = element_blank(), 
                     axis.line        = element_line(colour = "black"),
                     legend.title     = element_blank(), 
                     axis.title       = element_blank(), 
                     strip.background = element_blank()) + 
  labs(title = "Distribuição dos spreads - Idex",
       caption = "Capri FO com dados da JGP")

data %>% 
  group_by(date = Data) %>% 
  reframe(date = as.Date(last(Data)),
          spread = sum(`Peso no índice (%)`*`Spread de compra (%)`)*100 %>% 
            round(2),
          sd = sd(`Spread de compra (%)`)*100,
          maximus = max(`Spread de compra (%)`)*100,
          minimus = min(`Spread de compra (%)`)*100,
          sd_01 = trimmed_sd(`Spread de compra (%)`, 0.01, 0.99)*100,
          sd_10 = trimmed_sd(`Spread de compra (%)`, 0.1, 0.9)*100,
          sd_25 = trimmed_sd(`Spread de compra (%)`, 0.25, 0.75)*100) %>% 
  ggplot() +
  aes(x = date) +
  geom_line(aes(y = spread), 
            linewidth = .75) +
  coord_cartesian(ylim = c(0, 7)) +
  geom_ribbon(aes(ymin = spread - sd,  
                  ymax = spread + sd,  
                  fill = "Total"), 
               alpha = 0.4) +
  geom_ribbon(aes(ymin = spread - sd_25, 
                  ymax = spread + sd_25, 
                  fill = "P25-P75"), 
              alpha = 0.4) +
  geom_ribbon(aes(ymin = spread - sd_10, 
                  ymax = spread + sd_10, 
                  fill = "P10-P90"), 
              alpha = 0.4) +
  # geom_ribbon(aes(ymin = spread - sd_01, 
                  # ymax = spread + sd_01, 
                  # fill = "P01-P99"), 
              # alpha = 0.4) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom", 
        legend.title = element_blank()) +
  labs(title = "Spreads de crédito - Desvio padrão",
       x = NULL,
       y = NULL)

data %>% 
  # dplyr::filter(Data >= "2025-05-01") %>% 
  group_by(date = Data) %>% 
  reframe(date = as.Date(last(Data)),
          spread = sum(`Peso no índice (%)`*`Spread de compra (%)`)*100 %>% round(2),
          sd = median(`Spread de compra (%)`)*100,
          maximus = max(`Spread de compra (%)`)*100,
          minimus = min(`Spread de compra (%)`)*100,
          sd_01 = quantile(`Spread de compra (%)`, 0.01)*100,
          sd_99 = quantile(`Spread de compra (%)`, 0.99)*100,
          sd_10 = quantile(`Spread de compra (%)`, 0.1)*100,
          sd_90 = quantile(`Spread de compra (%)`, 0.9)*100,
          sd_25 = quantile(`Spread de compra (%)`, 0.25)*100,
          sd_75 = quantile(`Spread de compra (%)`, 0.75)*100) %>% 
  ggplot() +
  aes(x = date) +
  geom_line(aes(y = sd), 
            linewidth = .75) +
  coord_cartesian(ylim = c(0, 7)) + 
  # geom_ribbon(aes(ymin = minimus,
  #                 ymax = maximus,
  #                 fill = "Min-Max"),
  # alpha = 0.4) +
  geom_ribbon(aes(ymin = sd_25, 
                  ymax = sd_75, 
                  fill = "P25-P75"), 
              alpha = 0.4) +
  geom_ribbon(aes(ymin = sd_10, 
                  ymax = sd_90, 
                  fill = "P10-P90"), 
              alpha = 0.4) +
  geom_ribbon(aes(ymin = sd_01,
                  ymax = sd_99,
                  fill = "P01-P99"),
  alpha = 0.4) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom", 
        legend.title = element_blank()) +
  labs(title = "Spreads de crédito - Quartis",
       x = NULL,
       y = NULL)

data %>% 
  # dplyr::filter(Data >= "2025-05-01") %>% 
  group_by(date = Data) %>% 
  reframe(date = as.Date(last(Data)),
          spread = sum(`Peso no índice (%)`*`Spread de compra (%)`)*100 %>% round(2),
          sd = median(`Spread de compra (%)`)*100,
          maximus = max(`Spread de compra (%)`)*100,
          minimus = min(`Spread de compra (%)`)*100,
          sd_01 = quantile(`Spread de compra (%)`, 0.01)*100,
          sd_99 = quantile(`Spread de compra (%)`, 0.99)*100,
          sd_10 = quantile(`Spread de compra (%)`, 0.1)*100,
          sd_90 = quantile(`Spread de compra (%)`, 0.9)*100,
          sd_25 = quantile(`Spread de compra (%)`, 0.25)*100,
          sd_75 = quantile(`Spread de compra (%)`, 0.75)*100) %>% 
  ggplot() +
  aes(x = date) +
  geom_line(aes(y = sd), 
            linewidth = .75) +
  coord_cartesian(ylim = c(0, 7)) + 
  # geom_ribbon(aes(ymin = minimus,
                  # ymax = maximus,
                  # fill = "Min-Max"),
              # alpha = 0.4) +
  geom_ribbon(aes(ymin = sd_10, 
                  ymax = sd_90, 
                  fill = "P10-P90"), 
              alpha = 0.4) +
  geom_ribbon(aes(ymin = sd_25, 
                  ymax = sd_75, 
                  fill = "P25-P75"), 
              alpha = 0.4) +
  # geom_ribbon(aes(ymin = sd_01, 
  #                 ymax = sd_99, 
  #                 fill = "P01-P99"), 
              # alpha = 0.4) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom", 
        legend.title = element_blank()) +
  labs(title = "Spreads de crédito",
       x = NULL,
       y = NULL)

# Tratamento e Visualização de dados --------------------------------------

low_rated %>%
  dplyr::filter(Data == max(Data)) %>% 
  mutate(date = Data %>% as.Date(), 
         spread = `Peso no índice (%)`*`Spread de compra (%)`*100 %>% round(2),
         .keep = "none") %>% 
  ggplot() +
  geom_histogram(aes(x = spread), bins = 30, fill = "white", colour = "black") +
  geom_density(aes(x = spread), colour = "red", linewidth = .75) +
  theme_bw() + theme(panel.grid.minor = element_blank(), 
                     axis.line        = element_line(colour = "black"),
                     legend.title     = element_blank(), 
                     axis.title       = element_blank(), 
                     strip.background = element_blank()) + 
  labs(title = "Distribuição dos spreads - Idex",
       caption = "Capri FO com dados da JGP")

low_rated %>% 
  group_by(date = Data) %>% 
  reframe(date = as.Date(last(Data)),
          spread = sum(`Peso no índice (%)`*`Spread de compra (%)`)*100 %>% round(2),
          sd = sd(`Spread de compra (%)`)*100,
          maximus = max(`Spread de compra (%)`)*100,
          minimus = min(`Spread de compra (%)`)*100,
          sd_01 = trimmed_sd(`Spread de compra (%)`, 0.01, 0.99)*100,
          sd_10 = trimmed_sd(`Spread de compra (%)`, 0.1, 0.9)*100,
          sd_25 = trimmed_sd(`Spread de compra (%)`, 0.25, 0.75)*100) %>% 
  ggplot() +
  aes(x = date) +
  geom_line(aes(y = spread), 
            linewidth = .75) +
  coord_cartesian(ylim = c(0, 7)) +
  geom_ribbon(aes(ymin = spread - sd,  
                  ymax = spread + sd,  
                  fill = "Total"), 
              alpha = 0.4) +
  geom_ribbon(aes(ymin = spread - sd_25, 
                  ymax = spread + sd_25, 
                  fill = "P25-P75"), 
              alpha = 0.4) +
  geom_ribbon(aes(ymin = spread - sd_10, 
                  ymax = spread + sd_10, 
                  fill = "P10-P90"), 
              alpha = 0.4) +
  geom_ribbon(aes(ymin = spread - sd_01, 
                  ymax = spread + sd_01, 
                  fill = "P01-P99"), 
              alpha = 0.4) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom", 
        legend.title = element_blank()) +
  labs(title = "Spreads de crédito - Desvio padrão",
       x = NULL,
       y = NULL)

low_rated %>% 
  # dplyr::filter(Data >= "2025-05-01") %>% 
  group_by(date = Data) %>% 
  reframe(date = as.Date(last(Data)),
          spread = sum(`Peso no índice (%)`*`Spread de compra (%)`)*100 %>% round(2),
          sd = median(`Spread de compra (%)`)*100,
          maximus = max(`Spread de compra (%)`)*100,
          minimus = min(`Spread de compra (%)`)*100,
          sd_01 = quantile(`Spread de compra (%)`, 0.01)*100,
          sd_99 = quantile(`Spread de compra (%)`, 0.99)*100,
          sd_10 = quantile(`Spread de compra (%)`, 0.1)*100,
          sd_90 = quantile(`Spread de compra (%)`, 0.9)*100,
          sd_25 = quantile(`Spread de compra (%)`, 0.25)*100,
          sd_75 = quantile(`Spread de compra (%)`, 0.75)*100) %>% 
  ggplot() +
  aes(x = date) +
  geom_line(aes(y = sd), 
            linewidth = .75) +
  coord_cartesian(ylim = c(0, 7)) + 
  # geom_ribbon(aes(ymin = minimus,
  # ymax = maximus,
  # fill = "Min-Max"),
  # alpha = 0.4) +
  geom_ribbon(aes(ymin = sd_10, 
                  ymax = sd_90, 
                  fill = "P10-P90"), 
              alpha = 0.4) +
  geom_ribbon(aes(ymin = sd_25, 
                  ymax = sd_75, 
                  fill = "P25-P75"), 
              alpha = 0.4) +
  # geom_ribbon(aes(ymin = sd_01, 
  #                 ymax = sd_99, 
  #                 fill = "P01-P99"), 
  # alpha = 0.4) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom", 
        legend.title = element_blank()) +
  labs(title = "Spreads de crédito",
       x = NULL,
       y = NULL)

# Tratamento e Visualização de dados --------------------------------------

anti_join(x = data, y = low_rated, by = "Debênture") %>% 
  dplyr::filter(Data == max(Data)) %>% 
  mutate(date = Data %>% as.Date(), 
         spread = `Peso no índice (%)`*`Spread de compra (%)`*100 %>% round(2),
         .keep = "none") %>% 
  ggplot() +
  geom_histogram(aes(x = spread), bins = 30, fill = "white", colour = "black") +
  geom_density(aes(x = spread), colour = "red", linewidth = .75) +
  theme_bw() + theme(panel.grid.minor = element_blank(), 
                     axis.line        = element_line(colour = "black"),
                     legend.title     = element_blank(), 
                     axis.title       = element_blank(), 
                     strip.background = element_blank()) + 
  labs(title = "Distribuição dos spreads - Idex",
       caption = "Capri FO com dados da JGP")

anti_join(x = data, y = low_rated, by = "Debênture") %>% 
  group_by(date = Data) %>% 
  reframe(date = as.Date(last(Data)),
          spread = sum(`Peso no índice (%)`*`Spread de compra (%)`)*100 %>% round(2),
          sd = sd(`Spread de compra (%)`)*100,
          maximus = max(`Spread de compra (%)`)*100,
          minimus = min(`Spread de compra (%)`)*100,
          sd_01 = trimmed_sd(`Spread de compra (%)`, 0.01, 0.99)*100,
          sd_10 = trimmed_sd(`Spread de compra (%)`, 0.1, 0.9)*100,
          sd_25 = trimmed_sd(`Spread de compra (%)`, 0.25, 0.75)*100) %>% 
  ggplot() +
  aes(x = date) +
  geom_line(aes(y = spread), 
            linewidth = .75) +
  coord_cartesian(ylim = c(0, 7)) +
  geom_ribbon(aes(ymin = spread - sd,  
                  ymax = spread + sd,  
                  fill = "Total"), 
              alpha = 0.4) +
  geom_ribbon(aes(ymin = spread - sd_25, 
                  ymax = spread + sd_25, 
                  fill = "P25-P75"), 
              alpha = 0.4) +
  geom_ribbon(aes(ymin = spread - sd_10, 
                  ymax = spread + sd_10, 
                  fill = "P10-P90"), 
              alpha = 0.4) +
  geom_ribbon(aes(ymin = spread - sd_01, 
                  ymax = spread + sd_01, 
                  fill = "P01-P99"), 
              alpha = 0.4) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom", 
        legend.title = element_blank()) +
  labs(title = "Spreads de crédito - Desvio padrão",
       x = NULL,
       y = NULL)

anti_join(x = data, y = low_rated, by = "Debênture") %>% 
  # dplyr::filter(Data >= "2025-05-01") %>% 
  group_by(date = Data) %>% 
  reframe(date = as.Date(last(Data)),
          spread = sum(`Peso no índice (%)`*`Spread de compra (%)`)*100 %>% round(2),
          sd = median(`Spread de compra (%)`)*100,
          maximus = max(`Spread de compra (%)`)*100,
          minimus = min(`Spread de compra (%)`)*100,
          sd_01 = quantile(`Spread de compra (%)`, 0.01)*100,
          sd_99 = quantile(`Spread de compra (%)`, 0.99)*100,
          sd_10 = quantile(`Spread de compra (%)`, 0.1)*100,
          sd_90 = quantile(`Spread de compra (%)`, 0.9)*100,
          sd_25 = quantile(`Spread de compra (%)`, 0.25)*100,
          sd_75 = quantile(`Spread de compra (%)`, 0.75)*100) %>% 
  ggplot() +
  aes(x = date) +
  geom_line(aes(y = sd), 
            linewidth = .75) +
  coord_cartesian(ylim = c(0, 7)) + 
  # geom_ribbon(aes(ymin = minimus,
  # ymax = maximus,
  # fill = "Min-Max"),
  # alpha = 0.4) +
  geom_ribbon(aes(ymin = sd_10, 
                  ymax = sd_90, 
                  fill = "P10-P90"), 
              alpha = 0.4) +
  geom_ribbon(aes(ymin = sd_25, 
                  ymax = sd_75, 
                  fill = "P25-P75"), 
              alpha = 0.4) +
  # geom_ribbon(aes(ymin = sd_01, 
  #                 ymax = sd_99, 
  #                 fill = "P01-P99"), 
  # alpha = 0.4) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom", 
        legend.title = element_blank()) +
  labs(title = "Spreads de crédito",
       x = NULL,
       y = NULL)
