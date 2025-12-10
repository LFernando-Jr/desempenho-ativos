
# Pacotes -----------------------------------------------------------------

library(tidyverse)
library(YieldCurve)
library(rb3)

# Setup -------------------------------------------------------------------

rm(list = ls())

Sys.setenv("LANGUAGE" = "Pt")
Sys.setlocale("LC_ALL", "Portuguese")

# Pré x DI ----------------------------------------------------------------

## Coleta de dados --------------------------------------------------------

df = rbind(
  yc_get(refdate = as.Date("2025-04-11")),
  yc_get(refdate = as.Date("2025-04-01")),
  yc_get(refdate = as.Date("2025-03-11")),
  yc_get(refdate = as.Date("2025-01-02"))
  )

maturidades = c(252,
                378,
                504,
                630,
                756,
                882,
                1008,
                1134,
                1260,
                1386,
                1512,
                1638,
                1764,
                1890,
                2016,
                2142,
                2268,
                2394,
                2520)

y <- df %>% 
  select(c(refdate, biz_days, r_252)) %>% 
  filter(biz_days <= 2520) %>% 
  complete(refdate, biz_days = maturidades) %>%
  fill(r_252, .direction = "downup") %>%
  filter(biz_days %in% maturidades) %>%
  group_by(yearmonth = tsibble::yearmonth(refdate)) %>% 
  filter(refdate == max(refdate)) %>% 
  ungroup() %>% 
  mutate(date     = refdate,
         maturity = biz_days,
         yield    = r_252*100,
         .keep     = "none")

df %<>% 
  # filter(biz_days <= 2520) %>% 
  select(c(refdate, biz_days, r_252)) %>% 
  mutate(date = refdate,
         maturity = biz_days,
         yield = r_252*100,
         .keep = "none") %>% 
  pivot_wider(names_from  = maturity,
              values_from = yield) %>% 
  select(where(~ all(!is.na(.))))

maturidades = colnames(df)[-1] %>% as.numeric()

# Inicializa a matriz com NA para evitar problemas de comprimento zero no subconjunto
yc_matrix = as.matrix(df[,-1])

row.names(yc_matrix) = as.character(df$date)

yc_matrix

# Estimar o modelo Nelson-Siegel
t = maturidades / 252
ns_fit = Nelson.Siegel(yc_matrix, t)
nss_fit = Svensson(yc_matrix, t)

temp <- nss_fit %>% as.data.frame() %>% 
  cbind("date" = df$date, .) %>% 
  as_tibble()

# temp %>% 
#   group_by(month(date)) %>%
#   summarise(lambda = mean(lambda)) %>%
#   ggplot() +
#   aes(date, teste) +
  # geom_point() 

temp %<>% 
  # dplyr::mutate(date = tsibble::yearmonth(date)) %>%
  tsibble::as_tsibble(index = "date")

temp %>% 
  tsibble::fill_gaps() %>% 
  fabletools::model(feasts::STL(lambda)) %>% 
  fabletools::components() %>% 
  fabletools::autoplot()

temp %>% 
  tsibble::fill_gaps() %>% 
  feasts::gg_subseries(y = lambda)

y %>% pivot_longer(-1)

nelson_siegel <- function(tau, beta_0, beta_1, beta_2, lambda) {
  beta_0 + 
    beta_1 * ((1 - exp(-lambda * tau)) / (lambda * tau)) + 
    beta_2 * (((1 - exp(-lambda * tau)) / (lambda * tau)) - exp(-lambda * tau))
}

svensson <- function(tau, beta_0, beta_1, beta_2, beta_3, tau1, tau2) {
  beta_0 + 
    beta_1 * ((1 - exp(-tau1 * tau)) / (tau1 * tau)) + 
    beta_2 * (((1 - exp(-tau1 * tau)) / (tau1 * tau)) - exp(-tau1 * tau)) +
    beta_3 * (((1 - exp(-tau2 * tau)) / (tau2 * tau)) - exp(-tau2 * tau))
 }

temp %<>% 
  rowwise() %>% 
  mutate(Yields = list(map_dbl(t, svensson, beta_0, beta_1, beta_2, beta_3, tau1, tau2))) %>%
  unnest_wider(Yields, names_sep = "_") %>%
  rename_with(~ as.character(t), starts_with("Yields"))

temp[,-c(2:5)] %>% 
  # filter(year(date) == "2005") %>% 
  pivot_longer(-1) %>% 
  mutate(name = as.numeric(name)) %>% 
  ggplot() +
  aes(x = name, y = value, group = day(date), colour = day(date)) +
  geom_line()

temp[,c(1:5)] %>% 
  # filter(year(date) == "2005") %>% 
  pivot_longer(-1) %>% 
  ggplot() +
  aes(x = as.Date(date), y = value, group = name, colour = name) +
  geom_line() +
  facet_wrap(~year(name), scales = "free_y")

## Visualização de dados --------------------------------------------------

df %>%
  dplyr::filter(biz_days <= 2520) %>% 
  ggplot() +
  aes(x = biz_days, y = r_252, color = factor(refdate)) +
  geom_line(linewidth = .75) +
  theme_bw() + theme(panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     legend.title = element_blank(),
                     strip.background = element_blank()) + 
  scale_x_continuous(expand = c(0,0), n.breaks = 10) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Curva de juros",
       caption = "Elaboração do autor com dados da B3",
       x = NULL, y = "Taxa de juros")

ggsave("ETTJ_DI.png", 
       width = 4800, 
       height = 2160, 
       units = "px", 
       dpi = 576, 
       path = paste0(getwd(), "/saídas/"))


# IPCA --------------------------------------------------------------------

## Coleta de dados --------------------------------------------------------

df = rbind(
  yc_ipca_get(refdate = as.Date("2025-04-11")),
  yc_ipca_get(refdate = as.Date("2025-04-01")),
  yc_ipca_get(refdate = as.Date("2025-03-11")),
  yc_ipca_get(refdate = as.Date("2025-01-02"))
  )

## Visualização de dados --------------------------------------------------

df %>%
  dplyr::filter(biz_days >= 252) %>%
  ggplot() +
  aes(x = biz_days, y = r_252, color = factor(refdate)) +
  geom_line(linewidth = .75) +
  theme_bw() + theme(panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     legend.title = element_blank(),
                     strip.background = element_blank()) + 
  scale_x_continuous(expand = c(0,0), n.breaks = 10) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Curva de juros reais",
       caption = "Elaboração do autor com dados da B3",
       x = NULL, y = "Taxa de juros")

ggsave("ETTJ_IPCA.png", 
       width = 4800, 
       height = 2160, 
       units = "px",
       dpi = 576, 
       path = paste0(getwd(),
                     "/gráficos/curva de juros"))
