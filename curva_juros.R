
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
  yc_get(refdate = as.Date("2024-08-19")),
  yc_get(refdate = as.Date("2024-08-30"))
  )

## Visualização de dados --------------------------------------------------

df %>%
  filter(biz_days <= 2520) %>% 
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
       path = paste0(getwd(),
                     "/gráficos/curva de juros"))

# IPCA --------------------------------------------------------------------

## Coleta de dados --------------------------------------------------------

df = rbind(
  yc_ipca_get(refdate = Sys.Date() - 3),
  yc_ipca_get(refdate = Sys.Date() - 7)
  )

## Visualização de dados --------------------------------------------------

df %>%
  filter(biz_days >= 252,
         biz_days <= 2520) %>% 
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
