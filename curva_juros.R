
# Pacotes -----------------------------------------------------------------

library(tidyverse)
library(rb3)

# Setup -------------------------------------------------------------------

rm(list = ls())

Sys.setenv("LANGUAGE" = "Pt")
Sys.setlocale("LC_ALL", "Portuguese")

# Coleta de dados ---------------------------------------------------------

df <- rbind(yc_get(refdate = "2024-01-02"),
            yc_get(refdate = "2024-02-07"))

# Visualização de dados ---------------------------------------------------

df %>% 
  filter(forward_date <= "2030-01-01") %>% 
  ggplot() +
  aes(x = forward_date, y = r_252, color = factor(refdate)) +
  geom_line(linewidth = .75) +
  theme_bw() + theme(panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     legend.title = element_blank(), 
                     axis.title = element_blank(), 
                     strip.background = element_blank()) + 
  scale_x_date(expand = c(0,0), date_labels = "%Y", breaks = "12 months") +
  scale_y_continuous(labels = scales::percent) +
  scale_colour_manual(values = c("#2F47AD",
                                 "#AD4728")) +
  labs(title = "Curva de juros",
       caption = "Fonte: Capri FO com dados da B3",
       x = NULL, y = "Taxa de juros")

ggsave("curva de juros.png", width = 4800, height = 2160, units = "px", dpi = 576, path = paste(getwd(),
                                                                                                "/Gráficos/Curva de juros",
                                                                                                sep = ""))
