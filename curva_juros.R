
# Pacotes -----------------------------------------------------------------

library(tidyverse)
library(YieldCurve)
library(termstrc)
library(rb3)

# Setup -------------------------------------------------------------------

rm(list = ls())

Sys.setenv("LANGUAGE" = "Pt")
Sys.setlocale("LC_ALL", "Portuguese")

# Coleta de dados ---------------------------------------------------------

df <- rbind(
  # yc_get(refdate = "2024-01-02"),
  # yc_get(refdate = "2024-02-01"),
            # yc_get(refdate = Sys.Date() - 2),
            yc_get(refdate = Sys.Date() - 1)) %>% 
  filter(cur_days <= 3600)


df %>%
  ggplot() +
  aes(cur_days, r_252) +
  geom_line()


SvenssonParameters <- Svensson(df$r_252, df$cur_days)
Svensson.rate <- Srates(SvenssonParameters, maturity.ECB, "Spot")














# Visualização de dados ---------------------------------------------------

df %>%
  filter(cur_days <= 2646) %>% 
  ggplot() +
  aes(x = cur_days, y = r_252, color = factor(refdate)) +
  geom_line(linewidth = .75) +
  theme_bw() + theme(panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     legend.title = element_blank(), 
                     axis.title = element_blank(), 
                     strip.background = element_blank()) + 
  scale_x_continuous(expand = c(0,0), n.breaks = 10) +
  scale_y_continuous(labels = scales::percent) +
  # scale_colour_manual(values = c("#2F47AD",
  #                                "#AD4728")) +
  labs(title = "Curva de juros",
       caption = "Fonte: Capri FO com dados da B3",
       x = NULL, y = "Taxa de juros")

# ===============================================

data(ECBYieldCurve)
rate.ECB = first(ECBYieldCurve,'2 day')
maturity.ECB = c(0.25,0.5,seq(1,30,by=1))
SvenssonParameters <- Svensson(rate.ECB, maturity.ECB)
Svensson.rate <- Srates( SvenssonParameters ,maturity.ECB,"Spot")
plot(maturity.ECB, last(rate.ECB,'1 day'),main="Fitting Svensson yield curve",
     xlab=c("Pillars in years"), ylab=c("Rates"),type="o")
lines(maturity.ECB, last(Svensson.rate,'1 day'), col=2)
legend("topleft",legend=c("observed yield curve","fitted yield curve"),
       col=c(1,2),lty=1)
grid()

data(FedYieldCurve)
maturity.Fed <- c(3/12, 0.5, 1, 2, 3, 5, 7, 10)
SvenssonParameters <- Svensson(rate = first(FedYieldCurve,'10 month'), maturity = maturity.Fed)
y <- Srates(SvenssonParameters[5,], maturity.Fed)
plot(maturity.Fed,FedYieldCurve[5,], main = "Fitting Nelson-Siegel yield curve",
     xlab = c("Pillars in months"), type = "o")
lines(maturity.Fed,y, col=2)
legend("topleft",legend=c("observed yield curve","fitted yield curve"),
       col=c(1,2),lty=1)
grid()





















