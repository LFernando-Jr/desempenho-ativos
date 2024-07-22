
# Pacotes -----------------------------------------------------------------

library(tidyverse)
library(YieldCurve)
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
     
  yc_get(refdate = as.Date("2024-06-03")),
  yc_get(refdate = as.Date("2024-06-28")))
# yc_get(refdate = Sys.Date() - 1))

df = yc_mget(first_date = as.Date("2024-06-03"),
             last_date = as.Date("2024-06-28"))


# Visualização de dados ---------------------------------------------------

df[,c(1,2,5)] %>% 
  pivot_wider(id_cols = cur_days,
              names_from = refdate,
              values_from = r_252) %>% 
  view()
  
  

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

ggsave("ETTJ.png", width = 4800, height = 2160, units = "px", dpi = 576, path = paste(getwd(),
                                                                                      "/Gráficos",
                                                                                      sep = ""))

# Otras cositas mas -------------------------------------------------------

maturidades = c(30, 60, 90, 180, 270, 360, 720, 1080, 1800, 2520, 3600)
maturidades_adjacentes = sort(c(maturidades, maturidades + 1, maturidades + 2, maturidades + 3))

y = df[, -c(3,4,6)] %>%
  filter(cur_days %in% maturidades_adjacentes) %>% 
  complete(refdate, cur_days) %>%
  fill(r_252, .direction = "updown") %>%
  filter(cur_days %in% maturidades_adjacentes) %>% 
  mutate(date     = refdate,
         maturity = cur_days,
         yield    = r_252*100,
         .keep    = "unused")

y %>%
  ggplot() +
  aes(maturity, yield, color = date) +
  geom_line()

# Inicializa a matriz com NA para evitar problemas de comprimento zero no subconjunto
yc_matrix = matrix(NA, ncol = length(unique(y$maturity)), nrow = length(unique(y$date)), dimnames = list(as.Date(y$date %>% unique()),
                                                                                                         y$maturity %>% unique()))

# Preenche a matriz com as taxas de juros correspondentes
for (i in 1:nrow(yc_matrix)) {
  for (j in 1:ncol(yc_matrix)) {
    yc_matrix[i, j] = y$yield[y$date == unique(y$date)[i] & y$maturity == unique(y$maturity)[j]]
    print(paste("i = ", i))
    print(paste("j = ", j))
    yc_matrix
  }
}

t = unique(y$maturity)

test = y %>% 
  pivot_wider(names_from = maturity, values_from = yield)

test = ts(test[,-1], start = c(2024,6), frequency = 12)

test = as.xts(test)
plot(as.xtstest)
SvenssonParameters <- Svensson(test, unique(y$maturity)/30)
Svensson.rate <- Srates(SvenssonParameters, unique(y$maturity)/30, "Spot")

plot(unique(y$maturity)/30, last(test,'1 day'),main="Fitting Svensson yield curve",
     xlab=c("Pillars in years"), ylab=c("Rates"),type="o")
lines(unique(y$maturity)/30, last(Svensson.rate,'1 day'), col=2)
legend("topleft",legend=c("observed yield curve","fitted yield curve"),
       col=c(1,2),lty=1)
grid()

plot(rate.ECB)

data(ECBYieldCurve)
rate.ECB = first(ECBYieldCurve,'2 day')
maturity.ECB = c(0.25,0.5, seq(1,30,by=1))
SvenssonParameters <- Svensson(rate.ECB, maturity.ECB)
Svensson.rate <- Srates(SvenssonParameters, maturity.ECB,"Spot")

plot(maturity.ECB, last(rate.ECB,'1 day'),main="Fitting Svensson yield curve",
     xlab=c("Pillars in years"), ylab=c("Rates"),type="o")
lines(maturity.ECB, last(Svensson.rate,'1 day'), col=2)
legend("topleft",legend=c("observed yield curve","fitted yield curve"),
       col=c(1,2),lty=1)
grid()

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


# IPCA --------------------------------------------------------------------

df <- rbind(
  # yc_get(refdate = "2024-01-02"),
  # yc_get(refdate = "2024-02-01"),
  # yc_get(refdate = Sys.Date() - 2),
  yc_ipca_get(refdate = Sys.Date() - 3),
  yc_ipca_get(refdate = Sys.Date() - 7))


maturidades = c(360, 720, 1080, 1800, 2520, 3600)
maturidades_adjacentes = sort(c(maturidades, maturidades + 1, maturidades + 2, maturidades + 3))

y = df[, -c(3,4)] %>%
  filter(cur_days %in% maturidades_adjacentes) %>% 
  complete(refdate, cur_days) %>%
  fill(r_252, .direction = "updown") %>%
  filter(cur_days %in% maturidades_adjacentes) %>% 
  mutate(date     = refdate,
         maturity = cur_days,
         yield    = r_252*100,
         .keep    = "unused")

y %>%
  ggplot() +
  aes(maturity, yield, color = date) +
  geom_line()

# Inicializa a matriz com NA para evitar problemas de comprimento zero no subconjunto
yc_matrix = matrix(NA, ncol = length(unique(y$maturity)), nrow = length(unique(y$date)), dimnames = list(as.Date(y$date %>% unique()),
                                                                                                         y$maturity %>% unique()))

# Preenche a matriz com as taxas de juros correspondentes
for (i in 1:nrow(yc_matrix)) {
  for (j in 1:ncol(yc_matrix)) {
    yc_matrix[i, j] = y$yield[y$date == unique(y$date)[i] & y$maturity == unique(y$maturity)[j]]
    print(paste("i = ", i))
    print(paste("j = ", j))
    yc_matrix
  }
}

t = unique(y$maturity)

test = df[, -c(3,4)] %>% 
  filter(cur_days >= 360) %>% 
  pivot_wider(names_from = cur_days, values_from = r_252)

test = ts(test[,-1], start = c(2024,6), frequency = 12)

test = as.xts(test)
plot(as.xts(test))
t = as.numeric(colnames(test))
SvenssonParameters <- Svensson(test, t/30)
Svensson.rate <- Srates(SvenssonParameters, t/30, "Spot")

plot(t/30, last(test,'1 day'),main="Fitting Svensson yield curve",
     xlab=c("Pillars in years"), ylab=c("Rates"),type="o")
lines(t/30, last(Svensson.rate,'1 day'), col=2)
legend("topleft",legend=c("observed yield curve","fitted yield curve"),
       col=c(1,2),lty=1)
grid()