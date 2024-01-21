
# Pacotes -----------------------------------------------------------------

library(tidyverse)
library(Quandl)

# Setup -------------------------------------------------------------------

rm(list = ls())

Sys.setenv("LANGUAGE" = "Pt")
Sys.setlocale("LC_ALL", "Portuguese")

Quandl.api_key('Kn6L-n4knnqdN_j8FpAu')

# Coleta de dados ---------------------------------------------------------

Quandl.search(query = 'yield')

yc_all <- Quandl('USTREASURY/YIELD')

refdate <- as.Date(c("2024-01-05"))

yc <- yc_all %>% filter(Date %in% refdate)

yc 

nx <- names(yc)

curve_terms <- nx[-1] %>% 
  str_replace("MO", "months") %>% 
  str_replace("YR", "years")  %>% 
  map(fixedincome::as.term)

dc <- fixedincome::daycount("actual/360")

terms <- curve_terms  %>% 
  map_dbl(\(x) fixedincome::dib(dc) * fixedincome::toyears(dc, x)) %>% 
  fixedincome::term("days")

terms

rates <- yc[, -1] |>
  as.list() |>
  as.numeric()
rates <- rates / 100
ix <- !is.na(rates)

rates[ix]
terms[ix]

tr_curve <- fixedincome::spotratecurve(
  rates[ix], terms[ix],
  "simple", "actual/365", "actual",
  refdate = refdate
)

tr_curve

plot(tr_curve)

fixedincome::ggspotratecurveplot(tr_curve,
                                 title = "DI1 spot rates", subtitle = format(refdate), caption = "Data from {rb3} package")




terms <- c(1, 11, 26, 27, 28)
rates <- c(0.0719, 0.056, 0.0674, 0.0687, 0.07)



curve <- fixedincome::spotratecurve(rates, terms, "discrete", "actual/365", "actual")

plot(curve)




copom_dates <- as.Date(
  c("2022-03-17", "2022-05-05", "2022-06-17", "2022-08-04")
)
terms <- c(1, 3, 25, 44, 66, 87, 108, 131, 152, 172, 192, 214, 236, 277)
rates <- c(
  0.1065, 0.1064, 0.111, 0.1138, 0.1168, 0.1189, 0.1207, 0.1219,
  0.1227, 0.1235, 0.1234, 0.1236, 0.1235, 0.1235
)
curve <- fixedincome::spotratecurve(
  rates, terms, "discrete", "business/252", "Brazil/ANBIMA",
  refdate = as.Date("2022-02-23")
)
interpolation(curve) <- fixedincome::interp_flatforwardcopom(copom_dates, "second")

######################################################################
library(ustyc)
yc <- getYieldCurve()
summary(yc)
head(yc$df)

require(xts)
require(lattice)

xt = xts(yc$df,order.by=as.Date(rownames(yc$df)))
xyplot.ts(xt,scales=list(y=list(relation="same")),ylab="Yield (%)")








