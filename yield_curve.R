
library(Quandl)
library(tidyverse)

Quandl.api_key('on_Vk-ogkmufJBMudwhZ')

yc_all <- Quandl("USTREASURY/YIELD")

refdate <- as.Date(c("2024-01-04",
                     "2024-01-05"))

yc <- yc_all %>% filter(Date %in% refdate)

yc 

nx <- names(yc)

curve_terms <- nx[-1] |>
  str_replace("MO", "months") |>
  str_replace("YR", "years") |>
  map(fixedincome::as.term)

dc <- fixedincome::daycount("actual/360")

terms <- curve_terms  %>% 
  map_dbl(\(x) fixedincome::dib(dc) * fixedincome::toyears(dc, x)) %>% 
  fixedincome::term("days")

terms

rates <- yc[1, -1] |>
  as.list() |>
  as.numeric()
rates <- rates / 100
ix <- !is.na(rates)

rates[ix]

tr_curve <- fixedincome::spotratecurve(
  rates[ix], terms[ix],
  "simple", "actual/360", "actual",
  refdate = refdate
)

tr_curve

plot(tr_curve)

fixedincome::ggspotratecurveplot(tr_curve)
