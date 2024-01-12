library(rb3)
library(ggplot2)
library(stringr)
library(dplyr)

df_yc <- yc_mget(
  first_date = "2023-06-01",
  last_date = "2024-11-01"
)

df <- rbind(yc_get(refdate = "2024-01-10"),
            yc_get(refdate = "2023-12-11"))



p <-
  df %>% 
  filter(forward_date <= "2030-01-01") %>% 
  ggplot(
  aes(
    x = forward_date,
    y = r_252,
    group = refdate,
    color = factor(refdate))) +
  geom_line(linewidth = 1) +
  labs(
    title = "Yield Curves for Brazil",
    subtitle = "Built using interest rates future contracts",
    caption = str_glue("Data imported using rb3 at {Sys.Date()}"),
    x = "Forward Date",
    y = "Annual Interest Rate",
    color = "Reference Date"
  ) +
  theme_light() +
  scale_y_continuous(labels = scales::percent)

print(p)

library(ustyc)
yc <- getYieldCurve()
summary(yc)
head(yc$df)

require(xts)
require(lattice)

xt = xts(yc$df,order.by=as.Date(rownames(yc$df)))
xyplot.ts(xt,scales=list(y=list(relation="same")),ylab="Yield (%)")