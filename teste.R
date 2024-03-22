library(quantmod)

getSymbols("BTC-USD", from = "2000-01-01")

head(`BTC-USD`)

tail(`BTC-USD`)

chartSeries(`BTC-USD`, theme = chartTheme("white"), subset = "last 10 months", show.grid = TRUE)
