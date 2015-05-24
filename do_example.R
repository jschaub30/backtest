options(width = 200)
start.date <- as.Date("1994-12-31", "%Y-%m-%d")
end.date   <- as.Date("2014-11-30", "%Y-%m-%d")
lookback <- 12


symbols <- c("GSPC")

trim.prices <- TrimData(all.prices, symbols, 
  start.date, end.date, lookback)

df <- CalculateGains(trim.prices, lookback)
result <- ModelPerformance(df$lookback, 
  df$monthly)
	
# examine gains.summary for more info