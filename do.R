options("width"=200)
symbols <- c('GSPC', 'INTL', 'TBIL3')
symbols <- c('GSPC')

lookback <- 10
delta <- 5  # years

for(delta in c(5, 10, 20, 40)){
	start.date <- as.Date('1972-12-31',  "%Y-%m-%d")
	end.date <- start.date + years(delta) 
	end.date <- end.date %m-% months(1)
	result <- data.frame()

	while(end.date <= as.Date('2014-12-31',  "%Y-%m-%d")){
		for(symbols in list(c('GSPC'), c('INTL'), c('TBIL3'), c('GSPC', 'INTL', 'TBIL3'))){
			trim.prices <- TrimData(all.prices, symbols, start.date, end.date, lookback)
			df     <- CalculateGains(trim.prices, lookback)
			result <- rbind(result, ModelPerformance(df$lookback, df$monthly))
		}
		start.date <- start.date + years(delta)
		end.date <- start.date + years(delta)
		end.date <- end.date %m-% months(1)
	}

	d50 <- subset(result, symbol=="INTL")
	d50$symbol <- "50%GSPC-50%INTL"
	d50$gain  <- subset(result, symbol=="INTL")$gain/2 + subset(result, symbol=="GSPC")$gain/2
	d50$cmgr  <- subset(result, symbol=="INTL")$cmgr/2 + subset(result, symbol=="GSPC")$cmgr/2
	d50$stdev <- subset(result, symbol=="INTL")$stdev/2 + subset(result, symbol=="GSPC")$stdev/2
	result <- rbind(result, d50)
	fn <- sprintf('img/%d_year_window_%d_months.png', delta, lookback)
	png(fn,width=1000, height=600)
	p <- ggplot(result, aes(x=start.year, y=gain, fill=symbol)) 
	p <- p + geom_bar(stat="identity", position="dodge")
	print(p)
	tmp <- dev.off()
}