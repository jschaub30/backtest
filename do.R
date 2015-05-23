options("width"=200)
symbols <- c('GSPC', 'INTL', 'TBIL3')
symbols <- c('GSPC')

lookback <- 10
delta <- 5  # years

dir.create('html/img', showWarnings = FALSE, recursive = TRUE)

for(lookback in c(8, 10, 12)){
	for(delta in c(5, 10, 15, 20)){
		start.date <- as.Date('1972-12-31',  "%Y-%m-%d")
		end.date <- start.date + years(delta) 
		end.date <- end.date %m-% months(1)
		result <- data.frame()

		while(end.date <= as.Date('2014-12-31',  "%Y-%m-%d")){
			for(symbols in list(c('GSPC'), c('INTL'), c('TBIL3'), c('GOLD'), 
			                    c('GSPC', 'INTL', 'TBIL3', 'GOLD'))){
				trim.prices <- TrimData(all.prices, symbols, start.date, end.date, lookback)
				df     <- CalculateGains(trim.prices, lookback)
				result <- rbind(result, ModelPerformance(df$lookback, df$monthly))
			}
			#start.date <- start.date + years(delta) # Independent returns
			start.date <- start.date + years(1)  # Rolling returns
			end.date <- start.date + years(delta)
			end.date <- end.date %m-% months(1)
		}
	
		r1 <- subset(result, symbol=="GSPC-INTL-TBIL3-GOLD")$gain
		r2 <- subset(result, symbol=='GSPC')$gain
		tot <- length(r1)
		stat.string <- sprintf("4 momentum > GSPC in %d/%d years (%.2f%%)",
		                       sum(r1>r2), tot, sum(r1>r2)/tot*100)
		stat.string <- sprintf('%s\n4 momentum: mean=%.1f, med=%.1f',
												 	 stat.string, mean(r1), median(r1))
		stat.string <- sprintf('%s\nGSPC: mean=%.1f, med=%.1f',
										  			stat.string, mean(r2), median(r2))

		# d50 <- subset(result, symbol=="INTL")
		# d50$symbol <- "50%GSPC-50%INTL"
		# d50$gain  <- subset(result, symbol=="INTL")$gain/2 + subset(result,
		# 	symbol=="GSPC")$gain/2
		# d50$cmgr  <- subset(result, symbol=="INTL")$cmgr/2 + subset(result,
		# 	symbol=="GSPC")$cmgr/2
		# d50$stdev <- subset(result, symbol=="INTL")$stdev/2 + subset(result,
		# 	symbol=="GSPC")$stdev/2
		# result <- rbind(result, d50)

		fn <- sprintf('html/img/%d_year_window_%d_months.png', delta, lookback)
		png(fn,width=1000, height=600)
		p <- ggplot(result, aes(x=start.year, y=gain, color=symbol, group=symbol))
		#p <- p + geom_bar(stat="identity", position="dodge")
		p <- p + geom_point() + geom_line()
		p <- p + theme(axis.text.x=element_text(angle = -90, hjust = 0))
		p <- p + xlab("Start year") + ylab("Total gain") 
		p <- p + ggtitle(sprintf("%d year rolling gains, lookback=%d months", 
		                         delta, lookback))
		p <- p + annotate("text", x=result$start.year[round(length(result[,1])/2)], 
		                  y=0.92 * max(result$gain), label=stat.string, size=10)
		print(p)
		tmp <- dev.off()
		cat('Finished saving file', fn, '\n')
	}
}