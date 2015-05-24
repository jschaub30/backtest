options(width = 200)
dir.create("html/img", showWarnings = FALSE, recursive = TRUE)
for (strategy in c("GEM3", "GEM4")) {
	if (strategy == "GEM3") {
		stratety.title = "GEM3"
		all.symbols <- list(c("GSPC", "INTL", "TBIL3"), c("GSPC"), 
			c("INTL"), c("TBIL3"))
	}
	else if (strategy == "GEM4") {
		stratety.title = "GEM3 + Gold"
		all.symbols <- list(c("GSPC", "INTL", "TBIL3", "GOLD"), 
			c("GSPC"), c("INTL"), c("TBIL3"), c("GOLD"))
	}
	for (lookback in c(8, 10, 12)) {
		for (delta in c(5, 10, 15, 20)) {
			start.date <- as.Date("1973-1-31", "%Y-%m-%d")
			end.date <- start.date + years(delta)
			result <- data.frame()
			while (end.date <= as.Date("2015-1-31", "%Y-%m-%d")) {
			  for (symbols in all.symbols) {
				trim.prices <- TrimData(all.prices, symbols, 
				  start.date, end.date, lookback)
				df <- CalculateGains(trim.prices, lookback)
				result <- rbind(result, ModelPerformance(df$lookback, 
				  df$monthly))
			  }
			  start.date <- start.date + years(1)
			  end.date <- start.date + years(delta)
			}
			len <- length(result[, 1])
			r1 <- subset(result, symbol == result$symbol[1])$gain
			r2 <- subset(result, symbol == "GSPC")$gain
			tot <- length(r1)
			stat.string <- sprintf("%s > GSPC in %d/%d years (%.2f%%)", 
			  stratety.title, sum(r1 > r2), tot, sum(r1 > 
				r2)/tot * 100)
			stat.string <- sprintf("%s\n%s: mean=%.1f, med=%.1f", 
			  stat.string, stratety.title, mean(r1), median(r1))
			stat.string <- sprintf("%s\nGSPC: mean=%.1f, med=%.1f", 
			  stat.string, mean(r2), median(r2))
				
				
			fn <- sprintf("html/img/%s_%d_year_window_%d_months.png", 
			  strategy, delta, lookback)
			png(fn, width = 1000, height = 600)
			p <- ggplot(result, aes(x = start.year, y = gain, 
			  color = symbol, group = symbol))
			p <- p + geom_point() + geom_line()
			p <- p + theme(axis.text.x = element_text(angle = -90, 
			  hjust = 0))
			p <- p + xlab("Start year") + ylab("Total gain")
			p <- p + ggtitle(sprintf("%d year rolling gains, lookback=%d months", 
			  delta, lookback))
			p <- p + annotate("text", x = result$start.year[round(len/2)], 
			  y = 0.92 * max(result$gain), label = stat.string, 
			  size = 10)
			print(p)
			tmp <- dev.off()
			cat("Finished saving file", fn, "\n")
			
			
			fn <- sprintf("html/img/%s_%d_year_window_%d_months_box.png", 
			  strategy, delta, lookback)
			png(fn, width = 1000, height = 600)
			p <- ggplot(result, aes(x = symbol, y=gain, fill=symbol)) + geom_boxplot()
			p <- p + xlab("") + ylab("Total gain")
			p <- p + ggtitle(sprintf("1973-2014, %d year rolling gains, lookback=%d months", 
		  delta, lookback))
			print(p)
			tmp <- dev.off()
			cat("Finished saving file", fn, "\n")
			
		}
	}
}
