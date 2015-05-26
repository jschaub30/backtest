options(width = 200)
img.width  <- 1400
img.height <- 800

dir.create("html/img", showWarnings = FALSE, recursive = TRUE)
for (strategy in c("GEM2")) {
	all.symbols <- list(c("GOLD"), c("INTL"), c("TBIL3"), c("GSPC"), 
	c("GSPC", "TBIL3"),
	c("GSPC", "INTL", "TBIL3"),
	c("GSPC", "INTL", "TBIL3", "GOLD"))

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
			strategy.title <- "GEM3"
			strategy.symbol <- result$symbol[6]
			result$cagr <- 100*(result$gain^(1/(result$periods/12))-1)
			len <- length(result[, 1])
			r1 <- subset(result, symbol == strategy.symbol)$gain
			r2 <- subset(result, symbol == "GSPC")$gain
			tot <- length(r1)
			stat.string <- sprintf("%s > GSPC in %d/%d years (%.2f%%)", 
			  strategy.title, sum(r1 > r2), tot, sum(r1 > 
				r2)/tot * 100)
			stat.string <- sprintf("%s\n%s: mean=%.1f, med=%.1f", 
			  stat.string, strategy.title, mean(r1), median(r1))
			stat.string <- sprintf("%s\nGSPC: mean=%.1f, med=%.1f", 
			  stat.string, mean(r2), median(r2))
				
			fn <- sprintf("html/img/%d_year_window_%d_months.png", 
			  delta, lookback)
			png(fn, width = img.width, height = img.height)
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
			p <- p + theme(text = element_text(size = rel(6)))
			p <- p + theme(plot.title = element_text(size = rel(6)))
			p <- p + guides(fill = guide_legend(keywidth = 10, keyheight = 20))
			p <- p + theme(legend.text = element_text(size = rel(5)))
			print(p)
			tmp <- dev.off()
			cat("Finished saving file", fn, "\n")
			
			
			fn <- sprintf("html/img/%d_year_window_%d_months_box.png", 
			  delta, lookback)
			png(fn, width = img.width, height = img.height)
			p <- ggplot(result, aes(x = symbol, y=gain, fill=symbol)) + geom_boxplot()
			p <- p + xlab("") + ylab("Total gain")
			p <- p + ggtitle(sprintf("1973-2014, %d year rolling gains, lookback=%d months", 
		  delta, lookback))
			p <- p + theme(text = element_text(size = rel(6)))
			p <- p + theme(plot.title = element_text(size = rel(6)))
			p <- p + guides(fill = FALSE)
			print(p)
			tmp <- dev.off()
			cat("Finished saving file", fn, "\n")
			
			if (delta == 5) y.max = 40
			if (delta == 10) y.max = 30
			if (delta == 15) y.max = 30
			if (delta == 20) y.max = 25
			fn <- sprintf("html/img/%d_year_window_%d_months_box_cagr.png", 
			  delta, lookback)
			png(fn, width = img.width, height = img.height)
			p <- ggplot(result, aes(x = symbol, y=cagr, fill=symbol)) + geom_boxplot()
			p <- p + xlab("") + ylab("CAGR [ % ]")
			p <- p + ggtitle(sprintf("1973-2014, %d year rolling gains, lookback=%d months", 
		  delta, lookback))
			p <- p + theme(text = element_text(size = rel(6)))
      p <- p + theme(axis.text.x = element_text(angle = 20, hjust = 1))
			p <- p + theme(plot.title = element_text(size = rel(6)))
			p <- p + guides(fill = FALSE)
			p <- p + ylim(-5, y.max)
			print(p)
			tmp <- dev.off()
			cat("Finished saving file", fn, "\n")
			
		}
	}
}
