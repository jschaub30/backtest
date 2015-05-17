#!/usr/bin/Rscript

# Start with all dates in century on last day of month
all_dates <- as.Date('1930-01-31') %m+% months(1:(100*12))
all_symbols <- c('TBIL3', 'GOLD', 'INTL', 'GSPC')
LoadData <- function(sym){
	if(sym=='TBIL3'){
		# http://research.stlouisfed.org/fred2/series/TB3MS#
		df <- read.csv('data/tbill_3month.csv')
		y1 <- 1 + df[,2]/12/100  # Convert to monthly gain
		y1 <- cumprod(y1)    # Convert from gain to price
		data.frame(x = as.Date(df$date, "%Y-%m-%d"), y = y1)
	} 
	else if(sym=='GOLD'){
		# http://research.stlouisfed.org/fred2/series/TB3MS#
		df <- read.csv('data/gold_fixing.csv')
		idx <- !is.na(df$price)
		data.frame(x = as.Date(df$date[idx], "%Y-%m-%d"), y = df$price[idx])
	} 
	else if(sym=='GOLD2'){
		#view-source:http://www.macrotrends.net/1333/historical-gold-prices-100-year-chart
		# V1 is real--adjusted for inflation
		# V2 is nominal
		con <- file('data/gold.json', "r")
		df  <- ldply(fromJSON(con), data.frame)
		close(con)
		data.frame(x=as.Date(df$d), y=df$v2)
	} 
	else if(sym=='INTL'){
		# Load AWCI ex US data
		# https://www.msci.com/end-of-day-data-search
		df <- read.csv('data/msci_awci_ex_us.csv')
		data.frame(x=as.Date(df$Date, "%m/%d/%Y"), y=df[,2])
	} 
	else if(sym=='GSPC'){
		# Load Yahoo daily GSPC data
		# http://finance.yahoo.com/q/hp?s=%5EGSPC&a=11&b=31&c=1900&d=04&e=1&f=2015&g=d
		df <- read.csv('data/GSPC_daily.csv')
		len <- length(df[,1])
		x1 <- as.Date(df$Date[len:1], "%Y-%m-%d")   # data is in reverse order
		y1 <- df$Adj.Close[len:1]
		data.frame(x=x1, y=y1)
	}
	else if(sym=='GSPC1'){
		#view-source:http://www.macrotrends.net/1333/historical-gold-prices-100-year-chart
		con <- file('data/sp500.json', "r")
		df  <- ldply(fromJSON(con), data.frame)
		close(con)
		data.frame(x=as.Date(df$d), y=df$v1)
	} 
	else if(sym=='CASH'){
		data.frame(x=all_dates, y=1)
	}
	else {
		stop(paste('No data for symbol', sym))
	}
}
