TrimData <- function(all.prices, symbols, start.date, end.date, lookback){
	# Trim missing data
	trim.prices <- all.prices[,c('date',symbols)]
	for (sym in symbols){
		idx <- !is.na(trim.prices[[sym]])
		trim.prices <- trim.prices[idx,]
	}

	# Trim all data after end.date
	idx <- which(trim.prices$date<=end.date)
	trim.prices <- trim.prices[idx,]

	# Trim all data prior to lookback.date
	# use special %m-% %m+% lubridate operators
	lookback.date <- start.date %m-% months(lookback) 
	idx <- which(trim.prices$date>=lookback.date)
	trim.prices <- trim.prices[idx,]

	if (trim.prices$date[1] != lookback.date){
		lookback.date <- trim.prices$date[1]
		start.date <- lookback.date %m+% months(lookback) 
		write('---------------------------\n', stderr())
		write(sprintf('Insufficient data: adjusting start date to %s\n',
		as.Date(start.date)), stderr())
		write('---------------------------\n', stderr())
	}
	return(trim.prices)
}

CalculateGains <- function(trim.prices, lookback){
	
	len <- length(trim.prices$date)

	init.flag <- TRUE
	symbols <- colnames(trim.prices)[-1]

	for (sym in symbols){
		# Calculate gains each month
		p_vec <- trim.prices[[sym]]
		mg <- c(NA, p_vec[2:len]/p_vec[1:(len-1)])
		# Calculate gains over lookback period
		lg <- c(rep(NA,lookback), p_vec[(lookback+1):len]/p_vec[1:(len-lookback)])
		if(init.flag){
			monthly.gains  <- data.frame(d=mg)
			lookback.gains <- data.frame(d=lg)
			init.flag <- FALSE
		}
		else {
			monthly.gains[[sym]]  <- mg
			lookback.gains[[sym]] <- lg
		}
	}
	colnames(monthly.gains) <- symbols
	colnames(lookback.gains) <- symbols

	# gains.summary is only used for later inspection	
	gains.summary <<- trim.prices
	gains.summary <<- cbind(gains.summary, lookback.gains)
	
	idx <- apply(lookback.gains, 1, which.max)
	gains.summary[['signal']] <<- lapply(idx, function(x) symbols[x])
	gains.summary <<- cbind(gains.summary, monthly.gains)
	cn <<- c('date', symbols, 
	  paste(symbols, 'lg', sep='.'), 'signal', 
		  paste(symbols, 'mg', sep='.'))
  colnames(gains.summary) <<- cn

	# Now trim all data
	lookback.gains <- tail(lookback.gains, -lookback)
	prices         <- tail(trim.prices, -lookback)
	# Only care about FORWARD monthly gains--trim extra row
	monthly.gains  <- tail(monthly.gains, -(lookback + 1) )
  list(lookback=lookback.gains, monthly=monthly.gains)
}

ModelPerformance <- function(lookback.gains, monthly.gains){
	symbols <- colnames(lookback.gains)
	periods <- length(lookback.gains[,1]) - 1  # number of periods

	# Now select the security based on lookback gain that month
	idx <- unname(apply(lookback.gains, 1, which.max))
	long.syms <- unlist(lapply(idx, function(x) symbols[x]))
	num.trades <- sum(diff(idx)!=0, na.rm=TRUE)
	trades.per.year <- num.trades/periods*12

	model.str <- paste(symbols, collapse="-")			

	# Now calculate forward gains based on monthly gains
	model.gains <- rep(NA, periods)
	for (row in seq(1, periods)){
		model.gains[row] <- monthly.gains[row,idx[[row]]]
	}

	gain <- prod(model.gains)
	cmgr <- gain ^ (1/periods) - 1
	stdev <- sqrt(var(model.gains, na.rm=TRUE))
	cat(sprintf('%s,%s,%d,%d,%s,%0.2f,%.4f%%,%.4f,%0.4f\n',
	start.date, end.date, periods, lookback, model.str, gain, 
	100*cmgr, stdev, trades.per.year))

	data.frame(symbol=factor(model.str), 
	  start=start.date, start.year=factor(year(start.date)), end=end.date,
		periods=periods, lookback=lookback, 
		gain=gain, cmgr=cmgr, stdev=stdev, 
		trades_per_year=trades.per.year
	)		
}
