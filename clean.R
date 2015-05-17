
all.prices <- data.frame(date=all_dates)

for (sym in symbols){
	d <- LoadData(sym)
	all.prices[[sym]] <- approx(d$x, d$y, all.prices$date)$y
}
