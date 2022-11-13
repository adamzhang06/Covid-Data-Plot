cd <- read.csv(file = 'D:/SLU RESEARCH/DATA/time_series_covid19_confirmed_USA_20210618.csv')
head(cd)
cd[1:3,1:5]
print(cd[1,])
cd[1,1:11]
cd[1:20,1:11]
for(x in 1:20) {
	if(x==1) print(cd[x,1:11])
	if(x>1) cat(as.character(  cd[x,1:11])   )
	print("")
}
#comment
x1 = cd[1,12:525]
plot(as.numeric(x1))
cd[1,1:11]

x2 = cd[cd$Admin2=="St. Louis" & cd$Province_State=="Missouri",12:525]
jpeg("D:/SLU RESEARCH/stlcovidplot.jpg")
plot(as.numeric(x2), main="St. Louis, Missouri",
	xlab="Days",
	ylab="Number of Covid Cases",
	type="l",
	lwd=5,
	col="red"
)
dev.off()

covidLinePlot <- function(city,state) 
{
	data = cd[cd$Admin2==city & cd$Province_State==state,12:525]
	plot(
		as.numeric(data), main=paste(city, state, sep=", "),
		xlab="Days",
		ylab="Number of Covid Cases",
		type="l",
		lwd=5,
		col="red"
	)
}

covidLinePlot("St. Louis","Missouri")