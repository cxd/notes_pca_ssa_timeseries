require(rworldmap)
require(zoo)
source("lib/precp_data.R")

stations <- "data/HQDR_stations.txt"
basedir <- "data/daily_precip"

data <- read_data(basedir)

sdata <- read_stations(stations)

merged <- mergeStations(sdata, data)
allData <- merged$allData
allWide <- merged$allWide

summary(allWide)

australiaBox <- get_aust_bounds_lon_lat()


sdata <- sdata[complete.cases(sdata),]

map <- getMap(resolution = "low")
plot(map, xlim = c(australiaBox$left, australiaBox$right), ylim = c(australiaBox$bottom, australiaBox$top), asp = 1)
points(sdata$lon, sdata$lat, col="blue", cex=0.5)

station <- "CAPE MORETON LIGHTHOUSE"
srow <- sdata[sdata$name == station,]
id <- srow$station

series <- allWide[,c("date",id)]
names(series) <- c("date", "precip")

## filter the series so that the time lags will start in january.
## We have a measure for the earliest date


idx <- !is.nan(series$precip)
min(series[idx,]$date)
## Need to start the series from January 1888 to start "at the "zero" the time series.
series <- series[series$date >= "1888-01-01",]
min(series$date)

summary(series)
plot(series$precip ~ series$date )

sequence <- series$precip[!is.nan(series$precip)]

months



M <- slidingMatrix(sequence, 365)
dim(M)
head(M)


S <- 1/(nrow(M)-ncol(M)) * t(M)%*%M
mu <- colMeans(M)
mu
gamma <- 1/(nrow(M))*diag(t(M-mu)%*%(M-mu))
gamma
C <- cov(M)
C

M.pc <- princomp(M)

plot(M.pc)

## We seek the percentage of variance to determine the amount of variation explained
## by each respective component within the time series.
percentVar <- M.pc$sdev^2/(sum(M.pc$sdev^2))
percentVar
barplot(percentVar, names.arg=paste("PC", 1:length(percentVar), sep=""), main="Percent Variation Explained per Component")


## The eigen vectors provide the periods of oscillation associated with the 
ev1 <- M.pc$loadings[,1]
ev2 <- M.pc$loadings[,2]
ev3 <- M.pc$loadings[,3]

sumEv <- ev1 + ev2 + ev3

ylimits <- c(min(ev1,ev2,ev3, sumEv), max(ev1,ev2,ev3, sumEv))

plot(ev1, type="l", col="blue", 
     ylim=ylimits,
     xlim=c(-70,400),
     xlab="Lag Days (365 days per window)",
     ylab="EOF",
     main=paste("EOFs oscillations in Rainfall (Cape Moreton)", strftime(min(series$date), "%b %Y"), "to", strftime(max(series$date), "%b %Y")))
grid(col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)
lines(ev2, col="red")
lines(ev3, col="green")
lines(sumEv)
legend("bottomleft", lty=c(1,1,1), col=c("blue", "red", "green", "black"), legend=c("C1","C2","C3", "Sum"))


write.csv(series, file="data/example_cape_moreton.csv", row.names=FALSE)

## Note the eigenvalues are offset by \pi/2 at each value.

