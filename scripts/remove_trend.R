rm(list = ls())

library(tidyverse)
library(oceanwaves)
library(pracma)

#-----------#
data(wavedata)
detrended <- detrendHeight(wavedata$swDepth.m)
pt <- detrended[['pt']]
plot(wavedata$swDepth.m, type = 'l')
plot(pt, type = 'l')
abline(h = 0)

all(detrended[['pt']] == wavedata$swDepth.m)
plot(wavedata$swDepth.m, type = "l", col = "black")
plot(detrended[['pt']], type = "l", col = "blue")



#-----------#
table <- read.csv("data/tables/SST_ATL_anom/ATLA_ANOMALIES.csv", sep = ";")
ts <- table$ANOM

ts1 <- detrend(ts) %>% as.numeric()

ts_detrended <- detrendHeight(ts)
ts_pt <- ts_detrended[['pt']]

plot(ts, type = "l")
plot(ts1, type = "l", col = "red")
lines(ts_pt, type = "l", col = "blue")





ts_d2 <- diff(ts, differences = 2)
ts_d2d12 <- diff(ts_d2, lag = 12)



plot(ts, type = "l")
lines(ts_pt, type = "l", col = "GREEN")

lines(ts_d2, type = "l", col = "red")
lines(ts_d2d12, type = "l", col = "blue")

#---------------------------#
install.packages("fpp")
library(fpp)
library(forecast)
data(ausbeer)
timeserie_beer <- tail(head(ausbeer, 17*4+2),17*4-4)
plot(as.ts(timeserie_beer))
#--------#
trend_beer <- ma(timeserie_beer, order = 4, centre = T)
plot(as.ts(timeserie_beer))
lines(trend_beer)
plot(as.ts(trend_beer))
#--------#
detrend_beer <- timeserie_beer - trend_beer
plot(as.ts(detrend_beer))
#--------#
m_beer <- t(matrix(data = detrend_beer, nrow = 4))
seasonal_beer <- colMeans(m_beer, na.rm = T)
plot(as.ts(rep(seasonal_beer,16)))
#--------#
random_beer <- timeserie_beer - trend_beer - seasonal_beer
plot(as.ts(random_beer))
#--------#
recomposed_beer <- trend_beer+seasonal_beer+random_beer
plot(as.ts(recomposed_beer))
#--------#
ts_beer <- ts(timeserie_beer, frequency = 4)
decompose_beer <- decompose(ts_beer, "additive")
plot(as.ts(decompose_beer$seasonal))
plot(as.ts(decompose_beer$trend))
plot(as.ts(decompose_beer$random))
plot(decompose_beer)


ts_beer <- ts(timeserie_beer, frequency = 4)
stl_beer <- stl(ts_beer, "periodic")
seasonal_stl_beer <- stl_beer$time.series[, 1]
trend_stl_beer <- stl_beer$time.series[, 2]
random_stl_beer <- stl_beer$time.series[, 3]

plot(ts_beer)
plot(as.ts(seasonal_stl_beer))
plot(trend_stl_beer)
plot(random_stl_beer)
plot(stl_beer)


#---------------------------#
install.packages("Ecdat")
library(Ecdat)
data(AirPassengers)
timeserie_air <- AirPassengers
plot(as.ts(timeserie_air))
#--------#
install.packages("forecast")
library(forecast)
trend_air <- ma(timeserie_air, order = 12, centre = T)
plot(as.ts(timeserie_air))
lines(trend_air)
plot(as.ts(trend_air))
#--------#
detrend_air = timeserie_air / trend_air
plot(as.ts(detrend_air))
#--------#
m_air = t(matrix(data = detrend_air, nrow = 12))
seasonal_air = colMeans(m_air, na.rm = T)
plot(as.ts(rep(seasonal_air,12)))
#--------#
random_air <- timeserie_air / (trend_air * seasonal_air)
plot(as.ts(random_air))
#--------#
recomposed_air <- trend_air*seasonal_air*random_air
plot(as.ts(recomposed_air))
#--------#
ts_air <- ts(timeserie_air, frequency = 12)
decompose_air <- decompose(ts_air, "multiplicative")

plot(as.ts(decompose_air$seasonal))
plot(as.ts(decompose_air$trend))
plot(as.ts(decompose_air$random))
plot(decompose_air)


# HOW TO CALCULATE ANOMALIES ON TIME SERIES WITH OUTLIERS
set.seed(4)
data <- read.csv("data/tables/webTraffic.csv", sep = ",", header = T)
head(data)
tail(data)
days <- as.numeric(data$Visite)
for (i in 1:45) {
  pos <- floor(runif(1, 1, 50))
  days[i * 15 + pos] <- days[i * 15 + pos]^1.2
}
days[510 + pos] <- 0
plot(as.ts(days))

#-----#
# DESCOMPOSICIÓN DE LA SERIE DE TIEMPO
# install required library
install.packages("FBN")
library(FBN)

decomposed_days <- decompose(ts(days, frequency = 7), "multiplicative")
plot(decomposed_days)

# USO DE LA DISTRIBUCIÓN NORMAL PARA ENCONTRAR EL MÍNIMO Y EL MÁXIMO
random <- decomposed_days$random
min <- mean(random, na.rm = T) - 4 * sd(random, na.rm = T)
max <- mean(random, na.rm = T) + 4 * sd(random, na.rm = T)

plot(as.ts(as.vector(random)), ylim = c(-0.5, 2.5))
abline(h = max, col = "#e15f3f", lwd = 2)
abline(h = min, col = "#e15f3f", lwd = 2)

#install library
install.packages("forecast")
library(forecast)
library(stats)

trend <- runmed(days, 7)
plot(as.ts(trend))

detrend <- days / as.vector(trend)
m <- t(matrix(data = detrend, nrow = 7))
seasonal <- colMeans(m, na.rm = T)
random <- days / (trend * seasonal)
rm_random <- runmed(random[!is.na(random)], 3)

min <- mean(rm_random, na.rm = T) - 4 * sd(rm_random, na.rm = T)
max <- mean(rm_random, na.rm = T) + 4 * sd(rm_random, na.rm = T)
plot(as.ts(random))
abline(h = max, col = "#e15f3f", lwd = 2)
abline(h = min, col = "#e15f3f", lwd = 2)
#--------------------------#
high <- (trend * seasonal) * max
low <- (trend * seasonal) * min
plot(as.ts(days), ylim = c(0, 200))
lines(as.ts(high), col = "blue")
lines(as.ts(low), col = "red")

x <- c(seq(1, length(high)), seq(length(high), 1))
y <- c(high, rev(low))
length(x)
length(y)
polygon(x, y, density = 50, col = "skyblue")




