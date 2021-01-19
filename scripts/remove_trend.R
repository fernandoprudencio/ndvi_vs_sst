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
data(ausbeer)
timeserie_beer <- tail(head(ausbeer, 17*4+2),17*4-4)
plot(as.ts(timeserie_beer))
#--------#
library(forecast)
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
