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
lines(ts1, type = "l", col = "red")
lines(ts_pt, type = "l", col = "blue")





ts_d2 <- diff(ts, differences = 2)
ts_d2d12 <- diff(ts_d2, lag = 12)



plot(ts, type = "l")
lines(ts_pt, type = "l", col = "GREEN")

lines(ts_d2, type = "l", col = "red")
lines(ts_d2d12, type = "l", col = "blue")
