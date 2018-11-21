library(googlesheets)
library(dplyr)
library(data.table)
library(xts)
library(corpcor)
library(doMC)
registerDoMC(cores=7)

source('/home/aslepnev/git/ej/stropt.R')
source('/home/aslepnev/git/ej/strfunc.R')

#u = as.data.table(get(load('/home/aslepnev/git/ej/uni.RData')))
#u$sigma = abs(rnorm(nrow(u))*0.1)  # TODO
#p = get(load('/home/aslepnev/git/ej/uniprc.RData'))

s = gs_key('1y9KUgukyEvfjAVaDYCHPf1rkFn83q8LWS_0tybz2pIM')
hrange = as.data.frame(gs_read(s, 'hist', range='A1:Z1000', col_names=FALSE))
NAMES = as.character(hrange[1,])
DIVS = as.numeric(hrange[2,])
SIGMAS = as.numeric(hrange[3,])
quotes = data.table(hrange[-(1:3),])[, lapply(.SD, as.numeric)]

orange = as.data.frame(gs_read(s, 'wo-optimizer', range='A1:E1000', col_names=FALSE))
BARRIERS = as.numeric(orange[-(1:2),5])
SIZE = as.numeric(orange[2,2])
COUPON = as.numeric(orange[3,2])
RFR = as.numeric(orange[4,2])
TTM = length(BARRIERS)

UNI = if(orange[3,4] == 'ALL') NAMES else na.omit(orange[-(1:2),4])[[1]]

h = diff(log(xts(quotes, order.by=as.Date(1:nrow(quotes)))))[-1,]
names(h) = NAMES
h = h[, match(UNI, NAMES)]

res = if(min(BARRIERS)==100 & max(BARRIERS)==100)
          lowest_cormats(h, SIZE, 0.05) else highest_sds(h, SIGMAS, SIZE, 0.95)

x = foreach(i=1:length(res))%dopar%{
    if(i%%10==0) print(i)
    j = res[[i]]
    h1 = h[, j, with=FALSE]
    h1 = h1[rowSums(is.na(h1)) == 0, ]
    list(cm=cor(h1), cv=cor(h1), r=wo_calculate(TTM, BARRIERS, SIGMAS[j], make.positive.definite(cor(h1)), RFR, DIVS[j], COUPON))
}
x1 = x

x = foreach(i=1:length(res), .combine=c)%dopar%{
    if(i%%10==0) print(i)
    j = sample(1:ncol(h), SIZE)
    h1 = h[, j, with=FALSE]
    h1 = tail(h1[rowSums(is.na(h1)) == 0, ], WND)
    wo_calculate(TTM, BARRIERS, SIGMAS[j], make.positive.definite(cor(h1)), RFR[j], DIVS[j], COUPON)
}

