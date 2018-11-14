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
NAMES = hrange[1,]
DIVS = as.numeric(hrange[2,])
SIGMAS = as.numeric(hrange[3,])
quotes = data.table(hrange[-(1:3),])[, lapply(.SD, as.numeric)]

orange = as.data.frame(gs_read(s, 'optimizer', range='A1:D1000', col_names=FALSE))
BARRIERS = as.numeric(orange[-1,1])
SIZE = as.numeric(orange[2,3])
COUPON = as.numeric(orange[2,4])
RFR = 0.01
TTM = length(BARRIERS)

h = diff(log(xts(quotes, order.by=as.Date(1:nrow(quotes)))))[-1,]
UNI = if(orange[2,2] == 'ALL') NAMES else orange[-1,2]
names(h) = NAMES
h = h[, match(UNI, NAMES)]

if(min(BARRIERS)==100 & max(BARRIERS)==100) res = comb_ATM(h, SIZE, 0.1) else res = comb_OOM(h, SIGMAS, SIZE, 0.9)
res = foreach(i=res)%do%idxs[i]

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

