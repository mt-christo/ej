library(googlesheets)
library(dplyr)
library(data.table)
library(xts)
library(corpcor)
library(doMC)
registerDoMC(cores=7)

source('/home/aslepnev/git/ej/stropt.R')
source('/home/aslepnev/git/ej/strfunc.R')

#u$sigma = abs(rnorm(nrow(u))*0.1)  # TODO
p = get(load('/home/aslepnev/git/ej/uniprc.RData'))
u = as.data.table(get(load('/home/aslepnev/git/ej/uni.RData')))[1:ncol(p),]
h = diff(log(xts(p, order.by=as.Date(1:nrow(p)))))[-1,]
NAMES = u$SecName

s = gs_key('1y9KUgukyEvfjAVaDYCHPf1rkFn83q8LWS_0tybz2pIM')
#hrange = as.data.frame(gs_read(s, 'hist', range='A1:Z1000', col_names=FALSE))
#NAMES = as.character(hrange[1,])
#DIVS = as.numeric(hrange[2,])
#SIGMAS = as.numeric(hrange[3,])
#quotes = data.table(hrange[-(1:3),])[, lapply(.SD, as.numeric)]

orange = as.data.frame(gs_read(s, 'wo-optimizer', range='A1:E1000', col_names=FALSE))
SIZE = as.numeric(orange[2,2])
COUPON = as.numeric(gsub('%','',orange[3,2]))*0.01
RFR = as.numeric(gsub('%','',orange[4,2]))*0.01
BARRIERS = as.numeric(orange[-(1:2),5])
TTM = length(BARRIERS)
TAIL = 120

UNI = if(orange[3,4] == 'ALL') names(h) else na.omit(orange[-(1:2),4])
UNI = UNI[UNI%in%NAMES]

h = h[, match(UNI, NAMES)]
h = tail(h[rowSums(is.na(h)) == 0, ], TAIL)

pwcorr = pairwise_func(h, cor)[name1!=name2, ] #value:=scale(value, center=TRUE,scale=TRUE)]
pwcorr$value = pwcorr$value-mean(pwcorr$value); pwcorr$value = pwcorr$value/sd(pwcorr$value)
pwcov = pairwise_func(h, cov)[name1!=name2, ] #value:=scale(value, center=TRUE,scale=TRUE)]
pwcov$value = pwcov$value-mean(pwcov$value); pwcov$value = pwcov$value/sd(pwcov$value)
pwbest = copy(pwcorr); pwbest$value = (sum(BARRIERS==100)*pwcorr$value + sum(BARRIERS<100)*pwcov$value)/length(BARRIERS)

best_set = 


#res = if(min(BARRIERS)==100 & max(BARRIERS)==100)
#          lowest_cormats(h, SIZE, 0.05) else highest_sds(h, SIGMAS, SIZE, 0.95)

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

