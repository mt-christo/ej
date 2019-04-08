library(doMC)
library(data.table)
library(xts)
registerDoMC(cores=7)
source('/home/aslepnev/git/ej/stropt.R')


u = as.data.table(get(load('/home/aslepnev/git/ej/uni.RData')))
p = get(load('/home/aslepnev/git/ej/uniprc.RData'))
h = diff(log(p))

UN = 100; N = 30
idxs = sample(1:UN, N)

DIVS = array(0, 5)
SIGMAS = 0.1*abs(runif(5))
COUPON = 6.2
RFR = 0.01
TTM = 3
x=0; y=0
BARRIERS = c(100,90,90)
WND = 500
#BARRIERS = c(100,100,100)

if(min(BARRIERS)==100 & max(BARRIERS)==100) res = comb_ATM(h, idxs, 5, WND) else res = comb_OOM(h, idxs, SIGMAS, 5, WND)
res = foreach(i=res)%do%idxs[i]

x = foreach(i=1:length(res))%dopar%{
    if(i%%10==0) print(i)
    h1 = h[, res[[i]], with=FALSE]
    h1 = tail(h1[rowSums(is.na(h1)) == 0, ], WND)
    list(cm=cor(h1), cv=cor(h1), r=wo_calculate(TTM, BARRIERS, SIGMAS, cor(h1), RFR, DIVS, COUPON))
}
x1 = x

x = foreach(i=1:length(res), .combine=c)%dopar%{
    if(i%%10==0) print(i)
    h1 = h[, sample(idxs, 5), with=FALSE]
    h1 = tail(h1[rowSums(is.na(h1)) == 0, ], WND)
    wo_calculate(TTM, BARRIERS, SIGMAS, cor(h1), RFR, DIVS, COUPON)
}
