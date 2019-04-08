library(nloptr)
library(data.table)
library(doMC)
library(data.table)
library(foreach)
library(xts)
library(nloptr)
library(tseries)
registerDoMC(cores=7)
COB_CTL <<- list(xtol_rel=1e-8, maxeval=5000)

d1 = get(load('/home/aslepnev/webhub/zacks_data.RData'))
d2 = get(load('/home/aslepnev/webhub/sacha_etf_yhoo.RData'))

h0 = cbind(d1$h[, 1:5], d2$h[, 1:5])
h0 = h0[min(which(rowSums(is.na(h0))==0)):max(which(rowSums(is.na(h0))==0)), ]
h0 = h0[index(h0)>='2010-01-01',]
mytail = as.numeric(tail(h0, 1))
#h0 = na.fill(diff(log(1+na.locf(h0))), 0)[-1, ]
#for(j in 1:ncol(h0))
#    h0[abs(h0[,j])>0.5, j] = 0
#h0 = h0[-(1:min(which(rowSums(h0==0)==0))), ]

basket_ret = function(h, w) return( xts(log(((exp(h)-1)%*%w) + 1), order.by=index(h)) )
basket_perf = function(h, w) return( exp(cumsum(basket_ret(h, w))) )

# w = array(1/n, n)
cmon = function(w){
    r = foreach(j=1:ncol(h0),.combine='+')%do%{ h0[,j]*w[j]/mytail[j] }
    t = data.table(r=round(as.numeric(r),2), period=paste(period_func(index(r)), year(index(r))))
    return(-max(t[, length(unique(period)), by='r']$V1)/length(unique(t$period)))
}

nms = colnames(h0)
h = h0[, nms, with=FALSE]
n = ncol(h)
period_func <<- quarter
w0 = runif(n)
res = cobyla(x0=w0-mean(w0), fn=cmon, lower=array(-10, n), upper=array(10, n), hin=function(w){ -abs(sum(w)) }, control=COB_CTL); print(res$value)
#res = cobyla(x0=w0-mean(w0), fn=cmon, lower=array(-10, n), upper=array(10, n), control=COB_CTL); print(res$value)

w = res$par
r = foreach(j=1:ncol(h0),.combine='+')%do%{ h0[,j]*w[j]/mytail[j] }
plot(r)
save(res, file='/home/aslepnev/webhub/treh_coi1.RData')
