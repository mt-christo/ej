library(doMC)
registerDoMC(cores=4)
library(foreach)
library(xts)

w0 = c(0.3,0.1,0.6)
files = foreach(i=2006:2017)%do%{
    x = read.csv(paste(i,'csv',sep='.'),sep=';')[,c(1,3,4,5)]
    names(x) = c('ticker','mc','ivol','rnd')
    x
}

d = function(w){
    mean(foreach(i=1:(length(files)-1),.combine=c)%do%{
        x = files[[i]]
        x1 = files[[i+1]]

        x = x[!is.na(x$ticker) & x$ticker!="" & !x$ticker%in%c('JNJ US Equity','PFE US Equity','NOVN VX Equity','ROG VX Equity','MRK US Equity') & x$ticker%in%x1$ticker,]
        x$year = i
        r0 = x$mc[!is.na(x$mc)]; x$rm = (x$mc - min(r0))/(max(r0) - min(r0))
        r0 = x$ivol[!is.na(x$ivol)]; x$ri = (x$ivol - min(r0))/(max(r0) - min(r0))
        r0 = x$rnd[!is.na(x$rnd)]; x$rr = (x$rnd - min(r0))/(max(r0) - min(r0))
        
        ratios = x[,c('rm','ri','rr')]
        for(j in 1:length(w))
            ratios[is.na(ratios[,j]),j] = 0
        x$ratio = foreach(j=1:length(w),.combine='+')%do%{ w[j]*ratios[,j] } / foreach(j=1:length(w),.combine='+')%do%ifelse(ratios[,j]==0,0,w[j])
        t = as.character(x[order(x$ratio,decreasing=TRUE)[1:25],'ticker'])
        mean(x1$mc[match(t,x1$ticker)]/x$mc[match(t,x$ticker)])
    })
}

ww = foreach(i=1:1000,.combine=c)%do%{ x = runif(2)*0.66; list(c(x,1-sum(x))) }

res = foreach(k=1:length(ww))%dopar%{
    print(k)
    d(ww[[k]])
}



