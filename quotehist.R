library(tseries)
library(RJSONIO)
library(foreach)
library(data.table)
library(ggplot2)
library(reshape2)
library(CryptoTools)
library(xts)
library(doMC)
library(telegram)
registerDoMC(4)

# exchange='BitTrex'; ticker='ETH'; limit=20000; base_curr = 'USD'; 
get_history = function(exchange, ticker, limit, base_curr = 'BTC', loadtype='histohour'){
    t = limit
    to_ts = 0
    res = list()
    while(t > 0){
        url <- paste0("https://min-api.cryptocompare.com/data/loadtype?aggregate=1&e=BitTrex&extraParams=CryptoCompare&fsym=ETH&limit=2000&tryConversion=false&tsym=BTC")
        url = gsub('BitTrex',exchange, url)
        url = gsub('BTC', base_curr, url)
        url = gsub('ETH', ticker, url)
        url = gsub('loadtype', loadtype, url)
        if(to_ts!=0)
            url = paste0(url, "&toTs=", to_ts)
        
        r1 = data.frame(foreach(r=fromJSON(url)$Data,.combine=rbind)%do%r)
        if(nrow(r1)>0){
            to_ts = min(r1$time) - 10
            r1$date = as.POSIXct(r1$time, origin='1970-01-01')
            r1 = as.xts(r1[, !colnames(r1)%in%c("time","date")], order.by=r1$date)
            storage.mode(r1) = 'numeric'
            res[[as.character(to_ts)]] = r1
            Sys.sleep(0.2);
            print(url)
        }
        t = t - 2000
    }

    res = foreach(x=res,.combine=rbind)%do%x
    res[res$close!=0,]
}

#exchange='BitTrex'; ticker='ETC'; limit=2000; base_curr = 'BTC'
get_r_history = function(exchange, ticker, limit, base_curr = 'BTC', loadtype='histohour'){
    x = get_history(exchange, ticker, limit, base_curr, loadtype)
    x = x[!is.na(x$close) & x$close!=0, 'close']
    names(x)[1] = ticker
    diff(log(x))[-1]
}

#x1 = get_history('Bittrex','BCH',20000,'USD'); x1 = diff(log(x1$close))[-1]; x1 = x1[!is.na(x1) & x1 != Inf]
#x2 = get_history('Bittrex','LTC',20000,'USD'); x2 = diff(log(x2$close))[-1]; x2 = x2[!is.na(x2) & x2 != Inf]
#x1 = x1[index(x2)]
#x2 = x2[index(x1)]
#cov(as.numeric(x1),as.numeric(x2))/var(as.numeric(x2))

#setwd('~/R/ej')
#save_csv = function(e,t,l) { write.zoo(get_history(e,t,l), file = paste0('Data//',e,'-',t,'-',l,'.csv'), row.names=FALSE, sep=';')}

#save_csv('Bittrex','ETH',2000)
#save_csv('Bittrex','XRP',2000)
#save_csv('Bittrex','DASH',2000)
#save_csv('Bittrex','ETC',2000)
#save_csv('Bittrex','ZEC',2000)
#save_csv('Bittrex','STORJ',2000)
#save_csv('Bittrex','SNT',2000)
#save_csv('Bittrex','XLM',2000)

#r0h=r0; r1h=r1

r0 = foreach(t=c('BTC','ETH','NEO','XRP','BCH','LTC','DASH','XMR','ETC','ZEC'),.combine='merge.xts')%do%get_r_history('BitTrex',t,3000,'USD','histohour')
r0 = r0[rowSums(is.na(r0))==0 & index(r0)>as.Date(min(index(r0)))+30]
r1 = foreach(t=c('ETH','XRP','BCH','LTC','DASH','XMR','ETC','ZEC'),.combine='merge.xts')%do%get_r_history('BitTrex',t,8000,'BTC','histoday')
r1 = r1[rowSums(is.na(r1))==0 & index(r1)>as.Date(min(index(r1)))+30]
# save(r0, file='r0.RData'); save(r1, file='r1.RData'); 

r_tmp = r0
m = melt(cor(r_tmp))
m = m[m$Var1!=m$Var2,]
#cv = cov(r_tmp); b = foreach(i=1:nrow(m),.combine=c)%do%round(cv[m$Var1[i],m$Var2[i]]/cv[m$Var1[i],m$Var1[i]],2)
ggplot(data=m, aes(x=reorder(Var1,value),y=reorder(Var2,value),fill=value)) + geom_tile() + scale_fill_gradient(low='white',high='red') + geom_text(aes(label=round(m$value,2)), size=5)

foreach(x=names(r0)[1:3],.combine=rbind)%do%{
#    t = rollapply(r0[,x],10,FUN=mean)[-(1:10)]
    t = r0[,x]
    y = length(t)
    t = t[(y-200):(y-100)]
    c(x,sharpe(as.numeric(t)))
}




ggplot(data=m, aes(x=Var1,y=Var2,fill=value)) + geom_tile() + scale_fill_gradient(low='white',high='red') + geom_text(aes(label=round(m$value,2)), size=5)

#ggplot(data=m[order(m$value),], aes(x=Var1,y=Var2,fill=value)) + geom_tile() + scale_fill_gradient(low='white',high='red') + geom_text(aes(label=b[order(m$value)]), size=5)


x1 = get_r_history('BitTrex','XMR',3000,'USD')
x2 = get_r_history('BitTrex','LTC',3000,'USD')
x1 = x1[index(x2)]
x2 = x2[index(x1)]
x1 = x1[index(x1)>'2017-09-01']
x2 = x2[index(x2)>'2017-09-01']
cor(x1,x2)

foreach(i=1:ncol(r0),.combine=c)%do%sd(r0[,i])
hist(r0[,2])
plot(exp(cumsum(x1))); lines(exp(cumsum(x2)))


symbols = sort(names( fromJSON( paste0( "https://min-api.cryptocompare.com/data/all/coinlist" ) )$Data ))
x = get_cryptocompare_data( 'ETH', '2018-02-14', '2018-02-15', 'hour', 'CCCAGG', 'BTC', local=T )
get_cryptocompare_data( 'ETC', '2018-02-14', '2018-02-16', 'hour', 'CCCAGG', 'BTC', local=T )$time


save(r, file='storage_r.RData')
save(v, file='storage_v.RData')

r = foreach(s=symbols,.combine='merge.xts')%do%{
    print(s)
    x = get_cryptocompare_data( s, '2017-08-01', '2018-02-15', 'hour', 'CCCAGG', 'BTC', local=T )
    if(!is.null(x)){
        x = as.xts(x$close, order.by=x$time)
        names(x)[1] = s
        x
    }
}

v = foreach(s=symbols,.combine='merge.xts')%do%{
    print(s)
    x = get_cryptocompare_data( s, '2017-08-01', '2018-02-15', 'hour', 'CCCAGG', 'BTC', local=T )
    if(!is.null(x)){
        x = as.xts(x$volume_to, order.by=x$time)
        names(x)[1] = s
        x
    }
}

setwd('~/git/ej')
r = get(load('storage_r.RData'))
v = get(load('storage_v.RData'))

r = r[index(r)>'2017-08-01',]
v = v[index(v)>'2017-08-01',]
v = na.fill(v, 0)
r = na.locf(r)

P1 = 24*5
P2 = 24*24
J = 0.1
res = foreach(j=seq(0,0.15,by=0.005),.combine=c)%do%{

rbr = foreach(i = (P1+1):(length(index(r)) - P2),.combine=c)%dopar%{
    print(i)
    vs = v[(i-P1):i,]
    vs = as.numeric(t(rowSums(t(v[(i-P1):i,]))))
    idxv = order(vs, decreasing=TRUE)[20:50]

    # r[c((i-1):i, i+P2), intersect(idxv,idxr)]
    # rs[, intersect(idxv,idxr)]
    rs = diff(log(r[c((i-4):i, i+P2),]))
    idxr = which(as.numeric(rs[2,]) > j & as.numeric(rs[3,]) > 0 & as.numeric(rs[4,]) > 0 & as.numeric(rs[5,]) > 0)
    
    x = as.numeric(rs[6,intersect(idxv,idxr)])
    if(length(x) > 0){
        x
    }
    
}
    mean(rbr)
}

length(rbr)




res2 = foreach(j=seq(0,0.15,by=0.005),.combine=c)%do%{

rbr = foreach(i = (P1+1):(length(index(r)) - P2),.combine=c)%dopar%{
    print(i)
    vs = v[(i-P1):i,]
    vs = as.numeric(t(rowSums(t(v[(i-P1):i,]))))
    idxv = order(vs, decreasing=TRUE)[20:50]

    # r[c((i-1):i, i+P2), intersect(idxv,idxr)]
    # rs[, intersect(idxv,idxr)]
    rs = diff(log(r[c((i-3):i, i+P2),]))
    idxr = which(as.numeric(rs[2,]) > j & as.numeric(rs[3,]) > 0 & as.numeric(rs[4,]) > 0)
    
    x = as.numeric(rs[5,intersect(idxv,idxr)])
    if(length(x) > 0){
        x
    }
    
}
    mean(rbr)
}

readRenviron('~/git/ej/.Renviron')
bot <- TGBot$new(token = bot_token('CoinSight'))
bot$set_default_chat_id(282218584)
bot$sendMessage('aaa')

tmp = paste0(tempfile(),'.png')
png(tmp)
plot(res)
dev.off()
bot$sendDocument(tmp)
png(tmp)
plot(res2)
dev.off()
bot$sendDocument(tmp)
