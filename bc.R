library(xts)
library(RJSONIO)
library(Rbitcoin)
library(foreach)
library(data.table)

tobo = function(x){
    y = fromJSON(x)
    r1 = data.frame(foreach(r=y,.combine=rbind)%do%if(!is.null(r$bidPrice) && !is.null(r$askPrice)) { c(gsub('T',' ',gsub('Z','',r$timestamp)),r$bidPrice,r$askPrice) }, stringsAsFactors=FALSE)
    names(r1) = c('ts','bid','offer')
    r1$ts = as.POSIXlt(r1$ts)
    r1 = as.xts(r1[,c('bid','offer')], order.by=r1$ts)
    r1$bid = as.numeric(r1$bid)
    r1$offer = as.numeric(r1$offer)
    storage.mode(r1) = 'numeric'
    r1
}

tof = function(x){
    y = fromJSON(x)
    r1 = data.frame(foreach(r=y,.combine=rbind)%do%if(!is.null(r$fundingRate)) { c(gsub('T',' ',gsub('Z','',r$timestamp)),r$fundingRate) }, stringsAsFactors=FALSE)
    names(r1) = c('ts','rate')
    r1$ts = as.POSIXlt(r1$ts)
    r1$rate = as.numeric(r1$rate)
    r1 = as.xts(r1$rate, order.by=r1$ts)
    storage.mode(r1) = 'numeric'
    r1
}

toc = function(x){
    y = fromJSON(x)
    r1 = data.frame(foreach(r=y,.combine=rbind)%do%if(!is.null(r$close)) { c(gsub('T',' ',gsub('Z','',r$timestamp)),r$close) }, stringsAsFactors=FALSE)
    names(r1) = c('ts','c')
    r1$ts = as.POSIXlt(r1$ts)
    r1$c = as.numeric(r1$c)
    r1 = as.xts(r1$c, order.by=r1$ts)
    storage.mode(r1) = 'numeric'
    r1
}

#url <- "https://min-api.cryptocompare.com/data/histoday?aggregate=1&e=BitTrex&extraParams=CryptoCompare&fsym=ETH&limit=20&tryConversion=false&tsym=BTC"

#https://min-api.cryptocompare.com/data/histohour?aggregate=1&e=BitTrex&extraParams=CryptoCompare&fsym=ETH&limit=500000&tryConversion=false&tsym=BTC&toTs=1505563200

#url1 = "https://www.bitmex.com/api/v1/quote?count=500&start=1&reverse=false&symbol=XBTZ17"
#url2 = "https://www.bitmex.com/api/v1/quote?count=500&start=501&reverse=false&symbol=XBTZ17"

rf = foreach(i=0:240,.combine=rbind)%do%{ Sys.sleep(2);
    tobo(paste0("https://www.bitmex.com/api/v1/quote/bucketed?binSize=1m&partial=false&count=500&reverse=false&start=",10000 + i*500 + 1,"&symbol=XBTZ17")) }
rs = foreach(i=0:240,.combine=rbind)%do%{ Sys.sleep(2);
    tobo(paste0("https://www.bitmex.com/api/v1/quote/bucketed?binSize=1m&partial=false&count=500&reverse=false&start=",1045000 + i*500 + 1,"&symbol=XBTUSD")) }
ri = foreach(i=0:240,.combine=rbind)%do%{ Sys.sleep(2);
    toc(paste0("https://www.bitmex.com/api/v1/trade/bucketed?binSize=1m&partial=false&start=",275000 + i*500 + 1,"&count=500&reverse=false&symbol=.BXBT")) }
rfu = foreach(i=0:6,.combine=rbind)%do%{ Sys.sleep(1); tof(paste0("https://www.bitmex.com/api/v1/funding?start=",i*500 + 1,"&count=500&reverse=false")) }

rr = merge.xts(rs,rf,ri,rfu)
names(rr) = c('sb','so','fb','fo','i','u')
rr = rr[0 == rowSums(is.na(rr[,1:5]))]
rr = na.locf(rr)
rr = rr[0 == rowSums(is.na(rr))]

rx = merge.xts(lag(100*(rr$fb-rr$so)/rr$so,480),rr$u)

rx = merge.xts(lag(100*(rr$so-rr$i)/rr$so,480),rr$u)
rx = rx[0 == rowSums(is.na(rx))]
cor(rx)

y = as.numeric((rr$fb/rr$so - 1)[481:(nrow(rr)-480)])

fcor = function(n, low_factor){
    dcyn = (low_factor^(1:n/n))/sum(low_factor^(1:n/n))
    x = as.numeric(rollapply((rr$so/rr$i - 1),n,function(x) { sum(x*dcyn) })[(n+1):(nrow(rr)-480)])
    z = as.numeric(rr$u[(n+481):nrow(rr)])
    cor(x,z)
}

fcor(960,0.5)

cor(x,z)

mean()

plot(100*(rr$fb/rr$so-1))

setwd('~/R/ej')
rr = get(load('rr.RData'))


ct = 100*(rr$fb/rr$so-1)
dct = ct-lag(ct,60); ct = lag(ct,60); dct = dct[!is.na(dct)]; ct = ct[index(dct)];
cor(dct,ct)

hist(dct[abs(dct)<3],br=200)
hist(dct[ct>1],br=100)

plot(ct)
abline(h=0)

r1 = rr$fb - lag(rr$fb,60)
r2 = lag(ct,60); r1=r1[!is.na(r1)]; r2=r2[index(r1)]
cor(r1,r2)

save(rr,file='rr.RData')

bs1 <- fromJSON(url1) # returns a list
bs2 <- fromJSON(url2) # returns a list


r2 = data.table(foreach(r=bs2,.combine=rbind)%do%t(r))


bs_df <- do.call(rbind,lapply(bs_data,data.frame,stringsAsFactors=FALSE))
head(bs_df)
