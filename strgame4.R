





a


a = paste(readLines('https://finance.yahoo.com/screener/unsaved/8942a94a-1fe1-422d-9199-6549e327eb0c?offset=0&count=250'),collapse='')
delim = '"></path></svg></label><a href="/quote/'
tidx = gregexpr(str_replace_all(delim,"(\\W)", "\\\\\\1"), a, perl=T)[[1]]
i = tidx[1]

tickers = foreach(i=tidx,.combine=c)%do%{ s=substr(a,i+nchar(delim),i+nchar(delim)+50)[[1]]; substr(s,1, gregexpr('\\?',s)[[1]][1]-1) }

rank_idx=500:1500; n_sectors=5; n_stocks=2;
reval_idx = index(apply.quarterly(h[,1],FUN=length))
u1 = u[rank_idx,]
h1 = h[,rank_idx]
hidx = index(h1)
sectors = unique(u1$SECTOR)
sector_idx = foreach(s=sectors)%do%which(u1$SECTOR==s)
sectors_range = 1:length(sectors)

baskets = list()
for(i in tail(1:length(reval_idx),30)){
    x = list()
    for(j in 1:50000){
        if(j%%1000==0) print(j)
        sectors1 = sample(sectors_range, n_sectors)
        idx = c()
        for(k in sectors1) idx=c(idx, sample(sector_idx[[k]], n_stocks))
        h2 = h1[hidx[hidx>reval_idx[i-1] & hidx<reval_idx[i]], idx]
        h2 = exp(h2[rowSums(!is.na(h2)) == ncol(h2),])-1
        if(nrow(h2) > 30){
            h2 = exp(cumsum(log(1 + rowSums(h2)/ncol(h2))))
            x[[length(x)+1]] = list(sectors=sectors1, tickers=idx, sigma=sd(diff(log(h2)))*sqrt(250), cr=cor(1:length(h2),h2))
        }
    }

    sigmas=c(); crs=c()
    for(j in 1:length(x)) {
        sigmas[j]=x[[j]]$sigma
        crs[j]=x[[j]]$cr
    }
    y = data.table(sigma=sigmas, cr=crs)

    crq = as.numeric(quantile(y$cr,0.995))
    baskets[[length(baskets)+1]] = x[[which(y$sigma==y[cr>crq, min(sigma)])]]
}

reval_i = tail(1:length(reval_idx),30)
h3 = foreach(i=1:29,.combine=rbind)%do%{
    idx = baskets[[i]]$tickers
    h2 = h1[hidx[hidx>reval_idx[reval_i[i]] & hidx<reval_idx[reval_i[i+1]]], idx]
    h2 = exp(h2[rowSums(!is.na(h2)) == ncol(h2),])-1
}

h3r = xts(log(1 + rowSums(h3)/ncol(h3)), order.by=index(h3))
sd(h3r)*sqrt(250)

h31r = apply.monthly(xts(log(1 + rowSums(h3)/ncol(h3)), order.by=index(h3)),FUN=sum)
sd(h31r)*sqrt(250/20)


h4 = xts(exp(cumsum(log(1 + rowSums(h3)/ncol(h3)))), order.by=index(h3))
as.numeric(tail(h4,1))^(360/as.numeric(max(index(h4))-min(index(h4))))

plot(h4)
save(baskets, file='/home/aslepnev/git/ej/bask1.RData')



