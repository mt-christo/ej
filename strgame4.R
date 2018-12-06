source('strindexlib.R')

D = prep_data(TRUE)
H = D$h; U = D$u
rebal_dates = as.Date(index(apply.yearly(H[,1],FUN=length)))
U = prorate_universe_multiple(U, H, as.Date("2018-03-01"), rebal_dates)
# u=U; h=D$h; screen_func=screen_momentum; screen_params=list(N=5); vc_params=list(window=20, level=0.05, max_weight=2); weights=array(1/screen_params$N, screen_params$N)
IDX = build_index(U, H, rebal_dates, screen_func=screen_momentum, screen_params=list(N=5), vc_params=list(window=20, level=0.05, max_weight=2), weights=array(0.2,5))
plot(IDX)





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



for(i in 1:100000){
    print(i)
    sectors1 = sample(sectors, n_sectors)
    idx = c()
    for(j in sectors1) idx=c(idx, sample(which(u1$SECTOR==j), n_stocks))
    h1 = h[, idx]
    h1 = exp(tail(h1[rowSums(!is.na(h1)) == ncol(h1),], 500))-1
    if(nrow(h1) == 500){
        h1 = exp(cumsum(log(1 + rowSums(h1)/ncol(h1))))
        x[[length(x)+1]] = list(sectors=sectors1, tickers=idx, sigma=sd(diff(log(h1)))*sqrt(250), cr=cor(1:length(h1),h1))
    }
}

sigmas=c(); crs=c()
for(i in 1:length(x)) {
    sigmas[i]=x[[i]]$sigma
    crs[i]=x[[i]]$cr
}
y = data.table(sigma=sigmas, cr=crs)
#plot(y[cr>0.98,])
idx = x[[which(y$sigma==y[cr>0.98, min(sigma)])]]$tickers
h1 = h[, idx]
h1 = exp(tail(h1[rowSums(!is.na(h1)) == ncol(h1),], 500))-1
h1 = exp(cumsum(log(1 + rowSums(h1)/ncol(h1))))
plot(h1)


