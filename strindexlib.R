library(doMC)
library(data.table)
library(xts)
registerDoMC(cores=2)

enrich_data = function(fdata_path, hdata_path){
    #u = get(load('uni.RData')); u$dt = as.Date("2018-03-01"); colnames(u)=gsub(' ','_', colnames(u)); save(u, file='uni.RData')
    #p = get(load('uniprc.RData'))
    #h = diff(log(na.locf(p)))  # save(h, file='unih.RData')
    u = as.data.table(get(load('uni.RData'))) 
    h = get(load('unih.RData'))
    u = u[1:ncol(h), ]
    if(filter2){
        mx = foreach(i=1:ncol(h),.combine=c)%do%{x=h[,i]; max(x[!is.na(x)])}
        mn = foreach(i=1:ncol(h),.combine=c)%do%{x=h[,i]; min(x[!is.na(x)])}
        u = u[mn>-0.69 & mx<0.69, ]
        h = h[, mn>-0.69 & mx<0.69]
    }
    h90 = tail(h, 90)
    u$HSIGMA = foreach(i=1:ncol(h),.combine='c')%do%{ sd(h90[,i])*250/90 }
    return(list(u=u, h=h))
}

# u=D$u; h=D$h; d0=as.Date("2018-12-17"); d1=as.Date('2018-07-01')
# d0 = as.Date("2018-03-01"); d1=as.Date("2017-07-01")
prorate_universe = function(u, h, d0, d1){
    u1 = u[dt==d0, ]
    u1$dt = d1
    ret = as.numeric(exp(colSums(h[as.Date(d0:d1), ]))) * if(d0<d1)1 else -1
    u1$mcap = u[dt==d0, ][['mcap']] * ret
    return(u1)
}

# u=D$u; h=D$h; d0=as.Date("2018-03-01"); d1s=rebal_dates
prorate_universe_multiple = function(u, h, d0, d1s){
    return(rbindlist(foreach(d1=d1s)%do%prorate_universe(u, h, d0, d1)))
}

# lh_in=x$lh; u_in=x$u; params=screen_params
screen_momentum = function(lh_in, u_in, params){
    res = order(colSums(lh_in), decreasing=TRUE)[1:params$N]
    return(u_in[res, ticker])
}
screen_mycorr1 = function(lh_in, u_in, params){
    hh = na.fill(lh_in, fill=0.0)
    hh = cumsum(hh[,colSums(abs(hh))!=0])
    hhcor = foreach(i=1:ncol(hh),.combine=c)%do%cor(1:nrow(hh), hh[,i])
    res = colnames(hh)[order(hhcor, decreasing=TRUE)[1:params$N]]
    return(res)
}
screen_mycorr2 = function(lh_in, u_in, params){
    hh = na.fill(lh_in, fill=0.0)
    hh = cumsum(hh[,colSums(abs(hh))!=0])
    time_line = 1:nrow(hh)
    hhcor = foreach(i=1:ncol(hh),.combine=c)%do%cor(time_line, hh[,i])
    nn = t(combn(colnames(hh)[order(hhcor, decreasing=TRUE)[1:params$UNI]], params$N))
    rnk = foreach(j=1:nrow(nn),.combine=c)%do%cor(time_line, rowSums(hh[,nn[j,]]))
    return(nn[which.max(rnk),])
}

# r=h_res; params=vc_params
volcontrol = function(r, params){
    w = sqrt(250)*rollapply(r, params$window, FUN=sd)
    w[,1] = ifelse(is.na(w), 1, params$level/w)
    w[,1] = lag(ifelse(w > params$max_weight, params$max_weight, w), 1)
    return(log((exp(r) - 1)*w + 1)[-1,])
}

# u=D$u; h=D$h; screen_func=screen_mycorr; screen_params=list(N=3); vc_params=list(window=20, level=0.085, max_weight=2.5); weights=array(1/screen_params$N, screen_params$N)
# u=D$u[1:100,]; h=D$h[,u$ticker]; screen_func=screen_mycorr2; screen_params=list(N=4, UNI=20, window=60); vc_params=list(window=20, level=0.085, max_weight=2.5); weights=array(1/screen_params$N, screen_params$N)
build_index = function(u, h, rebal_dates, screen_func, screen_params, vc_params, weights){
    lh = na.fill(diff(log(1+na.locf(h))), 0)
    for(i in 1:ncol(lh))
        lh[abs(lh[,i])>0.5, i] = 0
    
    rebal_idx = match(rebal_dates[rebal_dates>='2008-09-30'], index(lh))
    calc_pieces = foreach(i=1:(length(rebal_idx)-1))%do%
        list(dt = index(lh)[rebal_idx[i]],
             lh = lh[(rebal_idx[i]-screen_params$window):rebal_idx[i], ],
             lh_next = lh[(rebal_idx[i]+1):rebal_idx[i+1], ],
#             u = u[dt==rebal_dates[i],])
             u = u)
    
    h_res = foreach(x=calc_pieces, .combine=rbind)%dopar%{
        print(x$dt)
        tickers = screen_func(x$lh, x$u, screen_params)
        he = x$lh_next[, tickers]
        r = xts(log(rowSums(as.matrix(exp(he) - 1)*as.numeric(weights)) + 1), order.by=index(he))
    }

    res = exp(cumsum(volcontrol(h_res, vc_params)))
    plot(res)
    
    return(res)
}

