library(doMC)
library(data.table)
library(xts)
registerDoMC(cores=7)

prep_data = function(filter2){
    #p = get(load('/home/aslepnev/git/ej/uniprc.RData'))
    #h = diff(log(na.locf(p)))  # save(h, file='/home/aslepnev/git/ej/unih.RData')
    u = as.data.table(get(load('/home/aslepnev/git/ej/uni.RData')))  # u$dt = as.Date("2018-03-01"); colnames(u)=gsub(' ','_', colnames(u)); save(u, file='/home/aslepnev/git/ej/uni.RData')
    h = get(load('/home/aslepnev/git/ej/unih.RData'))
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

# d0 = as.Date("2018-03-01"); d1=as.Date("2017-07-01")
prorate_u = function(u, h, d0, d1){
    u1 = u[dt==d0, ]
    u1$dt = d1
    ret = as.numeric(exp(colSums(h[as.Date(d0:d1), ]))) * if(d0<d1)1 else -1
    u1$MARKET_CAP = u[dt==d0, ][['MARKET_CAP']] * ret
    return(u1)
}

prorate_us = function(u, h, d0, d1s){
    return(rbindlist(foreach(d1=d1s)%do%prorate_u(u, h, d0, d1)))
}

# h_in=h[,1:50]; u_in=u[1:50,]; params=list(N=5)
screen_momentum = function(h_in, u_in, params){
    res = order(colSums(h_in), decreasing=TRUE)[1:params$N]
    return(res)
}

# r=res; params=vc_params
volcontrol = function(r, params){
    rsd = sqrt(250)*rollapply(r, params$window, FUN=sd)
    res = r * ifelse(is.na(rsd), 1, ifelse(rsd>params$level/params$max_weight, params$level/rsd, params$max_weight))
    return(res)
}

# u=U; h=D$h; screen_func=screen_momentum; screen_params=list(N=5); vc_params=list(window=20, level=0.05, max_weight=2); weights=array(1/screen_params$N, screen_params$N)
build_index = function(u, h, rebal_dates, screen_func, screen_params, vc_params, weights){
    calc_pieces = foreach(i=2:(length(rebal_dates)-1))%do%
        list(h = h[as.Date((rebal_dates[i-1]+1):rebal_dates[i]), ],
             h_next = h[as.Date((rebal_dates[i]+1):rebal_dates[i+1]), ],
             u = u[dt==rebal_dates[i],])
    
    res = foreach(x=calc_pieces, .combine=rbind)%dopar%{
        eidx = screen_func(x$h, x$u, screen_params)
        he = x$h_next[, eidx]
        r = xts(log(rowSums(as.matrix(exp(he) - 1)*as.numeric(weights)) + 1), order.by=index(he))
    }

    res = volcontrol(res, vc_params)
    res = exp(cumsum(res))  # plot(res)
    return(res)
}

