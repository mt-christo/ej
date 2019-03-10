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

pridex_metric = function(time_line, perf_in, w){  # perf_in is already EXPONENTIAL!!!!!
    return(cor(time_line, log(((perf_in-1)%*%w) + 1)))
}

optim_pridex_metric = function(time_line, perf_in, w, wcaps_low, wcaps_high){
    gradus = function(x){
        return(-pridex_metric(time_line, perf_in, x))
    }
    res = cobyla(x0=w, fn=gradus, lower=wcaps_low, upper=wcaps_high, hin=function(w){ -abs(1-sum(w)) }, control=COB_CTL)

    return(list(weights=res$par, metric=-res$value))
}

optim_sigma = function(h_in, volparams){
    comat = cov(h_in)
    n = ncol(h_in)
    gradus = function(x) return(abs(volparams$target - sqrt(252)*sqrt(x %*% comat %*% x)))
    res = cobyla(x0=array(1/n, n), fn=gradus, lower=array(volparams$wmin, n), upper=array(volparams$wmax, n), hin=function(w){ -abs(1-sum(w)) }, control=COB_CTL)
    return(res$par)
}

# d_in=d_stock1; n_in=2; volparams=list(wnd=500, min=0.2, max=0.3)
baskets_vol_range = function(d_in, n_in, volparams){
    nn = t(combn(1:nrow(d_in$u), n_in))
    mcap_nn = nn
    ticker_nn = nn
    for(i in 1:ncol(nn)){
        mcap_nn[, i] = d_in$u$mcap[nn[, i]]
        ticker_nn[, i] = d_in$u$ticker[nn[, i]]
    }
    
#    nn = t(combn(d_in$u$ticker, n_in))
    h = tail(d_in$h, volparams$wnd)
    w = array(1/n_in, n_in)
    comat = cov(h)
    sds = array(0, nrow(nn))
    mcaps = array(0, nrow(nn))
    for(j in 1:nrow(nn)){
        sds[j] = w %*% comat[nn[j, ], nn[j, ]] %*% w
        mcaps[j] = min(mcap_nn[j, ])
    }
    sds = sqrt(250)*sqrt(sds)
    baskets = ticker_nn[sds>=volparams$min & sds<=volparams$max, ]
    sds = sds[sds>=volparams$min & sds<=volparams$max]
    return(list(baskets=baskets, sds=sds, mcaps=mcaps))
}

# h_in = d$h; wmin=0.1; wmax=0.6; do_optimize=TRUE
# baskets=baskets_vol_range(d_etf, 3, volparams=list(wnd=500, min=0.2, max=0.3)); h_in=d_stock$h
best_pridex_basket = function(baskets, h_in, wmin=0, wmax=0, do_optimize=FALSE){  # h_in is supposed to be already time-constrained!
    n = ncol(baskets)
    w = array(1/n, n)  # equal weights
    perf = exp(cumsum(h_in))
    time_line = 1:nrow(perf)
    if(do_optimize){
        rnk = foreach(j=1:nrow(baskets))%do%optim_pridex_metric(time_line, perf[, baskets[j, ]], w, array(wmin, n), array(wmax, n))
        best_idx = which.max(unlist(lapply(rnk, '[[', 'metric')))
        res = list(basket=baskets[best_idx, ], weights=rnk[[best_idx]]$weights)
    } else {
        rnk = foreach(j=1:nrow(baskets))%do%pridex_metric(time_line, perf[, baskets[j, ]], w)
        best_idx = which.max(unlist(rnk))
        res = list(basket=baskets[best_idx, ], weights=w)
    }
    return(res)
}

# lh_in=lh_in['etfs']; u_in=u1_in['etfs']; pick_count=params$N
pridex_screen_prep = function(lh_in, u_in, pick_count){  # screening by proiex metric TODO refactor to single function
    r = lh_in[,colSums(abs(lh_in))!=0]
    perf = cumsum(r)
    uu = u_in[ticker%in%colnames(perf), ]
    time_line = 1:nrow(perf)
    metr = foreach(i=1:ncol(perf),.combine=c)%do%cor(time_line, perf[,i])
    uni = colnames(perf)[order(metr, decreasing=TRUE)[1:min(pick_count, nrow(uu))]]
    return(list(uni=uni, r=r, perf=perf, r_uni=r[, uni], perf_uni=perf[, uni], metr=metr, time_line=time_line))
}

# lh_in=lh_in[['main']]; u_in=u_in[['main']]; pick_count=params[['main']]$UNI; vtarget=params$voltarget; force_us=FALSE
sigma_screen_prep = function(lh_in, u_in, pick_count, vtarget, force_us){  # screening by volatility
    u = u_in[colSums(abs(lh_in))!=0, ]
    if(force_us)
        u = rbind(u[country!='US', ], u_in[country=='US', ][1:sum(u$country!='US'), ])  # hardcoded enforced 50% US companies!
    r = lh_in[, u$ticker]
    perf = cumsum(r)
    u = u[ticker%in%colnames(perf), ]

    perf_tail = as.numeric(tail(perf, 1))  # perf_tail[match(uni, colnames(perf))]
    uni = colnames(perf)
    uni = uni[order(perf_tail, decreasing=TRUE)[1:min(pick_count*2, length(uni))]]
    sds = foreach(i=uni,.combine=c)%do%(sd(r[,i])*sqrt(252))
    uni = uni[order(abs(sds - vtarget), decreasing=FALSE)[1:min(pick_count, length(uni))]]
    return(list(uni=uni, r=r, perf=perf, r_uni=r[, uni], perf_uni=perf[, uni]))
}

pridex_rank_baskets = function(prep_in, bsize){
    nn = t(combn(prep_in$uni, bsize))  # all N-baskets from universe
    w = array(1/bsize, bsize)  # equal weights
    rnk = foreach(j=1:nrow(nn),.combine=c)%do%pridex_metric(prep_in$time_line, prep_in$perf_uni[, nn[j, ]], w)  # calc metric for every basket
    return(nn[order(rnk, decreasing=TRUE), ])  # return baskets in the highest-to-lowest metric
}

# lh_in=x$lh; u_in=x$u; params=screen_params; lh_key='main'
screen_pridex_equalweight = function(lh_in, u_in, params, lh_key='main'){
    prep = pridex_screen_prep(lh_in[[lh_key]], u_in[[lh_key]], params[[lh_key]]$UNI)  # precal data
    n = params[[lh_key]]$N
    baskets = pridex_rank_baskets(prep, n)
    return(list(names=baskets[1, ], weights=array(1/n, n), prep=prep))
}

# lh_in=x$lh; u_in=x$u; params = screen_params
# lh_in=x$lh; u_in=x$u; params=list(voltarget=0.3, minw=0.02, maxw=0.8, etfs=list(N=3, UNI=20, window=40), stocks=list(UNI=10, window=40))
screen_voltarget = function(lh_in, u_in, params){
    prep = sigma_screen_prep(lh_in[['main']], u_in[['main']], params[['main']]$UNI, params$voltarget, params$force_us)
    perf <- prep$perf_uni; colnames(perf) = 1:ncol(perf)
    r <- prep$r_uni; colnames(r) = 1:ncol(r)

    # w = array(1/n, n)
    # w = res$par
    perf_tail = as.numeric(tail(perf,1))
    gradus = function(w){
        return(abs(params$voltarget - basket_vol(r, w))*10000 - sum(perf_tail*w))
    }
    n = ncol(perf)
    res = cobyla(x0=array(1/n, n), fn=gradus, lower=array(params$minw, n), upper=array(params$maxw, n), hin=function(w){ -abs(1-sum(w)) }, control=COB_CTL)

    return(list(main = list(names=prep$uni, weights=res$par[1:ncol(prep$r_uni)])))
}

screen_pridex_voltarget_stocksetfs = function(lh_in, u_in, params){
    e = screen_pridex_equalweight(lh_in, u_in, params, 'etfs')
    prep = pridex_screen_prep(lh_in[['stocks']], u_in[['stocks']], params[['stocks']]$UNI)
    perf <- cbind(prep$perf_uni, e$prep$perf_uni[, e$names]); colnames(perf) = 1:ncol(perf)
    r <- cbind(prep$r_uni, e$prep$r_uni[, e$names]); colnames(r) = 1:ncol(r)

    # w = array(1/n, n)
    # w = res$par
    gradus = function(w){
        return(abs(params$voltarget - basket_vol(r, w)) + abs(1 - pridex_metric(prep$time_line, perf, w)))
    }
    n = ncol(perf)
    res = cobyla(x0=array(1/n, n), fn=gradus, lower=array(params$minw, n), upper=array(params$maxw, n), hin=function(w){ -abs(1-sum(w)) }, control=COB_CTL)

    return(list(stocks = list(names=prep$uni, weights=res$par[1:ncol(prep$r_uni)]),
                etfs = list(names=e$names, weights=res$par[-(1:ncol(prep$r_uni))])))
}

# u_in=x$u[['main']]; dt_in=x$dt; lh_in=x$lh[['main']]
screen_mixed_top = function(u_in, dt_in, lh_in){
    n = 10
    dt1 = u_in[dt <= dt_in, max(dt)]
    dt2 = u_in[dt >= dt_in, min(dt)]
    r = rowSums(t(lh_in))
    rets = data.table(ticker=names(r), r)
    u1 = u_in[dt == dt1, .(ticker, mcap1=mcap)]
    u2 = u_in[dt == dt2, .(ticker, mcap2=mcap)]
    res = rets[u2[u1, on='ticker'], on='ticker'][, mcap:=(as.numeric(dt_in - dt1)*mcap2 + as.numeric(dt2 - dt_in)*mcap1)/as.numeric(dt2 - dt1)]
    resnorm = function(x) { y = x[!is.na(x)]; (x - min(y))/(max(y) - min(y)) }
#    res = u2[u1, on='ticker'][, mcap:=(as.numeric(dt_in - dt1)*mcap2 + as.numeric(dt2 - dt_in)*mcap1)/as.numeric(dt2 - dt1)]
    mres = resnorm(res$mcap) + 0.5*resnorm(res$r)
    res = res[order(mres, decreasing=TRUE), ][1:n, ]
#    w = res$r - min(res$r)
#    w = w/sum(w)
    w = array(1/n, n)
    return(list(main=list(names=res$ticker, weights=w)))
}

