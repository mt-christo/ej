# lh_in=x$lh; u_in=x$u; params=screen_params
screen_momentum = function(lh_in, u_in, params){
    res = order(colSums(lh_in), decreasing=TRUE)[1:params$N]
    return(u_in[res, ticker])
}

# h_in=x; screen_params=list(w=0.01)
screen_partial_momentum = function(h_in, screen_params){
    w = screen_params$w
    res = (1 - w) * array(1/ncol(h_in), ncol(h_in)) + w * norm_weights(colSums(h_in))
    res = res/sum(res)
    return( list(main = list(names=colnames(h_in), weights=res)) )
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

# h_in=tail(hcom, 500); volparams=list(target=vt, wmin=wmin, wmax=wmax)
optim_sigma = function(h_in, volparams){
    comat = cov(h_in[rowSums(is.na(h_in))==0, ])
    n = ncol(h_in)
    gradus = function(x) return(abs(volparams$target - sqrt(252)*sqrt(x %*% comat %*% x)))
    res = cobyla(x0=array(1/n, n), fn=gradus, lower=array(volparams$wmin, n), upper=array(volparams$wmax, n), hin=function(w){ -abs(1-sum(w)) }, control=COB_CTL)
    return(res$par)
}

# d_in=d_stock1; n_in=2; volparams=list(wnd=500, min=0.2, max=0.3)
# d_in=d_etf; n_in=n_etfs; volparams=list(wnd=250, min=vt-0.3, max=vt+0.1)
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
    pfs = array(0, nrow(nn))
    for(j in 1:nrow(nn)){
        sds[j] = w %*% comat[nn[j, ], nn[j, ]] %*% w
        mcaps[j] = min(mcap_nn[j, ])
        pfs[j] = as.numeric(tail(basket_perf(h[, nn[j, ]], w), 1))
    }
    sds = sqrt(250)*sqrt(sds)
    baskets = ticker_nn[sds>=volparams$min & sds<=volparams$max, ]
    sds = sds[sds>=volparams$min & sds<=volparams$max]
    mcaps = mcaps[sds>=volparams$min & sds<=volparams$max]
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
# metrics_in=u$equity_metrics; dt_in=as.Date('2016-05-15'); h_in=u[['h']]; screen_params=list(perf_weight=0.5, top_n=10, price_window=20)
screen_mixed_top = function(metrics_in, h_in, dt_in, screen_params){
    dt1 = metrics_in[dt <= dt_in, max(dt)]
    dt2 = metrics_in[dt >= dt_in, min(dt)]
    u1 = metrics_in[dt == dt1, .(ticker, mcap1=mcap)]
    u2 = metrics_in[dt == dt2, .(ticker, mcap2=mcap)]
    
    r = rowSums(t(h_in))
    r = data.table(ticker=names(r), r)
    r = r[u2[u1, on='ticker'], on='ticker'][, mcap:=(as.numeric(dt_in - dt1)*mcap2 + as.numeric(dt2 - dt_in)*mcap1)/as.numeric(dt2 - dt1)]
    r$mcap = ifelse(!is.na(r$mcap), r$mcap, r$mcap1)
#    r = r[metrics_in[dt == dt0, .(ticker, mcap)], on='ticker']
    
    rtng = norm_weights(r$mcap) + screen_params$perf_weight*norm_weights(r$r)  # the rating
    tickers = r[order(rtng, decreasing=TRUE), ticker][1:screen_params$top_n]
#    w = res$r - min(res$r)
#    w = w/sum(w)
    weights = array(1/screen_params$top_n, screen_params$top_n)

    return(list(main=list(names=tickers, weights=weights)))
}

# h_in=x$h; 
smidai_style_rebal = function(h_in, screen_params){
    good_tickers = colnames(h_in)[colSums(h_in)!=0]
    f1 = screen_params$funds[ticker%in%good_tickers, ]  # Only existing tickers on that date
    f1 = screen_params$baskets[, .(basket_id=id, basket_weight=weight)][f1, on='basket_id'][, .(ticker, weight=weight*basket_weight)]  # Join Baskets to get basket weights
    f1 = f1[, .(weight=sum(weight)), by=ticker]  # Aggregate by ticker (tickers may intersect among baskets)
    good_tickers = good_tickers[good_tickers%in%f1$ticker]

    y = h_in[, good_tickers]  # Our main history of returns now
    n = ncol(y)

    comat = cov(y)*252
    wmax = f1[good_tickers, on='ticker']$weight
    wmin = array(0, n)
    wstart = wmax/sum(wmax)
    wlimit = function(x){ -abs(1-sum(x)) }
    vt = screen_params$voltarget

    func_sigma = function(x) return( abs(vt - sqrt(x %*% comat %*% x)) )
    wsigma = cobyla(x0=wstart, fn=func_sigma, lower=wmin, upper=wmax, hin=wlimit, control=COB_CTL)$par

    wlimit2 = function(x) return( round(c(1-sum(x), vt - sqrt(x %*% comat %*% x), -1+sum(x), -vt + sqrt(x %*% comat %*% x)), 4) )
    gradus = function(x) return( -sum(basket_ret(y, x)) )
    res = cobyla(x0=wsigma, fn=gradus, lower=wmin, upper=wmax, hin=wlimit2, control=COB_CTL)
    w = res$par/sum(res$par)
    
    return( list(main = list(names=colnames(y), weights=w)) )
}
