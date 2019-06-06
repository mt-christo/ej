# D_in=pre_screen(D, d); rebal_dates=get_rebals(D, 'month'); screen_func=screen_mycorr2; 
# D_in=pre_screen(D, dd[[i]]); rebal_dates=get_rebals(D, 'month'); screen_func=screen_mycorr2; 
# D_in=list(main=pre_screen(D_STOCKS, d_stocks)); rebal_dates=get_rebals(D_STOCKS, 'month'); screen_func=screen_voltarget
build_index = function(D_in, rebal_dates, screen_func, screen_params, start_date){
    lh = list()
    for(i in names(D_in)){
        lh[[i]] = na.fill(diff(log(1+na.locf(D_in[[i]]$h))), 0)
        for(j in 1:ncol(lh[[i]]))
            lh[[i]][abs(lh[[i]][,j])>0.5, j] = 0
    }

    for(i in 1:length(lh))
        for(j in 1:length(lh))
            lh[[i]] = lh[[i]][index(lh[[i]])%in%index(lh[[j]])]
    
    rebal_idx = match(rebal_dates[rebal_dates>=start_date], index(lh[[1]]))  # indices of history in all elements iof lh are expected the same!
    calc_pieces = foreach(i=1:(length(rebal_idx) - 1))%do%{
        s = list(); s_next = list(); s_u = list()
        for(j in names(lh)){
            s[[j]] = lh[[j]][(rebal_idx[i] - screen_params$window):rebal_idx[i], ]
            s_next[[j]] = lh[[j]][(rebal_idx[i]+1):rebal_idx[i+1], ]
            s_u[[j]] = D_in[[j]]$u
        
        }
        list(dt = index(lh[[1]])[rebal_idx[i]], lh=s, lh_next=s_next, u=s_u)
    }

    # x = calc_pieces[[100]]
    h_res = foreach(x=calc_pieces)%dopar%{
#        print(paste(screen_params$N, screen_params$UNI, x$dt, sep=', '))
        print(x$dt)
        basket = screen_func(x$lh, x$u, screen_params)
        h = foreach(i=names(basket),.combine=cbind)%do%x$lh_next[[i]][, basket[[i]]$names]
        w = foreach(i=names(basket),.combine=c)%do%basket[[i]]$weights
        r = basket_ret(h, w) 
        list(h=r, basket=basket, dt=x$dt)
    }

    
#    res_vc = exp(cumsum(volcontrol(h_res, vc_params)))
#    res = exp(cumsum(h_res))
#    print(paste0('normal: ', round(tail(res, 1),2), ', volcontrolled: ', round(tail(res_vc, 1), 2)))
#    plot(res, cex=2, cex.main=2)
    
    return(h_res)
}

# u_in=u; rebal_freq='month'; screen_func=screen_mixed_top; screen_params=list(perf_weight=5.5, top_n=20, price_window=125); start_date='2012-11-29'
build_index_simpler = function(u_in, rebal_freq, screen_func, screen_params, start_date){
    h_in = u_in$h
    rebal_dates = get_rebals_h(h_in[index(h_in) >= start_date], rebal_freq)
    rebal_idx = match(rebal_dates[rebal_dates >= start_date], index(h_in))
    price_window = as.numeric(screen_params$price_window)
    
    calc_pieces = foreach(i=1:(length(rebal_idx) - 1))%do%
        list(dt = index(h_in)[rebal_idx[i]],
             h = h_in[(rebal_idx[i] - screen_params$price_window + 1):rebal_idx[i], ],
             h_next = h_in[(rebal_idx[i]+1):rebal_idx[i+1], ])

    # x = calc_pieces[[1]]
    h_res = foreach(x=calc_pieces)%dopar%{
        print(x$dt)
        basket = screen_func(u_in[['equity_metrics']], x$h, x$dt, screen_params)
        h = foreach(i=names(basket),.combine=cbind)%do%x$h_next[, basket[[i]]$names]
        w = foreach(i=names(basket),.combine=c)%do%basket[[i]]$weights
        r = basket_ret(h, w) 
        list(h=r, basket=basket, dt=x$dt)
    }

    return(h_res)
}

# h_in = shmae; rebal_dates=get_rebals_h(h_in, 'quarter'); screen_func=screen_partial_momentum; screen_params=list(window=40, w=0.1); start_date='2016-06-30'
# h_in = shmae; rebal_dates=get_rebals_h(h_in, 'month'); screen_func=screen_partial_momentum; screen_params=list(window=20, w=0.0); start_date='2016-06-30'
# h_in = h; rebal_dates=get_rebals_h(h_in, 'month'); screen_func=smidai_style_rebal; screen_params=list(funds=f, baskets=b, window=wnd, voltarget=0.08); start_date='2013-12-31'
# h_in = h; rebal_dates=get_rebals_h(h_in, 'month'); screen_func=smidai_style_rebal; start_date='2013-12-31'
build_index_simple = function(h_in, rebal_dates, screen_func, screen_params, start_date, vc_params){
    rebal_idx = match(rebal_dates[rebal_dates>=start_date], index(h_in))
    calc_pieces = foreach(i=1:(length(rebal_idx) - 1))%do%
        list(dt = index(h_in)[rebal_idx[i]],
             h = h_in[(rebal_idx[i] - screen_params$window + 1):rebal_idx[i], ],
             h_next = h_in[(rebal_idx[i]+1):rebal_idx[i+1], ])

    # x = calc_pieces[[1]]
    h_res = foreach(x=calc_pieces)%dopar%{
        print(x$dt)
        basket = screen_func(x$h, screen_params)
        h = foreach(i=names(basket),.combine=cbind)%do%x$h_next[, basket[[i]]$names]
        w = foreach(i=names(basket),.combine=c)%do%basket[[i]]$weights
        r = basket_ret_rebal(h, w)
#        r = basket_ret(h, w)

        # Per Romain's request - calculating volatility of actual basket proforma
        h_both = rbind(x$h, x$h_next)
        r_sd = foreach(tmp_wnd = vc_params$window)%do%{  # tmp_wnd = vc_params$window[1]
            sd1 = rollapplyr(basket_ret_rebal(h_both, w), as.numeric(tmp_wnd), FUN=sd)
            sd1[!is.na(sd1)][index(h)]
        }
        
        list(h=r, r_sd=r_sd, basket=basket, dt=x$dt)
    }
    
    return(h_res)
}
