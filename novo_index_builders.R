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

# D_in=list(main=D); rebal_dates=get_rebals(D, 'quarter'); screen_func=screen_voltarget; start_date='2012-12-31'
# D_in=list(main=DD); rebal_dates=get_rebals(DD, 'quarter'); screen_func=screen_mixed_top; start_date='2012-12-31'; screen_params = list(price_window=40)
# D_in=list(main=u); rebal_dates=get_rebals(u, 'quarter'); screen_func=get(params$screen_params$func); screen_params=params$screen_params; start_date=params$index_start
build_index_prorate = function(D_in, rebal_dates, screen_func, screen_params, start_date){
    lh = list()
    for(i in names(D_in)){
        lh[[i]] = na.fill(diff(log(1+na.locf(D_in[[i]]$h))), 0)
        for(j in 1:ncol(lh[[i]]))
            lh[[i]][abs(lh[[i]][,j])>0.5, j] = 0
    }

    for(i in 1:length(lh))
        for(j in 1:length(lh))
            lh[[i]] = lh[[i]][index(lh[[i]])%in%index(lh[[j]])]
    
    rebal_idx = match(rebal_dates[rebal_dates>=start_date], index(lh[[1]]))  # indices of history in all elements of lh are expected the same!
    price_window = as.numeric(screen_params$price_window)
    calc_pieces = foreach(i=1:(length(rebal_idx) - 1))%do%{
        s = list(); s_next = list(); s_u = list()
        for(j in names(lh)){
            s[[j]] = lh[[j]][(rebal_idx[i] - price_window):rebal_idx[i], ]
            s_next[[j]] = lh[[j]][rebal_idx[i]:(rebal_idx[i+1]-1), ]
            s_u[[j]] = D_in[[j]]$u        
        }
        list(dt = index(lh[[1]])[rebal_idx[i]], lh=s, lh_next=s_next, u=s_u)
    }

    # x = calc_pieces[[1]]
    h_res = foreach(x=calc_pieces)%dopar%{
#        print(paste(screen_params$N, screen_params$UNI, x$dt, sep=', '))
        print(x$dt)
        basket = screen_func(x$u[['main']], x$dt, x$lh[['main']])
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

# h_in = shmae; rebal_dates=get_rebals_h(h_in, 'quarter'); screen_func=screen_partial_momentum; screen_params=list(window=40, w=0.1); start_date='2016-06-30'
# h_in = shmae; rebal_dates=get_rebals_h(h_in, 'month'); screen_func=screen_partial_momentum; screen_params=list(window=20, w=0.0); start_date='2016-06-30'
# h_in = h; rebal_dates=get_rebals_h(h_in, 'month'); screen_func=smidai_style_rebal; screen_params=list(funds=f, baskets=b, window=20); start_date='2006-12-29'
build_index_simple = function(h_in, rebal_dates, screen_func, screen_params, start_date){
    rebal_idx = match(rebal_dates[rebal_dates>=start_date], index(h_in))
    calc_pieces = foreach(i=1:(length(rebal_idx) - 1))%do%
        list(dt = index(h_in)[rebal_idx[i]],
             h = h_in[(rebal_idx[i] - screen_params$window + 1):rebal_idx[i], ],
             h_next = h_in[(rebal_idx[i]+1):rebal_idx[i+1], ])

    # x = calc_pieces[[8]]
    h_res = foreach(x=calc_pieces)%dopar%{
        print(x$dt)
        basket = screen_func(x$h, screen_params)
        h = foreach(i=names(basket),.combine=cbind)%do%x$h_next[, basket[[i]]$names]
        w = foreach(i=names(basket),.combine=c)%do%basket[[i]]$weights
        r = basket_ret(h, w) 
        list(h=r, basket=basket, dt=x$dt)
    }
    
    return(h_res)
}
