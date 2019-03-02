uni_hist_sigmas = function(h_in){
    return( sqrt(252) * foreach(i=1:ncol(h_in), .combine=c)%do%sd(h_in[, i]) )
}

# basket = p$BASKET; corr_tail = 300; sigma_tail=120
uni_hist_report = function(u_in, basket, corr_tail, sigma_tail){
    h = tail(u_in$h[, basket], max(corr_tail, sigma_tail) + 5)
    h = na.fill(diff(log(1+na.locf(h))), 0)
    h1 = tail(h, sigma_tail)
    h_sigmas = if(sigma_tail>0) uni_hist_sigmas(tail(h1, sigma_tail)) else 0
    h_cor_mat = if(corr_tail>0) cor(tail(h, corr_tail)) else 0
    return( list(h=h, sigmas=h_sigmas, cor_mat=h_cor_mat) )
}

# u_in = NOVO_UNI2
uni_adapt = function(u_in){
    h = na.fill(diff(log(1 + na.locf(u_in$h))), 0)
    for(j in 1:ncol(h))
        h[abs(h[, j])>0.5, j] = 0

    wnds = c(120, 250)
    h1 = tail(h, max(wnds) + 10)
    res = u_in$u
    for(wnd in wnds)
        res[[paste0('sigma', wnd)]] = uni_hist_sigmas(tail(h1, wnd))
    
    return(list(u=res, h=h))
}

