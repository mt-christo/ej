h_to_log = function(h_in){
    lh = na.fill(diff(log(na.locf(h_in))), 0)
    for(i in 1:ncol(lh))
        lh[abs(lh[,i])>0.5, i] = 0
    return(lh)
}

xts_cbind_idx = function(x, y) {
    res = cbind(x[index(x)%in%index(y), ], y[index(y)%in%index(x), ])
    colnames(res) = c(colnames(x), colnames(y))
    return(res)
}

norm_weights = function(x) {
    y = x[!is.na(x)]
    return( (x - min(y))/(max(y) - min(y)) )
}

basket_ret = function(h_in, w_in) return( xts(log(((exp(h_in)-1)%*%w_in) + 1), order.by=index(h_in)) )

# h_in=h; w_in=w;
basket_ret_rebal = function(h_in, w_in) return( xts(diff(c(0, log(((exp(cumsum(h_in))-1)%*%w_in) + 1))), order.by=index(h_in)) )

basket_vol = function(h_in, w_in) return( sd(basket_ret(h_in[rowSums(is.na(h_in))==0, ], w_in))*sqrt(252) )

constituent_vols = function(h_in, vol_basis) return( foreach(i=1:ncol(h_in),.combine=c)%do%(sd(h_in[, i])*sqrt(vol_basis)) )

basket_perf = function(h_in, w_in) return( exp(cumsum(basket_ret(h_in[rowSums(is.na(h_in))==0, ], w_in))) )

fracperc = function(x, n, do_perc=TRUE) return( paste0(as.character(round(x*100, n)), if(do_perc) '%' else '') )

index_perf = function(x, append_start=TRUE){
    res = if(append_start) rbind(xts(0, order.by=index(x)[1]-1), x) else x
    res = exp(cumsum(res))
    return(res)
}

index_vol = function(x, days_base=252){
    return(sqrt(days_base)*sd(x))
}

basic_index_report = function(x, vol_basis) return( list(vol=sqrt(vol_basis)*sd(x), perf=tail(index_perf(x), 1)) )
