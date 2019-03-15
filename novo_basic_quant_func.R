h_to_log = function(h_in){
    lh = na.fill(diff(log(1 + na.locf(h_in))), 0)
    for(i in 1:ncol(lh))
        lh[abs(lh[,i])>0.5, i] = 0
    return(lh)
}

xts_cbind_idx = function(x, y) {
    res = cbind(x[index(x)%in%index(y), ], y[index(y)%in%index(x), ])
    colnames(res) = c(colnames(x), colnames(y))
    return(res)
}

basket_ret = function(h_in, w_in) return( xts(log(((exp(h_in)-1)%*%w_in) + 1), order.by=index(h_in)) )

basket_vol = function(h_in, w_in) return( sd(basket_ret(h_in[rowSums(is.na(h_in))==0, ], w_in))*sqrt(252) )

basket_perf = function(h_in, w_in) return( exp(cumsum(basket_ret(h_in[rowSums(is.na(h_in))==0, ], w_in))) )

fracperc = function(x, n) return( paste0(as.character(round(x*100, n)), '%') )

