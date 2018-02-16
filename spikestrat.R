sslice_idx = function(r0, v0, p_1, j0, nwait, i0){
    print(i0)
    vs = v0[(i0-p_1):i0,]
    vs = as.numeric(t(rowSums(t(vs))))
    idxv = order(vs, decreasing=TRUE)[10:40]

    rs = diff(log(r0[(i0-nwait):i0,]))
    idxr = as.numeric(rs[2,]) > j0
    if(nwait > 1)
        idxr = which(idxr & foreach(k=2:nwait,.combine='&')%do%{ as.numeric(rs[1+k,]) > 0 })
    
    return(intersect(idxv,idxr))
}

names_signal = function(){
    symbols = sort(names( fromJSON( paste0( "https://min-api.cryptocompare.com/data/all/coinlist" ) )$Data ))
    
    j0 = 0.08
    nwait = 4
    
    r1 = foreach(s=symbols,.combine='merge.xts')%do%{
        print(s)
        x = get_cryptocompare_data( s, as.character(Sys.Date()-6), as.character(Sys.Date()), 'hour', 'CCCAGG', 'BTC', local=T )
        if(!is.null(x) && dim(x)[1]>0){
            x = as.xts(x$close, order.by=x$time)
            names(x)[1] = s
            x
        }
    }
    
    v1 = foreach(s=symbols,.combine='merge.xts')%do%{
        print(s)
        x = get_cryptocompare_data( s, as.character(Sys.Date()-6), as.character(Sys.Date()), 'hour', 'CCCAGG', 'BTC', local=T )
        if(!is.null(x) && dim(x)[1]>0){
            x = as.xts(x$volume_to, order.by=x$time)
            names(x)[1] = s
            x
        }
    }

    sslice_idx(r1, v1, 24*5, 0.08, 4, length(index(r1)))
    
}
