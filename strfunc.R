lowest_cormats = function(h1, n, q){
    idcomb = combn(1:ncol(h1), n)
    cm = 0
    for(i in 1:ncol(idcomb)){
        if(i%%10000==0) print(i)
        cm[i] = mean(cor(h1[, idcomb[,i]]))
    }
    return(foreach(i=which(cm<quantile(cm, q)))%do%idcomb[, i])
}

# h1 = h; sigmas=SIGMAS; n=SIZE
highest_sds = function(h1, sigmas, n, q){
    idcomb = combn(1:ncol(h1), n)
    cm = 0
    for(i in 1:ncol(idcomb)){
        if(i%%10000==0) print(i)
        cm[i] = as.numeric(sum(diag(cor2cov(cor(h1[, idcomb[,i]]), sigmas[idcomb[,i]]))))
    }
    return(foreach(i=which(cm>quantile(cm, q)))%do%idcomb[, i])
}

