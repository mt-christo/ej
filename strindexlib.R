library(doMC)
library(data.table)
library(foreach)
library(xts)
library(nloptr)
library(tseries)
registerDoMC(cores=7)

COB_CTL <<- list(xtol_rel=1e-8, maxeval=5000)

enrich_data = function(fdata_path, hdata_path){
    #u = get(load('uni.RData')); u$dt = as.Date("2018-03-01"); colnames(u)=gsub(' ','_', colnames(u)); save(u, file='uni.RData')
    #p = get(load('uniprc.RData'))
    #h = diff(log(na.locf(p)))  # save(h, file='unih.RData')
    u = as.data.table(get(load('uni.RData'))) 
    h = get(load('unih.RData'))
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

# u=D$u; h=D$h; d0=as.Date("2018-12-17"); d1=as.Date('2018-07-01')
# d0 = as.Date("2018-03-01"); d1=as.Date("2017-07-01")
prorate_universe = function(u, h, d0, d1){
    u1 = u[dt==d0, ]
    u1$dt = d1
    ret = as.numeric(exp(colSums(h[as.Date(d0:d1), ]))) * if(d0<d1)1 else -1
    u1$mcap = u[dt==d0, ][['mcap']] * ret
    return(u1)
}

# u = D$u[,.SD[1,], by=focus]
# D=D_STOCKS; u = d_stocks
pre_screen = function(D, u, logret = FALSE){
    u1 = u
    h1 = D$h[, u1$ticker]
    if(logret){
        lh = na.fill(diff(log(1+na.locf(h1))), 0)
        for(i in 1:ncol(lh))
            lh[abs(lh[,i])>0.5, i] = 0
        h1 = lh
    }
    return(list(u=u1, h=h1))
}

get_rebals = function(D, period){
    f = if(period=='year') apply.yearly else if(period=='quarter') apply.quarterly else if(period=='month') apply.monthly else stop("Unknown period")
    return(as.Date(index(f(D$h[,1], FUN=length))))
}    

# u=D$u; h=D$h; d0=as.Date("2018-03-01"); d1s=rebal_dates
prorate_universe_multiple = function(u, h, d0, d1s){
    return(rbindlist(foreach(d1=d1s)%do%prorate_universe(u, h, d0, d1)))
}

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

pridex_metric = function(time_line, h, w){
    return(cor(time_line, log(((exp(h)-1)%*%w) + 1)))
}

basket_ret = function(h, w) return( xts(log(((exp(h)-1)%*%w) + 1), order.by=index(h)) )

basket_vol = function(h, w) return( sd(basket_ret(h, w))*sqrt(252) )

# lh_in=lh_in['etfs']; u_in=u1_in['etfs']; pick_count=params$N
pridex_screen_prep = function(lh_in, u_in, pick_count){
    r = lh_in[,colSums(abs(lh_in))!=0]
    perf = cumsum(r)
    uu = u_in[ticker%in%colnames(perf), ]
    time_line = 1:nrow(perf)
    metr = foreach(i=1:ncol(perf),.combine=c)%do%cor(time_line, perf[,i])
    uni = colnames(perf)[order(metr, decreasing=TRUE)[1:min(pick_count, nrow(uu))]]
    return(list(uni=uni, r=r, perf=perf, r_uni=r[, uni], perf_uni=perf[, uni], metr=metr, time_line=time_line))
}

pridex_rank_baskets = function(prep_in, bsize){
    nn = t(combn(prep_in$uni, bsize))  # all N-baskets from universe
    w = array(1/bsize, bsize)  # equal weights
    rnk = foreach(j=1:nrow(nn),.combine=c)%do%pridex_metric(prep_in$time_line, prep_in$perf_uni[, nn[j, ]], w)  # calc metric for every basket
    return(nn[order(rnk, decreasing=TRUE), ])  # return baskets in the highest-to-lowest metric
}

# lh_in=x$lh; u_in=x$u; params=screen_params
screen_pridex_equalweight = function(lh_in, u_in, params, lh_key='main'){
    prep = pridex_screen_prep(lh_in[[lh_key]], u_in[[lh_key]], params[[lh_key]]$UNI)  # precal data
    n = params[[lh_key]]$N
    baskets = pridex_rank_baskets(prep, n)
    return(list(names=baskets[1, ], weights=array(1/n, n), prep=prep))
}

# lh_in=x$lh; u_in=x$u; params=screen_params
# lh_in=x$lh; u_in=x$u; params=list(voltarget=0.3, minw=0.02, maxw=0.8, etfs=list(N=3, UNI=20, window=40), stocks=list(UNI=10, window=40))
screen_pridex_voltarget = function(lh_in, u_in, params){
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

# r=h_res; params=vc_params
volcontrol = function(r, params){
    w = sqrt(250)*rollapplyr(r, params$window, FUN=sd)
    w[,1] = ifelse(is.na(w), 1, params$level/w)
    w[,1] = lag(ifelse(w > params$max_weight, params$max_weight, w), 1)
    res = log((exp(r) - 1)*w + 1)[-1,]
    return(res)
}

if(FALSE){
    D_STOCKS = get(load('/home/aslepnev/webhub/zacks_data.RData'))
    ##### D_STOCKS$h = D_STOCKS$h[-3013]; save(D_STOCKS, file='/home/aslepnev/webhub/zacks_data.RData')
    d_stocks = head(D_STOCKS$u, 500)  # 500 biggest companies
    
    D_ETF = get(load('/home/aslepnev/webhub/sacha_etf_yhoo.RData'))
    ##### D_ETF$h = D_ETF$h[-2007]; save(D_ETF, file='/home/aslepnev/webhub/sacha_etf_yhoo.RData')
    my_tickers = c('ROBOTR','IXP','PNQI','SOXX','IBB','IYH','IHI','PJP','FBT','QQQ','MTUM','SPLV','EWZ','EEM','EFA','ILF','ASHR','FXI','IAU','IEO','PZA','TLT','LQD','EDV')
    my_niches = unique(D_ETF$u[ticker%in%my_tickers, .(mcap_focus2, style, geo_focus, geo_focus2, ind_focus, mcap_focus, ind_group, industry)])
    d_etf = D_ETF$u[my_niches, on=.(mcap_focus2, style, geo_focus, geo_focus2, ind_focus, mcap_focus, ind_group, industry)]
    D_ETF$h = D_ETF$h[index(D_ETF$h)%in%index(D_STOCKS$h)]
    D_STOCKS$h = D_STOCKS$h[index(D_STOCKS$h)%in%index(D_ETF$h)]
    

    d1 = D$u[order(mcap, decreasing=TRUE),][geo_focus=='European Region',][1:15,]
    d2 = D$u[order(mcap, decreasing=TRUE),][geo_focus2=='Emerging Market',][1:30,]
    d3 = D$u[order(mcap, decreasing=TRUE),][ind_focus=='Technology',][1:30,]
    d4 = D$u[order(mcap, decreasing=TRUE),][ind_focus=='Health Care',][1:30,]
    d5 = D$u[order(mcap, decreasing=TRUE),][ind_focus=='Financial',][1:20,]

    
#    D_in = pre_screen(D, D$u[1:100,]); 
#    D_in = pre_screen(D, D$u[,.SD[1,], by=focus])
#    D_in = pre_screen(D, D$u[,.SD[1:min(nrow(.SD),3),], by=category])
#    D_in = pre_screen(D, d)
#    screen_func = screen_mycorr2
#    rebal_dates = get_rebals(D, 'month')

#    screen_params=list(N=4, UNI=20, window=20, type='category', field='niche'); vc_params=list(window=20, level=0.085, max_weight=2.5); weights=array(1/screen_params$N, screen_params$N)

#    p0 = expand.grid(list(4:6,c(15,17,20,23,25)))
#    p0 = expand.grid(list(4:6,c(28,30)))

    dd = list(d1, d2, d3, d4, d5)  
    for(i in 1:length(dd)){
        screen_params=list(N=4, UNI=nrow(dd[[i]]), window=40, wtype='equalweight'); weights=array(1/screen_params$N, screen_params$N)
        res = build_index(pre_screen(D, dd[[i]]), get_rebals(D, 'month'), screen_mycorr2, screen_params, weights)
        save(res, file=paste0('/home/aslepnev/data/idx4_custom_',i,'.Rdata'))  # 3 for equalweight, 4 for vol-weight
    }
    hh = tail(pre_screen(D, dd[[2]], TRUE)$h, 252)
    sds = foreach(i=1:ncol(hh),.combine=c)%do%{ sd(hh[!is.na(hh[,i]),i])*sqrt(252) }
    sort(sds)


    screen_params = list(window=40, voltarget=0.3, minw=0.02, maxw=0.8, etfs=list(N=3, UNI=20, window=40), stocks=list(UNI=10, window=40))
    res = build_index(list(etfs=pre_screen(D_ETF, d_etf), stocks=pre_screen(D_STOCKS, d_stocks)), get_rebals(D_ETF, 'month'), screen_pridex_voltarget, screen_params)
    save(res, file='/home/aslepnev/data/idx5_custom1.Rdata')
    r = foreach(x=res,.combine=rbind)%do%x$h; print(sd(tail(r,120))*sqrt(252))
    perf = exp(cumsum(r))
    
    plot(exp(cumsum(foreach(x=get(load('/home/aslepnev/data/idx5_custom1.Rdata')),.combine=rbind)%do%x$h)))

    h_res = foreach(x=res,.combine=rbind)%do%x$h
    res_vc = exp(cumsum(volcontrol(h_res, list(window=20, level=0.01*8.5, max_weight=2.5))))
    plot(res_vc)
    lines(res_vc)

    res = get(load('/home/aslepnev/data/idx2_custom_4_25.Rdata'))
    h_res = foreach(x=res,.combine=rbind)%do%x$h
    res_vc = exp(cumsum(volcontrol(h_res, list(window=20, level=0.01*8.5, max_weight=2.5))))


    # 1 - equally weighted
    # 3 - best eq weitghted, then optimize
    # 4 - top n highest ranked
    # 5 - coctail
    
    p0 = expand.grid(list(4,22:35))
    for(ii in 1:nrow(p0)){
        pn = p0[ii,1]
        screen_params=list(N=pn, UNI=p0[ii,2], window=40, wtype='weights', field='niche', maxw=0.5); vc_params=list(window=20, level=0.085, max_weight=2.5); weights=array(1/screen_params$N, screen_params$N)
        res = build_index(pre_screen(D, d), get_rebals(D, 'month'), screen_mycorr2, screen_params, vc_params, weights)
        save(res, file=sprintf('/home/aslepnev/data/idx3_%s_%s_%s.Rdata', pn, screen_params$UNI, round(maxw,2)))
    }

    p0 = 2:50
    for(ii in p0){
        screen_params=list(N=ii, UNI=100, window=40, wtype='singles', field='niche', maxw=0.5); vc_params=list(window=20, level=0.085, max_weight=2.5); weights=array(1/screen_params$N, screen_params$N)
        res = build_index(pre_screen(D, d), get_rebals(D, 'month'), screen_mycorr2, screen_params, vc_params, weights)
        save(res, file=sprintf('/home/aslepnev/data/idx4_%s.Rdata', ii))
    }

    screen_params=list(N=4, UNI=25 window=40, wtype='simple', field='niche', maxw=0.5); vc_params=list(window=20, level=0.085, max_weight=2.5); weights=array(1/screen_params$N, screen_params$N)
    res = build_index(pre_screen(D, d), get_rebals(D, 'month'), screen_mycorr2, screen_params, vc_params, weights)
    save(res, file=sprintf('/home/aslepnev/data/idx4_%s.Rdata', ii))
    
 #   build_index(pre_screen(D, D$u[,.SD[1:min(nrow(.SD),3),], by=category]), get_rebals(D, 'month'), screen_mycorr2, screen_params, vc_params, weights)

    # filename='/home/aslepnev/data/idx0_custom_4_25.Rdata'; vcp=8.5
    # filename='/home/aslepnev/data/idx1_4_25.Rdata'; vcp=10
    ext_h = function(filename, vcp, start_dt = '1900-01-01'){
        d = get(load(filename))
        res = foreach(x=d,.combine=rbind)%do%x$h
        print(paste0('Simple vol: ', sd(tail(res,250))*sqrt(252)))
#        res = volcontrol(res, list(window=20, level=0.01*vcp, max_weight=2.5))
#        print(paste0('VC vol: ', sd(res)*sqrt(252)))
#        print(paste0('VC ret: ', tail(exp(cumsum(res)),1)))        
        res = exp(cumsum(res[index(res)>start_dt]))
        print(paste0('Simple ret: ', tail(res,1)))
        res
    }

    hh = foreach(k=8:9,.combine=cbind)%do%ext_h(sprintf('/home/aslepnev/data/idx1_%s_%s.Rdata', k, 252),8.5)
    hh = foreach(filename=Sys.glob('/home/aslepnev/data/idx3_custom*'),.combine=cbind)%do%ext_h(filename, 8.5)
    plot(hh)
    plot(ext_h(Sys.glob('/home/aslepnev/data/idx1_4_*'),8.5))
    plot(ext_h(Sys.glob('/home/aslepnev/data/idx1_4_*')[5],8.5))
    plot(ext_h('/home/aslepnev/data/idx1_4_25.Rdata',8.5))
    a = ext_h('/home/aslepnev/data/idx0_custom_4_25.Rdata',8.5)

    hh = foreach(filename=Sys.glob('/home/aslepnev/data/idx4_custom*'),.combine=cbind)%do%ext_h(filename, 8.5, '2009-04-01')
    plot(hh)
    
    plot(ext_h('/home/aslepnev/data/idx2_custom_4_25.Rdata',8.5))
    lines(ext_h('/home/aslepnev/data/idx2_custom_6_25.Rdata',8.5))

    print(paste0('normal: ', round(tail(res, 1),2), ', volcontrolled: ', round(tail(res_vc, 1), 2)))
    plot(100*res_vc, cex=2, cex.main=2)
    

}

# D_in=pre_screen(D, d); rebal_dates=get_rebals(D, 'month'); screen_func=screen_mycorr2; 
# D_in=pre_screen(D, dd[[i]]); rebal_dates=get_rebals(D, 'month'); screen_func=screen_mycorr2; 
# D_in=list(etfs=pre_screen(D_ETF, d_etf), stocks=pre_screen(D_STOCKS, d_stocks)); rebal_dates=get_rebals(D_ETF, 'month'); screen_func=screen_pridex_voltarget
build_index = function(D_in, rebal_dates, screen_func, screen_params){
    lh = list()
    for(i in names(D_in)){
        lh[[i]] = na.fill(diff(log(1+na.locf(D_in[[i]]$h))), 0)
        for(j in 1:ncol(lh[[i]]))
            lh[[i]][abs(lh[[i]][,j])>0.5, j] = 0
    }

    for(i in 1:length(lh))
        for(j in 1:length(lh))
            lh[[i]] = lh[[i]][index(lh[[i]])%in%index(lh[[j]])]
    
    rebal_idx = match(rebal_dates[rebal_dates>='2008-09-30'], index(lh[[1]]))  # indices of history in all elements iof lh are expected the same!
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

