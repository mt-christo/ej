library(doMC)
library(data.table)
library(xts)
library(nloptr)
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
pre_screen = function(D, u){
    u1 = u
    h1 = D$h[, u1$ticker]
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
# lh_in=x$lh; u_in=x$u; params=screen_params
# params=list(N=4, UNI=25, window=40, type='simple', wtype='weights', field='niche', maxw=0.5); vc_params=list(window=20, level=0.085, max_weight=2.5); weights=array(1/screen_params$N, screen_params$N)
screen_mycorr2 = function(lh_in, u_in, params){
    hh = na.fill(lh_in, fill=0.0)
    hh = cumsum(hh[,colSums(abs(hh))!=0])
    uu = u_in[ticker%in%colnames(hh), ]
    time_line = 1:nrow(hh)
    hhcor = foreach(i=1:ncol(hh),.combine=c)%do%cor(time_line, hh[,i])
    uni = colnames(hh)[order(hhcor, decreasing=TRUE)[1:params$UNI]]

    my_corr = function(n, w){ return(cor(time_line, log(((exp(hh[, n])-1)%*%w) + 1))) }
    this_screen = function(uni_in){  # uni_in=uni
        if(params$wtype=='singles') return(list(names=uni_in[1:params$N], weights=array(1/params$N, params$N)))

        nn = t(combn(uni_in, params$N))
        w = array(1/params$N, params$N)
        rnk = foreach(j=1:nrow(nn),.combine=c)%do%{ my_corr(nn[j,], w) }
        
        if(params$wtype=='equalweight') return(list(names=nn[which.max(rnk),], weights=array(1/params$N, params$N)))
        # w=x$par  array(1/params$N, params$N)
        eq_one = function(w){ -abs(1-sum(w)) }            
        rnks = order(rnk,decreasing=TRUE)[1:min(100,length(rnk))]
        # i=rnks[1]
        res = foreach(i=rnks)%do%{
            names <- nn[i,]
            gradus = function(w){ return(-my_corr(names, w)) }                
            res = cobyla(x0=array(1/params$N, params$N), fn=gradus, lower=array(0,params$N), upper=array(params$maxw,params$N), hin=eq_one, control=COB_CTL)
        }
        i = which.min(foreach(x=res,.combine=c)%do%x$value)
        return(list(names=nn[rnks[i],], weights=res[[i]]$par))
    }

    s1 = this_screen(uni)
#    s2 = this_screen(uni[!uni%in%s1$names])
#    return(list(names=c(s1$names, s2$names), weights=c(s1$weights, s2$weights)*0.5))
    return(list(names=s1$names, weights=s1$weights))
}

# r=h_res; params=vc_params
volcontrol = function(r, params){
    w = sqrt(250)*rollapply(r, params$window, FUN=sd)
    w[,1] = ifelse(is.na(w), 1, params$level/w)
    w[,1] = lag(ifelse(w > params$max_weight, params$max_weight, w), 1)
    return(log((exp(r) - 1)*w + 1)[-1,])
}

if(FALSE){
    D = get(load('/home/aslepnev/git/ej/etf_com_DATA.RData'))
    my_tickers = c('ROBOTR','IXP','PNQI','SOXX','IBB','IYH','IHI','PJP','FBT','QQQ','MTUM','SPLV','EWZ','EEM','EFA','ILF','ASHR','FXI','IAU','IEO','PZA','TLT','LQD','EDV')
    my_niches = unique(D$u[D$u$ticker%in%my_tickers, .(focus, niche, category, region, geography, strategy)])
    d = D$u[my_niches, on=.(focus, niche, category, region, geography, strategy)]
    
#    D_in = pre_screen(D, D$u[1:100,]); 
#    D_in = pre_screen(D, D$u[,.SD[1,], by=focus])
#    D_in = pre_screen(D, D$u[,.SD[1:min(nrow(.SD),3),], by=category])
#    D_in = pre_screen(D, d)
#    screen_func = screen_mycorr2
#    rebal_dates = get_rebals(D, 'month')

#    screen_params=list(N=4, UNI=20, window=20, type='category', field='niche'); vc_params=list(window=20, level=0.085, max_weight=2.5); weights=array(1/screen_params$N, screen_params$N)

#    p0 = expand.grid(list(4:6,c(15,17,20,23,25)))
#    p0 = expand.grid(list(4:6,c(28,30)))

    screen_params=list(N=6, UNI=25, window=40, type='simple', wtype='weights', field='niche', maxw=0.3); vc_params=list(window=20, level=0.085, max_weight=2.5); weights=array(1/screen_params$N, screen_params$N)
    res = build_index(pre_screen(D, d), get_rebals(D, 'month'), screen_mycorr2, screen_params, vc_params, weights)
    save(res, file='/home/aslepnev/data/idx2_custom_6_25.Rdata')


    save(res, file='/home/aslepnev/data/idx2_custom_4_25.Rdata')

    screen_params=list(N=5, UNI=30, window=40, type='simple', wtype='weights', field='niche', maxw=0.5); vc_params=list(window=20, level=0.085, max_weight=2.5); weights=array(1/screen_params$N, screen_params$N)
    res = build_index(pre_screen(D, d), get_rebals(D, 'month'), screen_mycorr2, screen_params, vc_params, weights)
    save(res, file='/home/aslepnev/data/idx2_custom2.Rdata')

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
    
    p0 = expand.grid(list(3:6,22:35))
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


    
    screen_params=list(N=5, UNI=25, window=40, type='simple', field='niche'); vc_params=list(window=20, level=0.085, max_weight=2.5); weights=array(1/screen_params$N, screen_params$N)
    build_index(pre_screen(D, d), get_rebals(D, 'month'), screen_mycorr2, screen_params, vc_params, weights)
 #   build_index(pre_screen(D, D$u[,.SD[1:min(nrow(.SD),3),], by=category]), get_rebals(D, 'month'), screen_mycorr2, screen_params, vc_params, weights)

    ext_h = function(filename, vcp){
        d = get(load(filename))
#        d = get(load('/home/aslepnev/data/idx2_custom.Rdata'))
        h_res = foreach(x=d,.combine=rbind)%do%x$h
        res_vc = exp(cumsum(volcontrol(h_res, list(window=20, level=0.01*vcp, max_weight=2.5))))
#        res = exp(cumsum(h_res))
        res_vc
    }

    hh = foreach(k=8:9,.combine=cbind)%do%ext_h(sprintf('/home/aslepnev/data/idx1_%s_%s.Rdata', k, 252),8.5)
    hh = foreach(filename=Sys.glob('/home/aslepnev/data/idx1_*'),.combine=cbind)%do%ext_h(filename, 8.5)
    plot(hh)
    plot(ext_h(Sys.glob('/home/aslepnev/data/idx1_4_*'),8.5))
    plot(ext_h(Sys.glob('/home/aslepnev/data/idx1_4_*')[5],8.5))
    plot(ext_h('/home/aslepnev/data/idx1_4_25.Rdata',8.5))
    lines(ext_h('/home/aslepnev/data/idx1_4_25.Rdata',8.5))

    plot(ext_h('/home/aslepnev/data/idx2_custom_4_25.Rdata',8.5))
    lines(ext_h('/home/aslepnev/data/idx2_custom_6_25.Rdata',8.5))

    print(paste0('normal: ', round(tail(res, 1),2), ', volcontrolled: ', round(tail(res_vc, 1), 2)))
    plot(100*res_vc, cex=2, cex.main=2)
    

}

# D_in=pre_screen(D, d); rebal_dates=get_rebals(D, 'month'); screen_func=screen_mycorr2; 
build_index = function(D_in, rebal_dates, screen_func, screen_params, vc_params, weights){
    u = D_in$u; h = D_in$h
    lh = na.fill(diff(log(1+na.locf(h))), 0)
    for(i in 1:ncol(lh))
        lh[abs(lh[,i])>0.5, i] = 0
    
    rebal_idx = match(rebal_dates[rebal_dates>='2008-09-30'], index(lh))
    calc_pieces = foreach(i=1:(length(rebal_idx)-1))%do%
        list(dt = index(lh)[rebal_idx[i]],
             lh = lh[(rebal_idx[i]-screen_params$window):rebal_idx[i], ],
             lh_next = lh[(rebal_idx[i]+1):rebal_idx[i+1], ],
#             u = u[dt==rebal_dates[i],])
             u = u)

    # x = calc_pieces[[100]]
    h_res = foreach(x=calc_pieces)%dopar%{
        print(paste(screen_params$N, screen_params$UNI, x$dt, sep=', '))
        basket = screen_func(x$lh, x$u, screen_params)
        he = x$lh_next[, basket$names]
        r = xts(log((exp(he) - 1)%*%(basket$weights) + 1), order.by=index(he))
        list(h=r, basket=basket, dt=x$dt)
    }

    
#    res_vc = exp(cumsum(volcontrol(h_res, vc_params)))
#    res = exp(cumsum(h_res))
#    print(paste0('normal: ', round(tail(res, 1),2), ', volcontrolled: ', round(tail(res_vc, 1), 2)))
#    plot(res, cex=2, cex.main=2)
    
    return(h_res)
}

