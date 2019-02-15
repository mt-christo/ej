library(ggplot2)
library(mailR)
library(ellipse)
library(googlesheets)
library(doMC)
library(data.table)
library(foreach)
library(xts)
library(nloptr)
library(tseries)
registerDoMC(cores=7)

COB_CTL <<- list(xtol_rel=1e-8, maxeval=5000)

refresh_s = function() {
    gs_auth(token = '/home/aslepnev/git/ej/gdoc_doc.R')
    return(gs_key('1y9KUgukyEvfjAVaDYCHPf1rkFn83q8LWS_0tybz2pIM'))
}

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

h_to_log = function(h_in){
    lh = na.fill(diff(log(1 + na.locf(h_in))), 0)
    for(i in 1:ncol(lh))
        lh[abs(lh[,i])>0.5, i] = 0
    return(lh)
}

etf_segment = function(u_in, segname, topn=1000000){
    geo_focus_asia = c('Japan', 'Asian Pacific Region', 'China', 'Asian Pacific Region ex Japan', 'Greater China', 'South Korea', 'Taiwan', 'Hong Kong', 'Greater China,Hong Kong', 'India', 'Indonesia', 'Singapore', 'Malaysia', 'Thailand')
    geo_focus_west = c('United States', 'California', 'European Region', 'New York', 'Pennsylvania', 'Minnesota', 'Canada', 'New Jersey', 'Ohio', 'Eurozone', 'Virginia', 'Massachusetts', 'Oregon', 'Missouri', 'North American Region', 'Michigan', 'Germany', 'Maryland', 'United Kingdom', 'Australia', 'European Region,Australia', 'Global,United States', 'Spain', 'Kentucky', 'Arizona', 'Switzerland', 'North Carolina', 'Hawaii', 'Colorado', 'France', 'Singapore')
    geo_focus_deveuro = c('Eurozone', 'Germany', 'United Kingdom', 'Spain', 'Switzerland', 'France')
    geo_focus_global = c('International', 'Global')
    
    res = if(segname=='Asia') u_in[geo_focus%in%geo_focus_asia | geo_focus2%in%c('Emerging Asia', 'Asia'), ] else
      if(segname=='West') u_in[geo_focus%in%geo_focus_west | geo_focus2%in%c('North America', 'Developed Europe'), ] else
      if(segname=='Developed Europe') u_in[(geo_focus=='European Region' & geo_focus2=='Developed Market') | geo_focus%in%geo_focus_deveuro, ] else
      if(segname=='Global') u_in[geo_focus%in%geo_focus_global | geo_focus2=='Global', ] else
      if(segname%in%u_in$ind_focus) u_in[ind_focus==segname, ]

    return(res[order(mcap, decreasing=TRUE), ][1:min(nrow(res), topn), ])
}

stock_segment = function(u_in, segname, topn=1000000){
    country_asia = c("CHINA", "INDIA", "SINGAPORE", "INDONESIA", "PHILIPPINES", "THAILAND", "BERMUDA", "HONG KONG", "BANGLADESH", "MALAYSIA", "VIETNAM")
    country_west = c("UNITED STATES", "SWITZERLAND", "FRANCE", "GERMANY", "IRELAND", "AUSTRALIA", "CANADA", "BRITAIN", "NORWAY", "NETHERLANDS", "SPAIN", "SWEDEN", "LUXEMBOURG", "ITALY", "ISRAEL", "AUSTRIA", "BELGIUM", "DENMARK", "POLAND", "NEW ZEALAND")
    country_deveuro = c("SWITZERLAND", "FRANCE", "GERMANY", "IRELAND", "BRITAIN", "NORWAY", "NETHERLANDS", "SPAIN", "SWEDEN", "LUXEMBOURG", "ITALY", "AUSTRIA", "BELGIUM", "DENMARK")
    
    res = if(segname=='Asia') u_in[country_name%in%country_asia, ] else
      if(segname=='West') u_in[country_name%in%country_west, ] else
      if(segname=='Developed Europe') u_in[country_name%in%country_deveuro, ] else
      if(segname%in%u_in$sector) u_in[sector==segname, ]

    return(res[order(mcap, decreasing=TRUE), ][1:min(nrow(res), topn), ])
}

# u = D$u[,.SD[1,], by=focus]
# D=D_STOCKS; u = d_stocks
# D_in=ds; u=stock_segment(ds_in$u, segstock, n_stock); smart=TRUE
pre_screen = function(D_in, u, smart = FALSE){
    u1 = u
    h1 = D_in$h[, u1$ticker]
    if(smart){
        h1 = h_to_log(h1)
#        rng = as.numeric(t(foreach(i=1:ncol(h1),.combine=cbind)%do%sum(abs(range(apply.yearly(h1[,i], sum))) < 0.6)))
#        res = list(u=u1[rng==2, ], h=h1[, rng==2])
        res = list(u=u1, h=h1)
        
        return(res)
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

optim_sigma = function(h_in, volparams){
    comat = cov(h_in)
    n = ncol(h_in)
    gradus = function(x) return(abs(volparams$target - sqrt(252)*sqrt(x %*% comat %*% x)))
    res = cobyla(x0=array(1/n, n), fn=gradus, lower=array(volparams$wmin, n), upper=array(volparams$wmax, n), hin=function(w){ -abs(1-sum(w)) }, control=COB_CTL)
    return(res$par)
}

xts_cbind_idx = function(x, y) return(cbind(x[index(x)%in%index(y), ], y[index(y)%in%index(x), ]))

basket_ret = function(h_in, w_in) return( xts(log(((exp(h_in)-1)%*%w_in) + 1), order.by=index(h_in)) )

basket_vol = function(h_in, w_in) return( sd(basket_ret(h_in, w_in))*sqrt(252) )

basket_perf = function(h_in, w_in) return( exp(cumsum(basket_ret(h_in, w_in))) )

# d_in=d_stock1; n_in=2; volparams=list(wnd=500, min=0.2, max=0.3)
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
    for(j in 1:nrow(nn)){
        sds[j] = w %*% comat[nn[j, ], nn[j, ]] %*% w
        mcaps[j] = min(mcap_nn[j, ])
    }
    sds = sqrt(250)*sqrt(sds)
    baskets = ticker_nn[sds>=volparams$min & sds<=volparams$max, ]
    sds = sds[sds>=volparams$min & sds<=volparams$max]
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

# r=h_res; params=vc_params
volcontrol = function(r, params){
    w = sqrt(250)*rollapplyr(r, params$window, FUN=sd)
    w[,1] = ifelse(is.na(w), 1, params$level/w)
    w[,1] = lag(ifelse(w > params$max_weight, params$max_weight, w), 1)
    res = log((exp(r) - 1)*w + 1)[-1,]
    return(res)
}

# ds_in=ds; de_in=de; segetf='Health Care'; n_etfs_uni=15; n_etfs=3; segstock='Health Care'; n_stock_uni=60; n_stock=2; vt=0.4
# ds_in=ds; de_in=de; segetf='Asia'; n_etfs=15; segstock='Asia'; n_stock=65; vt=0.3
# d_etf=pre_screen(de, etf_segment(de$u, 'Asia', 15), smart=TRUE); d_stock=pre_screen(ds, ds$u, smart=TRUE); n_etfs=3; n_stock=2; vt=0.4; wmin=0.15; wmax=0.6
index_vt_pridex_segment = function(d_etf, d_stock, n_etfs, n_stock, vt, wmin, wmax){
    d_stock1 = d_stock
    d_stock1$u = d_stock1$u[!ticker%in%d_etf$u$ticker, ]
    d_stock1$h = d_stock1$h[, !d_stock$u$ticker%in%d_etf$u$ticker]  # In case some stocks are ETFs at the same time - we want to avoid duplicates

    b_etf = best_pridex_basket(baskets_vol_range(d_etf, n_etfs, volparams=list(wnd=250, min=vt-0.2, max=vt+0.2))$baskets, d_etf$h)  # Basket with highest Pridex metric within this Volatility range
    b_stock = baskets_vol_range(d_stock1, n_stock, volparams=list(wnd=250, min=vt-0.1, max=vt+0.2))  # All baskets within Volatility range witn Volatility and Lowest Market cap info

    closest_baskets = order(abs(vt - b_stock$sds))[1:min(length(b_stock$sds), 10)]  # 10 baskets with vol closest to target
    best_basket = b_stock$baskets[closest_baskets[which.max(b_stock$mcaps[closest_baskets])], ]  # Basket with highest Low Market Cap
    b_stock = list(basket=best_basket, weights=array(1/n_stock, n_stock))
    
    hcom = xts_cbind_idx(d_etf$h[, b_etf$basket], d_stock1$h[, b_stock$basket])
    wcom = c(b_etf$weights, b_stock$weights)/(sum(b_etf$weights) + sum(b_stock$weights))
    wcom = optim_sigma(tail(hcom, 500), list(target=vt, wmin=wmin, wmax=wmax))
    print(paste('Volatility:', basket_vol(tail(hcom, 250), wcom), 'Performance:', tail(basket_perf(hcom, wcom), 1)))
    return(list(basket=c(b_etf$basket, b_stock$basket), weights=wcom, perf=basket_perf(hcom, wcom)))
}

get_grish_zacks = function(){
    D_STOCKS = get(load('/home/aslepnev/webhub/zacks_data.RData'))
    u = fread('/home/aslepnev/git/ej/grish_uni.csv')
    u$ticker = as.character(t(as.data.table(strsplit(u$ticker, ' ')))[, 1])
    D_STOCKS$u = u[D_STOCKS$u, on='ticker'][!is.na(country), ][, head(.SD, 1), by='ticker']
    return(D_STOCKS)
}

send_attach_to_email = function(filepath, filename, subject, eaddress){
    system(paste0('echo -e "to: ', eaddress, ' \nsubject: ', subject, '\n"| (cat && uuencode ', filepath, ' ', filename, ') | ssmtp ', eaddress))
}

# data=round(cor(liners), 2); data_name='gics_correlations'; subject='GICS sector index correlations'; eaddress='antonslepnev@gmail.com'
send_csv_to_email = function(data, data_name, subject, eaddress){
    filepath = paste0('/tmp/', data_name, '.csv')
    fwrite(data, filepath)
    send_attach_to_email(filepath, paste0(data_name, '.csv'), subject, eaddress)
}

send_xts_plot_and_csv_to_email = function(my_chart, my_tab, subject, eaddress){
    chart_path = paste0(tempfile(),'.png')
    png(chart_path)
    print(plot(my_chart))
    dev.off()
    csv_path = paste0(tempfile(),'.csv')
    fwrite(my_tab, csv_path)
    send.mail(from = 'novoxservice@gmail.com', to = eaddress, subject=subject, body = subject, encoding = "utf-8", smtp = list(host.name = "smtp.gmail.com", port = 465, user.name="novoxservice@gmail.com", passwd="Crestline_5", ssl=TRUE), authenticate = TRUE, send = TRUE , attach.files = c(chart_path, csv_path), html = TRUE, inline = TRUE )
}

# uni_in = pre_screen(data, data$u[gics_code%in%c(45, 50), ]); screen_params = list(window=40, voltarget=0.3, minw=0.01, maxw=0.3, force_us=FALSE, main=list(UNI=10, window=40)); rebal_freq='quarter'; index_code='SOLVIT'
# screen_params = list(window=40, voltarget=0.3, minw=0.01, maxw=0.3, force_us=FALSE, main=list(UNI=10, window=40)); rebal_freq='quarter'; index_code='SOLVIT'
# uni_in = pre_screen(D_STOCKS, d_stocks); screen_params = list(window=40, voltarget=0.4, minw=0.01, maxw=0.3, force_us=FALSE, main=list(UNI=10, window=40)); rebal_freq='quarter'; index_code='SOLCA'; start_date='2013-12-29'
gen_file_pack = function(uni_in, screen_params, rebal_freq, index_code, start_date){
    res = build_index(list(main=uni_in), rebal_dates=get_rebals(uni_in, rebal_freq), screen_voltarget, screen_params, start_date)
    
    r = foreach(x=res,.combine=rbind)%do%x$h
    print(sd(tail(r,120))*sqrt(252))
    perf = exp(cumsum(r)); print(tail(perf, 1))

    baskets = foreach(x=res,.combine=rbind)%do%cbind(data.table(dt=x$dt), t(x$basket$main$names))
    weights = foreach(x=res,.combine=rbind)%do%cbind(data.table(dt=x$dt), t(round(x$basket$main$weights, 3)))
    liner = exp(cumsum(foreach(x=res,.combine=rbind)%do%x$h))
    liner = data.table(dt=index(liner), value=liner)    

    #abet = c(LETTERS, paste0(LETTERS[1], LETTERS), paste0(LETTERS[2], LETTERS))
    #tryCatch({gs_ws_delete(refresh_s(), ws=index_code)}, error=function(e) {})
    #gs_ws_new(refresh_s(), ws_title=index_code, row_extent=5000, col_extent=100)
    #gs_edit_cells(refresh_s(), ws=index_code, anchor = "A1", input = liner, byrow = TRUE, col_names=FALSE)
    #gs_edit_cells(refresh_s(), ws=index_code, anchor = "D1", input = baskets, byrow = TRUE, col_names=FALSE)
    #gs_edit_cells(refresh_s(), ws=index_code, anchor = paste0(abet[4+ncol(baskets)+1], 1), input = weights, byrow = TRUE, col_names=FALSE)

    fwrite(liner, paste0('/home/aslepnev/git/ej/liner_', index_code, '.csv'))
    fwrite(baskets, paste0('/home/aslepnev/git/ej/baskets_', index_code, '.csv'))
    fwrite(weights, paste0('/home/aslepnev/git/ej/weights_', index_code, '.csv'))

    forw_items = list(liner=liner, baskets=baskets, weights=weights)
    for(item in names(forw_items)){
        filepath = paste0('/home/aslepnev/git/ej/', item, '_', index_code, '.csv')
        fwrite(forw_items[[item]], filepath)
        system(paste0('echo -e "to: antonslepnev@gmail.com \nsubject: ',index_code,' - ', item, '\n"| (cat && uuencode ',filepath, ' ', paste0(item, '_', index_code, '.csv'),') | ssmtp antonslepnev@gmail.com'))
    }

    
}

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

