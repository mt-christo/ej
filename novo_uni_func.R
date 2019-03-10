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

get_grish_zacks = function(){
    D_STOCKS = get(load('/home/aslepnev/webhub/zacks_data.RData'))
    u = fread('/home/aslepnev/git/ej/grish_uni.csv')
    u$ticker = as.character(t(as.data.table(strsplit(u$ticker, ' ')))[, 1])
    D_STOCKS$u = u[D_STOCKS$u, on='ticker'][!is.na(country), ][, head(.SD, 1), by='ticker']
    return(D_STOCKS)
}

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

