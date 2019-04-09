TAG_FILTERS <<- list()
TAG_FILTERS[[1]] = list(name='asia', target='etf', filter=list(list(field='geo_focus', value=c('Japan', 'Asian Pacific Region', 'China', 'Asian Pacific Region ex Japan', 'Greater China', 'South Korea', 'Taiwan', 'Hong Kong', 'Greater China,Hong Kong', 'India', 'Indonesia', 'Singapore', 'Malaysia', 'Thailand'))))
TAG_FILTERS[[2]] = list(name='asia', target='etf', filter=list(list(field='geo_focus2', value=c('Emerging Asia', 'Asia'))))
TAG_FILTERS[[3]] = list(name='west', target='etf', filter=list(list(field='geo_focus', value=c('United States', 'California', 'European Region', 'New York', 'Pennsylvania', 'Minnesota', 'Canada', 'New Jersey', 'Ohio', 'Eurozone', 'Virginia', 'Massachusetts', 'Oregon', 'Missouri', 'North American Region', 'Michigan', 'Germany', 'Maryland', 'United Kingdom', 'Australia', 'European Region,Australia', 'Global,United States', 'Spain', 'Kentucky', 'Arizona', 'Switzerland', 'North Carolina', 'Hawaii', 'Colorado', 'France', 'Singapore'))))
TAG_FILTERS[[4]] = list(name='west', target='etf', filter=list(list(field='geo_focus2', value=c('North America', 'Developed Europe'))))
TAG_FILTERS[[5]] = list(name='europe', target='etf', filter=list(list(field='geo_focus', value=c('European Region')), list(field='geo_focus2', value=c('Developed Market'))))
TAG_FILTERS[[6]] = list(name='europe', target='etf', filter=list(list(field='geo_focus', value=c('Eurozone', 'Germany', 'United Kingdom', 'Spain', 'Switzerland', 'France'))))
TAG_FILTERS[[7]] = list(name='europe', target='etf', filter=list(list(field='geo_focus', value=c('International', 'Global'))))
TAG_FILTERS[[8]] = list(name='global', target='etf', filter=list(list(field='geo_focus2', value=c('Global'))))
TAG_FILTERS[[9]] = list(name='asia', target='equity', filter=list(list(field='country_name', value=c("CHINA", "INDIA", "SINGAPORE", "INDONESIA", "PHILIPPINES", "THAILAND", "BERMUDA", "HONG KONG", "BANGLADESH", "MALAYSIA", "VIETNAM", "KOREA", "JAPAN", "TAIWAN"))))
TAG_FILTERS[[10]] = list(name='west', target='equity', filter=list(list(field='country_name', value=c("UNITED STATES", "SWITZERLAND", "FRANCE", "GERMANY", "IRELAND", "AUSTRALIA", "CANADA", "BRITAIN", "NORWAY", "NETHERLANDS", "SPAIN", "SWEDEN", "LUXEMBOURG", "ITALY", "ISRAEL", "AUSTRIA", "BELGIUM", "DENMARK", "POLAND", "NEW ZEALAND"))))
TAG_FILTERS[[11]] = list(name='europe', target='equity', filter=list(list(field='country_name', value=c("SWITZERLAND", "FRANCE", "GERMANY", "IRELAND", "BRITAIN", "NORWAY", "NETHERLANDS", "SPAIN", "SWEDEN", "LUXEMBOURG", "ITALY", "AUSTRIA", "BELGIUM", "DENMARK"))))

TAG_FILTERS = c(TAG_FILTERS, list(list(name='health', target='etf', filter=list(list(field='ind_focus', value=c('Health Care'))))))
TAG_FILTERS = c(TAG_FILTERS, list(list(name='tech', target='etf', filter=list(list(field='ind_focus', value=c('Technology'))))))
TAG_FILTERS = c(TAG_FILTERS, list(list(name='finance', target='etf', filter=list(list(field='ind_focus', value=c('Financial'))))))
TAG_FILTERS = c(TAG_FILTERS, list(list(name='staples', target='etf', filter=list(list(field='ind_focus', value=c('Consumer Staples'))))))
TAG_FILTERS = c(TAG_FILTERS, list(list(name='discret', target='etf', filter=list(list(field='ind_focus', value=c('Consumer Discretionary'))))))
TAG_FILTERS = c(TAG_FILTERS, list(list(name='industry', target='etf', filter=list(list(field='ind_focus', value=c('Industrials'))))))
TAG_FILTERS = c(TAG_FILTERS, list(list(name='us', target='equity', filter=list(list(field='country_name', value=c('UNITED STATES'))))))
TAG_FILTERS = c(TAG_FILTERS, list(list(name='tech', target='equity', filter=list(list(field='sector', value=c('Information Technology'))))))

tag_id = 0
TAG_FILTERS = foreach(t = TAG_FILTERS, .combine=rbind)%do%{
    tag_id = tag_id+1
    foreach(f = t$filter, .combine=rbind)%do%{ foreach(v = f$value, .combine=rbind)%do%data.table(id=c(tag_id), name=c(t$name), target=c(t$target), field=c(f$field), value=c(v)) }
}


#    res = if(segname=='Asia') u_in[geo_focus%in%geo_focus_asia | geo_focus2%in%, ] else
#      if(segname=='West') u_in[geo_focus%in%geo_focus_west | geo_focus2%in%, ] else
#      if(segname=='Developed Europe') u_in[(geo_focus=='European Region' & geo_focus2=='Developed Market') | geo_focus%in%geo_focus_deveuro, ] else
#      if(segname=='Global') u_in[geo_focus%in%geo_focus_global | geo_focus2=='Global', ] else
#      if(segname%in%u_in$ind_focus) u_in[ind_focus==segname, ]else
#      if(segname%in%u_in$geo_focus) u_in[geo_focus==segname, ]
#
#    return(res[order(mcap, decreasing=TRUE), ][1:min(nrow(res), topn), ])
#    
#    res = if(segname=='Asia') u_in[country_name%in%country_asia, ] else
#      if(segname=='West') u_in[country_name%in%country_west, ] else
#      if(segname=='Developed Europe') u_in[country_name%in%country_deveuro, ] else
#      if(segname%in%u_in$sector) u_in[sector==segname, ] else
#      if(segname%in%u_in$country_name) u_in[country_name==segname, ]

uni_skip_tickers = function(uni_in, skip_list){
    return( list(u=uni_in$u[!ticker%in%skip_list, ], h=uni_in$h[, !colnames(uni_in$h)%in%skip_list]) )
}

uni_skip_countries = function(uni_in, skip_list){
    ok_list = uni_in$u[!country_name%in%skip_list & !country_code%in%skip_list, ticker]
    return( list(u=uni_in$u[ticker%in%ok_list, ], h=uni_in$h[, colnames(uni_in$h)%in%ok_list]) )
}

uni_skip_countries_tickers = function(uni_in, countries_skip_list, tickers_skip_list, is_like=TRUE){
    return( uni_skip_countries(uni_skip_tickers(uni_in, tickers_skip_list), countries_skip_list) )
}

# uni_options = c('equity', 'etf', 'p', 'h', 'libors', 'currency', 'sigma 250', 'sigma 125')
# uni_options = c('equity', 'etf', 'h', 'libors')
# uni_options = c('etf', 'h', 'libors')
# uni_options = c('equity', 'equity_metrics', 'h', 'libors')
# filter_tags = list(field_filter=c('asia+europe', 'tech+health'), rank_filter=c('top 10 mcap'))
# filter_tags = list(field_filter=c('europe'), rank_filter=c('top 10 mcap'))
# filter_tags = list(field_filter=c('asia'), rank_filter=c('top 10 mcap'))
# filter_tags = list(field_filter=c('us', 'tech'), rank_filter=c('top 30 mcap'))
# filter_tags = c()
# TAG_FILTERS[[8]] = list(name='global', target='etf', filter=list(list(field='geo_focus2', value=c('Global'))))
load_uni = function(uni_options, filter_tags){
    res = foreach(n = uni_options)%do%get(load(paste0('/home/aslepnev/webhub/uber_uni_', n, '.RData')))
    names(res) = uni_options

    asset_classes = c('etf', 'equity')
    ts_datas = c('p', 'h')
    
    for(m in ts_datas)  # n = 'h'
        if(m%in%names(res))
            for(n in asset_classes){
                if(n%in%names(res))
                    res[[n]] = res[[n]][ticker%in%colnames(res[[m]]), ]

                n_metrics = paste0(n, '_metrics')
                if(n_metrics%in%names(res))
                    res[[n]] = res[[n]][ticker%in%colnames(res[[m]]), ]
            }
            
            
    tickers = foreach(n = asset_classes, .combine=c)%do%{ if(n%in%names(res)) unique(res[[n]]$ticker) else c() }  # n = asset_classes[2]
    tickers = tickers[foreach(n = filter_tags$field_filter, .combine='&')%do%{  # n = filter_tags$field_filter[1]
        tickers %in% (foreach(m = strsplit(n, '\\+')[[1]], .combine=c)%do%  # m = strsplit(n, '\\+')[[1]][2] 
            unique(foreach(fid = TAG_FILTERS[name==m & target%in%uni_options, unique(id)], .combine=c)%do%{  # fid = TAG_FILTERS[name==m & target%in%uni_options, unique(id)][1]
                ftr = TAG_FILTERS[id==fid, ]
                targ = res[[ftr$target[1]]]
                targ$ticker[foreach(fld = ftr[, unique(field)], .combine='&')%do%{ targ[[ fld ]]%in%( ftr[field==fld, value] ) }]  # fld = ftr[, unique(field)][1]
            }))
    }]
    
    for(n in asset_classes)  # n = 'equity'
        if(n%in%names(res)){
            res[[n]] = res[[n]][ticker%in%tickers, ]
            
            n_metrics = paste0(n, '_metrics')
            if(n_metrics%in%names(res)){
                res[[n_metrics]] = res[[n_metrics]][ticker%in%tickers, ]
           
                for(ftag in filter_tags$rank_filter)  # ftag = filter_tags$rank_filter[1]
#                    if(startsWith(ftag, 'top ') && endsWith(ftag, ' mcap')){
                    if(startsWith(ftag, 'top ') && as.logical(max(endsWith(ftag, colnames(res[[n_metrics]]))))){
                        m = as.numeric(gsub('top', '', gsub('mcap', '', ftag)))
                        res[[n_metrics]] = res[[n_metrics]][order(mcap), ][, tail(.SD, 1), by=c('dt', 'name')][, tail(.SD, m), by='dt']
                    }
                
                res[[n]] = res[[n]][ticker%in%res[[n_metrics]]$ticker, ]
            }
        }

    for(n in ts_datas)  # n = 'h'
        if(n%in%names(res))
            res[[n]] = res[[n]][, foreach(m = asset_classes, .combine=c)%do%{ if(m%in%names(res)) res[[m]]$ticker else c() }]

    return(res)
}


####
#uni_from_params = function(uni_params){
#    if(uni_params$name == 'it10')
#        res = get(load('/home/aslepnev/git/ej/it_top10_uni.RData'))
#    else if(uni_params$name=='asia' && uni_params$type=='stocks and etfs'){
#        ds = get(load('/home/aslepnev/webhub/grish_asia.RData'))
#        de = get(load('/home/aslepnev/webhub/sacha_etf_yhoo.RData'))
#        res = list(etfs = pre_screen(de, etf_segment(de$u, 'Asia', uni_params$etf_count), smart=TRUE),
#                   stocks = pre_screen(ds, stock_segment(ds$u, 'Asia', uni_params$stock_count), smart=TRUE))
#    }
#
#    return(res)
#}

#enrich_data = function(fdata_path, hdata_path){
#    #u = get(load('uni.RData')); u$dt = as.Date("2018-03-01"); colnames(u)=gsub(' ','_', colnames(u)); save(u, file='uni.RData')
#    #p = get(load('uniprc.RData'))
#    #h = diff(log(na.locf(p)))  # save(h, file='unih.RData')
#    u = as.data.table(get(load('uni.RData'))) 
#    h = get(load('unih.RData'))
#    u = u[1:ncol(h), ]
#    if(filter2){
#        mx = foreach(i=1:ncol(h),.combine=c)%do%{x=h[,i]; max(x[!is.na(x)])}
#        mn = foreach(i=1:ncol(h),.combine=c)%do%{x=h[,i]; min(x[!is.na(x)])}
#        u = u[mn>-0.69 & mx<0.69, ]
#        h = h[, mn>-0.69 & mx<0.69]
#    }
#    h90 = tail(h, 90)
#    u$HSIGMA = foreach(i=1:ncol(h),.combine='c')%do%{ sd(h90[,i])*250/90 }
#    return(list(u=u, h=h))
#}

# u=D$u; h=D$h; d0=as.Date("2018-12-17"); d1=as.Date('2018-07-01')
# d0 = as.Date("2018-03-01"); d1=as.Date("2017-07-01")
#prorate_universe = function(u, h, d0, d1){
#    u1 = u[dt==d0, ]
#    u1$dt = d1
#    ret = as.numeric(exp(colSums(h[as.Date(d0:d1), ]))) * if(d0<d1)1 else -1
#    u1$mcap = u[dt==d0, ][['mcap']] * ret
#    return(u1)
#}

# u_in=de$u; segname='United States'; topn=40
#etf_segment = function(u_in, segname, topn=1000000){
#    geo_focus_asia = c('Japan', 'Asian Pacific Region', 'China', 'Asian Pacific Region ex Japan', 'Greater China', 'South Korea', 'Taiwan', 'Hong Kong', 'Greater China,Hong Kong', 'India', 'Indonesia', 'Singapore', 'Malaysia', 'Thailand')
#    geo_focus_west = c('United States', 'California', 'European Region', 'New York', 'Pennsylvania', 'Minnesota', 'Canada', 'New Jersey', 'Ohio', 'Eurozone', 'Virginia', 'Massachusetts', 'Oregon', 'Missouri', 'North American Region', 'Michigan', 'Germany', 'Maryland', 'United Kingdom', 'Australia', 'European Region,Australia', 'Global,United States', 'Spain', 'Kentucky', 'Arizona', 'Switzerland', 'North Carolina', 'Hawaii', 'Colorado', 'France', 'Singapore')
#    geo_focus_deveuro = c('Eurozone', 'Germany', 'United Kingdom', 'Spain', 'Switzerland', 'France')
#    geo_focus_global = c('International', 'Global')
#    
#    res = if(segname=='Asia') u_in[geo_focus%in%geo_focus_asia | geo_focus2%in%c('Emerging Asia', 'Asia'), ] else
#      if(segname=='West') u_in[geo_focus%in%geo_focus_west | geo_focus2%in%c('North America', 'Developed Europe'), ] else
#      if(segname=='Developed Europe') u_in[(geo_focus=='European Region' & geo_focus2=='Developed Market') | geo_focus%in%geo_focus_deveuro, ] else
#      if(segname=='Global') u_in[geo_focus%in%geo_focus_global | geo_focus2=='Global', ] else
#      if(segname%in%u_in$ind_focus) u_in[ind_focus==segname, ]else
#      if(segname%in%u_in$geo_focus) u_in[geo_focus==segname, ]
#
#    return(res[order(mcap, decreasing=TRUE), ][1:min(nrow(res), topn), ])
#}

## u_in=ds$u; segname='UNITED STATES'; topn=40
#stock_segment = function(u_in, segname, topn=1000000){
#    country_asia = c("CHINA", "INDIA", "SINGAPORE", "INDONESIA", "PHILIPPINES", "THAILAND", "BERMUDA", "HONG KONG", "BANGLADESH", "MALAYSIA", "VIETNAM", "KOREA", "JAPAN", "TAIWAN")
#    country_west = c("UNITED STATES", "SWITZERLAND", "FRANCE", "GERMANY", "IRELAND", "AUSTRALIA", "CANADA", "BRITAIN", "NORWAY", "NETHERLANDS", "SPAIN", "SWEDEN", "LUXEMBOURG", "ITALY", "ISRAEL", "AUSTRIA", "BELGIUM", "DENMARK", "POLAND", "NEW ZEALAND")
#    country_deveuro = c("SWITZERLAND", "FRANCE", "GERMANY", "IRELAND", "BRITAIN", "NORWAY", "NETHERLANDS", "SPAIN", "SWEDEN", "LUXEMBOURG", "ITALY", "AUSTRIA", "BELGIUM", "DENMARK")
#    
#    res = if(segname=='Asia') u_in[country_name%in%country_asia, ] else
#      if(segname=='West') u_in[country_name%in%country_west, ] else
#      if(segname=='Developed Europe') u_in[country_name%in%country_deveuro, ] else
#      if(segname%in%u_in$sector) u_in[sector==segname, ] else
#      if(segname%in%u_in$country_name) u_in[country_name==segname, ]
#
#    return(res[order(mcap, decreasing=TRUE), ][1:min(nrow(res), topn), ])
#}

# u = D$u[,.SD[1,], by=focus]
# D=D_STOCKS; u = d_stocks
# D_in=ds; u=stock_segment(ds_in$u, segstock, n_stock); smart=TRUE
# D_in=list(h=tss); u=uu; smart=TRUE
#pre_screen = function(D_in, u, smart = FALSE){
#    u1 = u[ticker%in%colnames(D_in$h), ]
#    h1 = D_in$h[, u1$ticker]
#    if(smart){
#        h1 = h_to_log(h1)
##        rng = as.numeric(t(foreach(i=1:ncol(h1),.combine=cbind)%do%sum(abs(range(apply.yearly(h1[,i], sum))) < 0.6)))
##        res = list(u=u1[rng==2, ], h=h1[, rng==2])
#        res = list(u=u1, h=h1)
#        
#        return(res)
#    }
#    
#    return(list(u=u1, h=h1))
#}

rebals_func = function(period) return(if(period=='year') apply.yearly else if(period=='quarter') apply.quarterly else if(period=='month') apply.monthly else stop("Unknown period"))

get_rebals = function(D, period){
    f = rebals_func(period)
    return(as.Date(index(f(D$h[,1], FUN=length))))
}    

get_rebals_h = function(h_in, period){
    f = rebals_func(period)
    return(as.Date(index(f(h_in[, 1] , FUN=length))))
}

# u=D$u; h=D$h; d0=as.Date("2018-03-01"); d1s=rebal_dates
#prorate_universe_multiple = function(u, h, d0, d1s){
#    return(rbindlist(foreach(d1=d1s)%do%prorate_universe(u, h, d0, d1)))
#}

#get_grish_zacks = function(){
#    D_STOCKS = get(load('/home/aslepnev/webhub/zacks_data.RData'))
#    u = fread('/home/aslepnev/git/ej/grish_uni.csv')
#    u$ticker = as.character(t(as.data.table(strsplit(u$ticker, ' ')))[, 1])
#    D_STOCKS$u = u[D_STOCKS$u, on='ticker'][!is.na(country), ][, head(.SD, 1), by='ticker']
#    return(D_STOCKS)
#}

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

