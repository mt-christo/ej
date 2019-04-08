# ds_in=ds; de_in=de; segetf='Health Care'; n_etfs_uni=15; n_etfs=3; segstock='Health Care'; n_stock_uni=60; n_stock=2; vt=0.4
# ds_in=ds; de_in=de; segetf='Asia'; n_etfs=15; segstock='Asia'; n_stock=65; vt=0.3
# d_etf=pre_screen(de, etf_segment(de$u, 'Asia', 15), smart=TRUE); d_stock=pre_screen(ds, stock_segment(ds$u, 'Asia', 15), smart=TRUE); n_etfs=3; n_stock=3; vt=0.3; wmin=0.15; wmax=0.6
index_vt_pridex_segment = function(d_etf, d_stock, n_etfs, n_stock, vt, wmin, wmax){
    d_stock1 = d_stock
    d_stock1$u = d_stock1$u[!ticker%in%d_etf$u$ticker, ]
    d_stock1$h = d_stock1$h[, !d_stock$u$ticker%in%d_etf$u$ticker]  # In case some stocks are ETFs at the same time - we want to avoid duplicates

#    b_etf = best_pridex_basket(baskets_vol_range(d_etf, n_etfs, volparams=list(wnd=250, min=vt-0.2, max=vt+0.2))$baskets, d_etf$h)  # Basket with highest Pridex metric within this Volatility range
    b_etf = baskets_vol_range(d_etf, n_etfs, volparams=list(wnd=250, min=vt-0.2, max=vt+0.2))  # All baskets within Volatility range witn Volatility and Lowest Market cap info
    b_stock = baskets_vol_range(d_stock1, n_stock, volparams=list(wnd=250, min=vt-0.1, max=vt+0.2))  # All baskets within Volatility range witn Volatility and Lowest Market cap info

    stock_closest_baskets = order(abs(vt - b_stock$sds))[1:min(length(b_stock$sds), 10)]  # 10 baskets with vol closest to target
    stock_best_basket = b_stock$baskets[stock_closest_baskets[which.max(b_stock$mcaps[stock_closest_baskets])], ]  # Basket with highest Low Market Cap
    b_stock = list(basket=stock_best_basket, weights=array(1/n_stock, n_stock))
    
    etf_closest_baskets = order(abs(vt - b_etf$sds))[1:min(length(b_etf$sds), 10)]  # 10 baskets with vol closest to target
    etf_best_basket = b_etf$baskets[etf_closest_baskets[which.max(b_etf$mcaps[etf_closest_baskets])], ]  # Basket with highest Low Market Cap
    b_etf = list(basket=etf_best_basket, weights=array(1/n_etfs, n_etfs))

    hcom = xts_cbind_idx(d_etf$h[, b_etf$basket], d_stock1$h[, b_stock$basket])
    wcom = c(b_etf$weights, b_stock$weights)/(sum(b_etf$weights) + sum(b_stock$weights))
    wcom = optim_sigma(tail(hcom, 500), list(target=vt, wmin=wmin, wmax=wmax))
    print(paste('Volatility:', basket_vol(tail(hcom, 120), wcom), 'Performance:', tail(basket_perf(hcom, wcom), 1)))
    return(list(basket=c(b_etf$basket, b_stock$basket), weights=wcom, perf=basket_perf(hcom, wcom), vol250=basket_vol(tail(hcom, 250), wcom), vol120=basket_vol(tail(hcom, 120), wcom)))
}

# d_etf=pre_screen(de, etf_segment(de$u, 'Asia', 10), smart=TRUE); d_stock=uni_skip_countries(pre_screen(ds, stock_segment(ds$u, 'Asia', 10), smart=TRUE), c('KR')); n_etfs=2; n_stock=4; vt=0.30; wmin=0.1; wmax=0.3
# d_etf=pre_screen(de, etf_segment(de$u, etf_focus, n_etfs), smart=TRUE); d_stock=pre_screen(ds, stock_segment(ds$u, stock_focus, n_stocks), smart=TRUE); n_etfs=wo_params$etf_count; n_stock=wo_params$stock_count; vt=wo_params$vt; wmin=wo_params$minw; wmax=wo_params$maxw
index_vt_pridex_segment_similar = function(d_etf, d_stock, n_etfs, n_stock, vt, wmin, wmax){
    d_stock1 = d_stock
    d_stock1$u = d_stock1$u[!ticker%in%d_etf$u$ticker, ]
    d_stock1$h = d_stock1$h[, !d_stock$u$ticker%in%d_etf$u$ticker]  # In case some stocks are ETFs at the same time - we want to avoid duplicates
    d_all = list()
    d_all$h = cbind(d_etf$h, d_stock1$h)
    colnames(d_all$h) = c(colnames(d_etf$h), colnames(d_stock1$h))
    
    b_etf = baskets_vol_range(d_etf, n_etfs, volparams=list(wnd=250, min=vt-0.3, max=vt+0.2))  # All baskets within Volatility range witn Volatility and Lowest Market cap info
    b_stock = baskets_vol_range(d_stock1, n_stock, volparams=list(wnd=250, min=vt-0.2, max=vt+0.2))  # All baskets within Volatility range witn Volatility and Lowest Market cap info

    stock_best_baskets = b_stock$baskets[order(abs(vt - b_stock$sds))[1:min(length(b_stock$sds), 100)], ]  # 20 baskets with vol closest to target
    etf_best_baskets = b_etf$baskets[order(abs(vt - b_etf$sds))[1:min(length(b_etf$sds), 100)], ]  # 20 baskets with vol closest to target
    
    d_allh = d_all$h[1:max(which(rowSums(is.na(d_all$h))==0)), ]
    d_allh_tail = tail(d_allh, 120)
    w_all = array(1/(n_stock+n_etfs), n_stock+n_etfs)
    max_count = min(nrow(stock_best_baskets), nrow(etf_best_baskets))
    sds = foreach(i=1:max_count, .combine=c)%do%basket_vol(d_allh_tail[, c(stock_best_baskets[i,], etf_best_baskets[i,])], w_all)
#    perfs = foreach(i=1:max_count, .combine=c)%do%as.numeric(tail(basket_perf(d_allh[, c(stock_best_baskets[i,], etf_best_baskets[i,])], w_all), 1))
    
    best_i = which.min(abs(vt - sds))
    best_basket = c(stock_best_baskets[best_i,], etf_best_baskets[best_i,])
    hcom = d_allh[, best_basket]
    wcom = optim_sigma(tail(hcom, 500), list(target=vt, wmin=wmin, wmax=wmax))
    print(paste('Volatility:', basket_vol(tail(hcom, 120), wcom), 'Performance:', tail(basket_perf(hcom, wcom), 1)))
    return(list(basket=best_basket, weights=wcom, perf=basket_perf(hcom, wcom), vol250=basket_vol(tail(hcom, 250), wcom), vol120=basket_vol(tail(hcom, 120), wcom)))
}

send_csv_to_email = function(data, data_name, subject, eaddress){
    filepath = paste0('/tmp/', data_name, '.csv')
    fwrite(data, filepath)
    send_attach_to_email(filepath, paste0(data_name, '.csv'), subject, eaddress)
}

# tab=basket; filename='basket.pdf'
save_data_as_pdf = function(tab, filename){
    filepath = paste0('/home/aslepnev/webhub/', filename)
    pdf(filepath); grid.table(tab, rows=NULL); dev.off()
    return(filepath)
}

save_data_as_csv = function(tab, filename){
    filepath = paste0('/home/aslepnev/webhub/', filename)
    fwrite(tab, file=filepath)
    return(filepath)
}

save_data_as_chart = function(my_chart, chart_title, filename){
    filepath = paste0('/home/aslepnev/webhub/', filename)
    png(filepath); print(plot(my_chart, main=chart_title)); dev.off()
    return(filepath)
}

send_files_to_email = function(filepaths, subject, eaddress){
    send.mail(from = 'novoxservice@gmail.com', to = eaddress, subject=subject, body = subject, encoding = "utf-8", smtp = list(host.name = "smtp.gmail.com", port = 465, user.name="novoxservice@gmail.com", passwd=readChar('/home/aslepnev/webhub/xservice', nchars=20), ssl=TRUE), authenticate = TRUE, send = TRUE , attach.files = c(filepaths), html = TRUE, inline = TRUE )
}

#send_attach_to_email = function(filepath, filename, subject, eaddress){
#    system(paste0('echo -e "to: ', eaddress, ' \nsubject: ', subject, '\n"| (cat && uuencode ', filepath, ' ', filename, ') | ssmtp ', eaddress))
#}

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

# index_data = build_index_prorate(list(main=DD), get_rebals(DD, 'quarter'), screen_mixed_top, list(price_window=40), '2012-12-31'); params = list(index_excess=0.035, vc_params=list(window=20, type='max 10', level=0.14, max_weight=1.5, rfr=0.02))
index_report = function(index_data, params, libors){
    baskets = foreach(x=index_data, .combine=rbind)%do%cbind(data.table(dt=x$dt), t(x$basket$main$names))
    names(baskets) = c('dt', paste('position', 1:(ncol(baskets)-1)))
    r = foreach(x=index_data, .combine=rbind)%do%x$h
    if('vc_params'%in%names(params))
        r = volcontrol_excess(r, params$vc_params, libors)
    perf = lag(exp(cumsum(r)), 1)
    perf[1, 1] = 1
    vty = sd(tail(r, 250))*sqrt(252)
    res = list(perf=perf, endPerf=as.numeric(tail(perf, 1)), volatility=vty, baskets=baskets)
    return(res)
}

process_index_straight_request = function(params){
    u = uni_from_params(params)
    index_data = build_index_prorate(list(main=u), get_rebals(u, 'quarter'), get(params$screen_params$func), params$screen_params, params$index_start)
    res = index_report(index_data, params)
    return(res)
}

process_index_stocks_etfs_request = function(params){
    for(name in c('n_etfs', 'n_stocks', 'vt', 'min_weight', 'max_weight'))
        params[[name]] = as.numeric(params[[name]])
    for(name in c('etf_count', 'stock_count'))
        params$uni_params[[name]] = as.numeric(params$uni_params[[name]])
    u = uni_from_params(params$uni_params)

    p1 = index_vt_pridex_segment(u$etfs, u$stocks, params$n_etfs, params$n_stocks, params$vt, params$min_weight, params$max_weight)
    basket = rbind(de$u[, .(ticker, name, sectype='ETF')],
                   ds$u[!ticker%in%de$u, .(ticker, name, sectype='Stock')])[data.table(ticker=p1$basket, weight=p1$weights), on='ticker']
    basket[, weight:=fracperc(weight, 2)]
    colnames(basket) = c('Ticker', 'Name', 'Security Type', 'Weight')
#    send_files_to_email(c(save_data_as_pdf(basket, 'basket.pdf'),
#                          save_data_as_chart(p1$perf, paste0('Basket performance \n1 year vol: ', fracperc(p1$vol250, 0)), 'chart.png')),
#                        'Asia index', 'aslepnev@novo-x.info')
    p1[['endPerf']] = as.numeric(tail(p1$perf, 1))
    p1[['basket']] = basket
    
    return(p1)
}
