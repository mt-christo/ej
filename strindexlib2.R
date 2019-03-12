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


    
    D_STOCKS = get(load('/home/aslepnev/webhub/zacks_data.RData'))
    u = fread('/home/aslepnev/git/ej/grish_uni.csv')
    u$ticker = as.character(t(as.data.table(strsplit(u$ticker, ' ')))[, 1])
    D_STOCKS$u = u[D_STOCKS$u, on='ticker'][!is.na(country), ][, head(.SD, 1), by='ticker']
    d_stocks = D_STOCKS$u  #head(D_STOCKS$u, 100)  # 500 biggest companies
    d_stocks[, .N, by='country']
    d_stocks[, .N, by='gics_code']

    pre_screen(D_STOCKS, d_stocks)$h[1,]

    data = get_grish_zacks()

    d_stocks = D_STOCKS$u[gics_code%in%c(45, 50), ]
    gen_file_pack(pre_screen(data, data$u[gics_code%in%c(45, 50), ]), list(window=40, voltarget=0.3, minw=0.01, maxw=0.3, force_us=FALSE, main=list(UNI=10, window=40)), 'quarter', 'SOLVIT')


    D_STOCKS = get(load('/home/aslepnev/webhub/grish_asia.RData'))
    d_stocks = D_STOCKS$u  #[1:30, ]
    uni_in = pre_screen(D_STOCKS, d_stocks)
    gen_file_pack(D_STOCKS, d_stocks, list(window=40, voltarget=0.4, minw=0.01, maxw=0.3, force_us=FALSE, main=list(UNI=10, window=40)), 'quarter', 'SOLCA')


    
    
    data = get_grish_zacks()
    #gen_file_pack(, list(window=40, voltarget=0.3, minw=0.01, maxw=0.3, force_us=FALSE, main=list(UNI=10, window=40)), 'quarter', 'SOLVIT')

    gics_dict = data.table(t(matrix(c('energy',10,
                                      'materials',15,
                                      'industrials',20,
                                      'discret',25,
                                      'staples',30,
                                      'health',35,
                                      'finance',40,
                                      'IT',45,
                                      'telecom',50,
                                      'utilities',55,
                                      'estate',60),2))); colnames(gics_dict) = c('name', 'code')
    gics_codes = data$u[!is.na(gics_code), unique(gics_code)]
    gics_idcs = foreach(g=gics_dict$code)%do%{
        uni_in = pre_screen(data, data$u[gics_code == g, ])
        screen_params = list(window=40, voltarget=0.3, minw=0.01, maxw=0.3, force_us=FALSE, main=list(UNI=10, window=40))
        build_index(list(main=uni_in), rebal_dates=get_rebals(uni_in, 'quarter'), screen_voltarget, screen_params, '2012-01-01')
    }
    liners = foreach(y=gics_idcs,.combine=cbind)%do%{ foreach(x=y,.combine=rbind)%do%x$h }
    names(liners) = gics_dict$name
    comat = round(cor(liners)*100, 0)
    pwcom = data.table(melt(comat))[, head(.SD, 1), by='value'][order(value), ][Var1!=Var2, ][, .(pair=paste(Var1, Var2), value)]
    plot(pwcom$value)
    


    heatmap(comat, Colv=NA, Rowv=NA, scale='column')
    comat = cor(liners)
    plotcorr(comat, col=colorRampPalette(brewer.pal(5, 'Spectral'))(100)[comat*150+10], mar=c(1,1,1,1))
    +  geom_tile(aes(fill=value)) + geom_text(aes(label=value) + scale_fill_gradient(low='white', high='red'))

    
    foreach(i=1:ncol(liners),.combine=c)%do%(sd(tail(liners[,i], 120))*sqrt(252)*100)

    send_data = data.table(round(cor(liners), 2)); data=cbind(colnames(data), data)
    send_attach(send_data, 'gics_correlations', 'GICS sector index correlations', 'antonslepnev@gmail.com')    
    

    
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







    

    
    screen_params = list(window=40, voltarget=0.4, minw=0.01, maxw=0.3, force_us=FALSE, main=list(UNI=10, window=40))
    res = build_index(list(main=pre_screen(D_STOCKS, d_stocks)), rebal_dates=get_rebals(D_STOCKS, 'quarter'), screen_voltarget, screen_params)
    save(res, file='/home/aslepnev/data/idx6_custom_asia.Rdata')
    r = foreach(x=res,.combine=rbind)%do%x$h; print(sd(tail(r,120))*sqrt(252))
    perf = exp(cumsum(r))

    p = res[[41]]$basket$main$names
    w = res[[41]]$basket$main$weights
    h = D_STOCKS$h
    h = h[, p]
    h = na.fill(diff(log(1+na.locf(h))), 0)
    for(j in 1:ncol(h))
        h[abs(h[,j])>0.5, j] = 0
    h = h[index(res[[41]]$h), ]
    a = exp(cumsum(basket_ret(h, w)))
    b = perf[index(perf)>='2018-10-01', ]
    as.numeric(tail(a,1))/as.numeric(head(a,1))
    as.numeric(tail(b,1))/as.numeric(head(b,1))


    screen_params = list(window=40, voltarget=0.5, minw=0.02, maxw=0.4, main=list(UNI=10, window=40))
    res = build_index(list(main=pre_screen(D_STOCKS, d_stocks)), rebal_dates=get_rebals(D_STOCKS, 'quarter'), screen_pridex_voltarget, screen_params)
    baskets = foreach(x=res,.combine=rbind)%do%cbind(data.table(dt=x$dt), t(x$basket$main$names))
    weights = foreach(x=res,.combine=rbind)%do%cbind(data.table(dt=x$dt), t(round(x$basket$main$weights, 3)))
    liner = exp(cumsum(foreach(x=res,.combine=rbind)%do%x$h))
    fwrite(data.table(dt=index(liner), value=liner), '/home/aslepnev/git/ej/liner_SOLCA_10.csv')
    fwrite(baskets, '/home/aslepnev/git/ej/baskets_SOLCA_10.csv')
    fwrite(weights, '/home/aslepnev/git/ej/weights_SOLCA_10.csv')




    
    plot(exp(cumsum(foreach(x=get(load('/home/aslepnev/data/idx5_custom1.Rdata')),.combine=rbind)%do%x$h)))

    
    screen_params = list(window=40, voltarget=0.3, minw=0.02, maxw=0.8, etfs=list(N=4, UNI=20, window=40), stocks=list(UNI=10, window=40))
    res = build_index(list(etfs=pre_screen(D_ETF, d_etf), stocks=pre_screen(D_STOCKS, d_stocks)), get_rebals(D_ETF, 'month'), screen_pridex_voltarget, screen_params)
    save(res, file='/home/aslepnev/data/idx5_custom2.Rdata')
    r = foreach(x=res,.combine=rbind)%do%x$h; print(sd(tail(r,120))*sqrt(252))
    perf = exp(cumsum(r))


    screen_params = list(window=40, voltarget=0.3, minw=0.02, maxw=0.8, etfs=list(N=4, UNI=20, window=40), stocks=list(UNI=10, window=40))
    res = build_index(list(etfs=pre_screen(D_ETF, d_etf), stocks=pre_screen(D_STOCKS, d_stocks)), get_rebals(D_ETF, 'month'), screen_pridex_voltarget, screen_params)
    save(res, file='/home/aslepnev/data/idx5_custom2.Rdata')
    r = foreach(x=res,.combine=rbind)%do%x$h; print(sd(tail(r,120))*sqrt(252))
    perf = exp(cumsum(r))
    
    plot(exp(cumsum(foreach(x=get(load('/home/aslepnev/data/idx5_custom2.Rdata')),.combine=rbind)%do%x$h)))


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



e = pre_screen(de, de$u[)


 1:                        United States 2560
 2:                        International  426
 3:                               Global  340
 4:                           California   33
 5:                      European Region   19
 6:                             New York   18
 7:                                Japan   13
 8:                 Asian Pacific Region    8
 9:                                China    7
10:        Asian Pacific Region ex Japan    7
11:                         Pennsylvania    6
12:                            Minnesota    5
13:                                 N.A.    5
14:                               Canada    4
15:                           New Jersey    4
16:                                 Ohio    4
17:                             Eurozone    3
18:                             Virginia    3
19:                        Massachusetts    2
20:                               Russia    2
21:                Latin American Region    2
22:                               Oregon    2
23:                        Greater China    2
24:                             Missouri    2
25:                North American Region    2
26:                             Michigan    2
27:                               Brazil    1
28:                         Tiger Region    1
29: Asian Pacific Region,European Region    1
30:                          South Korea    1
31:                               Taiwan    1
32:                            Hong Kong    1
33:                              Germany    1
34:                             Maryland    1
35:                       United Kingdom    1
36:                            Australia    1
37:            European Region,Australia    1
38:                               Mexico    1
39:              Greater China,Hong Kong    1
40:                 Global,United States    1
41:                                Spain    1
42:                             Kentucky    1
43:                              Arizona    1
44:                          Switzerland    1
45:                                 EMEA    1
46:                       North Carolina    1
47:                                India    1
48:                               Hawaii    1
49:                             Colorado    1
50:                            Indonesia    1
51:                  European Reg. ex UK    1
52:                               France    1
53:                            Singapore    1
54:                       OECD Countries    1
55:                               Turkey    1
56:                             Malaysia    1
57:                             Thailand    1
58:                         South Africa    1
59:                                Chile    1
                               geo_focus    N



de$u[geo_focus%in%geo_focus_deveuro, ]

u_in = de$u


source('/home/aslepnev/git/ej/strindexlib.R')

#ds <<- get(load('/home/aslepnev/webhub/grish_iter0.RData'))
ds = get(load('/home/aslepnev/webhub/grish_asia.RData'))
de = get(load('/home/aslepnev/webhub/sacha_etf_yhoo.RData'))

p1 = index_vt_pridex_segment(pre_screen(de, etf_segment(de$u, 'Asia', 10), smart=TRUE),
                             pre_screen(ds, stock_segment(ds$u, 'Asia', 10), smart=TRUE), 2, 3, 0.3, 0.1, 0.6)
basket = rbind(de$u[, .(ticker, name, sectype='ETF')], ds$u[!ticker%in%de$u, .(ticker, name, sectype='Stock')])[data.table(ticker=p1$basket, weight=p1$weights), on='ticker']
basket[, weight:=fracperc(weight, 2)]
colnames(basket) = c('Ticker', 'Name', 'Security Type', 'Weight')
send_files_to_email(c(save_data_as_pdf(basket, 'basket.pdf'),
                      save_data_as_chart(p1$perf, paste0('Basket performance \n1 year vol: ', fracperc(p1$vol250, 0)), 'chart.png')),
                    'Asia index', 'aslepnev@novo-x.info')



p1 = index_vt_pridex_segment(pre_screen(de, etf_segment(de$u, 'Technology', 15), smart=TRUE),
                             pre_screen(ds, stock_segment(ds$u, 'Information Technology', 20), smart=TRUE), 3, 2, 0.4, 0.15, 0.6)
basket = rbind(de$u[, .(ticker, name, sectype='ETF')], ds$u[!ticker%in%de$u, .(ticker, name, sectype='Stock')])[data.table(ticker=p1$basket, weight=p1$weights), on='ticker']
basket[, weight:=fracperc(weight, 2)]
colnames(basket) = c('Ticker', 'Name', 'Security Type', 'Weight')
end_files_to_email(c(save_data_as_pdf(basket, 'basket.pdf'),
                      save_data_as_chart(p1$perf, paste0('Basket performance \n1 year vol: ', fracperc(p1$vol250, 0)), 'chart.png')),
                    'Asia index', 'aslepnev@novo-x.info')

> ds$u[,unique(sector)]
 [1] "Information Technology" "Consumer Discretionary" "Communication Services"
 [4] "Financials"             "Health Care"            "Energy"                
 [7] "#N/A N/A"               "Consumer Staples"       "Industrials"           
[10] "Materials"              "Utilities"              "Real Estate"           
> de$u[,unique(ind_focus)]
 [1] "N.A."                   "Communications"         "Technology"            
 [4] "Health Care"            "Real Estate"            "Financial"             
 [7] "Energy"                 "Consumer Discretionary" "Industrials"           
[10] "Consumer Staples"       "Utilities"              "Thematic"              
[13] "Materials"             


library(mailR)

# etf_focus='Technology'; stock_focus='Information Technology'; wo_params = list(etf_count=3, stock_count=2, vt=0.4, minw=0.15, maxw=0.6)
send_vol_basket = function(etf_focus, stock_focus, wo_params){
    p1 = index_vt_pridex_segment(pre_screen(de, etf_segment(de$u, etf_focus, 25), smart=TRUE),
                                 pre_screen(ds, stock_segment(ds$u, stock_focus, 40), smart=TRUE), wo_params$etf_count, wo_params$stock_count, wo_params$vt, wo_params$minw, wo_params$maxw)
    basket = rbind(de$u[, .(ticker, name, sectype='ETF')], ds$u[!ticker%in%de$u, .(ticker, name, sectype='Stock')])[data.table(ticker=p1$basket, weight=p1$weights), on='ticker']
    basket[, weight:=fracperc(weight, 2)]
    colnames(basket) = c('Ticker', 'Name', 'Security Type', 'Weight')
    send_files_to_email(c(save_data_as_pdf(basket, 'basket.pdf'),
                         save_data_as_chart(p1$perf, paste0('Basket performance \n1 year vol: ', fracperc(p1$vol250, 0)), 'chart.png')),
                       paste(etf_focus, stock_focus), 'aslepnev@novo-x.info')
}

send_vol_basket('Technology', 'Information Technology', list(etf_count=2, stock_count=3, vt=0.4, minw=0.15, maxw=0.6))
send_vol_basket('Health Care', 'Health Care', list(etf_count=2, stock_count=3, vt=0.4, minw=0.15, maxw=0.6))



p1$perf

send_xts_plot_to_email(1:10, 'chart.png', 'Please see chart attacheeeed', 'aslepnev@novo-x.info')

h = pre_screen(ds, stock_segmment(ds$u, 'Information Technology', 100), smart=TRUE)$h

u=stock_segment(ds$u, 'Information Technology', 100)


res = build_index(list(main=), rebal_dates=get_rebals(uni_in, rebal_freq), screen_voltarget, screen_params, start_date)





foreach(i=1:ncol(h),.combine=c)%do%{ sqrt(250)*sd(h[,i]) }



d_etf = pre_screen(de, etf_segment(de$u, 'Health Care', 15), smart=TRUE)
d_stock = pre_screen(ds, ds$u[sector=='Health Care', ][1:75, ], smart=TRUE)

b_etf = best_pridex_basket(baskets_vol_range(d_etf, 3, volparams=list(wnd=500, min=0.2, max=0.3)), d_etf$h)
b_stock = best_pridex_basket(baskets_vol_range(d_stock, 3, volparams=list(wnd=500, min=0.4, max=0.5)), d_stock$h)

hcom = xts_cbind_idx(d_etf$h[, b_etf$basket], d_stock$h[, b_stock$basket])
wcom = c(b_etf$weights, b_stock$weights)/sum(b_etf$weights+b_stock$weights)
wcom = optim_sigma(tail(hcom, 500), list(target=0.4, wmin=0.1, wmax=0.6))

basket_vol(tail(hcom, 500), wcom)
basket_perf(tail(hcom, 500), wcom)


basket_ret = function(h, w) return( xts(log(((exp(h)-1)%*%w) + 1), order.by=index(h)) )
basket_vol = function(h, w) return( sd(basket_ret(h, w))*sqrt(252) )



u = DD$u
DD$u = DD$u[!ticker%in%c('V', 'MA'), ]


source('/home/aslepnev/git/ej/strindexlib.R')
DD = get(load('/home/aslepnev/git/ej/it_top10_uni.RData'))  # DD$h = DD$h[, unique(DD$u$ticker)]; save(DD, file='/home/aslepnev/git/ej/it_top10_uni.RData')
#x = fread('/home/aslepnev/webhub/libor3m.csv'); libors = as.xts(as.numeric(x$libor3m), order.by=as.Date(x$date)); 
res = build_index_prorate(list(main=DD), get_rebals(DD, 'quarter'), prorate_uni, list(window=40), '2012-12-31')
r = foreach(x=res,.combine=rbind)%do%x$h; print(sd(tail(r,120))*sqrt(252))
perf = exp(cumsum(r))
tail(perf, 1)

h_res = foreach(x=res,.combine=rbind)%do%x$h
rfr = 0.02/252
excess_return = 0.035/252
res_vc = exp(cumsum(-excess_return + volcontrol(-rfr + h_res, list(window=20, maxvolwindow=10, level=0.01*14, max_weight=1.5))))
#res_vc = exp(cumsum(-excess_return + volcontrol(-rfr + h_res, list(window=20, level=0.01*14, max_weight=1.75))))
#res_vc = exp(cumsum(-excess_return + volcontrol(-rfr + h_res, list(window=20, avgvolwindow=10, level=0.01*14, max_weight=1.5))))

calc_env = list(uni_filename = '/home/aslepnev/git/ej/it_top10_uni.RData',
                screen_func = screen_mixed_top,
                screen_window = 40,
                index_start = '2012-12-31',
                vc_window = 20,
                vc_level = 0.14,
                vc_max_weight = 1.5,
                vc_type = 'max 10',
                vc_rfr = 0.02,
                vc_excess_type = 'rate-related excess',
                vc_excess = 0.0)
#                vc_excess = 0.035)

                
library(mailR)
source('/home/aslepnev/git/ej/strindexlib.R')
DD = get(load('/home/aslepnev/git/ej/it_top10_uni.RData'))

index_data = build_index_prorate(list(main=uni_skip_tickers(DD, c('V', 'MA'))), get_rebals(DD, 'quarter'), screen_mixed_top, list(price_window=40), '2012-12-31')
params = list(vc_params=list(window=20, type='max 10', excess_type = 'rate-related excess', excess=2.65, level=0.14, max_weight=1.5, rfr=0.02))
libors = DD$libor

idx = index_report(build_index_prorate(list(main=uni_skip_tickers(DD, c('V', 'MA'))), get_rebals(DD, 'quarter'), screen_mixed_top, list(price_window=40), '2012-12-31'),
#                   list(vc_params=list(window=20, type='max 10', excess_type = 'simple excess', excess=0.035, level=0.14, max_weight=1.5, rfr=0.02)),
                   list(vc_params=list(window=20, type='max 10', excess_type = 'rate-related excess', excess=2.65, level=0.14, max_weight=1.5, rfr=0.02)),
                   DD$libor)
#                   list(vc_params=list(window=20, type='max 10', excess_type = 'simple excess', excess=0.035, level=0.14, max_weight=1.5, rfr=0.02)))
tail(idx$perf, 1)

send_files_to_email(c(save_data_as_csv(idx$baskets, 'top10it_baskets.csv'),
                      save_data_as_csv(idx$perf, 'top10it_perf.csv'),
                      save_data_as_chart(idx$perf, 'Top 10 IT companies, quarterly', 'top10it.png')),
                    'Top 10 IT index', 'aslepnev@novo-x.info')

send_files_to_email(c(save_data_as_csv(DD$u, 'universe.csv')),
                    'Top 10 IT index', 'aslepnev@novo-x.info')





asia        
west 
deveuro 


#u1 = get(load('/home/aslepnev/webhub/grish_iter0.RData'))$u
#u1 = u1[, .(country_code=max(country_code)), by='country_name']
ds = get(load('/home/aslepnev/webhub/grish_asia.RData'));  # save(ds, file='/home/aslepnev/webhub/grish_asia.RData')
u1 = data.table(country_name=c("KOREA", "HONG KONG", "JAPAN", "TAIWAN"), country_code=c("KR", "HK", "JP", "TW"))
ds$u = u1[ds$u, on=.(country_code==country)]


 [1] "UNITED STATES" "CHINA"         "SWITZERLAND"   "FRANCE"       
 [5] "GERMANY"       "IRELAND"       "AUSTRALIA"     "INDIA"        
 [9] "CANADA"        "BRITAIN"       "NORWAY"        "NETHERLANDS"  
[13] "SPAIN"         "SAUDI ARABIA"  "SINGAPORE"     "PERU"         
[17] "SWEDEN"        "LUXEMBOURG"    "ITALY"         "RUSSIA"       
[21] "SOUTH AFRICA"  "ISRAEL"        "INDONESIA"     "PHILIPPINES"  
[25] "AUSTRIA"       "UAE"           "ARGENTINA"     "CURACAO"      
[29] "THAILAND"      "BERMUDA"       "BELGIUM"       "HONG KONG"    
[33] "DENMARK"       "BRAZIL"        "COLOMBIA"      "QATAR"        
[37] "POLAND"        "BANGLADESH"    "MALAYSIA"      "NEW ZEALAND"  
[41] "CHILE"         "ROMANIA"       "PUERTO RICO"   "VIETNAM"      


geo_focus    N

          geo_focus2    N
 1:    North America 1770
 2:             N.A.  982
 3: Developed Market  533
 4:           Global  173
 5:  Emerging Market   31
 6:    Emerging Asia    8
 7:  Emerging Europe    7
 8:    Latin America    4
 9:             Asia    3
10: Developed Europe    3

                 ind_focus    N
 1:                   N.A. 3191
 2:         Communications    5
 3:             Technology   47
 4:            Health Care   41
 5:            Real Estate   70
 6:              Financial   23
 7:                 Energy   39
 8: Consumer Discretionary    7
 9:            Industrials   10
10:       Consumer Staples    7
11:              Utilities   11
12:               Thematic   50
13:              Materials   13
