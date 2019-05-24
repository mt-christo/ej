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

dsds = pre_screen(ds, stock_segment(ds$u, 'Asia', 20), smart=TRUE)
b = get(load('/home/aslepnev/webhub/uber_uni_h.RData'))
dsds$u = dsds$u[ticker%in%colnames(b),]
dsds$h = b[, dsds$u$ticker]
fwrite(

p1 = index_vt_pridex_segment_similar(pre_screen(de, etf_segment(de$u, 'Asia', 20), smart=TRUE),
                                     dsds, 3, 1, 0.30, 0.15, 0.3)
p1 = index_vt_pridex_segment_similar(pre_screen(de, etf_segment(de$u, 'Asia', 20), smart=TRUE),
                                     uni_skip_countries(pre_screen(ds, stock_segment(ds$u, 'Asia', 20), smart=TRUE), c('KR')), 3, 3, 0.30, 0.15, 0.3)
basket = rbind(de$u[, .(ticker, name, sectype='ETF')], ds$u[!ticker%in%de$u, .(ticker, name, sectype='Stock')])[data.table(ticker=p1$basket, weight=p1$weights), on='ticker']
basket[, weight:=fracperc(weight, 2)]
colnames(basket) = c('Ticker', 'Name', 'Security Type', 'Weight')
send_files_to_email(c(save_data_as_pdf(basket, 'basket.pdf'),
                     save_data_as_csv(basket, 'basket.csv'),
                     save_data_as_chart(p1$perf, paste0('Basket performance \n6m vol: ', fracperc(p1$vol120, 0)), 'chart.png')),
                    'Asia index', 'aslepnev@novo-x.info')

pre_screen(de, etf_segment(de$u, 'Asia', 20), smart=TRUE)$u[1:20, ticker]

uh = xts_cbind_idx(pre_screen(de, etf_segment(de$u, 'Asia', 20), smart=TRUE)$h, uni_skip_countries(pre_screen(ds, stock_segment(ds$u, 'Asia', 20), smart=TRUE), c('KR'))$h)
uh = xts_cbind_idx(pre_screen(de, etf_segment(de$u, 'Asia', 200), smart=TRUE)$h, uni_skip_countries(pre_screen(ds, stock_segment(ds$u, 'Asia', 200), smart=TRUE), c('KR'))$h)

p1$basket[6] = '27 HK Equity'
p1$basket[5] = 'EWJ'


stocks = c('2454 TT', '388 HK', '27 HK')

basket_vol(tail(uh[, p1$basket], 90), p1$weights)



p1 = index_vt_pridex_segment(pre_screen(de, etf_segment(de$u, 'Technology', 15), smart=TRUE),
                             pre_screen(ds, stock_segment(ds$u, 'Information Technology', 20), smart=TRUE), 3, 2, 0.4, 0.15, 0.6)
basket = rbind(de$u[, .(ticker, name, sectype='ETF')], ds$u[!ticker%in%de$u, .(ticker, name, sectype='Stock')])[data.table(ticker=p1$basket, weight=p1$weights), on='ticker']
basket[, weight:=fracperc(weight, 2)]
colnames(basket) = c('Ticker', 'Name', 'Security Type', 'Weight')
send_files_to_email(c(save_data_as_pdf(basket, 'basket.pdf'),
                     save_data_as_csv(basket, 'basket.csv'),
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
# etf_focus='Technology'; stock_focus='Information Technology'; wo_params = list(etf_count=3, stock_count=2, vt=0.4, minw=0.15, maxw=0.6)
# etf_focus='Developed Europe'; stock_focus='Developed Europe'; wo_params = list(etf_count=3, stock_count=3, vt=0.3, minw=0.15, maxw=0.6); n_etfs=50; n_stocks=80
send_vol_basket = function(etf_focus, n_etfs, stock_focus, n_stocks, wo_params){
    p1 = index_vt_pridex_segment_similar(pre_screen(de, etf_segment(de$u, etf_focus, n_etfs), smart=TRUE),
                                 pre_screen(ds, stock_segment(ds$u, stock_focus, n_stocks), smart=TRUE), wo_params$etf_count, wo_params$stock_count, wo_params$vt, wo_params$minw, wo_params$maxw)
    basket = rbind(de$u[, .(ticker, name, sectype='ETF')], ds$u[!ticker%in%de$u$ticker, .(ticker, name, sectype='Stock')])[data.table(ticker=p1$basket, weight=p1$weights), on='ticker']
    basket[, weight:=fracperc(weight, 2)]
    colnames(basket) = c('Ticker', 'Name', 'Security Type', 'Weight')
    send_files_to_email(c(save_data_as_pdf(basket, 'basket.pdf'),
                         save_data_as_chart(p1$perf, paste0('Basket performance \n1 year vol: ', fracperc(p1$vol250, 0)), 'chart.png')),
                       paste(etf_focus, stock_focus), 'aslepnev@novo-x.info')
}

ds = get(load('/home/aslepnev/webhub/grish_2011_undated_uni.RData'))
de = get(load('/home/aslepnev/webhub/sacha_etf_yhoo.RData'))
send_vol_basket('Technology', 20, 'Information Technology', 20, list(etf_count=2, stock_count=3, vt=0.4, minw=0.15, maxw=0.4))
send_vol_basket('Health Care', 20, 'Health Care', 20, list(etf_count=2, stock_count=3, vt=0.4, minw=0.15, maxw=0.4))
send_vol_basket('Developed Europe', 20, 'Developed Europe', 20, list(etf_count=3, stock_count=3, vt=0.3, minw=0.15, maxw=0.4))
send_vol_basket('United States', 20, 'UNITED STATES', 20, list(etf_count=3, stock_count=3, vt=0.3, minw=0.15, maxw=0.4))



p1$perf
send_xts_plot_to_email(1:10, 'chart.png', 'Please see chart attacheeeed', 'aslepnev@novo-x.info')
h = pre_screen(ds, stock_segmment(ds$u, 'Information Technology', 100), smart=TRUE)$h
u=stock_segment(ds$u, 'Information Technology', 100)

res = build_index(list(main=), rebal_dates=get_rebals(uni_in, rebal_freq), screen_voltarget, screen_params, start_date)

foreach(i=1:ncol(h),.combine=c)%do%{ sqrt(250)*sd(h[,i]) }




source('/home/aslepnev/git/ej/strindexlib.R')
library(dplyr)
DD = get(load('/home/aslepnev/git/ej/it_top10_uni.RData'))
libors = DD$libor

x = fread('/home/aslepnev/webhub/Top10_IT_EquityIndex_CSV.csv')
x = xts(as.numeric(x$GTR), order.by=as.Date(x$Date))
colnames(x) = 'r'
x = diff(log(x[index(x)>='2012-12-31' & index(x)<='2019-02-04', ]/as.numeric(x[index(x)=='2012-12-31'])))[-1]

r = volcontrol_excess(x, list(window=c(20, 60), type='none', excess_type = 'simple excess', excess=3.5, level=0.14, max_weight=1.75), libors); print(sqrt(252)*sd(tail(r, 252))); print(tail(exp(cumsum(r)), 1))
r = volcontrol_excess(x, list(window=20, type='none', excess_type = 'simple excess', excess=3.5, level=0.14, max_weight=1.75), libors); print(sqrt(252)*sd(tail(r, 252))); print(tail(exp(cumsum(r)), 1))


x = fread('/home/aslepnev/webhub/smidai_index_solactive.csv')
x = xts(as.numeric(x$p), order.by=as.Date(x$date))
colnames(x) = 'r'
x = x[index(x)<='2019-02-14']
x3y = diff(log(x[index(x)>='2016-02-14', ]/as.numeric(x[index(x)=='2016-02-11'])))[-1]
x5y = diff(log(x[index(x)>='2014-02-14', ]/as.numeric(x[index(x)=='2014-02-14'])))[-1]

r = volcontrol_excess(x3y, list(window=20, type='simple', excess_type = 'fixed excess', excess=3.5, level=0.08, max_weight=2.5), libors); print(sqrt(252)*sd(tail(r, 252))); print(tail(exp(cumsum(r)), 1))
r = volcontrol_excess(x3y, list(window=20, type='max 5', excess_type = 'fixed excess', excess=3.5, level=0.08, max_weight=2.5), libors); print(sqrt(252)*sd(tail(r, 252))); print(tail(exp(cumsum(r)), 1))
r = volcontrol_excess(x5y, list(window=20, type='max 10', excess_type = 'fixed excess', excess=3.5, level=0.08, max_weight=2.5), libors); print(tail(exp(cumsum(r)), 1))


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




source('/home/aslepnev/git/ej/strindexlib.R')
DD = get(load('/home/aslepnev/git/ej/it_top10_uni.RData'))
libors = DD$libor
r1 = build_index_prorate(list(main=uni_skip_tickers(DD, c('V', 'MA'))), get_rebals(DD, 'quarter'), screen_mixed_top, list(price_window=40), '2012-12-31')
r1 = foreach(x=r1,.combine=rbind)%do%x$h
r1 = r1[index(r1)>='2012-12-31' & index(r1)<='2019-02-04', ]
r = volcontrol_excess(r1, list(window=c(20, 60), type='none', excess_type = 'simple excess', excess=3.5, level=0.12, max_weight=1.75), libors); print(sqrt(252)*sd(tail(r, 252))); print(tail(exp(cumsum(r)), 1))
r = volcontrol_excess(r1, list(window=20, type='none', excess_type = 'simple excess', excess=3.5, level=0.12, max_weight=1.75), libors); print(sqrt(252)*sd(tail(r, 252))); print(tail(exp(cumsum(r)), 1))


r2 = fread('/home/aslepnev/webhub/Top10_IT_EquityIndex_CSV.csv')
r2 = xts(as.numeric(r2$GTR), order.by=as.Date(r2$Date))
colnames(r2) = 'r'
r2 = diff(log(r2[index(r2)>='2012-12-31' & index(r2)<='2019-02-04', ]/as.numeric(r2[index(r2)=='2012-12-31'])))[-1]
r = volcontrol_excess(r2, list(window=c(20, 60), type='none', excess_type = 'simple excess', excess=3.5, level=0.14, max_weight=1.75), libors); print(sqrt(252)*sd(tail(r, 252))); print(tail(exp(cumsum(r)), 1))


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

fxx = get(load('/home/aslepnev/webhub/fxx.RData'))


    country_code   country_name    N
 1:           US  UNITED STATES 1282 USD
 2:           CH          CHINA  484 CNY
 3:           JN          JAPAN  323 JPY
 4:           GB        BRITAIN  166 GBP
 5:           CA         CANADA  115 CAD
 6:           IN          INDIA  114 INR
 7:           FR         FRANCE  101 EUR
 8:           HK      HONG KONG   95 HKD
 9:           GE        GERMANY   95 EUR
10:           SK    SOUTH KOREA   78 
11:           AU      AUSTRALIA   77
12:           SZ    SWITZERLAND   63
13:           BZ         BRAZIL   62
14:           RU         RUSSIA   48
15:           IT          ITALY   47
16:           TA         TAIWAN   47
17:           IR        IRELAND   43
18:           SW         SWEDEN   43
19:           SA   SOUTH AFRICA   41
20:           MX         MEXICO   40
21:           NE    NETHERLANDS   37
22:           SP          SPAIN   37
23:           MA       MALAYSIA   34
24:           TH       THAILAND   29
25:           SI      SINGAPORE   27
26:           SR   SAUDI ARABIA   26
27:           ID      INDONESIA   24
28:           TU         TURKEY   23
29:           DE        DENMARK   23
30:           PH    PHILIPPINES   22
31:           BE        BELGIUM   20
32:           CL          CHILE   20
33:           UA            UAE   20
34:           FI        FINLAND   15
35:           NO         NORWAY   15
36:           IS         ISRAEL   14
37:           PD         POLAND   14
38:           LX     LUXEMBOURG   13
39:           QA          QATAR   12
40:           PE           PERU   11
41:           CO       COLOMBIA   11
42:           VZ      VENEZUELA   11
43:           AS        AUSTRIA   10
44:           PO       PORTUGAL    9
45:           BD        BERMUDA    9
46:           VN        VIETNAM    8
47:           AR      ARGENTINA    7
48:           GR         GREECE    7
49:           NZ    NEW ZEALAND    7
50:           KU         KUWAIT    4
51:           EG          EGYPT    4
52:           UK        UKRAINE    4
53:           NG        NIGERIA    4
54:           CZ          CZECH    3
55:           MO        MOROCCO    3
56:           JO         JORDAN    3
57:           U2          MACAU    3
58:           JE         JERSEY    3
59:           PK       PAKISTAN    3
60:           HU        HUNGARY    2
61:           GS       GUERNSEY    2
62:           RO        ROMANIA    2
63:           PN         PANAMA    2
64:           KZ     KAZAKHSTAN    1
65:           PP PAPUA N.GUINEA    1
66:           BW     BANGLADESH    1
67:           CT        CURACAO    1
68:           HR        CROATIA    1
69:           ED        ECUADOR    1
70:           CC         CYPRUS    1
71:           KN          KENYA    1
72:           MB          MALTA    1
73:           IQ           IRAQ    1
74:           BJ        BAHRAIN    1
75:           IO    ISLE OF MAN    1
76:           PR    PUERTO RICO    1



DD = get(load('/home/aslepnev/git/ej/it_top10_uni.RData'))
libors = DD$libor
shmae = h_to_log(as.xts(fread('/home/aslepnev/webhub/shmaet_stocks.csv')[, Date := as.Date(Date)]))
# h_in = shmae; rebal_dates=get_rebals_h(h_in, 'quarter'); screen_func=screen_partial_momentum; screen_params=list(window=40, w=0.1); start_date='2016-06-30'
r = build_index_simple(shmae, get_rebals_h(shmae, 'month'), screen_partial_momentum, list(window=20, w=0.9), '2016-06-30')
r = foreach(x=r,.combine=rbind)%do%x$h
rvc = r; rvc = rvc[index(rvc)>='2016-08-25']; print(sqrt(252)*sd(tail(rvc, 252))); print(tail(exp(cumsum(rvc)), 1))
rvc = volcontrol_excess(r, list(window=20, type='none', excess_type = 'simple excess', excess=0, level=0.08, max_weight=1.25), libors); rvc = rvc[index(rvc)>='2016-08-25']; print(sqrt(252)*sd(tail(rvc, 252))); print(tail(exp(cumsum(rvc)), 1))
rvc = volcontrol_excess(r, list(window=20, type='none', excess_type = 'rate-related excess', excess=1, level=0.11, max_weight=1.25), libors); rvc = rvc[index(rvc)>='2016-08-25']; print(sqrt(252)*sd(tail(rvc, 252))); print(tail(exp(cumsum(rvc)), 1))

fwrite(exp(rvc), file='/home/aslepnev/webhub/test_shmae_rvc_new.csv')
y = fread('/home/aslepnev/webhub/shmaet_index.csv')





write.csv(w, file='/home/aslepnev/webhub/test_shmae_rvc_new1.csv')
write.csv(colnames(y), file='/home/aslepnev/webhub/test_shmae_rvc_new2.csv')

setwd('/home/aslepnev/webhub/PDFs')
Sweave('/home/aslepnev/git/ej/novo_latex1.Rnw')
system('pdflatex novo_latex1.tex')

a













-- SMIDAI --

source('/home/aslepnev/git/ej/strindexlib.R')
libors = get(load('/home/aslepnev/git/ej/it_top10_uni.RData'))$libor
#u = load_uni(c('equity', 'equity_metrics', 'h', 'libors', 'etf'), list())
#h = u$h[, u$etf$ticker]
#u$etf$ticker = as.character(t(as.data.table(strsplit(u$etf$ticker, ' ')))[,1])
#colnames(h) = u$etf$ticker
#f$ticker%in%u$etf$ticker

#h = fread('/home/aslepnev/webhub/smidai_hist_prices.csv')[, Dates := as.Date(Dates)]
h = fread('/home/aslepnev/webhub/smidai_etf_hist1.csv')[, Date := as.Date(Date)]
b = fread('/home/aslepnev/webhub/smidai_hist_baskets.csv')
f = fread('/home/aslepnev/webhub/smidai_hist_funds.csv')
f[ticker=='US0003M' & basket_id<7, weight:=0.6]

for(n in colnames(h)[-1]) {  # n=colnames(h)[10]
    h[[n]][h[, n, with=FALSE]=='#N/A N/A'] = NA
    h[[n]] = as.numeric(h[[n]])
}
h = as.xts(h)  #[, -4]
colnames(h) = foreach(i=colnames(h),.combine=c)%do%strsplit(i, ' ')[[1]][1]
h[, 'US0003M'] = cumprod(1+h[, 'US0003M']*0.01/252)
h = h_to_log(h)
#f = f[!ticker%in%c('US0003M', 'US003M'), ]

#r = build_index_simple(h, get_rebals_h(h, 'month'), smidai_style_rebal, screen_params=list(funds=f, baskets=b, window=125, voltarget=0.05), start_date='2013-12-29')
r = build_index_simple(h, get_rebals_h(h, 'month'), smidai_style_rebal, screen_params=list(funds=f, baskets=b, window=252, voltarget=0.075), start_date='2007-12-29')
r1 = foreach(x=r,.combine=rbind)%do%x$h

end_date='2019-02-04'
#rvc = volcontrol_excess(r1, list(window=20, type='max 5', excess_type = 'libor plus', add_rate=1, excess=3, level=0.08, max_weight=2.5, basis=365), libors);
#fwrite(100*index_perf(rvc),file='/home/aslepnev/webhub/smidai2_bt_20190424.csv')
#fwrite(100*index_perf(r1),file='/home/aslepnev/webhub/smidai2_bt_original_20190424.csv')
#fwrite(w, file='/home/aslepnev/webhub/smidai2_bt_weights_20190424.csv')
#rvc = rvc[index(rvc)>='2014-02-04' & index(rvc)<=end_date]; print(sqrt(252)*sd(tail(rvc, 252))); print(tail(exp(cumsum(rvc)), 1))
rvc = volcontrol_excess(r1, list(window=20, type='max 6', excess_type = 'libor plus', add_rate=1, excess=3, level=0.08, max_weight=2.5, basis=365), libors); rvc = rvc[index(rvc)>='2016-02-04' & index(rvc)<=end_date]; print(sqrt(365)*sd(tail(rvc, 252))); print(tail(exp(cumsum(rvc)), 1))





-- SALCA100 fixed

u = load_uni('data-20190506', c('equity', 'equity_metrics', 'h_ugly', 'libors'), list(fixed_list=c('9984 JP Equity', '6758 JP Equity', '6861 JP Equity', '7267 JP Equity')))






-- FASHION fixed
source('/home/aslepnev/git/ej/strindexlib.R')
u = load_uni('data-20190506', c('equity', 'equity_metrics', 'h_usd', 'fxx', 'libors'), list(fixed_list=c('4911 JP Equity', 'EL US Equity', 'PG US Equity', 'TSCO LN Equity', 'PEP US Equity', 'BN FP Equity', 'NESN SW Equity', 'WMT US Equity')))

fashion_fixed = function(u, vc_params){
    h = u$h_usd[index(u$h_usd)>='2011-01-01']
#h$libor = u$libors[index(h)]
#h$libor = na.locf(na.locf(h$libor), fromLast=TRUE)
#h$libor = h_to_log(cumprod(1+h$libor*0.01/252))
    b = data.table(id=1, weight=1, name='ALL')
    f = data.table(ticker=colnames(h))[, ':='(basket_id=1, name=ticker, weight=0.30)][, id:=1:ncol(h)]
    
    screen_params = list(funds=f, baskets=b, window=89, voltarget=0.08)
#    vc_params = list(window=20, src='self', type='none', excess_type = 'libor plus', add_rate=1, excess=3.5, level=0.14, max_weight=2, rate_basis=360, vc_basis=252)
    
    r = build_index_simple(h, get_rebals_h(h, 'month'), smidai_style_rebal, screen_params, start_date='2013-12-31', vc_params)
    r_smidai = foreach(x=r,.combine=rbind)%do%x$h
    vc_params$sd = foreach(x=r,.combine=rbind)%do%x$r_sd
    rvc_smidai = volcontrol_excess(r_smidai, vc_params, u$libors)

    return(rvc_smidai)
}

a1 = fashion_fixed(u, list(window=20, src='self', type='none', excess_type = 'libor plus', add_rate=1, excess=3.5, level=0.14, max_weight=2, rate_basis=360, vc_basis=252))
a2 = fashion_fixed(u, list(window=c(20, 60), src='self', type='none', excess_type = 'libor plus', add_rate=1, excess=3.5, level=0.14, max_weight=2, rate_basis=360, vc_basis=252))
a3 = fashion_fixed(u, list(window=20, src='self', type='none', excess_type = 'libor plus', add_rate=1, excess=3.5, level=0.12, max_weight=2, rate_basis=360, vc_basis=252))

print(unlist(basic_index_report(a1, 252)))
print(unlist(basic_index_report(a2, 252)))
print(unlist(basic_index_report(a3, 252)))



vc_params = list(window=20, src='self', type='none', excess_type = 'libor plus', add_rate=1, excess=3.5, level=0.14, max_weight=2, rate_basis=360, vc_basis=252)


tail(index_perf(rvc_smidai), 1)

bask = foreach(x=r,.combine=rbind)%do%{ y = x$basket$main$weights; names(y)=x$basket$main$names; y$dt=x$dt; as.data.table(y)[, c('dt', x$basket$main$names), with=FALSE] }
fwrite(bask, file='/home/aslepnev/webhub/basket8_weights.csv')
a = index_perf(rvc_smidai); write.csv(a, file='/home/aslepnev/webhub/basket8_backtest_precalc.csv', row.names=index(a))
a = index_perf(rvc_smidai); write.csv(a, file='/home/aslepnev/webhub/basket8_backtest_simple.csv', row.names=index(a))





#chart_data = list()
##r_mt = foreach(x=build_index_simpler(u, 'month', screen_mixed_top, screen_params=list(perf_weight=3, top_n=8, price_window=wnd, voltarget=0.075), '2013-12-31'),.combine=rbind)%do%x$h
##rvc_mt = volcontrol_excess(r_mt, list(window=20, type='none', excess_type = 'libor plus', add_rate=1, excess=3.5, level=vc_vt, max_weight=2, rate_basis=360, vc_basis=252), libors)
##chart_data = c(chart_data, list(list(segment=paste('topN', wnd, 2, '8/3/1/2.5/365'), rt=rvc_mt)))
#chart_data = c(chart_data, list(list(segment=paste('risk/return opt rebal,', wnd, vc_vt*100, ' vt/3.5 excess/1 fee/1.75 exp/252'), rt=rvc_smidai)))
#xly = load_uni('data-20190506', c('etf', 'h_usd'), list(fixed_list=c('XLY     US Equity')))$h_usd
#chart_data = c(chart_data, list(list(segment='XLY', rt=xly)))
#library(ggthemes)
#save_data_as_chart(multi_plot_1, rt_to_chart_data(chart_data), 'novo_chart.png', 400)























-- IT10
source('/home/aslepnev/git/ej/strindexlib.R')

u = load_uni(c('equity', 'equity_metrics', 'h_ugly', 'libors'),
             list(field_filter=c('us', 'tech'), skip_filter=c('no_card'), rank_filter=c('top 30 mcap')))
libors = u$libors
u[['h']] = u[['h_ugly']]
#u = load_uni(c('equity', 'equity_metrics', 'h', 'libors', 'p'),
#             list(field_filter=c('us', 'tech'), skip_filter=c('no_card'), rank_filter=c('top 30 mcap')))
r = build_index_simpler(u, 'month', screen_mixed_top, screen_params=list(perf_weight=0.5, top_n=10, price_window=135), '2012-12-29')  
r1 = foreach(x=r,.combine=rbind)%do%x$h


# libors=u$libors; r_in=r; r1_in=r1; terms_in=c(3, 5); vc_targets=c(0.12, 0.14); vc_types=c('max 10', 'simple');
#rvc = r1; print(sqrt(252)*sd(tail(rvc, 252))); print(tail(exp(cumsum(rvc)), 1))
rvc = volcontrol_excess(r1, list(window=20, type='none', excess_type = 'libor plus', add_rate=1.0, excess=3.5, level=0.14, max_weight=1.75, rate_basis=360, vc_basis=252), u$libors); print(sqrt(252)*sd(tail(rvc, 252))); print(tail(exp(cumsum(rvc)), 1))
exp(cumsum(rvc))
write.csv(1000*exp(cumsum(rvc)), file='/home/aslepnev/webhub/it10.csv', row.names=index(rvc))

rvc = volcontrol_excess(r1, list(window=20, type='none', excess_type = 'libor plus', add_rate=1.0, excess=3.5, level=0.14, max_weight=1.5, rate_basis=360, vc_basis=252), u$libors); print(sqrt(252)*sd(tail(rvc, 252))); print(tail(exp(cumsum(rvc)), 1))
exp(cumsum(rvc))
write.csv(1000*exp(cumsum(rvc)), file='/home/aslepnev/webhub/it_max_150.csv', row.names=index(rvc))

dt_start=as.Date('2012-12-29'); dt_end=as.Date('9999-03-01'); 
rvc = volcontrol_excess(r1, list(window=20, type='none', excess_type='libor plus', add_rate=0.5, excess=2, level=0.05, max_weight=2, basis=360), libors)
print(sqrt(252)*sd(tail(rvc, 252))); print(tail(exp(cumsum(rvc[index(rvc)>=dt_start & index(rvc)<=dt_end])), 1))


idx = index_report(build_index_simpler(load_uni(c('equity', 'equity_metrics', 'h', 'libors', 'p'),
                                                list(field_filter=c('us', 'tech'), skip_filter=c('no_card'), rank_filter=c('top 30 mcap'))),
                                       'month', screen_mixed_top, screen_params=list(perf_weight=0.5, top_n=10, price_window=125), '2012-12-31'),
                   list(vc_params=list(window=20, type='simple', excess_type = 'libor plus', add_rate=1, excess=3.5, level=0.14, max_weight=1.75)),
                   u$libors)

send_files_to_email(c(save_data_as_csv(idx$baskets, 'top10it_baskets.csv'),
                      save_data_as_csv(idx$perf, 'top10it_perf.csv'),
                      save_data_as_chart(idx$perf, 'Top 10 IT companies, monthly', 'top10it.png'),
                      latex_pm_card(idx$orig_data, foreach(x=idx$orig_data,.combine=rbind)%do%x$h, c(3,5),
                                    vc_targets=c(0.12, 0.14), vc_types=c('max 10', 'simple'), list(exrate=3.5, add_rate=1, max_weight=1.75), u$libors, 'it10')),
                    'Top 10 IT index', 'aslepnev@novo-x.info')

write.csv(exp(cumsum(rvc)), file='/home/aslepnev/webhub/it10.csv', row.names=index(rvc))
a = u$p[,u$equity_metrics[dt=="2017-02-01",][order(mcap,decreasing=TRUE),ticker],with=FALSE]
write.csv(a, file='/home/aslepnev/webhub/it10_backtest.csv', row.names=index(a))


-- sectoral top N mcap

u = load_uni(c('equity', 'equity_metrics', 'h', 'libors', 'p'),
#             list(field_filter=c('agriculture'), rank_filter=c('top 40 mcap')))
#             list(field_filter=c('beverage'), rank_filter=c('top 40 mcap')))
#             list(field_filter=c('leisure'), rank_filter=c('top 40 mcap')))
             list(field_filter=c('superdev', 'cosmetics+apparel'), rank_filter=c('top 30 mcap')))
#             list(field_filter=c('agriculture+cosmetics'), rank_filter=c('top 40 mcap')))
#r = build_index_simpler(u, 'month', screen_mixed_top, screen_params=list(perf_weight=1, top_n=20, price_window=250), '2012-12-29')  
r = build_index_simpler(u, 'month', smidai_mixed_rebal, screen_params=list(perf_weight=2, top_n=10, price_window=250, voltarget=0.15), '2012-12-31')  
#r = build_index_simpler(u, 'month', screen_mixed_top, screen_params=list(perf_weight=0.5, top_n=10, price_window=40), '2012-12-31')
r1 = foreach(x=r,.combine=rbind)%do%x$h
dt_start=as.Date('2012-12-29'); dt_end=as.Date('9999-03-01'); 
rvc = volcontrol_excess(r1, list(window=20, type='max 10', excess_type='libor plus', add_rate=1, excess=3, level=0.15, max_weight=2.5, basis=360), libors)
print(sqrt(252)*sd(tail(rvc, 252))); print(tail(exp(cumsum(rvc[index(rvc)>=dt_start & index(rvc)<=dt_end])), 1))

source('/home/aslepnev/git/ej/strindexlib.R')

latex_segment_compare(list(c('tech'), c('energy'), c('finance'), c('telecom'), c('staples'), c('discret'), c('industrial'), c('materials'), c('health'), c('estate'), c('utility')), 10)
latex_segment_compare(list(c('tech'), c('energy'), c('finance'), c('telecom'), c('staples'), c('discret'), c('industrial'), c('materials'), c('health'), c('estate'), c('utility')), 5)
latex_segment_compare(list(c('us', 'tech+telecom'), c('us', 'finance+industrial'), c('us', 'staples+discret'), c('us', 'materials+utilities'), c('us', 'health+staples')), 5)
latex_segment_compare(list(c('beverage'), c('leisure'), c('cosmetics'), c('apparel')), 5)
latex_segment_compare(list(c('us', 'beverage+leisure'), c('us', 'leisure+apparel'), c('us', 'cosmetics+apparel+leisure')), 10)

tags_list = foreach(x=TAG_FILTERS[target=='equity', unique(name)])%do%c(x)
top_mcap = 5
prep_data = foreach(i=0:42,.combine=c)%do%tryCatch({ latex_segment_compare_prep(tags_list[seq(i*5+1, min(length(tags_list), (i+1)*5))], top_mcap) }, error=function(e){ list() })
prep_data = prep_data[order(foreach(x=prep_data,.combine=c)%do%x$perfTotNumber, decreasing=TRUE)]
save(prep_data, file='/home/aslepnev/webhub/segment_curves_5cnt.RData')
top_mcap = 6
prep_data = foreach(i=0:42,.combine=c)%do%tryCatch({ latex_segment_compare_prep(tags_list[seq(i*5+1, min(length(tags_list), (i+1)*5))], top_mcap) }, error=function(e){ list() })
prep_data = prep_data[order(foreach(x=prep_data,.combine=c)%do%x$perfTotNumber, decreasing=TRUE)]
save(prep_data, file='/home/aslepnev/webhub/segment_curves_6cnt.RData')

library(mailR)
per_page = 11
source('/home/aslepnev/git/ej/novo_latex_func.R')
send_files_to_email(c(foreach(i=0:(length(prep_data)%/%per_page),.combine=c)%do%latex_segment_compare_path(prep_data[seq(i*per_page+1, min(length(prep_data), (i+1)*per_page))], top_mcap, i)),
                    'Segment performances', 'aslepnev@novo-x.info')











-- sectoral WO

u = load_uni(c('equity', 'h', 'libors'),
#             list(field_filter=c('agriculture'), rank_filter=c('top 40 mcap')))
#             list(field_filter=c('beverage'), rank_filter=c('top 40 mcap')))
#             list(field_filter=c('leisure'), rank_filter=c('top 40 mcap')))
             list(field_filter=c('west', 'tech')))
#             list(field_filter=c('agriculture+cosmetics'), rank_filter=c('top 40 mcap')))
#r = build_index_simpler(u, 'month', screen_mixed_top, screen_params=list(perf_weight=1, top_n=20, price_window=250), '2012-12-29')  
r = build_index_simpler(u, 'month', smidai_mixed_rebal, screen_params=list(perf_weight=2, top_n=10, price_window=250, voltarget=0.15), '2012-12-31')  
#r = build_index_simpler(u, 'month', screen_mixed_top, screen_params=list(perf_weight=0.5, top_n=10, price_window=40), '2012-12-31')
r1 = foreach(x=r,.combine=rbind)%do%x$h
dt_start=as.Date('2012-12-29'); dt_end=as.Date('9999-03-01'); 
rvc = volcontrol_excess(r1, list(window=20, type='max 10', excess_type='libor plus', add_rate=1, excess=3, level=0.15, max_weight=2.5, basis=360), libors)
print(sqrt(252)*sd(tail(rvc, 252))); print(tail(exp(cumsum(rvc[index(rvc)>=dt_start & index(rvc)<=dt_end])), 1))

source('/home/aslepnev/git/ej/strindexlib.R')

latex_segment_compare(list(c('tech'), c('energy'), c('finance'), c('telecom'), c('staples'), c('discret'), c('industrial'), c('materials'), c('health'), c('estate'), c('utility')), 10)
latex_segment_compare(list(c('tech'), c('energy'), c('finance'), c('telecom'), c('staples'), c('discret'), c('industrial'), c('materials'), c('health'), c('estate'), c('utility')), 5)
latex_segment_compare(list(c('us', 'tech+telecom'), c('us', 'finance+industrial'), c('us', 'staples+discret'), c('us', 'materials+utilities'), c('us', 'health+staples')), 5)
latex_segment_compare(list(c('beverage'), c('leisure'), c('cosmetics'), c('apparel')), 5)
latex_segment_compare(list(c('us', 'beverage+leisure'), c('us', 'leisure+apparel'), c('us', 'cosmetics+apparel+leisure')), 10)

tags_list = foreach(x=TAG_FILTERS[target=='equity', unique(name)])%do%c(x)
top_mcap = 5
prep_data = foreach(i=0:42,.combine=c)%do%tryCatch({ latex_segment_compare_prep(tags_list[seq(i*5+1, min(length(tags_list), (i+1)*5))], top_mcap) }, error=function(e){ list() })
prep_data = prep_data[order(foreach(x=prep_data,.combine=c)%do%x$perfTotNumber, decreasing=TRUE)]
save(prep_data, file='/home/aslepnev/webhub/segment_curves_5cnt.RData')
top_mcap = 6
prep_data = foreach(i=0:42,.combine=c)%do%tryCatch({ latex_segment_compare_prep(tags_list[seq(i*5+1, min(length(tags_list), (i+1)*5))], top_mcap) }, error=function(e){ list() })
prep_data = prep_data[order(foreach(x=prep_data,.combine=c)%do%x$perfTotNumber, decreasing=TRUE)]
save(prep_data, file='/home/aslepnev/webhub/segment_curves_6cnt.RData')

library(mailR)
per_page = 11
source('/home/aslepnev/git/ej/novo_latex_func.R')
send_files_to_email(c(foreach(i=0:(length(prep_data)%/%per_page),.combine=c)%do%latex_segment_compare_path(prep_data[seq(i*per_page+1, min(length(prep_data), (i+1)*per_page))], top_mcap, i)),
                    'Segment performances', 'aslepnev@novo-x.info')






}
