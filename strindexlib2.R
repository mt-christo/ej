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


ds = get(load('/home/aslepnev/webhub/grish_iter0.RData'))
de = get(load('/home/aslepnev/webhub/sacha_etf_yhoo.RData'))

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
d = pre_screen(de, etf_segment(de$u, 'Health Care', 15), smart=TRUE)

# d_in=d; n_in=3; volparams=list(wnd=500, min=0.2, max=0.3)
baskets_vol_range = function(d_in, n_in, volparams){
    nn = t(combn(d_in$u$ticker, n))
    h = tail(d_in$h, volparams$wnd)
    w = array(1/n, n)
    comat = cov(h)
    sds = sqrt(250)*sqrt(foreach(j=1:nrow(nn),.combine=c)%do%{ w %*% comat[nn[j, ], nn[j, ]] %*% w })
    uni = nn[which(sds>=volparams$min & sds<=volparams$max), ]
}


n = 3

nn[which.min(abs(sds-0.3)), ]

basket = c("XBI","ETAHX","MJ")
basket=c(1,2,5)
basket_vol(h[, basket], w)



d$u[ticker%in%basket,]
basket_vol(h[, c("XBI","ETAHX","MJ")], w)
basket_perf(d$h[, c("XBI","ETAHX","MJ")], w)


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
