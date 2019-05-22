price_wo_gdoc = function(){
#    p = params_via_file(GET$params, ''){
#    writeLines(gsub('NEWLINE','\n',GET$params), file('/home/slepnev/R/tmp_params_plot_web.txt'))
#    p = read.csv(file='/home/slepnev/R/tmp_params_plot_web.txt',header=FALSE,sep=',',stringsAsFactors=FALSE)

#    STOCKS = strsplit(p$V2[p$V1=='Tickers'],'@@')[[1]]
#    prices = log(get_eod_quotes(STOCKS, create_conn()))
#    w = array(1/length(STOCKS),length(STOCKS))
    
    x = apply.monthly(exp(cumsum(basket_returns(tail(prices,250*5),w))),mean)
    x = data.frame(date=index(x),value=as.numeric(x[,1]),stringsAsFactors=FALSE)
    cat(paste(x$date,collapse=';'))
    cat('@@')
    cat(paste(gsub('\\.',',',as.character(x$value)),collapse=';'))




    save(GET, file='/home/aslepnev/webhub/last_GET_price_wo_gdoc.RData')
    # GET = get(load('/home/aslepnev/webhub/last_GET_price_wo_gdoc.RData'))

    sink(file(tempfile(), open='wt'))
    g_token = refresh_gs_auth()
    p = load_wo_params_gdoc(g_token)
    sink()
    h_report = uni_hist_report(NOVO_UNI, p$BASKET, 250, 120)
    SIGMAS = ifelse(!is.na(p$SIGMAS), p$SIGMAS, h_report$sigmas)
    COR_MAT = h_report$cor_mat
    DIVS = ifelse(!is.na(p$DIVS), p$DIVS, 0)
    
    price = wo_calculate_an(p$TTM, p$BARRIERS, SIGMAS, COR_MAT, p$RFR, DIVS, p$COUPON)

    update_wo_data_gdoc(g_token, list(tickers=p$BASKET, cor_mat=COR_MAT))
    
    res = paste0(round(100*price, 2), '%')
    cat(res)
#    return(price)
#    web_datatable(list('ma'=ma, 'qres'=qres, 'results'=results))
}

optimize_wo_excel = function(){
    write(paste('Request', Sys.time()), file="/home/aslepnev/webhub/testhttp.txt", append=TRUE)
    write(paste('GET:'), file="/home/aslepnev/webhub/testhttp.txt", append=TRUE)
    write(paste(GET), file="/home/aslepnev/webhub/testhttp.txt", append=TRUE)
    write(paste('POST:'), file="/home/aslepnev/webhub/testhttp.txt", append=TRUE)
    write(paste(POST), file="/home/aslepnev/webhub/testhttp.txt", append=TRUE)
    params = params_via_file(GET$params, 'optimize_wo_excel')
    # params = params_via_file(NA, 'optimize_wo_excel')

    NOVO_UNI = load_uni('data-20190506', c('equity', 'equity_metrics', 'h_usd', 'libors'), list())

    h = NOVO_UNI$h_usd    
    u = NOVO_UNI$equity
    RFR = 0.01*as.numeric(tail(NOVO_UNI$libors, 1))
    setkey(params, name)
    
    SIZE = params['SIZE', as.numeric(value)]
    BARRIERS = params['STRIKE', as.numeric(value)]
    TTM = length(BARRIERS)
    TAIL = 120
    COUPON = 1
    UNI = u[ticker%in%params['UNI', value], ]
    
    h = h[, UNI$ticker]
    h = h[, !is.na(tail(h, TAIL)[1,])]  # TAIL-ago returns exist
    UNI = UNI[ticker%in%colnames(h), ][, ':='(ivol=NA, div=NA)]  # only TAIL-ago existing tickers are left in UNI
    
    h = tail(h[rowSums(is.na(h)) == 0, ], TAIL)  # last TAIL returns are taken
    SIGMAS = ifelse(is.na(UNI$ivol), constituent_vols(h, 252), UNI$ivol)
    DIVS = ifelse(is.na(UNI$div), 0, UNI$div)
    COR_MAT_ALL = cor(h)
    
    cmb = combn(1:nrow(UNI), SIZE)
    idx = 1:ncol(cmb)
    sectors = UNI[, sector]
    max_sectors = length(UNI[, unique(sector)])
    idx2 = c()
    for(i in idx) if(sum(is.na(sectors[cmb[,i]]))==0 && length(unique(sectors[cmb[,i]])) >= min(SIZE-1, max_sectors)) idx2[length(idx2)+1] = i
    
    #for(bi in 1:nrow(BARRIERS)){
    #    barriers_in = as.numeric(BARRIERS[bi, ])
    #    barriers_in = BARRIERS
    cheapest_baskets = function(barriers_in){
        res = foreach(i = idx2)%dopar%{
            if(i%%100==0) print(i)
            list(basket=cmb[,i], price=wo_calculate_an(TTM, barriers_in, SIGMAS[cmb[,i]], COR_MAT_ALL[cmb[,i], cmb[,i]], RFR, DIVS[cmb[,i]], COUPON))
        }
        
        prc = array(0, length(res))
        for(i in 1:length(res)) if(!is.null(res[[i]])) prc[i] = res[[i]]$price
        res = res[order(prc)]
        return(res)
    }
    
    baskets = cheapest_baskets(BARRIERS)
    res = as.data.table(foreach(b=baskets,.combine=rbind)%do%c(UNI[b$basket, ticker], round(b$price, 3)))[1:min(100, length(baskets)),]

    wait_pids()
    rm(h)
    rm(u)
    rm(NOVO_UNI)
#    cat(paste(t(res),collapse=';'))
    cat(web_simple_table(res))
}
