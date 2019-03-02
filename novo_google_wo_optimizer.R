price_wo_gdoc = function(){
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
