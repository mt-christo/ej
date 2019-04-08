refresh_gs_auth = function() {
    gs_auth(token = '/home/aslepnev/git/ej/gdoc_doc.R')
    return(gs_key('1y9KUgukyEvfjAVaDYCHPf1rkFn83q8LWS_0tybz2pIM'))
}

load_wo_params_gdoc = function(g_token){
    params = as.data.table(gs_read(g_token, 'wo-calculator', range='A2:G20', col_names=FALSE))
    names(params) = c('name', 'value', 'a1', 'a2', 'basket', 'sigma', 'div')
    
    COUPON = params[name == 'COUPON:', as.numeric(gsub('%', '', value))]*0.01
    BARRIERS = params[name == 'BARRIERS:', as.numeric(strsplit(value, '-')[[1]])]
    RFR = params[name == 'RFR:', as.numeric(gsub('%', '', value))]*0.01
    TTM = length(BARRIERS)

    basket_idx = which(params$basket == 'NAME') + 1
    BASKET = as.character(t(params[basket_idx:nrow(params), ][!is.na(basket), strsplit(basket, ' ')][1, ]))
    SIGMAS = params[basket_idx:(basket_idx + length(BASKET) - 1), sigma]
    DIVS = params[basket_idx:(basket_idx + length(BASKET) - 1), div]
    
    return(list(BASKET=BASKET, BARRIERS=BARRIERS, COUPON=COUPON, RFR=RFR, TTM=TTM, SIGMAS=SIGMAS, DIVS=DIVS))
}

update_wo_data_gdoc = function(g_token, gdata){
    tab = rbind(c(gdata$tickers, ''), cbind(gdata$cor_mat, gdata$tickers))
    gs_edit_cells(g_token, ws='wo-calculator', anchor = "H3", input = tab, byrow = TRUE, col_names=FALSE)
}
