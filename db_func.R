create_conn = function(conn_string = 'jdbc:sqlserver://10.245.67.30;user=RUser;password=Qwerty12;database=QUIK_REPO'){
    drv = JDBC('com.microsoft.sqlserver.jdbc.SQLServerDriver','/home/slepnev/R/sqljdbc4-2.0.jar')
    dbConnect(drv, conn_string)
}

# tickers = STOCKS; conn = create_conn(); quote_curr = 'EUR'; quote_type = 3; t=tickers[2]
get_eod_quotes = function(tickers, conn, quote_curr = 'USD', quote_type = 3){
    res = get_eod_quote(tickers[1],conn,quote_curr,quote_type)
    if(length(tickers) > 1)
        for(i in 2:length(tickers))
            res = merge.xts(res,get_eod_quote(tickers[i],conn,quote_curr,quote_type))
    res
}

get_eod_quote = function(t, conn, quote_curr = 'USD', quote_type = 3, conv_type = 'CONVERT'){
    y_params = dbGetQuery(conn, paste("Q_GetSecFieldData '",t,"', 'CURRENCY'",sep=''))
    y = dbGetQuery(conn, paste('select q.QuoteDate, q.Quote from Q_Quotes_EOD q  where q.SecID = ',y_params$SecID,' and q.QuoteTypeID=',quote_type,sep=''))
    y_curr_id = dbGetQuery(conn, paste("Q_FilterSecurities_ByFieldValue 'CURR_CODE', '",y_params$FieldValue,"'",sep=''))[1,1]
    q_curr_id = if(quote_curr=='USD') 0 else dbGetQuery(conn, paste("Q_FilterSecurities_ByFieldValue 'CURR_CODE', '",quote_curr,"'",sep=''))[1,1]
    y_rate = if(y_params$FieldValue == 'USD') data.frame(Quote=1,QuoteDate=y$QuoteDate) else
                       dbGetQuery(conn, paste('select QuoteDate, Quote from Q_Quotes_EOD where SecID = ',y_curr_id,' and QuoteTypeID=3',sep=''))
    q_rate = if(quote_curr=='USD') data.frame(Quote=1,QuoteDate=y$QuoteDate) else
                       dbGetQuery(conn, paste('select QuoteDate, Quote from Q_Quotes_EOD where SecID = ',q_curr_id,' and QuoteTypeID=3',sep=''))
    y = merge.xts(quote=as.xts(y$Quote,order.by=as.Date(y$QuoteDate)),
                      rate=as.xts(y_rate$Quote,order.by=as.Date(y_rate$QuoteDate)),
                      qrate=as.xts(q_rate$Quote,order.by=as.Date(q_rate$QuoteDate)))
    res = if(conv_type=='CONVERT') (y$quote*na.locf(y$qrate)/na.locf(y$rate))[!is.na(y$quote)]
             else y$quote[!is.na(y$quote)]
    res
}

# tickers = STOCKS; conn = create_conn(); quote_curr = 'EUR'; quote_type = 3; t=tickers[1]
get_eod_quotes_CURR = function(tickers, conn, quote_curr = 'USD', quote_type = 3){
    res = get_eod_quote_CURR(tickers[1],conn,quote_curr,quote_type)
    if(length(tickers) > 1)
        for(i in 2:length(tickers))
            res = merge.xts(res,get_eod_quote_CURR(tickers[i],conn,quote_curr,quote_type))
    res
}

get_eod_quote_CURR = function(t, conn, quote_curr = 'USD', quote_type = 3){
    y_params = dbGetQuery(conn, paste("Q_GetSecFieldData '",t,"', 'CURRENCY'",sep=''))
    y = dbGetQuery(conn, paste('select q.QuoteDate, q.Quote from Q_Quotes_EOD q  where q.SecID = ',y_params$SecID,' and q.QuoteTypeID=',quote_type,sep=''))
    y_curr_id = dbGetQuery(conn, paste("Q_FilterSecurities_ByFieldValue 'CURR_CODE', '",y_params$FieldValue,"'",sep=''))[1,1]
    q_curr_id = if(quote_curr=='USD') 0 else dbGetQuery(conn, paste("Q_FilterSecurities_ByFieldValue 'CURR_CODE', '",quote_curr,"'",sep=''))[1,1]
    y_rate = if(y_params$FieldValue == 'USD') data.frame(Quote=1,QuoteDate=y$QuoteDate) else
                       dbGetQuery(conn, paste('select QuoteDate, Quote from Q_Quotes_EOD where SecID = ',y_curr_id,' and QuoteTypeID=3',sep=''))
    q_rate = if(quote_curr=='USD') data.frame(Quote=1,QuoteDate=y$QuoteDate) else
                       dbGetQuery(conn, paste('select QuoteDate, Quote from Q_Quotes_EOD where SecID = ',q_curr_id,' and QuoteTypeID=3',sep=''))
    y = merge.xts(quote=as.xts(y$Quote,order.by=as.Date(y$QuoteDate)),
                      convquote=as.xts(y$Quote,order.by=as.Date(y$QuoteDate)),
                      rate=1/as.xts(y_rate$Quote,order.by=as.Date(y_rate$QuoteDate)),
                      qrate=as.xts(q_rate$Quote,order.by=as.Date(q_rate$QuoteDate)))
    y$convquote = (y$quote*na.locf(y$qrate)*na.locf(y$rate))
    y[!is.na(y$convquote),]
}

get_secs_field = function(tickers, field_name, conn){
    x = foreach(t=as.array(tickers),.combine=c)%do%{
        dbGetQuery(conn, paste("Q_GetSecurity_Field '",t,"', '",field_name,"'",sep=''))$FieldValue
    }
    ifelse(x=="#N/A N/A",NA,x)
}

get_secs_num_field = function(tickers, field_name, conn){
    x = foreach(t=as.array(tickers),.combine=c)%do%{
        dbGetQuery(conn, paste("Q_GetSecurity_Field '",t,"', '",field_name,"'",sep=''))$FieldValue
    }
    as.numeric(gsub(',','\\.',ifelse(x=="#N/A N/A",NA,x)))
}





