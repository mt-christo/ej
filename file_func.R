# source("rapache_eval1.R")
#library(xlsx)
library(RJDBC)

upload_file_func = function(){
	#writeLines('aaa',con=file('/home/slepnev/R/uploaded_files/file.RData'))
	save(POST,file='/home/slepnev/R/uploaded_files/file.RData')
	cat('aaa')
}

save_statics = function(conn,d,static_kind){
    fids = foreach(i=1:ncol(d),.combine=c)%do%as.numeric(dbGetQuery(conn, paste("Q_AddSecurityField '",d[1,i],"'",sep='')))
    secids = foreach(i=2:nrow(d),.combine=c)%do%as.numeric(dbGetQuery(conn, paste("Q_AddSecurity '",d[i,1],"'",sep='')))
    for(i in 1:length(secids)){
        q = "" 
        for(j in 1:length(fids))
            q = paste(q,"\nexec Q_AddSecurityStaticData ",secids[i],", ",fids[j],", '",gsub("'","",d[i+1,j]),"', ",static_kind,sep='')
        dbGetQuery(conn, q)
        print(i)
    }
}

process_quoteshist_csv = function(){
    drv = JDBC('com.microsoft.sqlserver.jdbc.SQLServerDriver','sqljdbc4-2.0.jar')
    conn = dbConnect(drv, 'jdbc:sqlserver://10.245.67.30;user=RUser;password=Qwerty12;database=QUIK_REPO')

    x = read.csv('/var/share/hist.csv',sep=';',header=FALSE,stringsAsFactors=FALSE)
    x_e = read.csv('/var/share/Equities.csv',sep=';',header=FALSE,stringsAsFactors=FALSE)
    x_f = read.csv('/var/share/Funds.csv',sep=';',header=FALSE,stringsAsFactors=FALSE)
    x_c = read.csv('/var/share/Currencies.csv',sep=';',header=FALSE,stringsAsFactors=FALSE)

    dbSendQuery(conn, 'truncate table Q_Security_StaticData; select 1')
    save_statics(conn,x_e,1)
    save_statics(conn,x_f,2)
    save_statics(conn,x_c,3)

    secids = foreach(i=seq(1,ncol(x)-1,by=2),.combine=c)%do%as.numeric(dbGetQuery(conn, paste("Q_AddSecurity '",x[1,i],"'",sep='')))
    col_indices = seq(1,ncol(x)-1,by=2)
    for(i in 1:length(secids)){
        tryCatch({
            y = x[-(1:2),col_indices[i]+0:1]
            y = y[!is.na(y[,2]) & !is.na(y[,1]) & y[,1]!="" & y[,2]!="",]

            if(dim(y)[1] > 0){
                y[,2] = gsub(',','.',y[,2])
                y[,1]=as.Date(y[,1],format='%d.%m.%Y')
                dbSendQuery(conn, paste("SET NOCOUNT ON; delete from q_quotes_eod where QuoteDate>='",min(y[,1]),"' and SecID=",secids[i],"; select 1",sep=""))
                if(nrow(y) > 1) dbWriteTable(conn,'q_quotes_eod',cbind(a1=secids[i],a2=3,y),overwrite=FALSE,append=TRUE)
                #print(secids[i])
            }

        }, error=function(cond) {
                print('Error:')
                print(secids[i])
            
        })
    }
}
