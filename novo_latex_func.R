# libors=u$libors; r_in=r; r1_in=r1; terms_in=c(3, 5); vc_targets=c(0.12, 0.14); vc_types=c('max 10', 'simple'); index_label='it10'
latex_pm_card = function(r_in, r1_in, terms_in, vc_targets, vc_types, fixed_vc_params, libors, index_label){  
# r2 = volcontrol_excess(r1, list(window=20, type='max 10', excess_type = 'libor plus', add_rate=1.5, excess=3.5, level=0.14, max_weight=1.75), u$libors); print(sqrt(252)*sd(tail(rvc, 252))); print(tail(exp(cumsum(rvc)), 1))

    exrate = fixed_vc_params$exrate  #3.5
    add_rate = fixed_vc_params$add_rate  #1.5
    max_weight = fixed_vc_params$max_weight  #1.75

    my_charts = list()
    res = rbindlist(foreach(t=terms_in, .combine=c)%do%{  # t=5
        foreach(vt=vc_targets, .combine=c)%do%{  # vt=0.14
            foreach(vctype=vc_types)%do%{  # vctype='max 10'
                vc_tmp = list(window=20, type=vctype, excess_type = 'libor plus', add_rate=add_rate, excess=exrate, level=vt, max_weight=max_weight)
                x = volcontrol_excess(r1_in, vc_tmp, libors);
                start_date = tail(index(x), 1); year(start_date) = year(start_date) - t; x = x[index(x) >= start_date]
                y = data.table(term = paste0(t, 'Y'),
                               rv = fracperc(as.numeric(sqrt(252)*sd(tail(x, 252))), 2),
                               ret_num = as.numeric(tail(exp(cumsum(x)), 1)),
                               ret = fracperc(as.numeric(tail(exp(cumsum(x)), 1))-1, 0),
                               vt = fracperc(vt, 0),
                               vctype = vctype)
                if(t==max(terms_in)) my_charts[[paste(y[1, .(vt, vctype)], collapse=' / ')]] = 100*(index_perf(x)-1)
                y
            }
        }
    })
    res = res[order(ret_num, decreasing=TRUE), ]

    my_table <- paste(paste(foreach(i=1:nrow(res), .combine=c)%do%gsub('%', "\\\\%", paste(res[i, .(term, ret, vt, vctype, rv)], collapse=' & ')), collapse=' \\\\[0.4em]\n'), ' \\\\[0.6em]\n')
    my_list <- paste(as.character(t(t(as.data.table(strsplit(tail(r_in, 1)[[1]]$basket$main$names, ' '))))[1,]), collapse=', ')
    my_index_desc <- 'Top 30 American Technology stocks are ranked by a performance metric based on their market cap and stock performance. Top 10 stocks with highest rank are then picked and constitute an equally-weighed portfolio for the next month.'
    my_index_name <- 'SLCGIT10 Index'
    
    exrate_txt <- paste0(exrate, '\\%')
    add_rate_txt <- paste0(add_rate, '\\%')
    max_weight_txt <- gsub('%', "\\\\%", fracperc(max_weight, 0))

    x = foreach(x=my_charts,.combine=cbind)%do%to.monthly(x, indexAt='endof')[, 'x.Open']
    colnames(x) = names(my_charts)
    x = melt(as.data.table(x), id='index', variable.name='Index', value.name='Price')
    colnames(x)[1] = 'Date'
    

    chart_path = '/home/aslepnev/webhub/PDFs/novo_chart.png'
    png(chart_path, height=400)
#    print(plot(my_chart, main=chart_title))

#            + theme(legend.position=c(0,1), legend.justification=c(0,1), legend.key = element_rect(fill = NA, color = 'white')) +
    plot_func = function(x){
        ggplot(x) + #geom_line(aes(x=Date, y=Price, group=Index, color=Index), size=1) +
            ggtitle('Parameterized Index performance') + # theme_economist_white() +
            theme_hc() +
            scale_colour_hc() +
            theme(legend.text=element_text(size=12, family='Palatino'), text=element_text(size=15, family='Palatino')) +
            labs(color='') +
            xlab('Time') +
            ylab('Performance, %') +
            stat_smooth(aes(x=Date, y=Price, group=Index, color=Index), formula=y~splines::ns(x,35), method='gam', se=FALSE, size=2)
    }

    print(plot_func(x))
    dev.off()
    
    knit('/home/aslepnev/git/ej/novo_latex2.tex', '/home/aslepnev/webhub/PDFs/index_card.tex')
    setwd('/home/aslepnev/webhub/PDFs')
    system('pdflatex index_card.tex')

    pdf_path = paste0('/home/aslepnev/webhub/PDFs/index_card_', index_label, '.pdf')
    system(paste0('mv /home/aslepnev/webhub/PDFs/index_card.pdf ', pdf_path))
#
#    library(mailR)
#    send_files_to_email(c(#save_data_as_csv(idx$baskets, 'top10it_baskets.csv'),
#        '/home/aslepnev/webhub/PDFs/index_card.pdf'),
#        'Top 10 IT index', 'aslepnev@novo-x.info')

    return(pdf_path)    
}

# filter_list = list(c('beverage'), c('leisure'), c('superdev', 'cosmetics+apparel')); top_mcap=10
# filter_list = list(c('Health Care'), c('tech'), c('finance'), c('staples'), c('discret'), c('industrial')); top_mcap=10
# filter_list = list(c('Enrg-Trnsprttn')); top_mcap=5
latex_segment_compare_prep = function(filter_list, top_mcap){
    start_date = as.Date('2012-12-29')
    res = foreach(f = filter_list)%dopar%{  # f = filter_list[[1]]
        print(f)
        u = load_uni(c('equity', 'equity_metrics', 'h', 'libors'),
                     list(field_filter=f, rank_filter=c(paste('top', top_mcap, 'mcap'))))
        if(nrow(u$equity) < top_mcap) stop('Zero companies - should-t be!')
        r = build_index_simpler(u, 'month', screen_mixed_top, screen_params=list(perf_weight=0, top_n=top_mcap, price_window=250), '2012-12-29')
        r1 = foreach(x=r,.combine=rbind)%do%x$h
        sds = apply.yearly(r1, FUN=index_vol)
        
        dt_start = start_date
        dt_end = as.Date('9999-03-01')        
        r_limited = r1[index(r1)>=dt_start & index(r1)<=dt_end]
        list(segment=gsub('\\+', ' +', paste(f, collapse='/ ')),
             sd252 = fracperc(index_vol(tail(r1, 252)), 1),
             sd_all = paste0(fracperc(mean(sds), 1, FALSE), '\\mypm ', fracperc(sd(sds), 0)),
             perfTot = fracperc(exp(sum(r_limited))-1, 0),
             perfTotNumber = exp(sum(r_limited)),
             perfTot252 = fracperc(exp(sum(tail(r1, 252)))-1, 0),
             perf = 100*(index_perf(r_limited)-1),
             basket = paste(gsub(' Equity', '', r[[length(r)]]$basket$main$names), collapse=', '))
    }
    
    return(res)
}

# prep_list=prep_data[seq(i*20+1, min(length(tags_list), (i+1)*20))]; top_mcap=5; file_postfix=0
# prep_list=prep_data[seq(i*per_page+1, min(length(prep_data), (i+1)*per_page))]; file_postfix=i
latex_segment_compare_path = function(prep_list, top_mcap, file_postfix){
    start_date = as.Date('2012-12-29')
    res = prep_list
    
    res = res[order(foreach(x=res,.combine=c)%do%x$perfTotNumber, decreasing=TRUE)]
    my_table <- paste(paste(foreach(x=res, .combine=c)%do%gsub('%', "\\\\%", paste(gsub('&', '/', x$segment), x$sd_all, x$perfTot252, x$perfTot, x$basket,
                                                                                   sep=' & ')), collapse=' \\\\[0.4em]\\hline\n'), ' \\\\[0.6em]\n')

    chart_path = save_data_as_chart(multi_plot_1, perf_to_chart_data(res), 'novo_chart.png', 400)
    if(FALSE){
        source('/home/aslepnev/git/ej/novo_latex_func.R')
        send_files_to_email(c(foreach(i=c(0),.combine=c)%do%latex_segment_compare_path(prep_data[seq(i*per_page+1, min(length(prep_data), (i+1)*per_page))], top_mcap, i)),
                            'Segment performances', 'aslepnev@novo-x.info')
    }
    
    knit('/home/aslepnev/git/ej/novo_latex3.tex', '/home/aslepnev/webhub/PDFs/segment_perf.tex')
    setwd('/home/aslepnev/webhub/PDFs')
    system('pdflatex segment_perf.tex')

    new_path = paste0('/home/aslepnev/webhub/PDFs/segment_perf_', file_postfix, '.pdf')
    system(paste0('cp segment_perf.pdf ', new_path))
    
#    library(mailR)
#    send_files_to_email(c(pdf_path), 'Segments', 'aslepnev@novo-x.info')

    return(new_path)
}
