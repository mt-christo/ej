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
