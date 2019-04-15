Term & Return & \vtop{\hbox{\strut Volatility}\hbox{\strut Target}} & VC type & \vtop{\hbox{\strut 1Y Realized}\hbox{\strut Volatility}}\\
\midrule
3Y & 8\% & max 10 back & 8.5\% & 23\% \\[0.6em]
5Y & 9\% & max 10 back & 8.5\% & 33\% \\[0.6em]
7Y & 10\% & avg 10 back & 8.5\% & 43\% \\[0.6em]
3Y & 8\% & max 10 back & 8.5\% & 23\% \\[0.6em]
5Y & 9\% & max 10 back & 8.5\% & 33\% \\[0.6em]
7Y & 10\% & avg 10 back & 8.5\% & 43\% \\[0.6em]

# libors=u$libors; r_in=r; r1_in=r1; terms_in=c(3, 5); vc_targets=c(0.12, 0.14); vc_types=c('max 10', 'simple');
latex_pm_card = function(r_in, r1_in, terms_in, vc_targets, vc_types, libors){  
# r2 = volcontrol_excess(r1, list(window=20, type='max 10', excess_type = 'libor plus', add_rate=1.5, excess=3.5, level=0.14, max_weight=1.75), u$libors); print(sqrt(252)*sd(tail(rvc, 252))); print(tail(exp(cumsum(rvc)), 1))

    exrate = 3.5
    add_rate = 1.5
    max_weight = 1.75

    my_charts = list()
    res = rbindlist(foreach(t=terms_in, .combine=c)%do%{  # t=5
        foreach(vt=vc_targets, .combine=c)%do%{  # vt=0.14
            foreach(vctype=vc_types)%do%{  # vctype='max 10'
                vc_tmp = list(window=20, type=vctype, excess_type = 'libor plus', add_rate=add_rate, excess=exrate, level=vt, max_weight=max_weight)
                x = volcontrol_excess(r1_in, vc_tmp, libors);
                start_date = tail(index(x), 1)
                year(start_date) = year(start_date) - t
                x = x[index(x) >= start_date]
                if(t==max(terms_in))
                    my_charts = c(my_charts, list(exp(cumsum(x))))
                data.table(term = paste0(t, 'Y'),
                           rv = fracperc(as.numeric(sqrt(252)*sd(tail(x, 252))), 2),
                           ret_num = as.numeric(tail(exp(cumsum(x)), 1)),
                           ret = fracperc(as.numeric(tail(exp(cumsum(x)), 1)), 0),
                           vt = fracperc(vt, 0),
                           vctype = vctype)
            }
        }
    })
    res = res[order(ret_num, decreasing=TRUE), ]

    my_table <- paste(paste(foreach(i=1:nrow(res), .combine=c)%do%gsub('%', "\\\\%", paste(res[i, .(term, ret, vt, vctype, rv)], collapse=' & ')), collapse=' \\\\[0.4em]\n'), ' \\\\[0.6em]\n')
    my_list <- paste(as.character(t(t(as.data.table(strsplit(tail(r_in, 1)[[1]]$basket$main$names, ' '))))[1,]), collapse=', ')
    exrate_txt <- paste0(exrate, '\\%')
    add_rate_txt <- paste0(add_rate, '\\%')
    max_weight_txt <- gsub('%', "\\\\%", fracperc(max_weight, 0))

    chart_path = save_data_as_chart(foreach(x=my_charts,.combine=cbind)%do%x, 'Index Performance', 'PDFs/novo_chart.png')
    knit('/home/aslepnev/webhub/PDFs/novo_latex2.tex', '/home/aslepnev/webhub/PDFs/index_card.tex')
    setwd('/home/aslepnev/webhub/PDFs')
    system('pdflatex index_card.tex')

    library(mailR)
    send_files_to_email(c(#save_data_as_csv(idx$baskets, 'top10it_baskets.csv'),
        '/home/aslepnev/webhub/PDFs/index_card.pdf'),
        'Top 10 IT index', 'aslepnev@novo-x.info')
}
