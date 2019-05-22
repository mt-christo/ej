-- FASHION fixed  --  2019-05-21

u = load_uni('data-20190506', c('equity', 'equity_metrics', 'h_usd', 'fxx', 'libors'), list(fixed_list=c('4911 JP Equity', 'EL US Equity', 'PG US Equity', 'TSCO LN Equity', 'PEP US Equity', 'BN FP Equity', 'NESN SW Equity', 'WMT US Equity')))

h = u$h_usd
b = data.table(id=1, weight=1, name='ALL')
f = data.table(ticker=u$equity$ticker)[, ':='(basket_id=1, name=ticker, weight=0.25)][, id:=1:nrow(u$equity)]

chart_data = list()
wnd = 80; vc_vt = 0.14
r = build_index_simple(h, get_rebals_h(h, 'month'), smidai_style_rebal, screen_params=list(funds=f, baskets=b, window=wnd, voltarget=0.08), start_date='2013-12-31')
r_smidai = foreach(x=r,.combine=rbind)%do%x$h
rvc_smidai = volcontrol_excess(r_smidai, list(window=20, type='none', excess_type = 'libor plus', add_rate=1, excess=3.5, level=vc_vt, max_weight=2, rate_basis=360, vc_basis=252), libors)
chart_data = c(chart_data, list(list(segment=paste('risk/return opt rebal,', wnd, vc_vt*100, ' vt/3.5 excess/1 fee/1.75 exp/252'), rt=rvc_smidai)))
tail(index_perf(rvc_smidai), 1)

#r_mt = foreach(x=build_index_simpler(u, 'month', screen_mixed_top, screen_params=list(perf_weight=3, top_n=8, price_window=wnd, voltarget=0.075), '2013-12-31'),.combine=rbind)%do%x$h
#rvc_mt = volcontrol_excess(r_mt, list(window=20, type='none', excess_type = 'libor plus', add_rate=1, excess=3.5, level=vc_vt, max_weight=2, rate_basis=360, vc_basis=252), libors)
#chart_data = c(chart_data, list(list(segment=paste('topN', wnd, 2, '8/3/1/2.5/365'), rt=rvc_mt)))


xly = load_uni('data-20190506', c('etf', 'h_usd'), list(fixed_list=c('XLY     US Equity')))$h_usd
chart_data = c(chart_data, list(list(segment='XLY', rt=xly)))

library(ggthemes)
save_data_as_chart(multi_plot_1, rt_to_chart_data(chart_data), 'novo_chart.png', 400)
a = index_perf(rvc_smidai); write.csv(a, file='/home/aslepnev/webhub/basket8_backtest.csv', row.names=index(a))









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
