# UNI FILTERS

TAG_FILTERS <<- list()
TAG_FILTERS[[1]] = list(name='asia', target='etf', filter=list(list(field='geo_focus', value=c('Japan', 'Asian Pacific Region', 'China', 'Asian Pacific Region ex Japan', 'Greater China', 'South Korea', 'Taiwan', 'Hong Kong', 'Greater China,Hong Kong', 'India', 'Indonesia', 'Singapore', 'Malaysia', 'Thailand'))))
TAG_FILTERS[[2]] = list(name='asia', target='etf', filter=list(list(field='geo_focus2', value=c('Emerging Asia', 'Asia'))))
TAG_FILTERS[[3]] = list(name='west', target='etf', filter=list(list(field='geo_focus', value=c('United States', 'California', 'European Region', 'New York', 'Pennsylvania', 'Minnesota', 'Canada', 'New Jersey', 'Ohio', 'Eurozone', 'Virginia', 'Massachusetts', 'Oregon', 'Missouri', 'North American Region', 'Michigan', 'Germany', 'Maryland', 'United Kingdom', 'Australia', 'European Region,Australia', 'Global,United States', 'Spain', 'Kentucky', 'Arizona', 'Switzerland', 'North Carolina', 'Hawaii', 'Colorado', 'France', 'Singapore'))))
TAG_FILTERS[[4]] = list(name='west', target='etf', filter=list(list(field='geo_focus2', value=c('North America', 'Developed Europe'))))
TAG_FILTERS[[5]] = list(name='europe', target='etf', filter=list(list(field='geo_focus', value=c('European Region')), list(field='geo_focus2', type='allow', value=c('Developed Market'))))
TAG_FILTERS[[6]] = list(name='europe', target='etf', filter=list(list(field='geo_focus', value=c('Eurozone', 'Germany', 'United Kingdom', 'Spain', 'Switzerland', 'France'))))
TAG_FILTERS[[7]] = list(name='europe', target='etf', filter=list(list(field='geo_focus', value=c('International', 'Global'))))
TAG_FILTERS[[8]] = list(name='global', target='etf', filter=list(list(field='geo_focus2', value=c('Global'))))
TAG_FILTERS[[9]] = list(name='asia', target='equity', filter=list(list(field='country_name', value=c("CHINA", "INDIA", "SINGAPORE", "INDONESIA", "PHILIPPINES", "THAILAND", "BERMUDA", "HONG KONG", "BANGLADESH", "MALAYSIA", "VIETNAM", "KOREA", "JAPAN", "TAIWAN"))))
TAG_FILTERS[[10]] = list(name='west', target='equity', filter=list(list(field='country_name', value=c("UNITED STATES", "SWITZERLAND", "FRANCE", "GERMANY", "IRELAND", "AUSTRALIA", "CANADA", "BRITAIN", "NORWAY", "NETHERLANDS", "SPAIN", "SWEDEN", "LUXEMBOURG", "ITALY", "ISRAEL", "AUSTRIA", "BELGIUM", "DENMARK", "POLAND", "NEW ZEALAND"))))
TAG_FILTERS[[11]] = list(name='europe', target='equity', filter=list(list(field='country_name', value=c("SWITZERLAND", "FRANCE", "GERMANY", "IRELAND", "BRITAIN", "NORWAY", "NETHERLANDS", "SPAIN", "SWEDEN", "LUXEMBOURG", "ITALY", "AUSTRIA", "BELGIUM", "DENMARK"))))

TAG_FILTERS = c(TAG_FILTERS, list(list(name='health', target='etf', filter=list(list(field='ind_focus', value=c('Health Care'))))))
TAG_FILTERS = c(TAG_FILTERS, list(list(name='tech', target='etf', filter=list(list(field='ind_focus', value=c('Technology'))))))
TAG_FILTERS = c(TAG_FILTERS, list(list(name='finance', target='etf', filter=list(list(field='ind_focus', value=c('Financial'))))))
TAG_FILTERS = c(TAG_FILTERS, list(list(name='staples', target='etf', filter=list(list(field='ind_focus', value=c('Consumer Staples'))))))
TAG_FILTERS = c(TAG_FILTERS, list(list(name='discret', target='etf', filter=list(list(field='ind_focus', value=c('Consumer Discretionary'))))))
TAG_FILTERS = c(TAG_FILTERS, list(list(name='industrial', target='etf', filter=list(list(field='ind_focus', value=c('Industrials'))))))

TAG_FILTERS = c(TAG_FILTERS, list(list(name='us', target='equity', filter=list(list(field='country_name', value=c('UNITED STATES'))))))
#TAG_FILTERS = c(TAG_FILTERS, list(list(name='developedwest', target='equity', filter=list(list(field='country_name', value=c('UNITED STATES', 'FRANCE', 'BRITAIN', 'NETHERLANDS', 'GERMANY', 'SWITZERLAND', 'ITALY'))))))
TAG_FILTERS = c(TAG_FILTERS, list(list(name='tech', target='equity', filter=list(list(field='sector', value=c('Information Technology'))))))
TAG_FILTERS = c(TAG_FILTERS, list(list(name='commtech', target='equity', filter=list(list(field='sector', value=c('Information Technology', 'Communication Services'))))))
#TAG_FILTERS = c(TAG_FILTERS, list(list(name='energy', target='equity', filter=list(list(field='sector', value=c('Energy'))))))
#TAG_FILTERS = c(TAG_FILTERS, list(list(name='finance', target='equity', filter=list(list(field='sector', value=c('Financials'))))))
#TAG_FILTERS = c(TAG_FILTERS, list(list(name='telecom', target='equity', filter=list(list(field='sector', value=c('Communication Services'))))))
#TAG_FILTERS = c(TAG_FILTERS, list(list(name='staples', target='equity', filter=list(list(field='sector', value=c('Consumer Staples'))))))
#TAG_FILTERS = c(TAG_FILTERS, list(list(name='discret', target='equity', filter=list(list(field='sector', value=c('Consumer Discretionary'))))))
#TAG_FILTERS = c(TAG_FILTERS, list(list(name='industrial', target='equity', filter=list(list(field='sector', value=c('Industrials'))))))
#TAG_FILTERS = c(TAG_FILTERS, list(list(name='materials', target='equity', filter=list(list(field='sector', value=c('Materials'))))))
#TAG_FILTERS = c(TAG_FILTERS, list(list(name='health', target='equity', filter=list(list(field='sector', value=c('Health Care'))))))
#TAG_FILTERS = c(TAG_FILTERS, list(list(name='estate', target='equity', filter=list(list(field='sector', value=c('Real Estate'))))))
#TAG_FILTERS = c(TAG_FILTERS, list(list(name='utility', target='equity', filter=list(list(field='sector', value=c('Utilities'))))))
TAG_FILTERS = c(TAG_FILTERS, list(list(name='no_card', target='equity', filter=list(list(field='ticker', value=c('V US Equity', 'MA US Equity'))))))

#TAG_FILTERS = c(TAG_FILTERS, list(list(name='retail-stap', target='equity', filter=list(list(field='sector', value=c('Consumer Staples')), list(field='industry_group', value=c('Retail'))))))
#TAG_FILTERS = c(TAG_FILTERS, list(list(name='agriculture', target='equity', filter=list(list(field='sector', value=c('Consumer Staples')), list(field='industry_group', value=c('Agriculture'))))))
#TAG_FILTERS = c(TAG_FILTERS, list(list(name='cosmetics', target='equity', filter=list(list(field='sector', value=c('Consumer Staples')), list(field='industry_group', value=c('Cosmetics/Personal Care'))))))
#TAG_FILTERS = c(TAG_FILTERS, list(list(name='beverage', target='equity', filter=list(list(field='sector', value=c('Consumer Staples')), list(field='industry_group', value=c('Beverages'))))))
#TAG_FILTERS = c(TAG_FILTERS, list(list(name='food', target='equity', filter=list(list(field='sector', value=c('Consumer Staples')), list(field='industry_group', value=c('Food'))))))
#TAG_FILTERS = c(TAG_FILTERS, list(list(name='household', target='equity', filter=list(list(field='sector', value=c('Consumer Staples')), list(field='industry_group', value=c('Household Products/Wares'))))))

#TAG_FILTERS = c(TAG_FILTERS, list(list(name='internet-discr', target='equity', filter=list(list(field='sector', value=c('Consumer Discretionary')), list(field='industry_group', value=c('Internet'))))))
#TAG_FILTERS = c(TAG_FILTERS, list(list(name='retail-discr', target='equity', filter=list(list(field='sector', value=c('Consumer Discretionary')), list(field='industry_group', value=c('Retail'))))))
#TAG_FILTERS = c(TAG_FILTERS, list(list(name='apparel', target='equity', filter=list(list(field='sector', value=c('Consumer Discretionary')), list(field='industry_group', value=c('Apparel'))))))
#TAG_FILTERS = c(TAG_FILTERS, list(list(name='lodging', target='equity', filter=list(list(field='sector', value=c('Consumer Discretionary')), list(field='industry_group', value=c('Lodging'))))))
#TAG_FILTERS = c(TAG_FILTERS, list(list(name='auto', target='equity', filter=list(list(field='sector', value=c('Consumer Discretionary')), list(field='industry_group', value=c('Auto Manufacturers'))))))
#TAG_FILTERS = c(TAG_FILTERS, list(list(name='leisure', target='equity', filter=list(list(field='sector', value=c('Consumer Discretionary')), list(field='industry_group', value=c('Leisure Time'))))))

all_tag_filters_industry_group = function(data_folder = 'data-20190506'){
    u = get(load(paste0('/home/aslepnev/webhub/', data_folder, '/uber_uni_equity.RData')))

    t = u[sector!='#N/A N/A', unique(sector)]
    res1 = foreach(i=t)%do%  # i=t[1]
        list(name = i,
             target = 'equity',
             filter = list(list(field='sector', value=i)))
    
    t = u[sector!='#N/A N/A' & industry_group!='#N/A N/A', .N, by=c('sector', 'industry_group')]
    res2 = foreach(i=1:nrow(t))%do%  # i=1
        list(name = paste0(t[i, sector], '/ ', t[i, industry_group]),
             target = 'equity',
             filter = list(list(field='sector', value=c(t[i, sector])), list(field='industry_group', value=c(t[i, industry_group]))))
    return(c(res1, res2))
}

TAG_FILTERS = c(TAG_FILTERS, all_tag_filters_industry_group())

tag_id = 0
TAG_FILTERS = rbindlist(foreach(t = TAG_FILTERS)%do%{
    tag_id = tag_id+1
    rbindlist(foreach(f = t$filter)%do%rbindlist(foreach(v = f$value)%do%data.table(id=c(tag_id), name=c(t$name), target=c(t$target), field=c(f$field), value=c(v)), fill=TRUE, use.names=TRUE),
              fill=TRUE, use.names=TRUE)
}, fill=TRUE, use.names=TRUE)

# -- UNI FILTERS



# FACTOR MODEL

METRIC_NORM_OVER_UNI = c('pe', 'div')
METRIC_NORM_OVER_SECTOR = c('logmcap', 'sharpe')

LOADING_NORM_OVER_UNI = c('value', 'size')
LOADING_NORM_OVER_SECTOR = c('momentum', 'volatility')

BASIC_MODEL = list(value = list(c('pe', 'div'), c(1, 1))
                   volatility = list(c('sigma20', 'sigma40'), c(1, 1))
                   size = list('logmcap', 1)
                   momentum = list(c('pm12m1m', 'sharpe'), c(1, 1)))


# -- FACTOR MODEL


