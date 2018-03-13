import requests, json

APIURLS = {'BITFINEX': ['https://api.bitfinex.com/v1/book/%SYMBOL%', 'BTCUSD'],
           'KRAKEN': ['https://api.kraken.com/0/public/Depth?pair=%SYMBOL%', 'XBTUSD'],
           'HITBTC': ['https://api.hitbtc.com/api/2/public/orderbook/%SYMBOL%', 'BTCUSD'],
           'GDAX': ['https://api.gdax.com/products/%SYMBOL%/book?level=2', 'BTC-USD'],
           'POLONIEX': ['https://poloniex.com/public?command=returnOrderBook&currencyPair=%SYMBOL%&depth=50', 'USDT_BTC'],
           'BITTREX': ['https://bittrex.com/api/v1.1/public/getorderbook?market=%SYMBOL%&type=both', 'USDT-BTC'],
           'ITBIT': ['https://api.itbit.com/v1/markets/%SYMBOL%/order_book', 'XBTUSD'],
           'UPHOLD': ['https://api.uphold.com/v0/ticker', 'BTCUSD'],
           'BINANCE': ['https://api.binance.com/api/v1/depth?symbol=%SYMBOL%', 'BTCUSDT']
}


def get_uphold():
    info = APIURLS['UPHOLD']
    r = json.loads(requests.get(info[0].replace('%SYMBOL%',info[1])).text)
    t = pd.DataFrame.from_dict(r)
    t = t[t.pair==info[1]]
    return({'bid':t.bid, 'ask':t.ask})

def get_uphold():
    info = APIURLS['UPHOLD']
    r = json.loads(requests.get(info[0].replace('%SYMBOL%',info[1])).text)
    t = pd.DataFrame.from_dict(r)
    t = t[t.pair==info[1]]
    return({'bid':t.bid, 'ask':t.ask})

def get_bittrex():
    info = APIURLS['BITTREX']
    r = json.loads(requests.get(info[0].replace('%SYMBOL%',info[1])).text)['result']
    res = {}
    for key in ['buy','sell']:
        t = pd.DataFrame.from_dict(r[key])
        t['cs'] = (t.Quantity.astype(float)*t.Rate.astype(float)).cumsum()
        res[key] = t[t.cs > 100000].reset_index().Rate.astype(float)[0]
    return({'bid':res['buy'], 'ask':res['sell']})

def get_hitbtc():
    info = APIURLS['HITBTC']
    r = json.loads(requests.get(info[0].replace('%SYMBOL%',info[1])).text)
    res = {}
    for key in ['bid','ask']:
        t = pd.DataFrame.from_dict(r[key])
        t['cs'] = (t.size.astype(float)*t.price.astype(float)).cumsum()
        res[key] = t[t.cs > 100000].reset_index().price.astype(float)[0]
    return({'bid':res['bid'], 'ask':res['ask']})

def get_gdax_type(exch):
    info = APIURLS[exch] # 'GDAX', 'POLONIEX', 'ITBIT'
    r = json.loads(requests.get(info[0].replace('%SYMBOL%',info[1])).text)
    res = {}
    for key in ['bids','asks']:
        t = pd.DataFrame.from_dict(r[key])
        t['cs'] = (t[1].astype(float)*t[0].astype(float)).cumsum()
        res[key] = t[t.cs > 100000].reset_index()[0].astype(float)[0]
    return({'bid':res['bids'], 'ask':res['asks']})

def get_kraken(ticker_hm): 
    info = APIURLS['KRAKEN']
    r = json.loads(requests.get(info[0].replace('%SYMBOL%',info[1])).text)['result'][ticker_hm]
    res = {}
    for key in ['bids','asks']:
        t = pd.DataFrame.from_dict(r[key])
        t['cs'] = (t[1].astype(float)*t[0].astype(float)).cumsum()
        res[key] = t[t.cs > 100000].reset_index()[0].astype(float)[0]
    return({'bid':res['bids'], 'ask':res['asks']})

def get_bitfinex():
    info = APIURLS['BITFINEX']
    r = json.loads(requests.get(info[0].replace('%SYMBOL%',info[1])).text)
    res = {}
    for key in ['bids','asks']:
        t = pd.DataFrame.from_dict(r[key])
        t['cs'] = (t.amount.astype(float)*t.price.astype(float)).cumsum()
        res[key] = t[t.cs > 100000].reset_index().price.astype(float)[0]
    return({'bid':res['bids'], 'ask':res['asks']})

credentials = ServiceAccountCredentials.from_json_keyfile_name('/home/aslepnev/a/gigi.json', ['https://spreadsheets.google.com/feeds'])
gc = gspread.authorize(credentials)
wks = gc.open("brokerboard")
wks = wks.worksheet('board')

res = {'BITFINEX': get_bitfinex(),
       'KRAKEN': get_kraken('XXBTZUSD'),
       'GDAX': get_gdax(),
       'HITBTC': get_hitbtc()}

ecells = wks.range('A3:A'+str(len(res)+2))
bcells = wks.range('B3:B'+str(len(res)+2))
ocells = wks.range('C3:C'+str(len(res)+2))

i = 0
for key in res.keys():
    ecells[i].value = key
    bcells[i].value = res[key]['bid']
    ocells[i].value = res[key]['ask']
    i = i+1 
    
wks.update_cells(ecells)
wks.update_cells(bcells)
wks.update_cells(ocells)
