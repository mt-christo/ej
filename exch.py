import requests, json

APIURLS = {'BITFINEX': ['https://api.bitfinex.com/v1/book/%SYMBOL%', 'BTCUSD'],
           'KRAKEN': ['https://api.kraken.com/0/public/Depth?pair=%SYMBOL%', 'XBTUSD'],
           'HITBTC': ['https://api.hitbtc.com/api/2/public/orderbook/%SYMBOL%', 'BTCUSD'],
           'GDAX': ['https://api.gdax.com/products/%SYMBOL%/book?level=2', 'BTC-USD']
}


def get_bitfinex():
    info = APIURLS['BITFINEX']
    r = json.loads(requests.get(info[0].replace('%SYMBOL%',info[1])).text)
    res = {}
    for key in ['bids','asks']:
        t = pd.DataFrame.from_dict(r[key])
        t['cs'] = (t.amount.astype(float)*t.price.astype(float)).cumsum()
        res[key] = t[t.cs > 100000].reset_index().price.astype(float)[0]
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

def get_gdax():
    info = APIURLS['GDAX']
    r = json.loads(requests.get(info[0].replace('%SYMBOL%',info[1])).text)
    res = {}
    for key in ['bids','asks']:
        t = pd.DataFrame.from_dict(r[key])
        t['cs'] = (t[1].astype(float)*t[0].astype(float)).cumsum()
        res[key] = t[t.cs > 100000].reset_index()[0].astype(float)[0]
    return({'bid':res['bids'], 'ask':res['asks']})

def get_hitbtc():
    info = APIURLS['HITBTC']
    r = json.loads(requests.get(info[0].replace('%SYMBOL%',info[1])).text)
    res = {}
    for key in ['bid','ask']:
        t = pd.DataFrame.from_dict(r[key])
        t['cs'] = (t.size.astype(float)*t.price.astype(float)).cumsum()
        res[key] = t[t.cs > 100000].reset_index().price.astype(float)[0]
    return({'bid':res['bid'], 'ask':res['ask']})

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
