CCURL = 'https://min-api.cryptocompare.com/data/histoday?fsym=TIE&tsym=USD&limit=600&aggregate=1&e=CCCAGG'
t = pd.DataFrame(json.loads(requests.get(CCURL).text)['Data'])
t = t[t.volumefrom > 0]
t.time = t.time.apply(datetime.datetime.fromtimestamp)
t.to_csv('~/git/ej/tieusd.csv',float_format='%.8f')





import gspread
from oauth2client.service_account import ServiceAccountCredentials
import datetime
import pandas as pd
import numpy as np
import requests, json
from bittrex.bittrex import *

CCURL = 'https://min-api.cryptocompare.com/data/histoday?aggregate=1&e=BitTrex&fsym=%SYMBOL%&limit=50&tsym=BTC'
curr_list = ['ETH','LTC','XMR','ZEC','BCH','ETC','DASH','XRP','NEO']

prc, r1, r3, r5, r10, r30 = [], [], [], [], [], []
for cur in curr_list:    
    t = pd.DataFrame(json.loads(requests.get(CCURL.replace('%SYMBOL%',cur if cur!='BCC' else 'BCH')).text)['Data'])
    t.time = t.time.apply(datetime.datetime.fromtimestamp)
    prc = prc + [float(t.tail(1).close)]
    r1 = r1 + [float(t.tail(1).close/t.shift(1).tail(1).close - 1)]
    r3 = r3 + [float(t.tail(1).close/t.shift(3).tail(1).close - 1)]
    r5 = r5 + [float(t.tail(1).close/t.shift(5).tail(1).close - 1)]
    r10 = r10 + [float(t.tail(1).close/t.shift(10).tail(1).close - 1)]
    r30 = r30 + [float(t.tail(1).close/t.shift(30).tail(1).close - 1)]

my_bittrex = Bittrex("2f6bfe2450ec4b5d8095ffc6c1631bc9", "163137471fa942d58b7ae7787b384873", api_version="v1.1")

credentials = ServiceAccountCredentials.from_json_keyfile_name('/home/aslepnev/a/gigi.json', ['https://spreadsheets.google.com/feeds'])
gc = gspread.authorize(credentials)
wks = gc.open("bittrex")
wks = wks.worksheet('portfolio')

acells = wks.range('A2:A'+str(len(curr_list)+1))
bcells = wks.range('B2:B'+str(len(curr_list)+1))
ccells = wks.range('C2:C'+str(len(curr_list)+1))
dcells = wks.range('D2:D'+str(len(curr_list)+1))
ecells = wks.range('E2:E'+str(len(curr_list)+1))
fcells = wks.range('F2:F'+str(len(curr_list)+1))
gcells = wks.range('G2:G'+str(len(curr_list)+1))

for i in range(len(curr_list)):
    acells[i].value = curr_list[i]
    bcells[i].value = prc[i] * my_bittrex.get_balance(curr_list[i] if curr_list[i]!='BCH' else 'BCC')['result']['Balance']
    ccells[i].value = r1[i]
    dcells[i].value = r3[i]
    ecells[i].value = r5[i]
    fcells[i].value = r10[i]
    gcells[i].value = r30[i]
    
wks.update_cells(acells)
wks.update_cells(bcells)
wks.update_cells(ccells)
wks.update_cells(dcells)
wks.update_cells(ecells)
wks.update_cells(fcells)
wks.update_cells(gcells)
