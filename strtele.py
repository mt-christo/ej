#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""Simple Bot to reply to Telegram messages.

This program is dedicated to the public domain under the CC0 license.

This Bot uses the Updater class to handle the bot.

First, a few handler functions are defined. Then, those functions are passed to
the Dispatcher and registered at their respective places.
Then, the bot is started and runs until we press Ctrl-C on the command line.

Usage:
Basic Echobot example, repeats messages.
Press Ctrl-C on the command line or send a signal to the process to stop the
bot.
"""

from rpy2.robjects.packages import importr
import rpy2.robjects as ro
from telegram.ext import Updater, CommandHandler, MessageHandler, Filters
import numpy as np
import pandas as pd
import collections
import random
import sys
import os
import pickle
import json
#import gspread
import httplib2
#import apiclient.discovery
from oauth2client.service_account import ServiceAccountCredentials
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

# Enable logging
#logging.basicConfig(format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
#                    level=logging.INFO)

#logger = logging.getLogger(__name__)


# Define a few command handlers. These usually take the two arguments bot and
# update. Error handlers also receive the raised TelegramError object in error.

BASKETS_PATH = '/home/aslepnev/git/ej/strbaskets.pickle'
U_PATH = '/home/aslepnev/webhub/grish_iter0_adapted_u.csv'
H_PATH = '/home/aslepnev/webhub/grish_iter0_adapted_h.csv'


def toGridRange(sheet, cellsRange):
    sheetId = sheet['sheets'][0]['properties']['sheetId']
#    if sheetId is None:
#        raise SheetNotSetError()
    if isinstance(cellsRange, str):
        startCell, endCell = cellsRange.split(":")[0:2]
        cellsRange = {}
        rangeAZ = range(ord('A'), ord('Z') + 1)
    if ord(startCell[0]) in rangeAZ:
        cellsRange["startColumnIndex"] = ord(startCell[0]) - ord('A')
        startCell = startCell[1:]
    if ord(endCell[0]) in rangeAZ:
        cellsRange["endColumnIndex"] = ord(endCell[0]) - ord('A') + 1
        endCell = endCell[1:]
    if len(startCell) > 0:
        cellsRange["startRowIndex"] = int(startCell) - 1
    if len(endCell) > 0:
        cellsRange["endRowIndex"] = int(endCell)
        cellsRange["sheetId"] = sheetId
    return cellsRange


def start(bot, update):
    """Send a message when the command /start is issued."""
    update.message.reply_text('Hi!')

    
def help(bot, update):
    """Send a message when the command /help is issued."""
    update.message.reply_text('Help!')


def match_tickers(tickers):
    return [U[U.ticker.str.startswith(x.upper())].ticker.iloc[0]
            for x in tickers]
    
    
def save_basket(text):
    baskets = get_all_baskets()
    baskets[bname] = {'name': text.split(' ')[0],
                      'desc': '',
                      'tickers': match_tickers(text.split(' ')[1:])}
    save_all_baskets(baskets)
    return 'Basket '+bname+' saved'


# basket = dict{ name, desc, tickers[] }
def get_all_baskets():
    b = pickle.load(open(BASKETS_PATH, 'rb'))
    return b


def save_all_baskets(baskets):
    with open(BASKETS_PATH, 'wb') as tmp:
        pickle.dump(baskets, tmp, protocol=pickle.HIGHEST_PROTOCOL)
    return 'Baskets saved'


def print_baskets(baskets):
    return '\n'.join([x + ':\n' +
                      (b[x]['desc'] + '\n' if b[x]['desc'] != '' else '') +
                      '* ' + '\n* '.join(b[x]['tickers']) + '\n'
                      for x in baskets])


def delete_basket(text):
    baskets = get_all_baskets()
    del baskets[text.split(' ')[0]]
    save_all_baskets(baskets)
    return 'Basket ' + bname + ' deleted'


def my_who(text):
    res = U[U.ticker.str.startswith(text.upper())]
    if len(res) > 0:
        return '\n'.join(res.apply(lambda x: str(x.CODE) + '\n(' +
                                   str(x.NAME) + ')\nmcap: ' +
                                   str(x.MCAP) + '\n', axis=1))
    else:
        return 'No matching securities'


# bname, tail_days = 'fin1', 250
def plot_basket_bunch(bname, tail_days):
    b = get_all_baskets()[bname]
    h = H.loc[:, b['tickers']]
    h1 = 100*(np.exp(h.tail(tail_days).cumsum()) - 1).rolling(3).mean()
    
    matplotlib.style.use('ggplot')
    
    p = h.plot(kind='line', x='date', y='val', linewidth=4, title=bname+' basket performance, %', legend=False, color='green', antialiased=True)
    f = p.get_figure()
    filename = 'plot.png'
    f.savefig(filename)
    plt.close()   
#    client.send_file('coinsight_bot','plot.png')
    return filename
    
    
def plot_basket_single(bname):
    t = pickle.load(open('/home/aslepnev/git/ej/strbaskets.pickle', 'rb'))[bname]
    h = hist[hist.ticker.isin(t['tickers'])].groupby('dt').agg({'val': 'mean'}).reset_index()
#    h1 = (np.exp(h) - 1).mean(axis=1)
    
    h['date'] = pd.to_datetime(h.dt)
    h.val = (h.val.cumsum().rolling(20).mean()+1)*100
    
    matplotlib.style.use('ggplot')
    
    p = h.plot(kind='line', x='date', y='val', linewidth=4, title=bname+' basket performance, %', legend=False, color='green', antialiased=True)
    f = p.get_figure()
    filename = 'plot.png'
    f.savefig(filename)
    plt.close()   
#    client.send_file('coinsight_bot','plot.png')
    return filename

def calc_wo(basket,params):
    params_fn = '~/git/ej/wo_params.csv'
    quotes_fn = '~/git/ej/wo_quotes.csv'
    pd.DataFrame({'param':['coupon','strikes'], 'value':params}).to_csv(params_fn)
    hist[hist.ticker.isin(basket['tickers'])].to_csv(quotes_fn)
    return np.asarray(ro.r('wo_calculator_web("'+params_fn+'","'+quotes_fn+'")'))[0]


def report_wo(val, bname, basket, params):
    t = ses[ses.CODE.isin(basket['tickers'])]

#    credentials = ServiceAccountCredentials.from_json_keyfile_name('/home/aslepnev/a/gigi.json', ['https://spreadsheets.google.com/feeds', 'https://www.googleapis.com/auth/drive'])

    credentials = ServiceAccountCredentials.from_json_keyfile_name('/home/aslepnev/a/gigi.json', ['https://www.googleapis.com/auth/spreadsheets', 'https://www.googleapis.com/auth/drive'])
    httpAuth = credentials.authorize(httplib2.Http())
    service = apiclient.discovery.build('sheets', 'v4', http = httpAuth)

    spreadsheet = service.spreadsheets().create(body = {
        'properties': {'title': "product_" + str(random.randint(1,999999999999)), 'locale': 'ru_RU'},
        'sheets': [{'properties': {'sheetType': 'GRID',
                                   'sheetId': 0,
                                   'title': 'Product',
                                   'gridProperties': {'rowCount': 30, 'columnCount': 25}}}]}).execute()
    
    driveService = apiclient.discovery.build('drive', 'v3', http = httpAuth)
    shareRes = driveService.permissions().create(
        fileId = spreadsheet['spreadsheetId'],
        sendNotificationEmail = False, 
        body = {'type': 'user', 'role': 'writer', 'emailAddress': 'antonslepnev@gmail.com'},
        fields = 'id'
    ).execute()
    

    results = service.spreadsheets().batchUpdate(spreadsheetId = spreadsheet['spreadsheetId'], body = {
        'requests':[
            {'repeatCell': {
                'range': toGridRange(spreadsheet, "A"+str(6+len(t) + 3)+":A"+str(6+len(t) + 3)),
                'cell': {
                    'userEnteredFormat': {
                        'textFormat': {
                            'red': 1.0,
                            'green': 1.0,
                            'blue': 0.5,
                        },
                    },
                },
                'fields': 'userEnteredFormat(backgroundColor)'}},
            ]
    }).execute()

    results = service.spreadsheets().values().batchUpdate(spreadsheetId = spreadsheet['spreadsheetId'], body = {
        "valueInputOption": "USER_ENTERED",
        "data": [
            {"range": "Product!A1:A5",
             "majorDimension": "COLUMNS",     # сначала заполнять ряды, затем столбцы (т.е. самые внутренние списки в values - это ряды)
             "values": [['Worst-Of product indicative pricing report', '', 'Basket:', bname, basket['desc']]]},            
            {"range": "Product!A6:C" + str(6+len(t) + 1),
             "majorDimension": "COLUMNS",  # сначала заполнять столбцы, затем ряды (т.е. самые внутренние списки в values - это столбцы)
             "values": [['CODE']+t.CODE.replace(np.nan,'').tolist(), ['NAME']+t.NAME.replace(np.nan,'No Data').tolist(), ['MARKET CAP']+t.MCAP.replace(np.nan,'').tolist()]},
            {"range": "Product!A"+str(6+len(t) + 2)+":A"+str(6+len(t) + 3),
             "majorDimension": "COLUMNS",     # сначала заполнять ряды, затем столбцы (т.е. самые внутренние списки в values - это ряды)
             "values": [['Product price:', str(val).replace('.',',')]]}, 
        ]
    }).execute()


    results = service.spreadsheets().batchUpdate(spreadsheetId = spreadsheet['spreadsheetId'], body = {
        'requests':[
            {'repeatCell': {
                'range': toGridRange(spreadsheet, "A"+str(6+len(t) + 3)+":A"+str(6+len(t) + 3)),
                'cell': {
                    'userEnteredFormat': {
                        'numberFormat': {
                            'type': 'NUMBER',
                            'pattern': '##.#%',
                        },
                    },
                },
                'fields': 'userEnteredFormat(numberFormat)'}},
            ]
    }).execute()

    return 'https://docs.google.com/spreadsheets/d/' + spreadsheet['spreadsheetId']


def reply_wo(text):
    items = text.split(' ')
    bname = items[0]
    params = items[1:3]
    basket = pickle.load(open('/home/aslepnev/git/ej/strbaskets.pickle', 'rb'))[bname] 
    val = calc_wo(basket,params)
    res = [str(val)+' %']
    if len(items) > 3:
        res = res + ['Please see product card on Google Drive: ' + report_wo(val, bname, basket, params)]
    return res


def smart_recognize(x):
    

    
def my_response(bot, update):
    """Echo the user message."""
    text = update.message.text.lower()
    res = ''
    if text == 'help':
        res = 'Commands:'
        res = res + '\n*products: list our product types'
        res = res + '\n*who <text>: list securities starting with <text>'
        res = res + '\n*baskets: list your baskets'
        res = res + '\n*basket <name sec1 sec2 ..>: save basket with members'
        res = res + '\n*delete <name>: delete basket <name>'
        res = res + '\n*plot <name>: price chart for equal-weighted basket <name>'
        res = res + '\n\nIndicative pricing:'
        res = res + '\n*worstof <name> <n1-n2-...>: worst-of product, basket <name> with strikes n[i]'
        res = res + '\n*basketcall <name> <n1-n2-...>: ATM Call option, basket <name>'
        update.message.reply_text(res)
    elif text == 'products':
        res = "worstof:  pays off coupon if all stocks are above that year's strike\n\nbasketcall:  pays Call option payoff on equal-weighted basket"
        update.message.reply_text(res)
    elif text == 'baskets':
        res = get_baskets()
        update.message.reply_text(res)
    elif text.startswith('who '):
        res = my_who(text[4:1000])
        update.message.reply_text(res)
    elif text.startswith('basket '):
        res = save_basket(text[7:1000])
        update.message.reply_text(res)
        update.message.reply_text(get_baskets())
    elif text.startswith('delete '):
        res = delete_basket(text[7:1000])
        update.message.reply_text(res)
        update.message.reply_text(get_baskets())
    elif text.startswith('plot '):
        print(text)
        res = plot_basket(text[5:1000])
        update.message.reply_text(text[5:1000] + ' price chart:')
        update.message.reply_photo(open(res, 'rb'))
    elif text.startswith('worstof '):
        update.message.reply_text('Calculating..')
        res = reply_wo(text[8:1000])
        for x in res:
            update.message.reply_text(x)

def error(bot, update, error):
    """Log Errors caused by Updates."""
    logger.warning('Update "%s" caused error "%s"', update, error)


def main():
    U = pd.read_csv(U_PATH, sep=',')
    H = pd.read_csv(H_PATH, sep=',')
    H = H.rename(columns={H.columns[0]: 'dt'})
    H['dt'] = pd.to_datetime(H['dt'])
    H = H.set_index(H['dt'])

#    hist = pd.read_csv('~/git/ej/hist_sm.csv')[['dt','ticker','val']]
#    hist.ticker = hist.ticker.str.replace('.Equity','').str.replace('.',' ')

    ro.r('source("/home/aslepnev/git/ej/novo_quant_func.R")')
 
    """Start the bot."""
    # Create the EventHandler and pass it your bot's token.
    updater = Updater("593240041:AAGP_UuWIb53NyNWm4ezUsTlxVEkPeSdT3k")

    # Get the dispatcher to register handlers
    dp = updater.dispatcher

    # on different commands - answer in Telegram
    dp.add_handler(CommandHandler("start", start))
    dp.add_handler(CommandHandler("help", help))

    # on noncommand i.e message - echo the message on Telegram
    dp.add_handler(MessageHandler(Filters.text, my_response))

    # log all errors
    dp.add_error_handler(error)

    # Start the Bot
    updater.start_polling()

    # Run the bot until you press Ctrl-C or the process receives SIGINT,
    # SIGTERM or SIGABRT. This should be used most of the time, since
    # start_polling() is non-blocking and will stop the bot gracefully.
    updater.idle()


if __name__ == '__main__':
    main()


    

#library(data.table)
#library(foreach)
#library(xts)
#s = data.frame(fread('~/git/ej/hist_small.csv'))
#p = foreach(i=seq(1,ncol(s)/2,by=2),.combine='merge.xts')%do%{ print(i); x=s[s[,i]!='',c(i,i+1)]; xts(x[,2],order.by=as.Date(x[,1],format='%d.%m.%Y')) }
#p = na.locf(p)
#p = exp(diff(log(p)))-1
#names(p) = names(s)[seq(1,ncol(s)/2,by=2)]
#t = foreach(i=1:ncol(p),.combine=rbind)%do%data.table(dt=index(p),ticker=names(p)[i],val=as.numeric(p[,i]))
#write.csv(t,file='~/git/ej/hist_sm.csv')
