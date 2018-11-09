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
import time
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
import apiclient.discovery
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

def start(bot, update):
    """Send a message when the command /start is issued."""
    update.message.reply_text('Hi!')

def help(bot, update):
    """Send a message when the command /help is issued."""
    update.message.reply_text('Help!')

def save_basket(text):
    bname = text.split(' ')[0]
    bsecs = text.split(' ')[1:]
    b = {'is_locked': False, 'desc': '', 'tickers': [ses[ses.CODE.str.startswith(x.upper())].CODE.iloc[0] for x in bsecs]}
    baskets = pickle.load(open('/home/aslepnev/git/ej/strbaskets.pickle', 'rb'))
    baskets[bname] = b 
    with open('/home/aslepnev/git/ej/strbaskets.pickle', 'wb') as tmp:
        pickle.dump(baskets, tmp, protocol=pickle.HIGHEST_PROTOCOL)
    return 'Basket '+bname+' saved'

def get_baskets():
    b = pickle.load(open('/home/aslepnev/git/ej/strbaskets.pickle', 'rb'))
    keys = [x if b[x]['is_locked'] else '' for x in b.keys()] + [x if not b[x]['is_locked'] else '' for x in b.keys()]
    keys = list(filter(None,keys))
    return '\n'.join([x+':\n'+(b[x]['desc']+'\n' if b[x]['desc']!='' else '')+'* '+'\n* '.join(b[x]['tickers'])+'\n' for x in keys])

def delete_basket(text):
    bname = text.split(' ')[0]
    baskets = pickle.load(open('/home/aslepnev/git/ej/strbaskets.pickle', 'rb'))
    res = ''
    if not baskets[bname]['is_locked']:
        del baskets[bname]
        res = 'Basket '+bname+' deleted'
    else:
        res = 'Cannot delete basket ' + bname
    with open('/home/aslepnev/git/ej/strbaskets.pickle', 'wb') as tmp:
        pickle.dump(baskets, tmp, protocol=pickle.HIGHEST_PROTOCOL)
    return res
        
def my_who(text):
    res = ses[ses.CODE.str.startswith(text.upper())]
    if len(res) > 0:
        return '\n'.join(res.apply(lambda x: str(x.CODE)+'\n('+str(x.NAME)+')\nmcap: '+str(x.MCAP)+'\n', axis=1))
    else:
        return 'No matching securities'

def plot_basket(bname):
    t = pickle.load(open('/home/aslepnev/git/ej/strbaskets.pickle', 'rb'))[bname]
    h = hist[hist.ticker.isin(t['tickers'])].groupby('dt').agg({'val': 'mean'}).reset_index()
    
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
    
    
def my_response(bot, update):
    text = update.message.text.lower()
    res = ''
    if text == 'help':
        res = 'Please type: "join swift"'
        update.message.reply_text(res)
    elif text.startswith('join swift'):
        update.message.reply_text('To join the network, please send the deposit to:')
        update.message.reply_text('3J98t1WpEZ73CNmQviecrnyiWrnqRhWNLy')
        time.sleep(1)
        update.message.reply_text('Deposit received')
        update.message.reply_text('Tell me your password (any number/letter combination):')
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
