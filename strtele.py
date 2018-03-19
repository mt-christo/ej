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

from telegram.ext import Updater, CommandHandler, MessageHandler, Filters
import numpy as np
import pandas as pd
import sys
import os
import pickle
import json
import gspread
from oauth2client.service_account import ServiceAccountCredentials

# Enable logging
logging.basicConfig(format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
                    level=logging.INFO)

logger = logging.getLogger(__name__)


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
    b = [ses[ses.CODE.str.startswith(x.upper())].CODE.iloc[0] for x in bsecs]
    baskets = pickle.load(open('/home/aslepnev/git/ej/strbaskets.pickle', 'rb'))
    baskets[bname] = b 
    with open('/home/aslepnev/git/ej/strbaskets.pickle', 'wb') as tmp:
        pickle.dump(baskets, tmp, protocol=pickle.HIGHEST_PROTOCOL)
    return 'Basket '+bname+' saved'

def get_baskets():
        b = pickle.load(open('/home/aslepnev/git/ej/strbaskets.pickle', 'rb'))
        return '\n'.join([x+':\n* '+'\n* '.join(b[x])+'\n' for x in b])

def delete_basket(text):
    bname = text.split(' ')[0]
    baskets = pickle.load(open('/home/aslepnev/git/ej/strbaskets.pickle', 'rb'))
    del baskets[bname]
    with open('/home/aslepnev/git/ej/strbaskets.pickle', 'wb') as tmp:
        pickle.dump(baskets, tmp, protocol=pickle.HIGHEST_PROTOCOL)
    return 'Basket '+bname+' deleted'
        
def my_who(text):
    res = ses[ses.CODE.str.startswith(text.upper())]
    if len(res) > 0:
        return '\n'.join(res.apply(lambda x: x.CODE+'\n('+x.NAME+')\nmcap: '+x.MCAP+'\n', axis=1))
    else:
        return 'No matching securities'

def my_response(bot, update):
    """Echo the user message."""
    text = update.message.text.lower()
    res = ''
    if text =='products':
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

def error(bot, update, error):
    """Log Errors caused by Updates."""
    logger.warning('Update "%s" caused error "%s"', update, error)


def main():
    ses = pd.read_csv('secs.csv', sep=';')
    ses.CODE = ses.CODE.apply(lambda x: ' '.join(x.replace('   ',' ').replace('  ',' ').split(' ')[:2]))
    ses.MCAP = ses.MCAP.apply(lambda x: '{:,.0f} MM'.format(x/1000000))
    ses.loc[ses.MCAP == 'nan MM', 'MCAP'] = 'No Data'

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



