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
import matplotlib.pyplot as plt
#import gspread
import httplib2
#import apiclient.discovery
from oauth2client.service_account import ServiceAccountCredentials
import matplotlib
matplotlib.use('Agg')

#from git.ej.novo_tele_state import init_state, state_path, save_state, get_state, update_current_state, save_state_csv
from novo_tele_state import init_state, state_path, save_state, get_state, update_current_state, save_state_csv

STATE_PATH = '/home/aslepnev/webhub/strtelestate_current.pickle'
R_STATE_PATH = '/home/aslepnev/webhub/strtelestate_current.csv'
R_STATE_DATA_MASK = '/home/aslepnev/webhub/strtelestate_current_name.csv'
BASKETS_PATH = '/home/aslepnev/git/ej/strbaskets.pickle'
U_PATH = '/home/aslepnev/webhub/grish_iter0_adapted_u.csv'
H_PATH = '/home/aslepnev/webhub/grish_iter0_adapted_h.csv'


def main():
    U = pd.read_csv(U_PATH, sep=',')
    H = pd.read_csv(H_PATH, sep=',')
    H = H.rename(columns={H.columns[0]: 'dt'})
    H['dt'] = pd.to_datetime(H['dt'])
    H = H.set_index(H['dt'])

    ro.r('source("/home/aslepnev/git/ej/strindexlib.R")')
 
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
