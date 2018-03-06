import numpy as np
import pandas as pd
import sys
import os
import pickle
import json
import gspread
from oauth2client.service_account import ServiceAccountCredentials


chat_id = YOUR_CHAT_ID

api_id = 107358
api_hash = '810df710b738b62f1ba9f37c74a33289'

from telethon import TelegramClient
from telethon.tl.types import PeerUser, PeerChat, PeerChannel
from telethon.tl.functions.channels import GetParticipantsRequest
from telethon.tl.types import ChannelParticipantsSearch
from time import sleep


client = TelegramClient('session_id', api_id=api_id, api_hash=api_hash)
client.start()

client.send_message('coinsight_bot','hi')
d = client.get_dialogs(limit=200)
e = client.get_entity('https://t.me/tiesdb')
e = client.get_entity(432315842)
e = client.get_entity(522573392)

h = client.get_message_history("tiesdb", limit=15000)
x = pd.DataFrame([i.to_dict() for i in h.data])
with open('/home/aslepnev/git/ej/teleties.pickle', 'wb') as tmp:
    pickle.dump(x, tmp, protocol=pickle.HIGHEST_PROTOCOL)

offset, limit, p = 0, 100, []
while True:
    participants = client(GetParticipantsRequest(client.get_entity('https://t.me/tiesdb'), ChannelParticipantsSearch(''), offset, limit, hash=0))
    if not participants.users:
        break
    p.extend(participants.users)
    offset += len(participants.users)

x = pd.DataFrame([i.to_dict() for i in p])
with open('/home/aslepnev/git/ej/tiesusers.pickle', 'wb') as tmp:
    pickle.dump(x, tmp, protocol=pickle.HIGHEST_PROTOCOL)
    
teleties = pickle.load(open('/home/aslepnev/git/ej/teleties.pickle', 'rb'))
tm = teleties
tiu = pickle.load(open('/home/aslepnev/git/ej/tiesusers.pickle', 'rb'))



x.to_csv('tele_ties.csv')


credentials = ServiceAccountCredentials.from_json_keyfile_name('/home/aslepnev/a/gigi.json', ['https://spreadsheets.google.com/feeds'])
gc = gspread.authorize(credentials)
wks = gc.open("cryptoboard").worksheet('Sheet1')

tu = teleties[[True if isinstance(i, dict) and '_' in i else False for i in teleties.action]]
tu['act_type'] = [i.get('_') for i in tu.action]
tu = tu[tu.act_type.isin(['MessageActionChatAddUser', 'MessageActionChatJoinedByLink', 'MessageActionChatDeleteUser'])]
tu['user_add'] = 0
tu['user_inv'] = 0
tu['user_del'] = 0
tu.loc[tu.act_type=='MessageActionChatAddUser', 'user_add'] = 1
tu.loc[tu.act_type=='MessageActionChatJoinedByLink', 'user_inv'] = 1
tu.loc[tu.act_type=='MessageActionChatDeleteUser', 'user_del'] = 1
tu = tu.reset_index().sort_values('date', ascending=True).reset_index()
#tu = tu.set_index(pd.DatetimeIndex(tu.date))
tu = tu.set_index(tu.date)
tuu = tu.groupby(pd.TimeGrouper(freq='W')).agg({'user_add':'sum', 'user_inv':'sum', 'user_del':'sum'}).reset_index()

tdates, tadds, tinvs, tdels  = tuu.date.dt.date, tuu.user_add.cumsum(), tuu.user_inv.cumsum(), tuu.user_del.cumsum()
tdatecells = wks.range('A2:A'+str(len(tdates)+1))
taddcells = wks.range('B2:B'+str(len(tdates)+1))
tinvcells = wks.range('C2:C'+str(len(tdates)+1))
tdelcells = wks.range('D2:D'+str(len(tdates)+1))
for i in range(len(tdatecells)):
    tdatecells[i].value = tdates[i]
    taddcells[i].value = tadds[i]
    tinvcells[i].value = tinvs[i]
    tdelcells[i].value = tdels[i]
    
wks.update_cells(tdatecells)
wks.update_cells(taddcells)
wks.update_cells(tinvcells)
wks.update_cells(tdelcells)


ums = tm[~tm.message.isnull()].groupby('from_id').agg({'message': lambda x: ';'.join([str(y) for y in x])}).reset_index()
ums['user'] = [client.get_entity(x) for x in ums.from_id]
ums['username'] = [x.username for x in ums.user]
ums['firstname'] = [x.first_name for x in ums.user]
ums['lastname'] = [x.last_name for x in ums.user]

credentials = ServiceAccountCredentials.from_json_keyfile_name('/home/aslepnev/a/gigi.json', ['https://spreadsheets.google.com/feeds'])
gc = gspread.authorize(credentials)
wks = gc.open("cryptoboard").worksheet('Sheet2')
alphabet = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
cols = ['username','firstname','lastname','youtube','reddit','promote','record','facebook','twitter','invest','buy','bitcointalk','create','blog','listing','commit']
for j in range(len(cols)):
    tmp = ums[cols[j]] if cols[j] in ['username','firstname','lastname'] else [x.count(cols[j]) for x in ums.message]
    rng =  wks.range(alphabet[j]+'1:'+alphabet[j]+str(len(tmp)+1))
    rng[0].value = cols[j]
    for i in range(len(tmp)):
        rng[i+1].value = tmp[i]        
    wks.update_cells(rng)



    
taddcells = wks.range('B2:B'+str(len(tdates)+1))
tinvcells = wks.range('C2:C'+str(len(tdates)+1))
tdelcells = wks.range('D2:D'+str(len(tdates)+1))
for i in range(len(tdatecells)):
    tdatecells[i].value = tdates[i]
    taddcells[i].value = tadds[i]
    tinvcells[i].value = tinvs[i]
    tdelcells[i].value = tdels[i]

    

e = client.get_entity(19509)







p = client(GetParticipantsRequest(client.get_entity('https://t.me/tiesdb'), ChannelParticipantsSearch(''), 1, 2000, hash=0))






    

client.connect()
chat = InputPeerChat(chat_id)

total_count, messages, senders = client.get_message_history(
                        chat, limit=10)

for msg in reversed(messages):
    # Format the message content
    if getattr(msg, 'media', None):
        content = '<{}> {}'.format(  # The media may or may not have a caption
        msg.media.__class__.__name__,
        getattr(msg.media, 'caption', ''))
    elif hasattr(msg, 'message'):
        content = msg.message
    elif hasattr(msg, 'action'):
        content = str(msg.action)
    else:
        # Unknown message, simply print its class name
        content = msg.__class__.__name__

    text = '[{}:{}] (ID={}) {}: {} type: {}'.format(
            msg.date.hour, msg.date.minute, msg.id, "no name",
            content)
    print (text)
chat_id = YOUR_CHAT_ID

api_id = 107358
api_hash = '810df710b738b62f1ba9f37c74a33289'

from telethon import TelegramClient
from telethon.tl.types.input_peer_chat import InputPeerChat

client = TelegramClient('session_id', api_id=api_id, api_hash=api_hash)
client.connect()
chat = InputPeerChat(chat_id)

total_count, messages, senders = client.get_message_history(
                        chat, limit=10)

for msg in reversed(messages):
    # Format the message content
    if getattr(msg, 'media', None):
        content = '<{}> {}'.format(  # The media may or may not have a caption
        msg.media.__class__.__name__,
        getattr(msg.media, 'caption', ''))
    elif hasattr(msg, 'message'):
        content = msg.message
    elif hasattr(msg, 'action'):
        content = str(msg.action)
    else:
        # Unknown message, simply print its class name
        content = msg.__class__.__name__

    text = '[{}:{}] (ID={}) {}: {} type: {}'.format(
            msg.date.hour, msg.date.minute, msg.id, "no name",
            content)
    print (text)
