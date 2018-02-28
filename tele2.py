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
h = client.get_message_history("tiesdb", limit=10)


p = client(GetParticipantsRequest(client.get_entity('https://t.me/tiesdb'), ChannelParticipantsSearch(''), 1, 10, hash=0))

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
