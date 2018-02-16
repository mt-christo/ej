library(telegram)

readRenviron('~/git/ej/.Renviron')
bot <- TGBot$new(token = bot_token('CoinSight'))
bot$set_default_chat_id(282218584)

