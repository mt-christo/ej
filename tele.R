library(telegram)

readRenviron('~/git/ej/.Renviron')
bot <- TGBot$new(token = bot_token('CoinSight'))
bot$set_default_chat_id(282218584)

exec_tele = function(func, text = ''){
    tmp = paste0(tempfile(),'.png')
    png(tmp)
    func()
    dev.off()
    bot$sendMessage(text)
    bot$sendDocument(tmp)
}

plot_tele = function(x, y=NULL) { if(is.null(y)) exec_tele(function() { plot(x) }) else exec_tele(function() { plot(x, y) }) }

hist_tele = function(x, b=0) { exec_tele(function() { if(b==0) hist(x) else hist(x, br=b)}) }




