library(twilio)

Sys.setenv(TWILIO_SID = "AC4e9fd3a43636a1cd35378b3ba4cea636")
Sys.setenv(TWILIO_TOKEN = "a23e4aeab29502e507132b911d7a8059")
# Send a simple text message
tw_send_message("whatsapp:+79252093586", "whatsapp:+14155238886", "It is me your husband Anton :)))")
tw_send_message("whatsapp:+79164893312", "whatsapp:+79252093586", "It is me your husband Anton :)))")
# Send a picture message
#tw_send_message("2125557634", "9178675903", media_url = "https://www.r-project.org/logo/Rlogo.png")
# Send a picture message with text
#tw_send_message("2125557634", "9178675903", "Do you like the new logo?",
