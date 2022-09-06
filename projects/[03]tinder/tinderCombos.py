from langdetect import detect

messages_file = open('tinder_combo.csv','r')

for i in messages_file:
    language = detect(i)
    

