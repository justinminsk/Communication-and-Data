library(tidytext)
library(tm)
library(wordcloud)

#--------------

lines <- scan("lyrics.txt", what = character(), sep = '\n')
#take all of the lyrics from the text doc and create an array by line

lines_df <- data_frame(line = 1:119, text = lines)
#make a data frame with na index and each line

words_df <- lines_df%>%
  unnest_tokens(word, text)
#store the words and maintain what line they where on

words_df <- words_df%>%
  filter(!(word %in% stop_words$word))
#takes out all of the common words form the list of words form the songs

words_free <- words_df%>%
  group_by(word)%>%
  summarise(count = n())
#get a count of the words

wordcloud(words_free$word, words_free$count, colors =  brewer.pal(5, "Dark2"))
#create a wordcloud

