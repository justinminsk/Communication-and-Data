library(tidytext)
library(tm)
library(wordcloud)
library(stringr)
library(dplyr)
library(knitr)
library(gutenbergr)
library(ggplot2)
library(wordcloud2)
library(reshape2)

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

#-------------

library(janeaustenr)

sns <- austen_books()
#bring in all of jane austen's books

sns <- sns%>%
  filter(book == "Sense & Sensibility")
#filter to just get Sense and Sensibility text

sns$book <- as.character(sns$book)
#un factor the book column

str_detect(sns$text, "^CHAPTER")
#make a bool list with true for anyhting begining with CHAPTER

sns <- sns%>%
  filter(!str_detect(sns$text, "^CHAPTER"))
#filter out all of the CHAPTER lines

sns[1:11, ]
#see only row 1 -11 all columns

sns <- sns[12:12574, ]
#make sns start after the title 

sns <- sns[1:12560, ]
#get rid of the end

words_df <- sns%>%
  unnest_tokens(word, text)
#split the lines into words

words_df <- words_df%>%
  filter(!(word %in% stop_words$word))
#get rid of common words that are not unique (the, a, etc.)

words_free <- words_df%>%
  group_by(word)%>%
  summarise(count = n())
#make a count of the word

wordcloud(words_free$word, words_free$count, min.freq = 25)
#make a word cloud

#---------------

packages <- c('dplyr', 'stringr', 'tidytext', 'tm', 'wordcloud')
knitr::write_bib(packages, 'bib.bib')
#add packages to references 

gutenberg_works(author == str_extract(author, "Poe, Edgar Allan"))
#see Poes work's IDs

df <- gutenberg_download(10031)
#get the complete poetical works of Edgar Allan Poe

words_df <- df%>%
  unnest_tokens(word, text)
#split the lines into words

words_df <- words_df%>%
  filter(!(word %in% stop_words$word))
#get rid of common words that are not unique (the, a, etc.)

words_df <- words_df%>%
  filter(!word == "thy" & !word == "thou" & !word == "thee")
#some older words not in out stop_word list that are not useful

words_free <- words_df%>%
  group_by(word)%>%
  summarise(count = n())%>%
  arrange(-count)
#make a count of the word

wordcloud(words_free$word, words_free$count, min.freq = 25)
#make a word cloud

#-------------

sentiments
#list of dicts of sentiments with assinged to words

get_sentiments('nrc')
#gives words a sentiment like fear, or happy, etc.

gutenberg_works(title == "Dracula")
#get Dracula id

nrc <- get_sentiments('nrc')
#put the nrc into a dataframe

nrc_fear <- nrc%>%
  filter(sentiment == 'fear')
#get only the fear words from nrc

dracula <- gutenberg_download(345)
#get dracula text

dracula_words <- dracula%>%
  unnest_tokens(word, text)
#split the lines into words

dracula_fear_words <- inner_join(nrc_fear, dracula_words)
#find words in dracula that also are in nrc_fear

dfw_free <- dracula_fear_words%>%
  group_by(word)%>%
  summarise(count = n())
#make a count of the word

wordcloud(dfw_free$word, dfw_free$count, min.freq = 7)
#create a word cloud of the fear words

#----------------

bing = get_sentiments('bing')
#give negtive or postive for words

dracula <- gutenberg_download(345)
#get dracula text

dracula$line <- 1:15568
#add the line column with a line number

dracula_words <- dracula%>%
  unnest_tokens(word, text)
#split the lines into words

dracula_words$group <- dracula_words$line %/% 80
#divide the lines into groups of 80 lines

dracula_sent <- inner_join(bing, dracula_words)
#join all the words in dracula and bing words

dracula_sent$gutenberg_id <- NULL
#get rid of the null column

dracula_sent$score <- 1
#make a new column and add a 1 to every spot

neg_rows <- which(dracula_sent$sentiment == 'negative')
# all the rows which contain negative

dracula_sent$score[neg_rows] <- -1
#make all of the scores where sent is negative to -1

dracula_sent <- dracula_sent%>%
  group_by(group)%>%
  summarise(group_sentiment = sum(score))
#group by the groups of 80 lines and add together the sent score

ggplot() +
  geom_col(data = dracula_sent, aes(x = group, y = group_sentiment), stat = 'identity', fill = '#e65c00', color = 'black')
#puts a column where each point is of group and group_sentiment
#used html color picker to find the color I wanted to use then used the hex code

#------------

afin = get_sentiments('afin')

dracula <- gutenberg_download(345)

dracula$line <- 1:15568

dracula$group <- dracula$line %/% 80

dracula_word <- dracula %>%
  unnest_tokens(word, text)

dracula_sent <-inner_join(afin, dracula_word)

dracula_sent <- dracula_sent %>%
  group_by(group) %>%
  summarise(group_sent = sum(score))

ggplot() +
  geom_col(data = dracula_sent, aes(x = group, y = group_sent), stat = 'identity', fill = '#e65c00', color = 'black')

#------------------

dracula <- gutenberg_download(345)
#get dracula

bing = get_sentiments('bing')
#get snetiment

dracula$line <- 1:15568
#get the lines

dracula_words <- dracula%>%
  unnest_tokens(word, text)
# break up the words from the lines

dracula_words$group <- dracula_words$line %/% 80
#break the lines into groups of 80

dracula_sent <- inner_join(bing, dracula_words)
#join bing and dracula words

dracula_sent$gutenberg_id <- NULL
#get rid of gutenberg_id

#------------------

dracula_pos <- dracula_sent%>%
  filter(sentiment == 'positive')%>%
  group_by(word)%>%
  summarise(count = n())%>%
  arrange(desc(count))%>%
  filter(count >= 66)%>%
  top_n(10, wt = count)
#get the top 10 words that are postive from smallest to greatest

dracula_pos$sentiment <- 'postive'
#add back postive sentiment

dracula_pos$word <- factor(dracula_pos$word, level = dracula_pos$word)
#factor out the words

ggplot()+
  geom_bar(data = dracula_pos, aes(x = word, y = count), stat = 'identity')+
  coord_flip()
#graph pos words

#----------------

dracula_neg <- dracula_sent%>%
  filter(sentiment == 'negative')%>%
  group_by(word)%>%
  summarise(count = n())%>%
  arrange(desc(count))%>%
  filter(count >= 53)%>%
  top_n(10, wt = count)
#get top 10 negtive words from smallest to greatest

dracula_neg$sentiment <- 'negative'
#add back negtive sentiment

dracula_neg$word <- factor(dracula_neg$word, level = dracula_neg$word)
#create factors by word

ggplot()+
  geom_bar(data = dracula_neg, aes(x = word, y = count), stat = 'identity')+
  coord_flip()
#graph neg words

#--------------------

dracula_comp <- rbind(dracula_neg, dracula_pos)
#rbind the two tables

ggplot()+
  geom_bar(data = dracula_comp, aes(x = word, y = count, color = sentiment, fill = sentiment), stat = 'identity')+
  coord_flip()+
  facet_wrap(~sentiment, scales = 'free_y')+
  scale_fill_manual(values = c('black', '#ea6205'))+
  scale_color_manual(values = c('#ea6205', 'black'))
#graph both sentiments top 10s with negtive on one side and postives on the other

#------------------

dracula <- gutenberg_download(345)
#get dracula

bing = get_sentiments('bing')
#get snetiment

dracula_words <- dracula%>%
  unnest_tokens(word, text)
# break up the words from the lines

dracula_words$gutenberg_id <- NULL
#get rid of gutenberg_id

dracula_words <- dracula_words%>%
  group_by(word)%>%
  summarise(freq = n())
#get the count of each word

dracula_sent <- inner_join(bing, dracula_words)
#join bing and dracula words

wordcloud(dracula_sent$word, dracula_sent$freq, min.freq = 5)
#worcloud of the words

dracula_matrix <- acast(dracula_sent, word~sentiment, value.var = 'freq', fill = 0)
#create a matrix with freq and sentiment combined into their own columns

comparison.cloud(dracula_matrix, colors = c('black', '#ff7433'))
#make a word cloud looking at freq and sentiment

dracula_sent$sentiment <- NULL
#get rid of sentiment

wordcloud2(dracula_sent, figPath = 'bat.jpg', backgroundColor = 'black', size = 0.5)
#make a prettier word cloud

#------------------------
#pdflatex article
#bibtex article
#pdflatex article
#\citep