#---------------
hurst_scan<-scan('mercyhurst.txt',what=character(),sep='\n')
hurst_lines<-data_frame(line=1:24066,text=hurst_scan)
hurst_lines$group<-hurst_lines$line %/% 80
hurst_words<-unnest_tokens(hurst_lines,word,text)

afinn<-get_sentiments('afinn')

hurst_words<-inner_join(hurst_words,afinn)

hurst_groups<-hurst_words%>%
  group_by(group)%>%
  summarize(sentiment=sum(score))

ggplot()+
  geom_col(data=hurst_groups,aes(x=group,y=sentiment))
#------------------------
gutenberg_works(str_detect(title,'Frankenstein'))
frankenstein<-gutenberg_download(84)
frankenstein_words<-unnest_tokens(frankenstein,word,text)
frankenstein_words$word_number<-1:75175
frankenstein_words$gutenberg_id<-NULL

afinn<-get_sentiments('afinn')
frankenstein_words<-inner_join(frankenstein_words,afinn)
frankenstein_words$acc_sent<-cumsum(frankenstein_words$score)

ggplot()+
  geom_line(data=frankenstein_words,aes(x=word_number,y=acc_sent))
#-------------------
#\documentclass{article}
#\usepackage{natbib}
#\begin{document}
#\title{My Article}
#\author{Charles Redmond}
#\maketitle
#\section{My First Section}
#Lorem ipsum dolor sit amet, consectetur adipiscing elit. Duis ornare, libero quis sodales bibendum, dolor augue placerat arcu, sed pellentesque ante urna vel ex. Pellentesque eget purus eget velit ornare sodales et nec augue. In nec diam nibh. Nulla facilisi. Praesent tristique scelerisque lacus, at sollicitudin magna ultrices non. Mauris imperdiet eleifend sapien at mollis. Nullam id justo sollicitudin, euismod purus at, molestie nunc. Quisque et tellus sed risus sollicitudin lacinia viverra sit amet felis. Morbi hendrerit ultrices mauris, non iaculis justo cursus in. Pellentesque eget velit eget neque lobortis scelerisque quis et felis. Mauris commodo magna leo, eu tempor odio varius sed. Etiam erat ipsum, porttitor pellentesque dolor vitae, porta rhoncus ligula.
#Ut mi magna, congue vitae sem quis, sodales porttitor urna. Nunc ornare faucibus arcu id tempor. Etiam congue vulputate mattis. Aliquam sollicitudin accumsan turpis vitae malesuada. Quisque aliquet lorem sit amet odio ullamcorper porta. Pellentesque lobortis augue nec dolor sagittis, ut rutrum sem placerat. Duis eu viverra nulla.
#Nullam sit amet tempor mi. Curabitur sed odio vitae erat sodales consectetur vitae convallis nulla. Curabitur feugiat mollis consequat. Duis fermentum aliquet sem sit amet tincidunt. Etiam vitae orci dolor. Fusce ac sodales libero. Sed sodales non lorem a auctor. Nunc in tellus ornare, gravida elit a, venenatis justo. Vivamus ipsum augue, dapibus vitae nisi sit amet, faucibus vehicula quam. Ut quis elit accumsan, rhoncus massa tempor, blandit sapien. Quisque eleifend turpis vel nisi porttitor efficitur.
#Nulla pretium in ipsum at ultricies. Integer maximus orci ut orci sagittis ornare. Praesent ac pellentesque nibh. Curabitur lacinia, nunc ut lobortis vulputate, augue ipsum consectetur enim, eu eleifend eros magna eget leo. Nulla sit amet laoreet nulla. Duis leo elit, accumsan eu ultrices id, rhoncus eget nibh. Duis eleifend accumsan tortor a bibendum \citep{Lakshmanan}.
#Aliquam erat volutpat. Etiam convallis ultrices tortor, vel faucibus sapien finibus in. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Nullam nulla dui, imperdiet et elementum eget, vehicula non risus. Duis varius orci id ex egestas vulputate. Donec eleifend tempor sem ac commodo. Vivamus ut nunc nisi. Sed quis lorem erat. Aliquam imperdiet tempus justo, ut laoreet lorem finibus ac. 
#\bibliographystyle{apa}
#\bibliography{article}
#\nocite{*}
#\end{document}
#----------
#@book{Lakshmanan,
  #author={Valliappa Lakshmanan},
  #title={Data Science on the Google Cloud Platform: Implementing End-to-End Real-time Data Pipelines: From Ingest to Machine Learning},
  #publisher={O'Reilly Inc.},
#year={2018}
#}

#pdflatex article
#bibtex article
#pdflatex article
#\citep