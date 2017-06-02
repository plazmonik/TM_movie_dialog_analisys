
# calculations
library(tm) 
library(dplyr)
library(SnowballC)

#visualization
library(wordcloud2)


path = file.path(getwd(), "dialogs_selected/")
category_list = dir(path)

corpus <- Corpus(DirSource(path, recursive=T))


remAllCap <- function (x){gsub("\\b[A-Z]+\\b", "", x)}
corpus <- tm_map(corpus, remAllCap)

corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, c(stopwords("english")))
remSwer <- function(x){gsub("fuck", "f**k", x)}
corpus <- tm_map(corpus, remSwer)
corpus_org <-corpus
corpus_org <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, stemDocument)
corpus <- tm_map(corpus, stripWhitespace)




tdm <- TermDocumentMatrix(corpus)

hu.liu.pos = scan(file.path(getwd(), "opinion-lexicon-English","positive-words.txt"),
                  what='character', comment.char=';')
hu.liu.neg = scan(file.path(getwd(), "opinion-lexicon-English","negative-words.txt"),
                  what='character', comment.char=';')

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    word.list = str_split(sentence, '\\s+')
    words = unlist(word.list)
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    # print(length(words))
    score = c(sum(pos.matches)/length(words), sum(neg.matches)/length(words))
    return(score)
  }, pos.words, neg.words, .progress=.progress )
}

# films = dir(path)
max = length(list.files(path=path, recursive = T))

i=1
names = c()
pos = c()
neg=c()

while(i<=max){
  
  sample.text = corpus[[i]]$content
  result = score.sentiment(sample.text, hu.liu.pos , hu.liu.neg)
  # print(result[[1]])
  names = c(names, gsub("_dialog.txt","",corpus[[i]]$meta$id))
  # print(result)
  pos =c(pos,result[1])
  neg = c(neg, result[2])
  if(i==max){
    break
  }
  i=i+1
}
df = data.frame(names, pos, neg )
df <- df %>% filter(pos >0.05 & neg >0.05) %>% distinct() %>% mutate(per_pos = pos/(pos+neg))
df.pos <- df %>%  arrange(desc(per_pos)) 
df.neg <- df %>%  arrange(per_pos) 
top_n(df.pos, 15)
top_n(df.neg, -15)




for.cloud = function(name, names, corpus_org){
  id = match(name, names)
  print(corpus_org[[id]]$meta$id)
  print(id)
  ##POS taging
  library(NLP)
  library(openNLP)
  library(tm)
  sent_token_annotator <-  Maxent_Sent_Token_Annotator()
  word_token_annotator <-  Maxent_Word_Token_Annotator()
  sample.text = corpus_org[[id]]$content
  a1 <- annotate(sample.text,list(sent_token_annotator,word_token_annotator))
  pos_tag_annotator <-  Maxent_POS_Tag_Annotator()
  a3 <- annotate(sample.text, pos_tag_annotator, a1)
  a3w <- subset(a3, type=='word')
  max = length(a3w)
  k = 1
  words = c()
  while(k<=max){
    p = unlist(a3w[k]$features)
    if(p=="NN" || p=="VB"){
      word <- substr(sample.text,a3w[k]$start, a3w[k]$end)
    }
    words = c(words, word)
    k = k + 1
  }
  words= words[words!='m' & words!='ll' & words!="ve" & words!="dont"]
  tb <- as.data.frame(table(words))
  colnames(tb) <- c('word','freq')
  tb <- tb %>% arrange(desc(freq))
  return(tb)
}
words.pos <- for.cloud('amadeus', names, corpus_org)
head(words.pos)

words.neg <- for.cloud('wildhogs', names, corpus_org)
head(words.neg)

path.png = file.path(getwd(), "sample pictures/movie.png")
wordcloud2(data = words.pos, figPath = path.png, size = 1)


path.png = file.path(getwd(), "sample pictures/play.png")
wordcloud2(data = words.neg, figPath = path.png, size = 1.5)


