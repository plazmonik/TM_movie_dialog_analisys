
# calculations
library(tm) 
library(dplyr)
library(SnowballC)

#visualization
library(wordcloud2)


path = file.path(getwd(), "dialogs/")
category_list = dir(path)
category_list
category_list = c('Action','Drama','Horror','Romance','Comedy')
category_list

category="Action"

path = file.path(getwd(), "dialogs",category)
corpus <- Corpus(DirSource(path, recursive=T))

remAllCap <- function (x){gsub("\\b[A-Z]+\\b", "", x)}
corpus <- tm_map(corpus, remAllCap)

corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, c(stopwords("english")))
corpus <- tm_map(corpus, stemDocument)
remSwer <- function(x){gsub("fuck", "f**k", x)}
corpus <- tm_map(corpus, remSwer)
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
    score = ( sum(pos.matches)-sum(neg.matches))/(sum(pos.matches)+sum(neg.matches))# - 
    return(score)
  }, pos.words, neg.words, .progress=.progress )
}

films = dir(path)
max = length(films)

i=1
names = c()
sentiment = c()

for(file in films){
  
  sample.text = corpus[[i]]$content
  result = score.sentiment(sample.text, hu.liu.pos , hu.liu.neg)
  # print(result[[1]])
  names = c(names, gsub("_dialog.txt","",corpus[[i]]$meta$id))
  sentiment = c(sentiment, result[[1]])
  if(i==max){
    break
  }
  i=i+1
}
df = data.frame(names, sentiment)
df <- df %>% arrange(desc(sentiment)) %>% top_n(10)
df

set.seed(123)
m <- as.matrix(tdm)
v <- sort(rowSums(m), decreasing=TRUE)
words <- names(v)
d <- data.frame(word=words, freq=v)
#path.png = file.path(getwd(), "sample pictures/movie.png")
path.png = file.path(getwd(), "sample pictures/action.png")
pct <- wordcloud2(data = d, figPath = path.png, size = 1.5)


