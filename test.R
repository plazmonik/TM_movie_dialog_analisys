id = match('topgun', names)
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
a3 = annotate(sample.text, pos_tag_annotator, a1)
a3w = subset(a3, type=='word')
max = length(a3w)
k = 1
words <- c()
while(k<=max){
  p = unlist(a3w[k]$features)
  if(p=="NN" || p=="VB"){
    word <- substr(sample.text,a3w[k]$start, a3w[k]$end)
  }
  words = c(words, word)
  k = k + 1
}
words <- unlist(words)
words= words[words!='m' & words!='ll' & words!="ve" & words!="dont"]
tb <- as.data.frame(table(words))
colnames(tb) <- c('word','freq')
tb <- tb %>% arrange(desc(freq))
tb

path.png = file.path(getwd(), "sample pictures/jet.jpg")
wordcloud2(data = tb, figPath = path.png, size = 1.0)
