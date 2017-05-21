"""1. Używamy danych z Film Corpus, 1068 films in txt files: 
  https://nlds.soe.ucsc.edu/fc2
1068 ścieżek dialogowych z filmów podzielonych na 23 kategorię (część filmów przypisana do kilku kategorii)

Zadanie: chcemy rozróżniać kategorie filmowe na podstawie analizy NLP samych ścieżek dialogowych. Czy proste analizy (np. analiza sentymentu, operacje na macierzach DTM i TDM itp ) nie będą lepsze od metod stricte "text similarity"?
Dodatkowo: czy ma znaczenie, czy analizujemy całą ścieżkę, czy tylko niewielką część (np. pierwsze albo ostatnie 200 słów).

Pierwsza obserwacja: 23 kategorie to za dużo! Ograniczmy się do:
  Action, Comedy, Drama, Horror, Romance
"""

library(tm) 
library(dplyr)
library(SnowballC)

path = file.path(getwd(), "dialogs/Biography")
dir(path)
biography_corpus <- Corpus(DirSource(path))
summary(biography_corpus)

#W uzyskanym źróDle  imiona bohaterów oraz opisy są pisane samymi dużymi literami. 
#Dlatego napiszemy niestandardową funkcję do preprocessingu, usuwającą je. 
#Wyleci w ten sposób też trochę "okrzyków", ale chyba możemy z tym żyć
#Użyjemy funkcji regexpowej do znalezienia takich wyrazów. 
remAllCap <- function (x){gsub("\\b[A-Z]+\\b", "", x)}


#Funkcja dokonująca preprocessingu, używając przetwarzania potokowego.  
preProcess <- function(corpus){
     corpus %>% tm_map(removeWords, c(stopwords("SMART"))) %>% tm_map(removePunctuation) %>% tm_map(stripWhitespace) %>%
       tm_map(removeNumbers) %>% 
       tm_map(remAllCap) %>% 
       tm_map(tolower) %>% tm_map(removeWords, c(stopwords("SMART"))) %>% tm_map(stemDocument)
}

category_list = c('Action','Drama','Horror','Romance','Comedy')
#Tutaj podać kategorię do analizy,
category = "Drama"

#wczytanie z pliku
path = file.path(getwd(), "dialogs",category)
#weryfikacja, czy nazwa jest prawidłowa
dir(path)

#utworzenie korpusu i preprocessing
corpus <- Corpus(DirSource(path))
corpus <-preProcess(corpus)
head(strwrap(corpus[[1]]),20)

#To jest w zasadzie tyle ważnego, ile przygotowałem na razie! Ale poniżej zabawa z wordcloud, którą możemy wykorzystać!

library(wordcloud)

tdm <- TermDocumentMatrix(corpus)
m <- as.matrix(tdm)
v <- sort(rowSums(m), decreasing=TRUE)
head(v,15)
words <- names(v)
d <- data.frame(word=words, freq=v)
wordcloud(d$word, d$freq,max.words=200, min.freq=20, colors=brewer.pal(6, "Dark2"))