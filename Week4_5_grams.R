rm(list=ls())
require(quanteda)
require(dplyr)
require(tidyr)
require(tidytext)
require(tidyverse)
require(stringr)
require(sqldf)
require(data.table)
require(ggplot2)
require(corpus)


(opt <- quanteda_options()) # threads = 2
#quanteda_options(verbose = TRUE)
quanteda_options(threads = 4)

# Load "bad words" file from Google.
profanity <- readLines("./data/badwords.txt", encoding = "UTF-8", skipNul = TRUE,  warn = FALSE)
profanity <- tolower(profanity)

# make specific profanity a generic literal
lemma <- rep("profanity", length(profanity))


###################################################### news ######################################################

# read in raw news file
newsPath <- "./data/news.rds"
news <- readRDS(newsPath)
newsLines <- length(news)
newsLines # 1010242
class(news) # character

# take a sample and close main file
set.seed = 941
sampleSize <- 0.001
newsSample <- sample(news, size = round(newsLines * sampleSize), replace = FALSE)
newsSample <- iconv(newsSample, from = "UTF-8", to = "ASCII", sub = "")
saveRDS(newsSample, "./data/newsSample.rds")
rm("news", "newsPath", "newsLines")

# Convert to sentances
system.time(news_sen <- lapply(newsSample,function(x){
  system.time(theText <- corpus(x))
  system.time(sentences <- corpus_reshape(theText, to = "sentences", verbose = FALSE))
  sentences # return to caller
}))

# user  system elapsed 
# 898.95    4.34  908.25 
saveRDS(news_sen, "./data/news_sen.rds")
class(news_sen) # list
rm(newsSample)

# corpus and cleaning
length(news_sen) # 1010
head(news_sen, 2)  # "Ball was travel editor of the New Orleans Times-Picayune fro..."
                      # "abrettman@oregonian.com ; oregonlive.com/playbooks-profits; ..."
# news_senV <- sapply(news_sen, paste0, collapse="") # turn into a vector for imput to tokens
install.packages("corpus")

# tidyverse http://uc-r.github.io/creating-text-features
news_2gram <- 
  data.table(news_senV) %>%
  rename (newsText = news_senV) %>%
  unnest_tokens(bigram, 'newsText', token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter (  !word1 %in% stop_words$word
          , !str_detect(word1, pattern = "[[:digit:]]") # words with numbers
          , !str_detect(word1, pattern = "[[:punct:]]") # punctuation
          , !str_detect(word1, pattern = "(.)\\1{2,}")  # 3 or more repeated letters
          , !str_detect(word1, pattern = "\\b(.)\\b")   # single letter words            
          , !word2 %in% stop_words$word
          , !str_detect(word2, pattern = "[[:digit:]]") # words with numbers
          , !str_detect(word2, pattern = "[[:punct:]]") # punctuation
          , !str_detect(word2, pattern = "(.)\\1{2,}")  # 3 or more repeated letters
          , !str_detect(word2, pattern = "\\b(.)\\b")   # single letter words
          ) 
# ggplot(aes(n)) +  geom_histogram() + scale_x_log10() # see low count words

# what do we have?
head(news_2gram)

# log likelihood ratio test
# n2 = news bigram, w1 = word1, w2 = word2

# count for bigrams
cnt_n2w1 <- news_2gram %>%
  count(word1) #3454

cnt_n2w2 <- news_2gram %>%
  count(word2) #3618

cnt_n2w12 <- news_2gram %>%
  count(word1, word2) #5390

N <-nrow(news_2gram)  # 5545


LL <- cnt_n2w12 %>%
  left_join(cnt_n2w1, by = "word1") %>%
  left_join(cnt_n2w2, by = "word2") %>%  # word1       word2           n.x   n.y     n
  rename(c_w1 = n.y, c_w2 = n, c_w12 = n.x) %>%
  mutate(
        p  =  c_w2  / N
      , p1 =  c_w12 / c_w1
      , p2 = (c_w2  - c_w12) / (N - c_w1)
      , LL = log((pbinom(c_w12, c_w1, p)  * pbinom(c_w2 - c_w12, N - c_w1, p ))
          / 
                 (pbinom(c_w12, c_w1, p1) * pbinom(c_w2 - c_w12, N - c_w1, p)))
                               
  )
head(LL)













# combine 



# query
sqldf("select phrase, cnt 
      from news
      where phrase like 'presid%' ")


# close news coprus file and sample files


# save news collocation
saveRDS(news_5_coll, "./data/news_5_coll.rds") 


###################################################### blogs file ######################################################
# read in raw blogs file
blogsPath <- "./data/blogs.rds"
blogs <- readRDS(blogsPath)
blogsLines <- length(blogs)
blogsLines # 899288
class(blogs) # character
blogs <- iconv(blogs, from = "UTF-8", to = "ASCII", sub = "")


###################################################### twitter file ######################################################
# read in raw twitter file
twitterPath <- "./data/twitter.rds"
twitter <- readRDS(twitterPpath)
twitterLines <- length(twitter)
twitterLines # 2360148
class(twitter) # character
twitter <- iconv(twitter, from = "UTF-8", to = "ASCII", sub = "")




###################################################### combined ######################################################
# combine file summaries

# read in coll files
news_5_coll_path <- "./data/news_5_coll.rds"
news_5_coll <- readRDS(news_5_coll_path)

blogs_5_coll_path <- "./data/blogs_5_coll.rds"
blogs_5_coll <- readRDS(blogs_5_coll_path)


all_5_coll <- sqldf("
select phrase, sum(cnt) as cnt
from (
        select phrase, cnt
        from news_5_coll
  union all                   
        select phrase, cnt 
        from blogs_5_coll
  union all                   
        select phrase, cnt 
        from twitter_5_coll        
     )
group by phrase                  
order by phrase, cnt desc
")
nrow(all_5_coll) #59434

# save all collocation
saveRDS(all_5_coll, "./data/all_5_coll.rds") 

# this runs 
all_5_coll2 <- sqldf("select substr(phrase, 1, instr(phrase, ' ') - 1) as word1
, substr(phrase, instr(phrase, ' '),  length(phrase)) as word234
, substr(phrase, instr(phrase, ' '),  length(phrase)) as word234
, phrase
, cnt 
from all_5_coll 
order by cnt desc      ")

saveRDS(all_5_coll2, "./data/all_5_col2l.rds") 

# Quiz 2 
#1 "guy"     "front"   "just"    "bought"  "pound"   "bacon"   "bouquet" "case" 
#2 "reason"   "smile"    "everyday" "can"      "follow"   "pleas"    "mean"  
#3 "hey"     "sunshin" "can"     "follow"  "make" 
#4 "earli"   "observ"  "bill"    "game"    "offens"  "still"   "struggl"
#5 "go"     "romant" "date"
#6 "well"   "pretti" "sure"   "granni" "old"    "bagpip" "garag"  "dust" 
#7 "ohhhhh"      "#pointbreak" "tomorrow"    "love"        "film"        "seen"        "quit" 
#8 "ice"      "bucket"   "challeng" "loui"     "push"     "long"     "wet"      "hair"     "eye"      "littl" 
#9 "grate" "good"  "time"  "keep"  "faith"
#10 "cutest" "thing"  "ever"   "seen"   "must" 

# query
sqldf("select word1,word2, cnt from all_5_coll2 where rtrim(ltrim(word1)) = 'case' order by word1, cnt desc ") 
sqldf("select word1,word2, cnt from all_5_coll2 where rtrim(ltrim(word1)) = 'mean' order by word1, cnt desc ") 
sqldf("select word1,word2, cnt from all_5_coll2 where rtrim(ltrim(word1)) = 'make' order by word1, cnt desce ") 
sqldf("select word1,word2, cnt from all_5_coll2 where rtrim(ltrim(word1)) = 'struggl' order by word1, cnt desc ")
sqldf("select word1,word2, cnt from all_5_coll2 where rtrim(ltrim(word1)) = 'date' order by word1, cnt desc ")
sqldf("select word1,word2, cnt from all_5_coll2 where rtrim(ltrim(word1)) = 'my' order by word1, cnt desc ")
sqldf("select word1,word2, cnt from all_5_coll2 where rtrim(ltrim(word1)) = 'some' order by word1, cnt desc ")
sqldf("select word1,word2, cnt from all_5_coll2 where rtrim(ltrim(word1)) = 'littl' order by word1, cnt desc ")
sqldf("select word1,word2, cnt from all_5_coll2 where rtrim(ltrim(word1)) = 'fait%' order by word1, cnt desc ")
sqldf("select word1,word2, cnt from all_5_coll2 where rtrim(ltrim(word1)) = 'must' order by word1, cnt desc ")

# convert to tokens
tokens(corpus("If this isn't the cutest thing you've ever seen, then you must be")
 , remove_punct = TRUE, remove_numbers = TRUE, remove_url = TRUE, remove_symbols = TRUE) %>% 
tokens_select( stopwords('english'), selection = 'remove') %>%
tokens_tolower() %>%
tokens_replace(profanity, lemma, valuetype="fixed") %>%
tokens_wordstem() 


