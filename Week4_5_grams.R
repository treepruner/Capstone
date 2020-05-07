rm(list=ls())
require(quanteda)
require(dplyr)
require(sqldf)
require(data.table)

(opt <- quanteda_options()) # threads = 2
quanteda_options(verbose = TRUE)
quanteda_options(threads = 4)

# Load "bad words" file from Google.
profanity <- readLines("./data/badwords.txt", encoding = "UTF-8", skipNul = TRUE,  warn = FALSE)
profanity <- tolower(profanity)

# make specific profanity a generic literal
lemma <- rep("profanity", length(profanity))


###################################################### read in all file ######################################################

# read in raw news file
news_path <- "./data/news.rds"
news <- readRDS(news_path)
newsLines <- length(news)
newsLines # 1010242
class(news) # character
news <- iconv(news, from = "UTF-8", to = "ASCII", sub = "")

# read in raw blogs file
blogs_path <- "./data/blogs.rds"
blogs <- readRDS(blogs_path)
blogsLines <- length(blogs)
blogsLines # 899288
class(blogs) # character
blogs <- iconv(blogs, from = "UTF-8", to = "ASCII", sub = "")

# read in raw twitter file
twitter_path <- "./data/twitter.rds"
twitter <- readRDS(twitter_path)
twitterLines <- length(twitter)
twitterLines # 2360148
class(twitter) # character
twitter <- iconv(twitter, from = "UTF-8", to = "ASCII", sub = "")

# make one big file
all <- list(blogs = blogs, news = news, twitter = twitter)

# all <- c(news, blogs, twitter)

all <- length(all) 
all # 3
class(all) # list
head(all)

rm("news", "newsLines", "news_path", "blogs", "blogsLines", "blogs_path", "twitter", "twitterLines", "twitter_path" )

# Convert to sentances

system.time(all <- lapply(all,function(x){
  system.time(theText <- corpus(x))
  system.time(sentences <- corpus_reshape(theText,to="sentences",verbose=FALSE))
  sentences # return to caller
}))



# corpus and cleaning
news_corpus <- corpus(news)
length(news_corpus) # 101242
head(news_corpus, 1) # "He wasn't home alone, apparently."

# close raw file
rm(news)

# run 10 times (25k to 7 k at 10th run)  
news_5_coll10 <- setDT( tokens(corpus_sample(news_corpus, size = 70000, replace = FALSE) 
                            , remove_punct = TRUE 
                            , remove_numbers = TRUE
                            , remove_url = TRUE
                            , remove_symbols = TRUE) %>% 
        tokens_select( stopwords('english'), selection = 'remove') %>%
        tokens_tolower() %>%
        tokens_replace(profanity, lemma, valuetype="fixed") %>%
        tokens_wordstem() %>% 
        textstat_collocations(method = "lambda", size = 4) %>% 
        arrange(-count, collocation))
        


head(news_5_coll10, 50 ); tail(news_5_coll1, 50)


# what did it output?
class(news_5_coll3) # data table
sapply(news_5_coll3, class) 
# collocation        count count_nested       length       lambda  z 
# "character"    "integer"    "integer"    "numeric"    "numeric" "numeric"

# com5ne all 10 news samples
news_5_coll <- sqldf("
select collocation as phrase, sum(count) as cnt
from (  select collocation, count
        from news_5_coll1
  union all                   
        select collocation, count 
        from news_5_coll2
  union all                   
        select collocation, count 
        from news_5_coll3        
  union all                   
        select collocation, count 
        from news_5_coll4
  union all                   
        select collocation, count 
        from news_5_coll5
  union all                   
        select collocation, count 
        from news_5_coll6
  union all                   
        select collocation, count 
        from news_5_coll7
  union all                   
        select collocation, count 
        from news_5_coll8
  union all                   
        select collocation, count 
        from news_5_coll9
  union all                   
        select collocation, count 
        from news_5_coll10        
     )
group by collocation                  
order by collocation
")
nrow(news_5_coll) #24068

# query
sqldf("select phrase, cnt 
      from news_5_coll 
      where phrase like 'presid%' ")


# close news coprus file and sample files
rm(news_corpus, news_5_coll1, news_5_coll2, news_5_coll3, news_5_coll4, news_5_coll5, news_5_coll6, news_5_coll7, news_5_coll8, news_5_coll9, news_5_coll10)


# save news collocation
saveRDS(news_5_coll, "./data/news_5_coll.rds") 


###################################################### news file ######################################################




# corpus and cleaning
blogs_corpus <- corpus(blogs)
length(blogs_corpus) # 899288
head(blogs_corpus, 1) # "In the years thereafter, most of the Oil fields and platform..."

# close raw file
rm(blogs)

# run 10 times
blogs_5_coll10 <- setDT( tokens(corpus_sample(blogs_corpus, size = 75000, replace = FALSE)
                             , remove_punct = TRUE 
                             , remove_numbers = TRUE
                             , remove_url = TRUE
                             , remove_symbols = TRUE        ) %>% 
                              tokens_select( stopwords('english'), selection = 'remove') %>%
                              tokens_tolower() %>%
                              tokens_replace(profanity, lemma, valuetype="fixed") %>%
                              tokens_wordstem() %>% 
                              textstat_collocations(method = "lambda", size = 4) %>% 
                              arrange(-count, collocation) )

head(blogs_5_coll1, 5 ); tail(blogs_5_coll1, 5)


# what did it output?
class(blogs_5_coll3) # data table
sapply(blogs_5_coll3, class) 
# collocation        count count_nested       length       lambda  z 
# "character"    "integer"    "integer"    "numeric"    "numeric" "numeric"

# com5ne all 10 blogs samples
blogs_5_coll <- sqldf("
select collocation as phrase, sum(count) as cnt
from (
        select collocation, count
        from blogs_5_coll1
  union all                   
        select collocation, count 
        from blogs_5_coll2
  union all                   
        select collocation, count 
        from blogs_5_coll3        
  union all                   
        select collocation, count 
        from blogs_5_coll4
  union all                   
        select collocation, count 
        from blogs_5_coll5
  union all                   
        select collocation, count 
        from blogs_5_coll6
  union all                   
        select collocation, count 
        from blogs_5_coll7
  union all                   
        select collocation, count 
        from blogs_5_coll8
  union all                   
        select collocation, count 
        from blogs_5_coll9
  union all                   
        select collocation, count 
        from blogs_5_coll10        
     )
group by collocation                  
order by collocation
")
nrow(blogs_5_coll) # 11674
blogs_5_coll

# query
sqldf("select phrase, cnt 
      from blogs_5_coll 
      where phrase like 'presid%' ")


# close blogs coprus file and sample files
rm(blogs_corpus, blogs_5_coll1, blogs_5_coll2, blogs_5_coll3, blogs_5_coll4, blogs_5_coll5, blogs_5_coll6, blogs_5_coll7, blogs_5_coll8, blogs_5_coll9, blogs_5_coll10)


# save blogs collocation
saveRDS(blogs_5_coll, "./data/blogs_5_coll.rds") 

###################################################### twitter file ######################################################




# corpus and cleaning
twitter_corpus <- corpus(twitter)
length(twitter_corpus) # 2360148
head(twitter_corpus, 1) # "How are you? Btw thanks for the RT. You gonna be in DC anyti..."

# close raw file
rm(twitter)

# run 10 times (200000)
twitter_quad_coll10 <- setDT( tokens(corpus_sample(twitter_corpus, size = 150000, replace = FALSE)
                                     , remove_punct = TRUE 
                                     , remove_numbers = TRUE
                                     , remove_url = TRUE
                                     , remove_symbols = TRUE        ) %>% 
                                tokens_select( stopwords('english'), selection = 'remove') %>%
                                tokens_tolower() %>%
                                tokens_replace(profanity, lemma, valuetype="fixed") %>%
                                tokens_wordstem() %>% 
                                textstat_collocations(method = "lambda", size = 5) %>% 
                                arrange(-count, collocation) )



head(twitter_5_coll1, 5 ); tail(twitter_5_coll1, 5)


# what did it output?
class(twitter_5_coll3) # data table
sapply(twitter_5_coll3, class) 
# collocation        count count_nested       length       lambda  z 
# "character"    "integer"    "integer"    "numeric"    "numeric" "numeric"

# com5ne all 10 twitter samples
twitter_5_coll <- sqldf("
select collocation as phrase, sum(count) as cnt
from (
        select collocation, count
        from twitter_5_coll1
  union all                   
        select collocation, count 
        from twitter_5_coll2
  union all                   
        select collocation, count 
        from twitter_5_coll3        
  union all                   
        select collocation, count 
        from twitter_5_coll4
  union all                   
        select collocation, count 
        from twitter_5_coll5
  union all                   
        select collocation, count 
        from twitter_5_coll6
  union all                   
        select collocation, count 
        from twitter_5_coll7
  union all                   
        select collocation, count 
        from twitter_5_coll8
  union all                   
        select collocation, count 
        from twitter_5_coll9
  union all                   
        select collocation, count 
        from twitter_5_coll10        
     )
group by collocation                  
order by collocation
")
nrow(twitter_5_coll) #24452
twitter_5_coll

# query
sqldf("select phrase, cnt 
      from twitter_5_coll 
      where phrase like 'presid%' ")


# close twitter coprus file and sample files
rm(twitter_corpus, twitter_5_coll1, twitter_5_coll2, twitter_5_coll3, twitter_5_coll4, twitter_5_coll5, twitter_5_coll6, twitter_5_coll7, twitter_5_coll8, twitter_5_coll9, twitter_5_coll10)


# save twitter collocation
saveRDS(twitter_5_coll, "./data/twitter_5_coll.rds") 


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


