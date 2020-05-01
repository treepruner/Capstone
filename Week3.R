rm(list=ls())
require(quanteda)
require(dplyr)
require(sqldf)


# Load "bad words" file from Google.
profanity <- readLines("./data/badwords.txt", encoding = "UTF-8", skipNul = TRUE,  warn = FALSE)
profanity <- tolower(profanity)

# make specific profanity a generic literal
lemma <- rep("profanity", length(profanity))


###################################################### news file ######################################################

# read in raw news file
news_path <- "./data/news.rds"
news <- readRDS(news_path)
newsLines <- length(news)
newsLines # 1010242
class(news) # character


# corpus and cleaning
news_corpus <- corpus(news)
length(news_corpus) # 101242
head(news_corpus, 1) # "He wasn't home alone, apparently."

# close raw file
rm(news)

# run 10 times
news_coll10 <- setDT( tokens(corpus_sample(news_corpus, size = 10000, replace = FALSE)
                            , remove_punct = TRUE 
                            , remove_numbers = TRUE) %>% 
        tokens_select( stopwords('english'), selection = 'remove') %>%
        tokens_tolower() %>%
        tokens_replace(profanity, lemma, valuetype="fixed") %>%
        tokens_wordstem() %>% 
        textstat_collocations(method = "lambda", size = 3) %>% 
        arrange(-count, collocation) )

head(news_coll10, 50 ); tail(news_coll10, 50)

# what did it output?
class(news_coll3) # data table
sapply(news_coll3, class) 
# collocation        count count_nested       length       lambda  z 
# "character"    "integer"    "integer"    "numeric"    "numeric" "numeric"

# combine all 10 news samples
news_coll <- sqldf("
select collocation as phrase, sum(count) as cnt
from (
        select collocation, count
        from news_coll1
  union all                   
        select collocation, count 
        from news_coll2
  union all                   
        select collocation, count 
        from news_coll3        
  union all                   
        select collocation, count 
        from news_coll4
  union all                   
        select collocation, count 
        from news_coll5
  union all                   
        select collocation, count 
        from news_coll6
  union all                   
        select collocation, count 
        from news_coll7
  union all                   
        select collocation, count 
        from news_coll8
  union all                   
        select collocation, count 
        from news_coll9
  union all                   
        select collocation, count 
        from news_coll10        
     )
group by collocation                  
order by collocation
")
news_coll

# query
sqldf("select phrase, cnt 
      from news_coll 
      where phrase like 'presid%' ")


# close news coprus file and sample files
rm(news_corpus, news_coll1, news_coll2, news_coll3, news_coll4, news_coll5, news_coll6, news_coll7, news_coll8, news_coll9, news_coll10)


# save news collocation
saveRDS(news_coll, "./data/news_coll.rds") 


###################################################### news file ######################################################

# read in raw blogs file
blogs_path <- "./data/blogs.rds"
blogs <- readRDS(blogs_path)
blogsLines <- length(blogs)
blogsLines # 899288
class(blogs) # character


# corpus and cleaning
blogs_corpus <- corpus(blogs)
length(blogs_corpus) # 899288
head(blogs_corpus, 1) # "In the years thereafter, most of the Oil fields and platform..."

# close raw file
rm(blogs)

# run 10 times

blogs_coll10 <- setDT( tokens(corpus_sample(blogs_corpus, size = 10000, replace = FALSE)
                             , remove_punct = TRUE 
                             , remove_numbers = TRUE
                             , remove_url = TRUE
                             , remove_symbols = TRUE        ) %>% 
                              tokens_select( stopwords('english'), selection = 'remove') %>%
                              tokens_tolower() %>%
                              tokens_replace(profanity, lemma, valuetype="fixed") %>%
                              tokens_wordstem() %>% 
                              textstat_collocations(method = "lambda", size = 3) %>% 
                              arrange(-count, collocation) )

head(blogs_coll10, 5 ); tail(blogs_coll10, 5)


# what did it output?
class(blogs_coll3) # data table
sapply(blogs_coll3, class) 
# collocation        count count_nested       length       lambda  z 
# "character"    "integer"    "integer"    "numeric"    "numeric" "numeric"

# combine all 10 blogs samples
blogs_coll <- sqldf("
select collocation as phrase, sum(count) as cnt
from (
        select collocation, count
        from blogs_coll1
  union all                   
        select collocation, count 
        from blogs_coll2
  union all                   
        select collocation, count 
        from blogs_coll3        
  union all                   
        select collocation, count 
        from blogs_coll4
  union all                   
        select collocation, count 
        from blogs_coll5
  union all                   
        select collocation, count 
        from blogs_coll6
  union all                   
        select collocation, count 
        from blogs_coll7
  union all                   
        select collocation, count 
        from blogs_coll8
  union all                   
        select collocation, count 
        from blogs_coll9
  union all                   
        select collocation, count 
        from blogs_coll10        
     )
group by collocation                  
order by collocation
")
blogs_coll

# query
sqldf("select phrase, cnt 
      from blogs_coll 
      where phrase like 'presid%' ")


# close blogs coprus file and sample files
rm(blogs_corpus, blogs_coll1, blogs_coll2, blogs_coll3, blogs_coll4, blogs_coll5, blogs_coll6, blogs_coll7, blogs_coll8, blogs_coll9, blogs_coll10)


# save blogs collocation
saveRDS(blogs_coll, "./data/blogs_coll.rds") 

###################################################### twitter file ######################################################

# read in raw twitter file
twitter_path <- "./data/twitter.rds"
twitter <- readRDS(twitter_path)
twitterLines <- length(twitter)
twitterLines # 2360148
class(twitter) # character


# corpus and cleaning
twitter_corpus <- corpus(twitter)
length(twitter_corpus) # 899288
head(twitter_corpus, 1) # "How are you? Btw thanks for the RT. You gonna be in DC anyti..."

# close raw file
rm(twitter)

# run 10 times
twitter_coll10<- setDT( tokens(corpus_sample(twitter_corpus, size = 10000, replace = FALSE)
                              , remove_punct = TRUE 
                              , remove_numbers = TRUE
                              , remove_url = TRUE
                              , remove_symbols = TRUE        ) %>% 
                               tokens_select( stopwords('english'), selection = 'remove') %>%
                               tokens_tolower() %>%
                               tokens_replace(profanity, lemma, valuetype="fixed") %>%
                               tokens_wordstem() %>% 
                               textstat_collocations(method = "lambda", size = 3) %>% 
                               arrange(-count, collocation) )

head(twitter_coll10, 5 ); tail(twitter_coll10, 5)


# what did it output?
class(twitter_coll3) # data table
sapply(twitter_coll3, class) 
# collocation        count count_nested       length       lambda  z 
# "character"    "integer"    "integer"    "numeric"    "numeric" "numeric"

# combine all 10 twitter samples
twitter_coll <- sqldf("
select collocation as phrase, sum(count) as cnt
from (
        select collocation, count
        from twitter_coll1
  union all                   
        select collocation, count 
        from twitter_coll2
  union all                   
        select collocation, count 
        from twitter_coll3        
  union all                   
        select collocation, count 
        from twitter_coll4
  union all                   
        select collocation, count 
        from twitter_coll5
  union all                   
        select collocation, count 
        from twitter_coll6
  union all                   
        select collocation, count 
        from twitter_coll7
  union all                   
        select collocation, count 
        from twitter_coll8
  union all                   
        select collocation, count 
        from twitter_coll9
  union all                   
        select collocation, count 
        from twitter_coll10        
     )
group by collocation                  
order by collocation
")
twitter_coll

# query
sqldf("select phrase, cnt 
      from twitter_coll 
      where phrase like 'presid%' ")


# close twitter coprus file and sample files
rm(twitter_corpus, twitter_coll1, twitter_coll2, twitter_coll3, twitter_coll4, twitter_coll5, twitter_coll6, twitter_coll7, twitter_coll8, twitter_coll9, twitter_coll10)


# save twitter collocation
saveRDS(twitter_coll, "./data/twitter_coll.rds") 


###################################################### combined ######################################################
# combine file summaries
all_coll <- sqldf("
select phrase, sum(cnt) as cnt
from (
        select phrase, cnt
        from news_coll
  union all                   
        select phrase, cnt 
        from blogs_coll
  union all                   
        select phrase, cnt 
        from twitter_coll        
       
     )
group by phrase                  
order by phrase, cnt
")
all_coll

# , instr(phrase, ' ',2)  as loc_instr bad syntax
charindex(' ', phrase) and instr(phrase, ' ')  are the same

sqldf("select substr(phrase, 1, instr(phrase, ' ') - 1) as word1
        , substr(phrase, instr(phrase, ' ') + 1, instr(phrase, ' ') -2)  as word2a
        , substr(phrase, instr(phrase, ' ') + 1, charindex(phrase, ' ', 3) -1)  as word2b
        , charindex(' ', phrase)  as loc_char
        , instr(phrase, ' ')  as loc_instr 

        , phrase
        , cnt 
      from all_coll 
      where phrase like 'presid%' ")