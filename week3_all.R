rm(list=ls())
require(quanteda)
require(dplyr)
require(sqldf)
?sqldf
require(data.table)
require(stringr)
require(tidyr)

###################################################### combinned ######################################################
# combine file summaries

# file paths
all_bi_coll_path <- "./data/twitter_bi_coll.rds"
all_tri_coll_path <- "./data/news_tri_coll.rds"
all_quad_coll_path <- "./data/news_quad_coll.rds"


# read in coll files
all_quad_coll <- readRDS(all_quad_coll_path)
all_tri_coll <- readRDS(all_tri_coll_path)
all_bi_coll <- readRDS(all_bi_coll_path)


all_234 <- sqldf("
select phrase, sum(cnt) as cnt
from (
        select phrase, cnt
        from all_quad_coll
  union all                   
        select phrase, cnt 
        from all_tri_coll
  union all                   
        select phrase, cnt 
        from all_bi_coll        
     )
group by phrase                  
order by phrase, cnt desc
")
nrow(all_234) #114374
head(all_234)

# save all collocation
saveRDS(all_234, "./data/all_234.rds") 

# this runs 
all_234_spread <- 
sqldf("select  phrase
, substr(phrase, 1, instr(phrase, ' ') - 1) as word1
, substr(phrase, instr(phrase, ' '),  length(phrase)) as word234
, cnt 
from all_234 
order by cnt desc      ")

sqldf("select  phrase
, length(phrase) -  length(replace(phrase, ' ', '')) as spaces
, substr(phrase, 1, instr(phrase, ' ') - 1) as word1
, replace(phrase, rtrim(phrase, replace(phrase, ' ', '')), '') as wordX
, substr(phrase, instr(phrase, ' '),  length(phrase)) as word234
, cnt 
from all_234
where trim(phrase) like '%now%'
order by cnt desc      ")


saveRDS(all_234_spread, "./data/all_234_spread.rds") 

# Quiz 2 
ex_1 <- "The guy in front of me just bought a pound of bacon, a bouquet, and a case of"
ex_1t <- tokens(corpus(ex_1)
                , remove_punct = TRUE, remove_numbers = TRUE, remove_url = TRUE, remove_symbols = TRUE) %>% 
        tokens_select( stopwords('english'), selection = 'remove') %>%
        tokens_tolower() %>%
        tokens_replace(profanity, lemma, valuetype="fixed") %>%
        tokens_wordstem() 
ex_1t # "guy"     "front"   "just"    "bought"  "pound"   "bacon"   "bouquet" "case"  


ex_1_dt <- as.tibble(setDT((as.list(ex_1t))))
ex_1_dt # now all in separate rows
spread(ex_1_dt, text1)

ex_2 <- "You're the reason why I smile everyday. Can you follow me please? It would mean the"
ex_3 <- "Hey sunshine, can you follow me and make me the"       
ex_4 <- "Very early observations on the Bills game: Offense still struggling but the"
ex_5 <- "Go on a romantic date at the"
ex_6 <- "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my"
ex_7 <- "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some"
ex_8 <- "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little"
ex_9 <- "Be grateful for the good times and keep the faith during the"
ex_10 <- "If this isn't the cutest thing you've ever seen, then you must be"

# convert to tokens
ex_1t <- tokens(corpus(ex_1)
       , remove_punct = TRUE, remove_numbers = TRUE, remove_url = TRUE, remove_symbols = TRUE) %>% 
        tokens_select( stopwords('english'), selection = 'remove') %>%
        tokens_tolower() %>%
        tokens_replace(profanity, lemma, valuetype="fixed") %>%
        tokens_wordstem() 

ex_2t <- tokens(corpus("ex_2")
                , remove_punct = TRUE, remove_numbers = TRUE, remove_url = TRUE, remove_symbols = TRUE) %>% 
        tokens_select( stopwords('english'), selection = 'remove') %>%
        tokens_tolower() %>%
        tokens_replace(profanity, lemma, valuetype="fixed") %>%
        tokens_wordstem() 

ex_3t <- tokens(corpus("ex_3")
                , remove_punct = TRUE, remove_numbers = TRUE, remove_url = TRUE, remove_symbols = TRUE) %>% 
        tokens_select( stopwords('english'), selection = 'remove') %>%
        tokens_tolower() %>%
        tokens_replace(profanity, lemma, valuetype="fixed") %>%
        tokens_wordstem()

ex_4t <- tokens(corpus("ex_4")
                , remove_punct = TRUE, remove_numbers = TRUE, remove_url = TRUE, remove_symbols = TRUE) %>% 
        tokens_select( stopwords('english'), selection = 'remove') %>%
        tokens_tolower() %>%
        tokens_replace(profanity, lemma, valuetype="fixed") %>%
        tokens_wordstem()

ex_5t <- tokens(corpus("ex_5")
                , remove_punct = TRUE, remove_numbers = TRUE, remove_url = TRUE, remove_symbols = TRUE) %>% 
        tokens_select( stopwords('english'), selection = 'remove') %>%
        tokens_tolower() %>%
        tokens_replace(profanity, lemma, valuetype="fixed") %>%
        tokens_wordstem()

ex_6t <- tokens(corpus("ex_6")
                , remove_punct = TRUE, remove_numbers = TRUE, remove_url = TRUE, remove_symbols = TRUE) %>% 
        tokens_select( stopwords('english'), selection = 'remove') %>%
        tokens_tolower() %>%
        tokens_replace(profanity, lemma, valuetype="fixed") %>%
        tokens_wordstem()

ex_7t <- tokens(corpus("ex_7")
                , remove_punct = TRUE, remove_numbers = TRUE, remove_url = TRUE, remove_symbols = TRUE) %>% 
        tokens_select( stopwords('english'), selection = 'remove') %>%
        tokens_tolower() %>%
        tokens_replace(profanity, lemma, valuetype="fixed") %>%
        tokens_wordstem()

ex_8t <- tokens(corpus("ex_8")
                , remove_punct = TRUE, remove_numbers = TRUE, remove_url = TRUE, remove_symbols = TRUE) %>% 
        tokens_select( stopwords('english'), selection = 'remove') %>%
        tokens_tolower() %>%
        tokens_replace(profanity, lemma, valuetype="fixed") %>%
        tokens_wordstem()

ex_9t <- tokens(corpus("ex_9")
                , remove_punct = TRUE, remove_numbers = TRUE, remove_url = TRUE, remove_symbols = TRUE) %>% 
        tokens_select( stopwords('english'), selection = 'remove') %>%
        tokens_tolower() %>%
        tokens_replace(profanity, lemma, valuetype="fixed") %>%
        tokens_wordstem()

ex_10t <- tokens(corpus("ex_10")
                , remove_punct = TRUE, remove_numbers = TRUE, remove_url = TRUE, remove_symbols = TRUE) %>% 
        tokens_select( stopwords('english'), selection = 'remove') %>%
        tokens_tolower() %>%
        tokens_replace(profanity, lemma, valuetype="fixed") %>%
        tokens_wordstem()


# this runs 
all_234_spread <- 
        sqldf("select  phrase
, substr(phrase, 1, instr(phrase, ' ') - 1) as word1
, substr(phrase, instr(phrase, ' '),  length(phrase)) as word234
, length(phrase) as len
, charindex(' ', phrase) as charSpace1
, instr(phrase, ' ') as instSpace1
, cnt 
from all_234 
order by cnt desc      ")


# query
sqldf("select phrase, cnt from all_234_spread where phrase like '%case%' order by phrase, cnt desc ") # nothing found guess beer

sqldf("select phrase, cnt from all_234_spread where phrase like '%mean%' order by cnt desc ")  # found world

sqldf("select phrase, cnt from all_234_spread where phrase like '%make%' order by cnt desc ") #happi

sqldf("select phrase, cnt from all_234_spread where phrase like '%struggl%' order by cnt desc ") #not found guess defence

sqldf("select phrase, cnt from all_234_spread where phrase like '%date%' order by cnt desc ") #not found; movies, beach wrong

sqldf("select phrase, cnt from all_234_spread where phrase like '%be%' order by cnt desc ") #not found; way 

sqldf("select phrase, cnt from all_234_spread where phrase like '%some%' order by  phrase ")#not found time

sqldf("select phrase, cnt from all_234_spread where phrase like '%littl%' order by cnt desc ") #not found fingers

sqldf("select phrase, cnt from all_234_spread where phrase like '%good%' order by phrase, cnt desc ") #not found bad

sqldf("select phrase, cnt from all_234_spread where phrase like '%must%' order by cnt desc ") #not found wrong aspeep and insane

