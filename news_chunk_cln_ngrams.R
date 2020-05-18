######################################################
# Clear environment and load packages
######################################################
rm(list=ls())

require(dplyr)
require(tidyr)
require(tidytext)
require(tidyverse)
require(textclean)
require(textshape)
require(stringr)
require(sqldf)
require(data.table)
require(ggplot2)
require(quanteda)

(opt <- quanteda_options()) # threads = 2
#quanteda_options(verbose = TRUE)
quanteda_options(threads = 4)


######################################################
# Load previously downloaded files from disk 
######################################################
setwd("C:/Users/testsubject941/Documents/GitHub/Capstone")

## Read in raw news file & divide into chunks
#### https://stackoverflow.com/questions/3318333/split-a-vector-into-chunks-in-r

newsPath <- "./data/news.rds"
news <- readRDS(newsPath)
newsLines <- length(news)
newsLines # 1010242 /4 = 1:252560, 252561:505120 , 505121:757680, 757681:1010242
class(news) # character

news1 <- news[1:252560]
saveRDS(news1, "./data/news1.rds")

news2 <- news[252561:505120]
saveRDS(news2, "./data/news2.rds")

news3 <- news[505121:757680]
saveRDS(news3, "./data/news3.rds")

news4 <- news[757681:1010242]
saveRDS(news4, "./data/news4.rds")

rm("news", "newsPath", "newsLines", "news1", "news2", "news3", "news4")

## Read in raw blogs file & divide into chunks
blogsPath <- "./data/blogs.rds"
blogs <- readRDS(blogsPath)
blogsLines <- length(blogs)
blogsLines # 899288 /4 , 1:224822, 224823:449644, 449645:674467, 674468:899288
class(blogs) # character

blogs1 <- blogs[1:224822]
saveRDS(blogs1, "./data/blogs1.rds")

blogs2 <- blogs[224823:449644]
saveRDS(blogs2, "./data/blogs2.rds")

blogs3 <- blogs[449645:674467]
saveRDS(blogs3, "./data/blogs3.rds")

blogs4 <- blogs[674468:899288]
saveRDS(blogs4, "./data/blogs4.rds")

rm("blogs", "blogsPath", "blogsLines", "blogs1", "blogs2", "blogs3", "blogs4")


## Read in raw twitter file & take chunks
twitterPath <- "./data/twitter.rds"
twitter <- readRDS(twitterPath)
twitterLines <- length(twitter)
twitterLines # 2360148 /4 , 1:590037, 590038:1180075, 1180076:1770112, 1770113:2360148
class(twitter) # character


twitter1 <- twitter[1:224822]
saveRDS(twitter1, "./data/twitter1.rds")

twitter2 <- twitter[224823:449644]
saveRDS(twitter2, "./data/twitter2.rds")

twitter3 <- twitter[449645:674467]
saveRDS(twitter3, "./data/twitter3.rds")

twitter4 <- twitter[674468:899288]
saveRDS(twitter4, "./data/twitter4.rds")

rm("twitter", "twitterPath", "twitterLines", "twitter1", "twitter2", "twitter3", "twitter4")


######################################################
# Load "bad words" file from Google.
######################################################
profanity <- readLines("./data/badwords.txt", encoding = "UTF-8", skipNul = TRUE,  warn = FALSE)
profanity <- tolower(profanity)
head(profanity)

# Make specific profanity a generic literal
lemma <- rep("profanity", length(profanity))
head(lemma)



######################################################################
# Load news chunks
# and Clean out symbols, emoticons, emoji, etc with textclean package
######################################################################

###################
## Read in news1 ##
###################
newsPath <- "./data/news1.rds"
news1 <- readRDS(newsPath)
news1Lines <- length(news1)
news1Lines # 252560
class(news1) # character
head(news1,1)
# "He wasn't home alone, apparently."

system.time( news1Cln <- replace_email(news1, replacement = 'email') )  # 23.65
system.time( news1Cln <- replace_url(news1Cln, replacement = 'url')  )  # 2.23
system.time( news1Cln <- replace_non_ascii(news1Cln) )                  # 60.70
system.time( news1Cln <- replace_emoticon(news1Cln) )                   # 69.42
system.time( news1Cln <- replace_symbol(news1Cln) )                     # 1.77
system.time( news1Cln <- replace_hash(news1Cln, replacement = '$3') )   # 1.44
system.time( news1Cln <- replace_tokens(news1Cln, profanity, "profanity"))# 26.58
system.time( news1Cln <- replace_white(news1Cln) )                      # 6.89
system.time( news1Cln <- tolower(news1Cln))                             # 2.14
system.time( news1Cln <- replace_contraction(news1Cln) )                # 145.08
system.time( news1Cln <- replace_emoji(news1Cln) )                      # 559.27
system.time( news1Cln <- replace_internet_slang(news1Cln) )             # 545.03

saveRDS(news1Cln, "./data/news1Cln.rds")
rm(news1, news1Lines, news1Path )

######################################################
# Create news1 n2grams
# and remove numbers, punctuation, repeated letters, single letter words
######################################################

# tidyverse 
# http://uc-r.github.io/creating-text-features
# tidytextmining.com/ngrams

news1_2gram <- 
  data.table(news1Cln) %>%
  unnest_tokens(bigram, 'news1Cln', token = "ngrams", n = 2) %>%
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
saveRDS(news1_2gram, "./data/news1_2gram.rds")

## news1_2gram examples
head(news1_2gram)
#     word1    word2
# 1         st      louis
# 2      louis      plant
# 3        age    workers



######################################################
# Create news1 3grams
# and remove numbers, punctuation, repeated letters, single letter words
######################################################

# tidyverse 
# http://uc-r.github.io/creating-text-features
# tidytextmining.com/ngrams

news1_3gram <- 
  data.table(news1Cln) %>%
  unnest_tokens(trigram, 'news1Cln', token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
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
            , !word3 %in% stop_words$word
            , !str_detect(word3, pattern = "[[:digit:]]") # words with numbers
            , !str_detect(word3, pattern = "[[:punct:]]") # punctuation
            , !str_detect(word3, pattern = "(.)\\1{2,}")  # 3 or more repeated letters
            , !str_detect(word3, pattern = "\\b(.)\\b")   # single letter words
              ) 

saveRDS(news1_3gram, "./data/news1_3gram.rds")

# what do we have?
head(news1_3gram)
#       word1    word2    word3
# 1       st      louis      plant
# 2     mass automotive production

# close all news1 files
rm(news1Cln, news1_2gram, news1_3gram, newsLines, newsPath)

###################
## Read in news2 ##
###################
newsPath <- "./data/news2.rds"
news2 <- readRDS(newsPath)
news2Lines <- length(news2)
news2Lines # 252560
class(news2) # character
head(news2,1)
# "To reserve a table, call Ace's at 814-536-4176"

system.time( news2Cln <- replace_email(news2, replacement = 'email') )  # 23.07
system.time( news2Cln <- replace_url(news2Cln, replacement = 'url')  )  # 2.22
system.time( news2Cln <- replace_non_ascii(news2Cln) )                  # 64.06
system.time( news2Cln <- replace_emoticon(news2Cln) )                   # 119.68
system.time( news2Cln <- replace_symbol(news2Cln) )                     # 1.88
system.time( news2Cln <- replace_hash(news2Cln, replacement = '$3') )   # 1.41
system.time( news2Cln <- replace_tokens(news2Cln, profanity, "profanity"))# 22.71
system.time( news2Cln <- replace_white(news2Cln) )                      # 6.99
system.time( news2Cln <- tolower(news2Cln))                             # 2.18
system.time( news2Cln <- replace_contraction(news2Cln) )                # 164.03
system.time( news2Cln <- replace_emoji(news2Cln) )                      # 573.93
system.time( news2Cln <- replace_internet_slang(news2Cln) )             # 569.19

saveRDS(news2Cln, "./data/news2Cln.rds")
rm(news2, news2Lines, newsPath )

######################################################
# Create news2 n2grams
# and remove numbers, punctuation, repeated letters, single letter words
######################################################

# tidyverse 
# http://uc-r.github.io/creating-text-features
# tidytextmining.com/ngrams

news2_2gram <- 
  data.table(news2Cln) %>%
  unnest_tokens(bigram, 'news2Cln', token = "ngrams", n = 2) %>%
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
saveRDS(news2_2gram, "./data/news2_2gram.rds")

## news2_2gram examples
head(news2_2gram)
#     word1    word2
# 1     table      call
# 2 respected  national
# 3  national   academy



######################################################
# Create news2 3grams
# and remove numbers, punctuation, repeated letters, single letter words
######################################################

# tidyverse 
# http://uc-r.github.io/creating-text-features
# tidytextmining.com/ngrams

news2_3gram <- 
  data.table(news2Cln) %>%
  unnest_tokens(trigram, 'news2Cln', token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
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
            , !word3 %in% stop_words$word
            , !str_detect(word3, pattern = "[[:digit:]]") # words with numbers
            , !str_detect(word3, pattern = "[[:punct:]]") # punctuation
            , !str_detect(word3, pattern = "(.)\\1{2,}")  # 3 or more repeated letters
            , !str_detect(word3, pattern = "\\b(.)\\b")   # single letter words
  ) 

saveRDS(news2_3gram, "./data/news2_3gram.rds")

# what do we have?
head(news2_3gram)
#       word1    word2    word3
# 1 respected  national  academy
# 2    stakes decisions   firing
# 3 decisions    firing teachers

rm(news2Cln, news2_2gram, news2_3gram, newsPath)


###################
## Read in news3 ##
###################
newsPath <- "./data/news3.rds"
news3 <- readRDS(newsPath)
news3Lines <- length(news3)
news3Lines # 252560
class(news3) # character
head(news3,1)
# "To reserve a table, call Ace's at 814-536-4176"

system.time( news3Cln <- replace_email(news3, replacement = 'email') )  # 22.97
system.time( news3Cln <- replace_url(news3Cln, replacement = 'url')  )  # 2.22
system.time( news3Cln <- replace_non_ascii(news3Cln) )                  # 62.33
system.time( news3Cln <- replace_emoticon(news3Cln) )                   # 120.82
system.time( news3Cln <- replace_symbol(news3Cln) )                     # 2.05
system.time( news3Cln <- replace_hash(news3Cln, replacement = '$3') )   # 1.57
system.time( news3Cln <- replace_tokens(news3Cln, profanity, "profanity"))# 24.08
system.time( news3Cln <- replace_white(news3Cln) )                      # 6.88
system.time( news3Cln <- tolower(news3Cln))                             # 2.08
system.time( news3Cln <- replace_contraction(news3Cln) )                # 163.10
system.time( news3Cln <- replace_emoji(news3Cln) )                      # 567.07
system.time( news3Cln <- replace_internet_slang(news3Cln) )             # 547.42

saveRDS(news3Cln, "./data/news3Cln.rds")
rm(news3, news3Lines, newsPath )

######################################################
# Create news3 n2grams
# and remove numbers, punctuation, repeated letters, single letter words
######################################################

# tidyverse 
# http://uc-r.github.io/creating-text-features
# tidytextmining.com/ngrams

news3_2gram <- 
  data.table(news3Cln) %>%
  unnest_tokens(bigram, 'news3Cln', token = "ngrams", n = 2) %>%
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
saveRDS(news3_2gram, "./data/news3_2gram.rds")

## news3_2gram examples
head(news3_2gram)
#     word1    word2
# 1 reservation information
# 2 information     contact
# 3     contact         jim



######################################################
# Create news3 3grams
# and remove numbers, punctuation, repeated letters, single letter words
######################################################

# tidyverse 
# http://uc-r.github.io/creating-text-features
# tidytextmining.com/ngrams

news3_3gram <- 
  data.table(news3Cln) %>%
  unnest_tokens(trigram, 'news3Cln', token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
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
            , !word3 %in% stop_words$word
            , !str_detect(word3, pattern = "[[:digit:]]") # words with numbers
            , !str_detect(word3, pattern = "[[:punct:]]") # punctuation
            , !str_detect(word3, pattern = "(.)\\1{2,}")  # 3 or more repeated letters
            , !str_detect(word3, pattern = "\\b(.)\\b")   # single letter words
  ) 

saveRDS(news3_3gram, "./data/news3_3gram.rds")

# what do we have?
head(news3_3gram)
#       word1    word2    word3
# 1 reservation information contact
# 2 information     contact     jim
# 3     contact         jim kollias

rm(news3Cln, news3_2gram, news3_3gram)



###################
## Read in news4 ##
###################
newsPath <- "./data/news4.rds"
news4 <- readRDS(newsPath)
news4Lines <- length(news4)
news4Lines # 252560
class(news4) # character
head(news4,1)
# "Shawn Green, Los Angeles, May 23, 2002: Green went 6-for-6, with a single and double to go along with his homers. He set a major-league record with 19 total bases and drove in seven runs as the Dodgers won 16-3 at Milwaukee.""

system.time( news4Cln <- replace_email(news4, replacement = 'email') )  # 23.62
system.time( news4Cln <- replace_url(news4Cln, replacement = 'url')  )  # 2.12
system.time( news4Cln <- replace_non_ascii(news4Cln) )                  # 62.17
system.time( news4Cln <- replace_emoticon(news4Cln) )                   # 122.94
system.time( news4Cln <- replace_symbol(news4Cln) )                     # 2.14
system.time( news4Cln <- replace_hash(news4Cln, replacement = '$3') )   # 1.33
system.time( news4Cln <- replace_tokens(news4Cln, profanity, "profanity"))# 22.04
system.time( news4Cln <- replace_white(news4Cln) )                      # 6.90
system.time( news4Cln <- tolower(news4Cln))                             # 2.67
system.time( news4Cln <- replace_contraction(news4Cln) )                # 171.03
system.time( news4Cln <- replace_emoji(news4Cln) )                      # 581.17
system.time( news4Cln <- replace_internet_slang(news4Cln) )             # 580.03

saveRDS(news4Cln, "./data/news4Cln.rds")
rm(news4, news4Lines, newsPath )

######################################################
# Create news4 n2grams
# and remove numbers, punctuation, repeated letters, single letter words
######################################################

# tidyverse 
# http://uc-r.github.io/creating-text-features
# tidytextmining.com/ngrams

news4_2gram <- 
  data.table(news4Cln) %>%
  unnest_tokens(bigram, 'news4Cln', token = "ngrams", n = 2) %>%
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
saveRDS(news4_2gram, "./data/news4_2gram.rds")

## news4_2gram examples
head(news4_2gram)
#     word1    word2
# 1 1  shawn   green
# 2  green     los
# 3    los angeles



######################################################
# Create news4 3grams
# and remove numbers, punctuation, repeated letters, single letter words
######################################################

# tidyverse 
# http://uc-r.github.io/creating-text-features
# tidytextmining.com/ngrams

news4_3gram <- 
  data.table(news4Cln) %>%
  unnest_tokens(trigram, 'news4Cln', token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
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
            , !word3 %in% stop_words$word
            , !str_detect(word3, pattern = "[[:digit:]]") # words with numbers
            , !str_detect(word3, pattern = "[[:punct:]]") # punctuation
            , !str_detect(word3, pattern = "(.)\\1{2,}")  # 3 or more repeated letters
            , !str_detect(word3, pattern = "\\b(.)\\b")   # single letter words
  ) 

saveRDS(news4_3gram, "./data/news4_3gram.rds")

# what do we have?
head(news4_3gram)
#       word1    word2    word3
# 1   shawn      green      los
# 2   green        los  angeles
# 3   major     league   record

rm(news4Cln, news4_2gram, news4_3gram)
