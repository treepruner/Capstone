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
# Load twitter chunks
# and Clean out symbols, emoticons, emoji, etc with textclean package
######################################################################

###################
## Read in twitter1 ##
###################
twitterPath <- "./data/twitter1.rds"
twitter1 <- readRDS(twitterPath)
twitter1Lines <- length(twitter1)
twitter1Lines # 224822
class(twitter1) # character
head(twitter1,1)
# ""How are you? Btw thanks for the RT. You gonna be in DC anytime soon? Love to see you. Been way, way too long."

system.time( twitter1Cln <- replace_email(twitter1, replacement = 'email') )  # 6.39
system.time( twitter1Cln <- replace_url(twitter1Cln, replacement = 'url')  )  # 1.02
system.time( twitter1Cln <- replace_non_ascii(twitter1Cln) )                  # 20.32
system.time( twitter1Cln <- replace_emoticon(twitter1Cln) )                   # 41.54
system.time( twitter1Cln <- replace_symbol(twitter1Cln) )                     # 0.89
system.time( twitter1Cln <- replace_hash(twitter1Cln, replacement = '$3') )   # 0.78
system.time( twitter1Cln <- replace_tokens(twitter1Cln, profanity, "profanity"))# 11.08
system.time( twitter1Cln <- replace_white(twitter1Cln) )                      # 2.42
system.time( twitter1Cln <- tolower(twitter1Cln))                             # 0.74
system.time( twitter1Cln <- replace_contraction(twitter1Cln) )                # 64.05
system.time( twitter1Cln <- replace_emoji(twitter1Cln) )                      # 194.91
system.time( twitter1Cln <- replace_internet_slang(twitter1Cln) )             # 209.72

saveRDS(twitter1Cln, "./data/twitter1Cln.rds")
rm(twitter1, twitter1Lines, twitterPath )

######################################################
# Create twitter1 n2grams
# and remove numbers, punctuation, repeated letters, single letter words
######################################################

# tidyverse 
# http://uc-r.github.io/creating-text-features
# tidytextmining.com/ngrams

twitter1_2gram <- 
  data.table(twitter1Cln) %>%
  unnest_tokens(bigram, 'twitter1Cln', token = "ngrams", n = 2) %>%
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
saveRDS(twitter1_2gram, "./data/twitter1_2gram.rds")

## twitter1_2gram examples
head(twitter1_2gram)
#     word1    word2
# 1     dc anytime
# 2  tired  horror
# 3 horror  played



######################################################
# Create twitter1 3grams
# and remove numbers, punctuation, repeated letters, single letter words
######################################################

# tidyverse 
# http://uc-r.github.io/creating-text-features
# tidytextmining.com/ngrams

twitter1_3gram <- 
  data.table(twitter1Cln) %>%
  unnest_tokens(trigram, 'twitter1Cln', token = "ngrams", n = 3) %>%
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

saveRDS(twitter1_3gram, "./data/twitter1_3gram.rds")

# what do we have?
head(twitter1_3gram)
#       word1    word2    word3
# 1   tired     horror played
# 2  horror     played  lazer
# 3  played      lazer    tag

# close all twitter1 files
rm(twitter1Cln, twitter1_2gram, twitter1_3gram)

###################
## Read in twitter2 ##
###################
twitterPath <- "./data/twitter2.rds"
twitter2 <- readRDS(twitterPath)
twitter2Lines <- length(twitter2)
twitter2Lines # 224822
class(twitter2) # character
head(twitter2,1)
# "see you then!"

system.time( twitter2Cln <- replace_email(twitter2, replacement = 'email') )  # 7.06
system.time( twitter2Cln <- replace_url(twitter2Cln, replacement = 'url')  )  # 1.14
system.time( twitter2Cln <- replace_non_ascii(twitter2Cln) )                  # 21.67
system.time( twitter2Cln <- replace_emoticon(twitter2Cln) )                   # 40.30
system.time( twitter2Cln <- replace_symbol(twitter2Cln) )                     # 0.89
system.time( twitter2Cln <- replace_hash(twitter2Cln, replacement = '$3') )   # 0.79
system.time( twitter2Cln <- replace_tokens(twitter2Cln, profanity, "profanity"))# 10.33
system.time( twitter2Cln <- replace_white(twitter2Cln) )                      # 2.28
system.time( twitter2Cln <- tolower(twitter2Cln))                             # 0.74
system.time( twitter2Cln <- replace_contraction(twitter2Cln) )                # 54.08
system.time( twitter2Cln <- replace_emoji(twitter2Cln) )                      # 198.44
system.time( twitter2Cln <- replace_internet_slang(twitter2Cln) )             # 192.03

saveRDS(twitter2Cln, "./data/twitter2Cln.rds")
rm(twitter2, twitter2Lines, twitterPath )

######################################################
# Create twitter2 n2grams
# and remove numbers, punctuation, repeated letters, single letter words
######################################################

# tidyverse 
# http://uc-r.github.io/creating-text-features
# tidytextmining.com/ngrams

twitter2_2gram <- 
  data.table(twitter2Cln) %>%
  unnest_tokens(bigram, 'twitter2Cln', token = "ngrams", n = 2) %>%
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
saveRDS(twitter2_2gram, "./data/twitter2_2gram.rds")

## twitter2_2gram examples
head(twitter2_2gram)
#     word1    word2
# 1  sneak   peek
# 2   peek runnin
# 3 runnin  round



######################################################
# Create twitter2 3grams
# and remove numbers, punctuation, repeated letters, single letter words
######################################################

# tidyverse 
# http://uc-r.github.io/creating-text-features
# tidytextmining.com/ngrams

twitter2_3gram <- 
  data.table(twitter2Cln) %>%
  unnest_tokens(trigram, 'twitter2Cln', token = "ngrams", n = 3) %>%
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

saveRDS(twitter2_3gram, "./data/twitter2_3gram.rds")

# what do we have?
head(twitter2_3gram)
#       word1    word2    word3
# 1     sneak      peek   runnin
# 2      peek    runnin    round
# 3    runnin     round   dallas

rm(twitter2Cln, twitter2_2gram, twitter2_3gram)


###################
## Read in twitter3 ##
###################
twitterPath <- "./data/twitter3.rds"
twitter3 <- readRDS(twitterPath)
twitter3Lines <- length(twitter3)
twitter3Lines # 224823
class(twitter3) # character
head(twitter3,1)
# "Getting fat off of pizza and I don't give a fuck"

system.time( twitter3Cln <- replace_email(twitter3, replacement = 'email') )  # 6.734
system.time( twitter3Cln <- replace_url(twitter3Cln, replacement = 'url')  )  # 0.98
system.time( twitter3Cln <- replace_non_ascii(twitter3Cln) )                  # 21.00
system.time( twitter3Cln <- replace_emoticon(twitter3Cln) )                   # 39.97
system.time( twitter3Cln <- replace_symbol(twitter3Cln) )                     # 0.87
system.time( twitter3Cln <- replace_hash(twitter3Cln, replacement = '$3') )   # 0.73
system.time( twitter3Cln <- replace_tokens(twitter3Cln, profanity, "profanity"))# 10.34
system.time( twitter3Cln <- replace_white(twitter3Cln) )                      # 2.31
system.time( twitter3Cln <- tolower(twitter3Cln))                             # 0.75
system.time( twitter3Cln <- replace_contraction(twitter3Cln) )                # 53.36
system.time( twitter3Cln <- replace_emoji(twitter3Cln) )                      # 196.92
system.time( twitter3Cln <- replace_internet_slang(twitter3Cln) )             # 186.08

saveRDS(twitter3Cln, "./data/twitter3Cln.rds")
rm(twitter3, twitter3Lines, twitterPath )

######################################################
# Create twitter3 n2grams
# and remove numbers, punctuation, repeated letters, single letter words
######################################################

# tidyverse 
# http://uc-r.github.io/creating-text-features
# tidytextmining.com/ngrams

twitter3_2gram <- 
  data.table(twitter3Cln) %>%
  unnest_tokens(bigram, 'twitter3Cln', token = "ngrams", n = 2) %>%
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
saveRDS(twitter3_2gram, "./data/twitter3_2gram.rds")

## twitter3_2gram examples
head(twitter3_2gram)
#     word1    word2
# 1 internet people
# 2   people    hey
# 3  deandre jordan



######################################################
# Create twitter3 3grams
# and remove numbers, punctuation, repeated letters, single letter words
######################################################

# tidyverse 
# http://uc-r.github.io/creating-text-features
# tidytextmining.com/ngrams

twitter3_3gram <- 
  data.table(twitter3Cln) %>%
  unnest_tokens(trigram, 'twitter3Cln', token = "ngrams", n = 3) %>%
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

saveRDS(twitter3_3gram, "./data/twitter3_3gram.rds")

# what do we have?
head(twitter3_3gram)
#       word1    word2    word3
# 1 internet people    hey
# 2 remember     ha  brian
# 3   kindly smiley   hope

rm(twitter3Cln, twitter3_2gram, twitter3_3gram)



###################
## Read in twitter4 ##
###################
twitterPath <- "./data/twitter4.rds"
twitter4 <- readRDS(twitterPath)
twitter4Lines <- length(twitter4)
twitter4Lines # 224821
class(twitter4) # character
head(twitter4,1)
# "Completely done with any work for school. Time to burn every notebook i own"

system.time( twitter4Cln <- replace_email(twitter4, replacement = 'email') )  # 6.74
system.time( twitter4Cln <- replace_url(twitter4Cln, replacement = 'url')  )  # 1.01
system.time( twitter4Cln <- replace_non_ascii(twitter4Cln) )                  # 21.20
system.time( twitter4Cln <- replace_emoticon(twitter4Cln) )                   # 39.96
system.time( twitter4Cln <- replace_symbol(twitter4Cln) )                     # 0.90
system.time( twitter4Cln <- replace_hash(twitter4Cln, replacement = '$3') )   # 0.77
system.time( twitter4Cln <- replace_tokens(twitter4Cln, profanity, "profanity"))# 10.69
system.time( twitter4Cln <- replace_white(twitter4Cln) )                      # 2.53
system.time( twitter4Cln <- tolower(twitter4Cln))                             # 0.82
system.time( twitter4Cln <- replace_contraction(twitter4Cln) )                # 57.00
system.time( twitter4Cln <- replace_emoji(twitter4Cln) )                      # 194.04
system.time( twitter4Cln <- replace_internet_slang(twitter4Cln) )             # 189.82

saveRDS(twitter4Cln, "./data/twitter4Cln.rds")
rm(twitter4, twitter4Lines, twitterPath )

######################################################
# Create twitter4 n2grams
# and remove numbers, punctuation, repeated letters, single letter words
######################################################

# tidyverse 
# http://uc-r.github.io/creating-text-features
# tidytextmining.com/ngrams

twitter4_2gram <- 
  data.table(twitter4Cln) %>%
  unnest_tokens(bigram, 'twitter4Cln', token = "ngrams", n = 2) %>%
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
saveRDS(twitter4_2gram, "./data/twitter4_2gram.rds")

## twitter4_2gram examples
head(twitter4_2gram)
#     word1    word2
# 1 school     time
# 2   gran  totally
# 3 smiley      hey



######################################################
# Create twitter4 3grams
# and remove numbers, punctuation, repeated letters, single letter words
######################################################

# tidyverse 
# http://uc-r.github.io/creating-text-features
# tidytextmining.com/ngrams

twitter4_3gram <- 
  data.table(twitter4Cln) %>%
  unnest_tokens(trigram, 'twitter4Cln', token = "ngrams", n = 3) %>%
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

saveRDS(twitter4_3gram, "./data/twitter4_3gram.rds")

# what do we have?
head(twitter4_3gram)
#       word1    word2    word3
# 1     smiley      hey  remember
# 2      major       db  material
# 3   pursuing   acting     dream

rm(twitter4Cln, twitter4_2gram, twitter4_3gram)
