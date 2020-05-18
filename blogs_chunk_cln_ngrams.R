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
# Load blogs chunks
# and Clean out symbols, emoticons, emoji, etc with textclean package
######################################################################

###################
## Read in blogs1 ##
###################
blogsPath <- "./data/blogs1.rds"
blogs1 <- readRDS(blogsPath)
blogs1Lines <- length(blogs1)
blogs1Lines # 224822
class(blogs1) # character
head(blogs1,1)
# "In the years thereafter, most of the Oil fields and platforms were named after pagan "gods"

system.time( blogs1Cln <- replace_email(blogs1, replacement = 'email') )  # 22.70
system.time( blogs1Cln <- replace_url(blogs1Cln, replacement = 'url')  )  # 2.14
system.time( blogs1Cln <- replace_non_ascii(blogs1Cln) )                  # 61.08
system.time( blogs1Cln <- replace_emoticon(blogs1Cln) )                   # 123.36
system.time( blogs1Cln <- replace_symbol(blogs1Cln) )                     # 2.11
system.time( blogs1Cln <- replace_hash(blogs1Cln, replacement = '$3') )   # 1.30
system.time( blogs1Cln <- replace_tokens(blogs1Cln, profanity, "profanity"))# 21.87
system.time( blogs1Cln <- replace_white(blogs1Cln) )                      # 7.13
system.time( blogs1Cln <- tolower(blogs1Cln))                             # 2.14
system.time( blogs1Cln <- replace_contraction(blogs1Cln) )                # 156.89
system.time( blogs1Cln <- replace_emoji(blogs1Cln) )                      # 578.49
system.time( blogs1Cln <- replace_internet_slang(blogs1Cln) )             # 564.72

saveRDS(blogs1Cln, "./data/blogs1Cln.rds")
rm(blogs1, blogs1Lines, blogsPath )

######################################################
# Create blogs1 n2grams
# and remove numbers, punctuation, repeated letters, single letter words
######################################################

# tidyverse 
# http://uc-r.github.io/creating-text-features
# tidytextmining.com/ngrams

blogs1_2gram <- 
  data.table(blogs1Cln) %>%
  unnest_tokens(bigram, 'blogs1Cln', token = "ngrams", n = 2) %>%
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
saveRDS(blogs1_2gram, "./data/blogs1_2gram.rds")

## blogs1_2gram examples
head(blogs1_2gram)
#     word1    word2
# 1     oil    fields
# 2   pagan      gods
# 3   brown      chad



######################################################
# Create blogs1 3grams
# and remove numbers, punctuation, repeated letters, single letter words
######################################################

# tidyverse 
# http://uc-r.github.io/creating-text-features
# tidytextmining.com/ngrams

blogs1_3gram <- 
  data.table(blogs1Cln) %>%
  unnest_tokens(trigram, 'blogs1Cln', token = "ngrams", n = 3) %>%
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

saveRDS(blogs1_3gram, "./data/blogs1_3gram.rds")

# what do we have?
head(blogs1_3gram)
#       word1    word2    word3
# 1  dollar      dollar      dollar
# 2 letting        lola        feel
# 3    home       decor inspiration

# close all blogs1 files
rm(blogs1Cln, blogs1_2gram, blogs1_3gram, blogsPath)

###################
## Read in blogs2 ##
###################
blogsPath <- "./data/blogs2.rds"
blogs2 <- readRDS(blogsPath)
blogs2Lines <- length(blogs2)
blogs2Lines # 224822
class(blogs2) # character
head(blogs2,1)
# "Oh Divine redeemer from the Congo,"

system.time( blogs2Cln <- replace_email(blogs2, replacement = 'email') )  # 22.57
system.time( blogs2Cln <- replace_url(blogs2Cln, replacement = 'url')  )  # 2.06
system.time( blogs2Cln <- replace_non_ascii(blogs2Cln) )                  # 60.86
system.time( blogs2Cln <- replace_emoticon(blogs2Cln) )                   # 123.08
system.time( blogs2Cln <- replace_symbol(blogs2Cln) )                     # 2.10
system.time( blogs2Cln <- replace_hash(blogs2Cln, replacement = '$3') )   # 1.23
system.time( blogs2Cln <- replace_tokens(blogs2Cln, profanity, "profanity"))# 21.36
system.time( blogs2Cln <- replace_white(blogs2Cln) )                      # 7.85
system.time( blogs2Cln <- tolower(blogs2Cln))                             # 2.24
system.time( blogs2Cln <- replace_contraction(blogs2Cln) )                # 151.77
system.time( blogs2Cln <- replace_emoji(blogs2Cln) )                      # 576.77
system.time( blogs2Cln <- replace_internet_slang(blogs2Cln) )             # 549.63

saveRDS(blogs2Cln, "./data/blogs2Cln.rds")
rm(blogs2, blogs2Lines, blogsPath )

######################################################
# Create blogs2 n2grams
# and remove numbers, punctuation, repeated letters, single letter words
######################################################

# tidyverse 
# http://uc-r.github.io/creating-text-features
# tidytextmining.com/ngrams

blogs2_2gram <- 
  data.table(blogs2Cln) %>%
  unnest_tokens(bigram, 'blogs2Cln', token = "ngrams", n = 2) %>%
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
saveRDS(blogs2_2gram, "./data/blogs2_2gram.rds")

## blogs2_2gram examples
head(blogs2_2gram)
#     word1    word2
# 1   divine  redeemer
# 2 positive      note
# 3   secret   killing



######################################################
# Create blogs2 3grams
# and remove numbers, punctuation, repeated letters, single letter words
######################################################

# tidyverse 
# http://uc-r.github.io/creating-text-features
# tidytextmining.com/ngrams

blogs2_3gram <- 
  data.table(blogs2Cln) %>%
  unnest_tokens(trigram, 'blogs2Cln', token = "ngrams", n = 3) %>%
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

saveRDS(blogs2_3gram, "./data/blogs2_3gram.rds")

# what do we have?
head(blogs2_3gram)
#       word1    word2    word3
# 1    cambodia   guatemala           el
# 2   guatemala          el     salvador
# 3      secret    killings     strength

rm(blogs2Cln, blogs2_2gram, blogs2_3gram)


###################
## Read in blogs3 ##
###################
blogsPath <- "./data/blogs3.rds"
blogs3 <- readRDS(blogsPath)
blogs3Lines <- length(blogs3)
blogs3Lines # 224823
class(blogs3) # character
head(blogs3,1)
# "sent an email to the university asking why I was refused a place."

system.time( blogs3Cln <- replace_email(blogs3, replacement = 'email') )  # 22.54
system.time( blogs3Cln <- replace_url(blogs3Cln, replacement = 'url')  )  # 2.12
system.time( blogs3Cln <- replace_non_ascii(blogs3Cln) )                  # 62.02
system.time( blogs3Cln <- replace_emoticon(blogs3Cln) )                   # 124.80
system.time( blogs3Cln <- replace_symbol(blogs3Cln) )                     # 2.13
system.time( blogs3Cln <- replace_hash(blogs3Cln, replacement = '$3') )   # 1.26
system.time( blogs3Cln <- replace_tokens(blogs3Cln, profanity, "profanity"))# 21.47
system.time( blogs3Cln <- replace_white(blogs3Cln) )                      # 6.98
system.time( blogs3Cln <- tolower(blogs3Cln))                             # 2.10
system.time( blogs3Cln <- replace_contraction(blogs3Cln) )                # 150.36
system.time( blogs3Cln <- replace_emoji(blogs3Cln) )                      # 577.80
system.time( blogs3Cln <- replace_internet_slang(blogs3Cln) )             # 551.84

saveRDS(blogs3Cln, "./data/blogs3Cln.rds")
rm(blogs3, blogs3Lines, blogsPath )

######################################################
# Create blogs3 n2grams
# and remove numbers, punctuation, repeated letters, single letter words
######################################################

# tidyverse 
# http://uc-r.github.io/creating-text-features
# tidytextmining.com/ngrams

blogs3_2gram <- 
  data.table(blogs3Cln) %>%
  unnest_tokens(bigram, 'blogs3Cln', token = "ngrams", n = 2) %>%
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
saveRDS(blogs3_2gram, "./data/blogs3_2gram.rds")

## blogs3_2gram examples
head(blogs3_2gram)
#     word1    word2
# 1    product      named
# 2      named   sulfacet
# 3     sulfur    product



######################################################
# Create blogs3 3grams
# and remove numbers, punctuation, repeated letters, single letter words
######################################################

# tidyverse 
# http://uc-r.github.io/creating-text-features
# tidytextmining.com/ngrams

blogs3_3gram <- 
  data.table(blogs3Cln) %>%
  unnest_tokens(trigram, 'blogs3Cln', token = "ngrams", n = 3) %>%
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

saveRDS(blogs3_3gram, "./data/blogs3_3gram.rds")

# what do we have?
head(blogs3_3gram)
#       word1    word2    word3
# 1  product    named   sulfacet
# 2 treating     acne     sulfur
# 3  flaking     skin       rash

rm(blogs3Cln, blogs3_2gram, blogs3_3gram)



###################
## Read in blogs4 ##
###################
blogsPath <- "./data/blogs4.rds"
blogs4 <- readRDS(blogsPath)
blogs4Lines <- length(blogs4)
blogs4Lines # 224821
class(blogs4) # character
head(blogs4,1)
# "We finished out the night at Mainstreet After Hours in downtown Lakeville; a coffee shop/wine bar that I've wanted check out. We've been there a few times for coffees but never for a glass of wine. We took some seats next to a table by the gas fireplace and settled in. It was a little discouraging to see such a small crowd on a Friday night but the woman behind the bar assured us that they're usually quite busy but that this has been an off week. That's good to know. We really liked it and intend to go back there regularly. There's an atmosphere within that can't be found at some of the more generic stamped-out-franchise places that we typically frequent. Plus, I always enjoy supporting a local business!"

system.time( blogs4Cln <- replace_email(blogs4, replacement = 'email') )  # 22.47
system.time( blogs4Cln <- replace_url(blogs4Cln, replacement = 'url')  )  # 2.18
system.time( blogs4Cln <- replace_non_ascii(blogs4Cln) )                  # 62.89
system.time( blogs4Cln <- replace_emoticon(blogs4Cln) )                   # 124.39
system.time( blogs4Cln <- replace_symbol(blogs4Cln) )                     # 2.11
system.time( blogs4Cln <- replace_hash(blogs4Cln, replacement = '$3') )   # 1.28
system.time( blogs4Cln <- replace_tokens(blogs4Cln, profanity, "profanity"))# 22.08
system.time( blogs4Cln <- replace_white(blogs4Cln) )                      # 7.43
system.time( blogs4Cln <- tolower(blogs4Cln))                             # 2.66
system.time( blogs4Cln <- replace_contraction(blogs4Cln) )                # 153.87
system.time( blogs4Cln <- replace_emoji(blogs4Cln) )                      # 575.99
system.time( blogs4Cln <- replace_internet_slang(blogs4Cln) )             # 552.64

saveRDS(blogs4Cln, "./data/blogs4Cln.rds")
rm(blogs4, blogs4Lines, blogsPath )

######################################################
# Create blogs4 n2grams
# and remove numbers, punctuation, repeated letters, single letter words
######################################################

# tidyverse 
# http://uc-r.github.io/creating-text-features
# tidytextmining.com/ngrams

blogs4_2gram <- 
  data.table(blogs4Cln) %>%
  unnest_tokens(bigram, 'blogs4Cln', token = "ngrams", n = 2) %>%
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
saveRDS(blogs4_2gram, "./data/blogs4_2gram.rds")

## blogs4_2gram examples
head(blogs4_2gram)
#     word1    word2
# 1 downtown lakeville
# 2   coffee      shop
# 3     shop      wine



######################################################
# Create blogs4 3grams
# and remove numbers, punctuation, repeated letters, single letter words
######################################################

# tidyverse 
# http://uc-r.github.io/creating-text-features
# tidytextmining.com/ngrams

blogs4_3gram <- 
  data.table(blogs4Cln) %>%
  unnest_tokens(trigram, 'blogs4Cln', token = "ngrams", n = 3) %>%
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

saveRDS(blogs4_3gram, "./data/blogs4_3gram.rds")

# what do we have?
head(blogs4_3gram)
#       word1    word2    word3
# 1   coffee       shop   wine
# 2     shop       wine    bar
# 3    local   business eating

rm(blogs4Cln, blogs4_2gram, blogs4_3gram)
