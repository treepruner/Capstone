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
# profanity - fix records missed in cleaning
######################################################

# read in
profanity <- readLines(".\data\badwords.txt", encoding = "UTF-8", skipNul = TRUE,  warn = FALSE)

# raw data examples and counts 
profanity_example <- head(profanity,2)
profanity_rowCnt <- length(profanity) 
class(profanity)

# change to lowercase
profanity <- tolower(profanity)

# make specific profanity a generic literal
lemma <- rep("profanity", length(profanity))

profanity_ref <- as.data.frame(cbind(profanity, lemma))
head(profanity_ref)


######################################################
# Load 2grams
######################################################

getwd()
setwd("C:/Users/testsubject941/Documents/GitHub/Capstone")

all_2gramsPath <- "./data/all_2grams.rds"
all_2grams <- readRDS(all_2gramsPath)
all_2gramsLines <- nrow(all_2grams)
all_2gramsLines # 12,305,888
head(all_2grams)

# source      word1      word2
# 1   news         st      louis
# 2   news      louis      plant
# 3   news        age    workers


######################################################
# clean 2grams profanity
######################################################
all_2grams <- sqldf( c( "update all_2grams
        set word1 = 'profanity'
        where word1 = (select p.profanity 
                        from profanity_ref p
                        where all_2grams.word1 = p.profanity)",
                             "select * from all_2grams"))

all_2grams <- sqldf( c( "update all_2grams
        set word2 = 'profanity'
        where word2 = (select p.profanity 
                        from profanity_ref p
                        where all_2grams.word2 = p.profanity)",
                             "select * from all_2grams"))




sqldf( "select * from all_2gramss
where bigram like '%fuck%' 
order by bigram desc " )

######################################################
# Create all_n2grams statistics
######################################################


## log likelihood ratio test naming convention
# all = news, blogs and twitter files, n2 =  bigram, w1 = word1, w2 = word2

# count for all_n2gramss
cnt_n2w1  <- all_2grams %>% count(word1) #270,333
cnt_n2w2  <- all_2grams %>% count(word2) #279,108
cnt_n2w12 <- all_2grams %>% count(word1, word2) #5,965,393
N2 <-nrow(all_2grams)  # 12,305,888

LL_test_n2 <- cnt_n2w12 %>%
  left_join(cnt_n2w1, by = "word1") %>%
  left_join(cnt_n2w2, by = "word2") %>%  # word1       word2           n.x   n.y     n
  rename(c_w1 = n.y, c_w2 = n, c_w12 = n.x) %>%
  mutate(
       p  =  c_w2  / N2
    , p1  =  c_w12 / c_w1
    , p2  = (c_w2  - c_w12) / (N2 - c_w1)
    , LL  = log ( (pbinom(c_w12, c_w1, p)  * pbinom(c_w2 - c_w12, N2 - c_w1, p ))
                      / 
                  (pbinom(c_w12, c_w1, p1) * pbinom(c_w2 - c_w12, N2 - c_w1, p)) )
                )

saveRDS(LL_test_n2, "./data/LL_test_n2.rds")

LL_test_n2[1093:1096, ]
nrow(LL_test_n2) # 5965393

# word1       word2           c_w12  c_w1  c_w2     p     p1        p2        LL
# <chr> <chr>      <int> <int> <int>     <dbl>  <dbl>     <dbl> <dbl>
# A tibble: 18 x 9
# 1 aaronovitch philip           1     1   581 0.0000472  1       0.0000471  0    
# 2 aarp        adds             1   105  1124 0.0000913  0.00952 0.0000913  0.307
# 3 aarp        angels           1   105   785 0.0000638  0.00952 0.0000637  0.307
# 4 aarp        appointments     1   105   429 0.0000349  0.00952 0.0000348  0.307



# where is a good cutoff?
sqldf(" select count(*) from LL_test_n2 where c_w12 >2") # 624,457
sqldf(" select count(*) from LL_test_n2 where c_w12 >3") # 408,472
sqldf(" select count(*) from LL_test_n2 where c_w12 >4") # 299,965
sqldf(" select count(*) from LL_test_n2 where c_w12 >5") # 235,664
sqldf(" select count(*) from LL_test_n2 where c_w12 >6") # 192,769
sqldf(" select count(*) from LL_test_n2 where c_w12 >7") # 162,411
sqldf(" select count(*) from LL_test_n2 where c_w12 >8") # 139,977
sqldf(" select count(*) from LL_test_n2 where c_w12 >=9") # 122,566 keeps "zucchini squash"

LL_test_n2 <- readRDS("./data/LL_test_n2.rds")

# code runs, but can't view table 
freq_2grams <- LL_test_n2 %>%
  mutate(
    Chi_value = -2 * LL
    , pvalue = pchisq(LL, df = 1)
    , bigram = paste0(word1, "_", word2)
    ) %>%
  filter(pvalue < 0.05 & c_w12 >= 9) %>%
  select (word1, word2, c_w12, c_w1, c_w2, bigram) %>%
  arrange (word1, desc(c_w12) )

head(freq_2grams)
nrow(freq_2grams)
class(freq_2grams)  # object pvalue not found


# word1      word2  c_w12  c_w1  c_w2
# <chr>      <chr>  <int> <int> <int>
# 1 aarion     penton     9     9    20
# 2 ablin      chief     19    19  5432
# 3 action     excuse    12  4115 43522
# 4 add        people    10  5702 35174
# 5 added      people    11  3938 35174
# 6 additional people    10  5142 35174

saveRDS(freq_2grams, "./data/freq_2grams")

rm(all_2grams, all_2gramsLines, all_2gramsPath, cnt_n2w1, cnt_n2w2, cnt_n2w12, N2, LL_test_n2, freq_2grams)


######################################################
# Create all_3grams statistics
######################################################

# tidyverse 
# http://uc-r.github.io/creating-text-features
# tidytextmining.com/ngrams

all_3gramsPath <- "./data/all_3grams.rds"
all_3grams <- readRDS(all_3gramsPath)
all_3gramsLines <- nrow(all_3grams)
all_3gramsLines # 4,635,555
head(all_3grams)

##############################################################
# Clearn to fix missed profanity
##############################################################
all_3grams <- sqldf( c( "update all_3grams
        set word1 = 'profanity'
        where word1 = (select p.profanity 
                        from profanity_ref p
                        where all_3grams.word1 = p.profanity)",
                             "select * from all_3grams"))

all_3grams <- sqldf( c( "update all_3grams
        set word2 = 'profanity'
        where word2 = (select p.profanity 
                        from profanity_ref p
                        where all_3grams.word2 = p.profanity)",
                             "select * from all_3grams"))

all_3grams <- sqldf( c( "update all_3grams
        set word3 = 'profanity'
        where word3 = (select p.profanity 
                        from profanity_ref p
                        where all_3grams.word3 = p.profanity)",
                             "select * from all_3grams"))



sqldf( "select * from all_3grams
where trigram like '%fuck%' 
order by trigram desc " )




freq_3grams <-
  sqldf("select word1, word2, word3, word1 || '_' || word2 || '_' || word3 as trigram, count (*) as cnt 
        from all_3grams
        group by word1, word2, word3 
        having count(*) >= 2")

saveRDS(freq_3grams, "./data/freq_3grams")

head(freq_3grams); tail(freq_3grams)
nrow(freq_3grams) # >8 13592; > 4 35674;  > 2 94756 ; >= 2 254850

#   word1          word2        word3                        trigram cnt
# 1    aa       baseball      tuesday            aa_baseball_tuesday   4
# 2    aa           city championship           aa_city_championship   3
# 3    aa       football championship       aa_football_championship   2
# 4    aa          girls   basketball            aa_girls_basketball   2
# 5    aa intersectional championship aa_intersectional_championship   3
# 6    aa intersectional        final        aa_intersectional_final   3


 
