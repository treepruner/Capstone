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


    select(word1, word2) %>%
      unite(bigram, word1, word2, sep = " ")

head(freq_2grams)
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

freq_2grams <- readRDS("./data/freq_2grams")
head(freq_2grams)

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


 
########################################### does not work begin ###########################################
### get an error like an out of memory message when using the all_grams file. Worked with sample file.
### not sure my formulas make sense anyway!

# log likelihood ratio test
# n3 =  trigram, w1 = word1, w2 = word2, w3 = word3

# count for trigrams
cnt_n3w3 <- all_3grams %>%   count(word3) # 194,622
cnt_n3w12 <- all_3grams %>%   count(word1, word2) # 2,691,782
cnt_n3w123 <- all_3grams %>%   count(word1, word2, word3) # 3,934,255
N3 <-nrow(all_3grams)  # 4,635,555

rm(all_3grams)

head(cnt_n3w3)
head(cnt_n3w12 )
head(cnt_n3w123)

#  how to rename columns and fix the formulas?
# word1 word2     word3        n.x   n.y  n.x.x   n.y.y
# <chr> <chr>     <chr>       <int> <int>  <int>  <int>
# 1 aaron  standard  time       1    29     37    582
# 2 aarp   discounts cruise     1     1      4     20
# 3 aarron pilot     lady       1     1     24     57

# treat first 2 words as single word and follow n2gram LL formula
#  w12 = w123, w1 = w12, w2 = w3


LL_test_n3 <- cnt_n3w123 %>%
  left_join(cnt_n3w12, by = "word1" , "word2") %>%  # 
  left_join(cnt_n3w3, by = "word3") %>%  # word1  word2  word3  n.x  n.y  n.x.x  n.y.y
  rename(c_w12 = n.y, c_w3 = n, c_w123 = n.x) %>%
  mutate( 
       p  =  c_w3  / N3
    , p1  =  c_w123 / c_w12
    , p2  = (c_w3  - c_w123) / (N3 - c_w12)
    , LL  = log ( (pbinom(c_w123, c_w12, p)  * pbinom(c_w3 - c_w123, N3 - c_w12, p ))
                  / 
                    (pbinom(c_w123, c_w12, p1) * pbinom(c_w3 - c_w123, N3 - c_w12, p)) )
  )

saveRDS(LL_test_n3, "./data/LL_test_n3.rds")  
  
head(LL_test_n3)
# word1 word2.x  word3   c_w123 word2.y        c_w12  c_w3          p    p1    p2    LL
# <chr> <chr>    <chr>    <int> <chr>          <int> <int>      <dbl> <dbl> <dbl> <dbl>
# 1 aa    baseball frieder      1 baseball           2     1 0.00000357   0.5     0 0.288
# 2 aa    baseball frieder      1 boys               1     1 0.00000357   1       0 0    
# 3 aa    baseball frieder      1 flight             1     1 0.00000357   1       0 0    
# 4 aa    baseball frieder      1 frootloop          1     1 0.00000357   1       0 0    
# 5 aa    baseball frieder      1 fye                1     1 0.00000357   1       0 0    
# 6 aa    baseball frieder      1 intersectional     2     1 0.00000357   0.5     0 0.288


u_n3grams <- LL_test_n3 %>%
  mutate(
    Chi_value = -2 * LL
    , pvalue = pchisq(LL, df = 1)) %>%
  filter(pvalue < 0.05) %>%
  #  select(word1, word2) %>%
  #    unite(bigram, word1, word2, sep = " ")

saveRDS(u_n3grams, "./data/u_n3grams")  
    
head(u_n3grams)
# word1 word2.x  word3   c_w123 word2.y   c_w12  c_w3          p    p1    p2    LL Chi_value pvalue
# <chr> <chr>    <chr>    <int> <chr>     <int> <int>      <dbl> <dbl> <dbl> <dbl>     <dbl>  <dbl>
# 1 aa    baseball frieder      1 boys          1     1 0.00000357     1     0     0         0      0
# 2 aa    baseball frieder      1 flight        1     1 0.00000357     1     0     0         0      0
# 3 aa    baseball frieder      1 frootloop     1     1 0.00000357     1     0     0         0      0
# 4 aa    baseball frieder      1 fye           1     1 0.00000357     1     0     0         0      0
# 5 aa    baseball frieder      1 irukke        1     1 0.00000357     1     0     0         0      0
# 6 aa    baseball frieder      1 meetings      1     1 0.00000357     1     0     0         0      0

 
########################################### does not work end ###########################################











