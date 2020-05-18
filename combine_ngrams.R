rm(list=ls())
require(quanteda)
require(dplyr)
require(sqldf)
require(data.table)




###################################################### 2 grams ######################################################

#####################
# read in news 2grams
#####################
news1_2gram <- readRDS("./data/news1_2gram.rds")
news1_2gramLines <- nrow(news1_2gram)
news1_2gramLines # 1515398
head(news1_2gram)

news2_2gram <- readRDS("./data/news2_2gram.rds")
news2_2gramLines <- nrow(news2_2gram)
news2_2gramLines # 1536841

news3_2gram <- readRDS("./data/news3_2gram.rds")
news3_2gramLines <- nrow(news3_2gram)
news3_2gramLines # 1536841

news4_2gram <- readRDS("./data/news4_2gram.rds")
news4_2gramLines <- nrow(news4_2gram)
news4_2gramLines # 1532233


# combine all news 2grams
news_2grams <- sqldf("
select 'news' as source, word1, word2
from (  select word1, word2
        from news1_2gram
  union all                   
        select word1, word2
        from news2_2gram
  union all                   
        select word1, word2
        from news3_2gram       
  union all                   
        select word1, word2
        from news4_2gram
     )
                     ")

saveRDS(news_2grams, "./data/news_2grams.rds") 

# close news 2grams
rm(news1_2gramLines, news2_2gramLines, news3_2gramLines, news4_2gramLines)
rm(news1_2gram , news2_2gram , news3_2gram , news4_2gram , news_2grams )


#####################
# read in blogs 2grams
#####################
blogs1_2gram <- readRDS("./data/blogs1_2gram.rds")
blogs1_2gramLines <- nrow(blogs1_2gram)
blogs1_2gramLines # 1116125
head(blogs1_2gram)

blogs2_2gram <- readRDS("./data/blogs2_2gram.rds")
blogs2_2gramLines <- nrow(blogs2_2gram)
blogs2_2gramLines # 1111630

blogs3_2gram <- readRDS("./data/blogs3_2gram.rds")
blogs3_2gramLines <- nrow(blogs3_2gram)
blogs3_2gramLines # 1120973

blogs4_2gram <- readRDS("./data/blogs4_2gram.rds")
blogs4_2gramLines <- nrow(blogs4_2gram)
blogs4_2gramLines # 1121500


# combine all blogs 2grams
blogs_2grams <- sqldf("
select 'blogs' as source, word1, word2
from (  select word1, word2
        from blogs1_2gram
  union all                   
        select word1, word2
        from blogs2_2gram
  union all                   
        select word1, word2
        from blogs3_2gram       
  union all                   
        select word1, word2
        from blogs4_2gram
     )
                     ")

saveRDS(blogs_2grams, "./data/blogs_2grams.rds") 

# close blogs 2grams
rm(blogs1_2gramLines, blogs2_2gramLines, blogs3_2gramLines, blogs4_2gramLines)
rm(blogs1_2gram , blogs2_2gram , blogs3_2gram , blogs4_2gram , blogs_2grams )


#####################
# read in twitter 2grams
#####################
twitter1_2gram <- readRDS("./data/twitter1_2gram.rds")
twitter1_2gramLines <- nrow(twitter1_2gram)
twitter1_2gramLines # 427943
head(twitter1_2gram)

twitter2_2gram <- readRDS("./data/twitter2_2gram.rds")
twitter2_2gramLines <- nrow(twitter2_2gram)
twitter2_2gramLines # 429361

twitter3_2gram <- readRDS("./data/twitter3_2gram.rds")
twitter3_2gramLines <- nrow(twitter3_2gram)
twitter3_2gramLines # 430141

twitter4_2gram <- readRDS("./data/twitter4_2gram.rds")
twitter4_2gramLines <- nrow(twitter4_2gram)
twitter4_2gramLines # 429424


# combine all twitter 2grams
twitter_2grams <- sqldf("
select 'twitter' as source, word1, word2
from (  select word1, word2
        from twitter1_2gram
  union all                   
        select word1, word2
        from twitter2_2gram
  union all                   
        select word1, word2
        from twitter3_2gram       
  union all                   
        select word1, word2
        from twitter4_2gram
     )
                     ")

saveRDS(twitter_2grams, "./data/twitter_2grams.rds") 

# close twitter 2grams
rm(twitter1_2gramLines , twitter2_2gramLines , twitter3_2gramLines , twitter4_2gramLines)
rm(twitter1_2gram , twitter2_2gram , twitter3_2gram , twitter4_2gram , twitter_2grams )

