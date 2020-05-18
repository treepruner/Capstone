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




#####################
# read in 2grams
#####################
news_2grams <- readRDS("./data/news_2grams.rds")
news_2gramLines <- nrow(news_2grams)
news_2gramLines # 6118791

blogs_2grams <- readRDS("./data/blogs_2grams.rds")
blogs_2gramLines <- nrow(blogs_2grams)
blogs_2gramLines # 4470228

twitter_2grams <- readRDS("./data/twitter_2grams.rds")
twitter_2gramLines <- nrow(twitter_2grams)
twitter_2gramLines # 1716869

# combine all 2grams
all_2grams <- sqldf("
select source, word1, word2
from (  select source, word1, word2
        from news_2grams
  union all                   
        select source, word1, word2
        from blogs_2grams
  union all                   
        select source, word1, word2
        from twitter_2grams       
     )
                     ")

saveRDS(all_2grams, "./data/all_2grams.rds") 

rm(news_2gramLines, blogs_2gramLines, twitter_2gramLines)
rm(news_2grams, blogs_2grams, twitter_2grams)



#####################################################################################################################
###############################################      3 grams       ################################################
#####################################################################################################################

#####################
# read in news 3grams
#####################
news1_3gram <- readRDS("./data/news1_3gram.rds")
news1_3gramLines <- nrow(news1_3gram)
news1_3gramLines #  615491
head(news1_3gram)

news2_3gram <- readRDS("./data/news2_3gram.rds")
news2_3gramLines <- nrow(news2_3gram)
news2_3gramLines # 618295
head(news2_3gram)

news3_3gram <- readRDS("./data/news3_3gram.rds")
news3_3gramLines <- nrow(news3_3gram)
news3_3gramLines # 615802
head(news3_3gram)

news4_3gram <- readRDS("./data/news4_3gram.rds")
news4_3gramLines <- nrow(news4_3gram)
news4_3gramLines # 614973
head(news4_3gram)

# combine all news 3grams
news_3grams <- sqldf("
select 'news' as source, word1, word2, word3
from (  select word1, word2, word3
        from news1_3gram
  union all                   
        select word1, word2, word3
        from news2_3gram
  union all                   
        select word1, word2, word3
        from news3_3gram       
  union all                   
        select word1, word2, word3
        from news4_3gram
     )
                     ")

head(news_3grams)

saveRDS(news_3grams, "./data/news_3grams.rds") 

# close news 3grams
rm(news1_3gramLines, news2_3gramLines, news3_3gramLines, news4_3gramLines)
rm(news1_3gram , news2_3gram , news3_3gram , news4_3gram , news_3grams )


#####################
# read in blogs 3grams
#####################
blogs1_3gram <- readRDS("./data/blogs1_3gram.rds")
blogs1_3gramLines <- nrow(blogs1_3gram)
blogs1_3gramLines # 377496
head(blogs1_3gram)

blogs2_3gram <- readRDS("./data/blogs2_3gram.rds")
blogs2_3gramLines <- nrow(blogs2_3gram)
blogs2_3gramLines # 373880
head(blogs2_3gram)

blogs3_3gram <- readRDS("./data/blogs3_3gram.rds")
blogs3_3gramLines <- nrow(blogs3_3gram)
blogs3_3gramLines # 377301
head(blogs3_3gram)

blogs4_3gram <- readRDS("./data/blogs4_3gram.rds")
blogs4_3gramLines <- nrow(blogs4_3gram)
blogs4_3gramLines # 378141
head(blogs4_3gram)

# combine all blogs 3grams
blogs_3grams <- sqldf("
select 'blogs' as source, word1, word2, word3
from (  select word1, word2, word3
        from blogs1_3gram
  union all                   
        select word1, word2, word3
        from blogs2_3gram
  union all                   
        select word1, word2, word3
        from blogs3_3gram       
  union all                   
        select word1, word2, word3
        from blogs4_3gram
     )
                     ")
saveRDS(blogs_3grams, "./data/blogs_3grams.rds") 

# close blogs 3grams
rm(blogs1_3gramLines, blogs2_3gramLines, blogs3_3gramLines, blogs4_3gramLines)
rm(blogs1_3gram , blogs2_3gram , blogs3_3gram , blogs4_3gram , blogs_3grams )


#####################
# read in twitter 3grams
#####################
twitter1_3gram <- readRDS("./data/twitter1_3gram.rds")
twitter1_3gramLines <- nrow(twitter1_3gram)
twitter1_3gramLines # 165480
head(twitter1_3gram)

twitter2_3gram <- readRDS("./data/twitter2_3gram.rds")
twitter2_3gramLines <- nrow(twitter2_3gram)
twitter2_3gramLines # 165838

twitter3_3gram <- readRDS("./data/twitter3_3gram.rds")
twitter3_3gramLines <- nrow(twitter3_3gram)
twitter3_3gramLines # 166732

twitter4_3gram <- readRDS("./data/twitter4_3gram.rds")
twitter4_3gramLines <- nrow(twitter4_3gram)
twitter4_3gramLines # 166126


# combine all twitter 3grams
twitter_3grams <- sqldf("
select 'twitter' as source, word1, word2, word3
from (  select word1, word2, word3
        from twitter1_3gram
  union all                   
        select word1, word2, word3
        from twitter2_3gram
  union all                   
        select word1, word2, word3
        from twitter3_3gram       
  union all                   
        select word1, word2, word3
        from twitter4_3gram
     )
                     ")

saveRDS(twitter_3grams, "./data/twitter_3grams.rds") 

# close twitter 3grams
rm(twitter1_3gramLines , twitter2_3gramLines , twitter3_3gramLines , twitter4_3gramLines)
rm(twitter1_3gram , twitter2_3gram , twitter3_3gram , twitter4_3gram , twitter_3grams )


#####################
# read in 3grams
#####################
news_3grams <- readRDS("./data/news_3grams.rds")
news_3gramLines <- nrow(news_3grams)
news_3gramLines # 2464561
head(news_3grams) # 3 words

blogs_3grams <- readRDS("./data/blogs_3grams.rds")
blogs_3gramLines <- nrow(blogs_3grams)
blogs_3gramLines # 1506818
head(blogs_3grams) # 3 words

twitter_3grams <- readRDS("./data/twitter_3grams.rds")
twitter_3gramLines <- nrow(twitter_3grams)
twitter_3gramLines # 664176
head(twitter_3grams) # 3 words

# combine all 3grams
all_3grams <- sqldf("
select source, word1, word2, word3
from (  select source, word1, word2, word3
        from news_3grams
  union all                   
        select source, word1, word2, word3
        from blogs_3grams
  union all                   
        select source, word1, word2, word3
        from twitter_3grams       
     )
                     ")

saveRDS(all_3grams, "./data/all_3grams.rds") 
head(all_3grams)

rm(news_3gramLines, blogs_3gramLines, twitter_3gramLines)
rm(news_3grams, blogs_3grams, twitter_3grams)


