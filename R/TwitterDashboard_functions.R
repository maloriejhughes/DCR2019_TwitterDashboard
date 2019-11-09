# Twitter dashboard functions
exclude_screen_name_list=c("@rstatstweet"
                           , "@rstatsnyc"
                           , "@rstatsdc"
                          , "@Work_Bench" 
                          , "@LanderAnalytics"
                          , "@GamerGeekNews"
                          , "@rstats4ds"  
                          , "@jaredlander"
                          ,"@R4DScommunity")

# GENERL PURPOSE FUNCTIONS ------------------------------------------------


get_tweets.fun <- function( tag , max.tweets, twitter_token ){


 
  seed.num <- 1234
  
  rstats_tweets <- search_tweets( q = tag
                                  , n = max.tweets)
  
  temp<-gsub('\"',"", rstats_tweets$mentions_screen_name, fixed=TRUE)
  temp<-gsub('c(',"", temp, fixed=TRUE)
  temp<-gsub(")","",temp)
  temp<-gsub(" ","",temp)
  temp<-gsub(","," @", temp, fixed=TRUE)
  temp<-paste0("@",temp)
  rstats_tweets$mentions<-temp
  temp<-gsub('\"',"", rstats_tweets$hashtags, fixed=TRUE)
  temp<-gsub('c(',"", temp, fixed=TRUE)
  temp<-gsub(")","",temp)
  temp<-gsub(" ","",temp)
  temp<-gsub(","," #", temp, fixed=TRUE)
  temp<-paste0("#",temp)
  rstats_tweets$tags<-temp
  rstats_tweets$screen_name<-paste0("@",rstats_tweets$screen_name)
  rstats_tweets$created_at <-with_tz( rstats_tweets$created_at, "America/New_York")
  return(rstats_tweets)
}


nwords.fun<-function(string, pseudo=F){
  ifelse( pseudo, 
          pattern <- "\\S+", 
          pattern <- "[[:alpha:]]+" 
  )
  str_count(string, pattern)
}

# kindly ignore how stupid this function is. 
ngram.fun<-function(comment_data,n){
  # if(n==1){
  stopWords <- stopwords("en")
  comment_data$text<- removeWords(comment_data$text, c(stopWords)) 
  comment_data <- comment_data %>% filter(nchar(text)>0)
  # }
  # stopWords <- stopwords("en")
  #comment_data$text <- gsub('[[:punct:] ]+',' ',comment_data$text)
  comment_data$text<-gsub(".", "", comment_data$text, fixed = TRUE)
  comment_data$text<-gsub("--&gt", "", comment_data$text, fixed = TRUE)
  comment_data$text<-gsub(",", "", comment_data$text, fixed = TRUE)
  comment_data$text<-gsub(".", "", comment_data$text, fixed = TRUE)
  comment_data$text<-gsub(";", "", comment_data$text, fixed = TRUE)
  comment_data$text<-gsub(")", " ", comment_data$text, fixed = TRUE)
  comment_data$text<-gsub("(", " ", comment_data$text, fixed = TRUE)
  
  comment_data$text<- trimws(comment_data$text, "both")
  comment_data$text<-gsub("    ", " ", comment_data$text, fixed = TRUE)
  comment_data$text<-gsub("   ", " ", comment_data$text, fixed = TRUE)
  comment_data$text<-gsub("  ", " ", comment_data$text, fixed = TRUE)
  comment_data$word_count<-sapply(strsplit(comment_data$text, " "), length)
  comment_data<-subset(comment_data, word_count>=n)
  
  ng <- ngram(comment_data$text, n=n)
  get.phrasetable(ng)
}




# SUCCESS METRIC 2 --------------------------------------------------------


##### SUCCESS METRICS

prep_metrics_for_composite_score.fun<-function(rstats_tweets, exclude_screen_name_list){
new_dat <- rstats_tweets
new_dat$text<- removeWords(new_dat$text, c(stopWords)) 

user_all_words<- new_dat %>% filter(screen_name!="@rstatstweet" & screen_name!="@rstatsnyc" & screen_name!="@rstatsdc"
                                    & screen_name!="@Work_Bench" & screen_name!="@LanderAnalytics"   & screen_name!=  "@GamerGeekNews"           & screen_name!="@rstats4ds"             & screen_name!="@jaredlander") %>%
  group_by(screen_name) %>% 
  summarise(num_tweets=n()
            , retweets=length(is_retweet[is_retweet==TRUE])          # num retweets
            , original_tweets=length(is_retweet[is_retweet==FALSE])  # num orig tweets
            , retweeted_by_others = sum(retweet_count, na.rm=TRUE)   # total times X's post is shared
            , liked_by_others = sum(favorite_count, na.rm=TRUE)      # total favorites on X's tweets
            , followers=max(followers_count)                         # num followers
            , num_photos=length(media_type[media_type=="photo"])     # num photos
            , all_text= paste(tolower(text),collapse=" ")  # text from all posts
            , total_words= nwords.fun(all_text)      # total words from all posts
            
            , num_users_mentions= str_count(all_text,"@")            # total mentions made by X
            , num_hashtags= str_count(all_text,"#")                  # total tags made by X
            , favorites_per_follower = liked_by_others/followers
  ) %>%
  mutate( `Contribution & Engagement` = round( ifelse(original_tweets>0,1,0)*(5*retweeted_by_others + 3*liked_by_others  )/(log(followers+1) + 1)
                                    +  (num_photos+ num_users_mentions + num_hashtags)/(original_tweets+1)  )
          , `Dedicated Small Players` = round( (num_photos+ num_users_mentions + num_hashtags + original_tweets+ retweets )/ifelse(followers<=500,1,  sqrt(followers-500) ) ) 
          ) %>%
  arrange(desc( `Contribution & Engagement`))
return(user_all_words)
}
