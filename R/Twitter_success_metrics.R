### for handle X

# unique tags made by X in X's tweets
# unique mentions made by X in X's tweets
# unique handles shared by X


#gsub("c(","","c("cbirunda", "StatModeling")")


require(stringr)
nwords.fun <- function(string, pseudo=F){
  ifelse( pseudo, 
          pattern <- "\\S+", 
          pattern <- "[[:alpha:]]+" 
  )
  str_count(string, pattern)
}



# Success Metrics ---------------------------------------------------------


stopWords <- stopwords("en")
new_dat <- rstats_tweets
new_dat$text<- removeWords(new_dat$text, c(stopWords)) 

user_all_words<- new_dat %>% 
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
            , unique_vocabulary=nwords.fun( unique(str_split(trimws(gsub('[[:punct:] ]+',' ',removeWords( all_text,c(stopWords)) ), "both"),' ')))
            , num_users_mentions= str_count(all_text,"@")            # total mentions made by X
            , num_hashtags= str_count(all_text,"#")                  # total tags made by X
            , favorites_per_follower = liked_by_others/followers
            )
           

# unique vocabulary (non-stopwords)
#  , unique_words=nwords.fun(set([unique(all_text)]) )             
### unique vocabulary (non-stopwords)
install.packages("text2vec")
library(text2vec)
data("movie_review")
txt = movie_review[['review']][1:100]
it = itoken(txt, tolower, word_tokenizer, n_chunks = 10)
vocab = create_vocabulary(it)
nrow(vocab)
temp<-create_vocabulary(str_split(user_all_words$all_text[1]," "))


trimws("both")
unique(str_split(gsub("  "," ",user_all_words$all_text[1],"both")," "))

#comment_data$text <- 
nwords.fun( unique(str_split(trimws(gsub('[[:punct:] ]+',' ',removeWords( user_all_words$all_text[1],c(stopWords)) ), "both"),' ')))

#  favorites/followers


