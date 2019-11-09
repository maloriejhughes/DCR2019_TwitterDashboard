# build the gif

# Load --------------------------------------------------------------------

library(magick)
library(tidyverse)
library(rtweet)
library(raster)
library(rgdal)

# Functions ---------------------------------------------------------------

tag='rstatsdc'

get_tweets.fun<-function(tag){
  
  max.tweets=5000
  #since=as.character(Sys.Date()-1)
  #until=as.character(Sys.Date()+1)
  
  
  seed.num=1234
  # Set API Keys
  api_key <- ""
  api_secret <- ""
  access_token <- ""
  access_token_secret <- ""
  
  
  
  twitter_token <- create_token(
    
    consumer_key = api_key,
    consumer_secret =  api_secret
    ,access_token=access_token
    , access_secret = access_token_secret)
  
  rstats_tweets <- search_tweets(q = "#rstatsdc",
                                 n = 2000)
  
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
  
  
  return(rstats_tweets)
}


# Query & Clean -----------------------------------------------------------


rstats_tweets<-get_tweets.fun(tag=tag)


media_id<-data.frame(media_id=unlist(rstats_tweets$media_url)
                     , date=      as_date(rstats_tweets$created_at)
                     , screen_name=rstats_tweets$screen_name
                     , hashtags=   rstats_tweets$tags
                     , mentions=   rstats_tweets$mentions
) %>% filter(date >= "2019-11-06" , !is.na(media_id))%>% group_by(date) %>% sample_n(min(25,n()))
 

# Gif! --------------------------------------------------------------------

#### How I did it with pipes before I wanted to add a different annotation per image and I gave up trying to pipe it and wrote a loop. 
#### Forgive me.
#image_read(as.character(media_id$media_id))%>%
# image_join() %>% # joins image
# image_animate(fps=2) %>% # animates, can opt for number of loops
# image_write("dcr.gif")


img<-NULL
for(i in 1:nrow(media_id)){
  
  img2<- image_read(as.character(media_id$media_id[i])) 
  img2<- image_scale(img2,  paste0("x",as.character(min(400,image_info(img2)$height)))) %>%
    image_annotate(as.character(media_id$screen_name[i]), size = 18,  boxcolor = "black", color="white"
                   , degrees= as.integer( sample(seq(330:390),1)), location = "+45+45") %>%
    image_annotate(paste0("Mentions: ",as.character(media_id$mentions[i])), size = 18,  boxcolor = "white", color="black"
                   ,  gravity="south") %>%
    image_annotate(as.character(media_id$hashtags[i]), size = 18,  boxcolor = "lightgrey", color="black"
                   ,  gravity="north")
  
  img<- img %>% image_join(img2)
  
}

img %>% #image_join() %>% # joins image
  image_animate(fps=1) %>% # animates, can opt for number of loops
  image_write("IMAGES/dcr2019_3.gif")

