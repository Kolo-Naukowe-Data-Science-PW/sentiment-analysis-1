library(rvest)
library(stringi)
library(tm)
# Data from:
# http://www.theguardian.com/uk
# Game of Thrones: episode by episode
# http://www.theguardian.com/tv-and-radio/series/game-of-thrones-episode-by-episode


#url for episodes
url_episodes1<-"http://www.theguardian.com/tv-and-radio/series/game-of-thrones-episode-by-episode"
url_episodes2<-"http://www.theguardian.com/tv-and-radio/series/game-of-thrones-episode-by-episode?page=2")
html <- html(url_episodes)
epsiode <- html_nodes(html,".u-faux-block-link__overlay")%>% html_attr("href")
n<-length(epsiode)


#function which make data.frame for one episode
TGUepisodeDataFrame<-function(url){
   #url page with discussion
   url_comments <-html_nodes(html(url),".discussion__top-border > div:nth-child(1) > div:nth-child(1) > div:nth-child(1) > div:nth-child(1) > h2:nth-child(1) > a:nth-child(1)")
   url_comments <-html_attr(url_comments,"href")
      
   html <- html(url_comments)
   
   comment_author<-html_nodes(html,".d-comment__author span")%>% html_text()
   comment_date<-html_nodes(html,".js-timestamp")%>% html_text()
   
   mylocale <- Sys.getlocale("LC_TIME")
   Sys.setlocale("LC_TIME", "English")
   comment_date <- as.Date(comment_date, "%d %B %Y %R")
   
   comment_body <- html_nodes(html,".d-comment__body")%>% html_text()
   corpus <- Corpus(VectorSource(comment_body))
   #remove white space
   corpus <- tm_map(corpus, stripWhitespace)
   #remove punctuation
   corpus <- tm_map(corpus, removePunctuation)
   #transformation to lowercase
   corpus <- tm_map(corpus, content_transformer(tolower))                  
   comment_body<-unlist(sapply(corpus, `[`, "content"))
   

   episode.df<-data.frame(author=comment_author,
                          date=comment_date,
                          body=comment_body)
   
   #number of page with comments:
   pages_num<-html_nodes(html,".pagination__action--last")%>% html_text()
   pages_num<-as.numeric(pages_num)
   if (length(pages_num)==0){
      pages_num<-html_nodes(html,".js-discussion-change-page")%>% html_text()
      pages_num<-length(pages_num)+1
   }
   if (pages_num>=2){
      for (i in 2:pages_num){
         html<-html(stri_paste(url_comments,"?page=",i))
         
         comment_author<-html_nodes(html,".d-comment__author span")%>% html_text()
         
         comment_date<-html_nodes(html,".js-timestamp")%>% html_text()
         comment_date <- as.Date(comment_date, "%d %B %Y %R")
         
         comment_body <- html_nodes(html,".d-comment__body")%>% html_text()
         corpus <- Corpus(VectorSource(comment_body))
         #remove white space
         corpus <- tm_map(corpus, stripWhitespace)
         #remove punctuation
         corpus <- tm_map(corpus, removePunctuation)
         #transformation to lowercase
         corpus <- tm_map(corpus, content_transformer(tolower))                  
         comment_body<-unlist(sapply(corpus, `[`, "content"))
         episode.df<-rbind(episode.df,data.frame(author=comment_author,
                                 body=comment_body,
                                 date=comment_date))
      }
   }
   Sys.setlocale("LC_TIME", mylocale)
   episode.df
}


