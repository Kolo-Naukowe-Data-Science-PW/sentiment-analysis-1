# Data from http://asoiaf.westeros.org/index.php/forum/ - forum in which all episodes are discussed
library(rvest)
library(stringi)

#url for seasons 
urlSeason1 <- "http://asoiaf.westeros.org/index.php/forum/40-season-1/"
urlSeason2 <- "http://asoiaf.westeros.org/index.php/forum/59-season-2/"
urlSeason3 <- "http://asoiaf.westeros.org/index.php/forum/74-season-3/"
urlSeason4 <- "http://asoiaf.westeros.org/index.php/forum/86-season-4/"

#for every season we create list of data frames - one data frame for one episode
listSeason1 <- makeSeasonDataFrameList(urlSeason1)
listSeason2 <- makeSeasonDataFrameList(urlSeason2)
listSeason3 <- makeSeasonDataFrameList(urlSeason3)
listSeason4 <- makeSeasonDataFrameList(urlSeason4)

#making list of data frames (each data frame for one episode)
makeSeasonDataFrameList <- function(url){
   htmlSeason <- html(url)
   urlEpisodes <- html_nodes(htmlSeason, ".col_c_forum a") %>% html_attr("href")
   numOfEpisodes <- length(urlEpisodes)
   
   episodeList <- list()
   for (i in 1: numOfEpisodes){
      episodeList[[i]] <- makeEpisodeDataFrame(urlEpisodes[i])
   }
   return(episodeList)
}


#making data frame for the whole episode
makeEpisodeDataFrame <- function(urlEpisode){
   htmlEpisode <- html(urlEpisode)
   htmlEpisodeNodes <- html_nodes(htmlEpisode, ".topic_controls.clearfix, .topic_title")
   htmlText <- html_text(htmlEpisodeNodes[1])
   howManyTopicPages <- unlist(stri_extract_all_regex(htmlText, "(?<=totalPages: )[0-9]{1,4}"))
   #we count how many topic pages are for chosen episode
   if (!is.na(howManyTopicPages)){
      howManyTopicPages <- as.numeric(howManyTopicPages)
   } else {
      howManyTopicPages <- 1
   }
   urlTopicsList <- list()
   for (i in 1:howManyTopicPages){
      urlPage <- paste(urlEpisode, "/page-", i, sep="")
      htmlOnePage <- html(urlPage)
      htmlOnePageNodes <- html_nodes(htmlOnePage, ".ipsBox .topic_title")
      urlTopic <- htmlOnePageNodes %>%  html_attr("href")
      urlTopicsList[[i]] <- urlTopic
   } 
   #we create a vector with urls for every topic in chosen episode
   urlTopics <- unlist(urlTopicsList) 
   n <- length(urlTopics)
   dfList <- list()
   for (i in 1:n){
      #for every topic we make list of data frames and then we bind them
      list <- makeTopicDataFrameList(urlTopics[i])
      #we put data frame (in which there is the whole topic) as element of list
      dfList[[i]] <- rbindDataFrameList(list)
   }
   #then again we bind data frames with topics (so finally we get data frame for episode)
   dfWholeEpisode <- rbindDataFrameList(dfList)
   return(dfWholeEpisode)
}


#making list of data frames for one topic - so if the topic has more than one page 
#we create one data frame for one page and put it into the list
#number of elements in list is number of pages for topic
makeTopicDataFrameList <- function(urlTopic){
   htmlCode <- html_nodes(html(urlTopic), ".topic_controls")
   htmlCodeText <- html_text(htmlCode)[1]
   howManyPages <- unlist(stri_extract_all_regex(htmlCodeText, "(?<=totalPages: )[0-9]{1,4}"))
   if (!is.na(howManyPages)){
      howManyPages <- as.numeric(howManyPages)
   } else {
      howManyPages <- 1
   }
   dfList <- list()
   for (i in 1:howManyPages){
      urlPage <- paste(urlTopic, "/page-", i, sep="")
      df <- makeOnePageDataFrame(urlPage)
      dfList[[i]] <- df
   } 
   return(dfList)
}


#making data frame for comments only from one page (no more than 20 comments)
#data frame consists of three column - nick, date and comment
makeOnePageDataFrame <- function(urlOnePage){
   html <- html(urlOnePage)
   #page <- html_nodes(html, ".vcard , .published , .entry-content")
   page <- html_nodes(html, ".row2 , .published , .entry-content")
   rawData <- html_text(page)
   
   n <- length(rawData)
   
   nick <- rawData[seq(1,n,3)]
   nick <- stri_trim_both(nick)
   tmp <- unlist(stri_extract_all_regex(nick, "(?<=\t).+"))
   nick <- tmp[seq(6, length(tmp), 6)]
   
   #to extract data we have to change temporarly locale to english
   date <- rawData[seq(2,n,3)]
   mylocale <- Sys.setlocale("LC_TIME")
   Sys.setlocale("LC_TIME", "English")
   date <- as.Date(date, "%d %B %Y")
   Sys.setlocale("LC_TIME", mylocale)
   
   comment <- rawData[seq(3,n,3)]
   comment <- stri_trim_both(comment)
   
   onePageDataFrame <- data.frame("nick"=nick, "date"=date, "comment"=comment)
   return(onePageDataFrame) 
}


#function to bind few data frames (data frames are put as the elements of some list)
rbindDataFrameList <- function(dataFrameList){
   n <- length(dataFrameList)
   if (n==1){
      return(dataFrameList[[1]])
   } else {
      tmpDataFrame <- dataFrameList[[1]]
      for (i in 2:n){
         bindDataFrames <- rbind(tmpDataFrame, dataFrameList[[i]])
         tmpDataFrame <- bindDataFrames
      }
      return(tmpDataFrame)
   }
}