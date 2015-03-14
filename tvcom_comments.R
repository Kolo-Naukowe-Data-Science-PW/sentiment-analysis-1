###---------tv.com------game-of-thrones

#comments for each episode will be set in a data frame
#in order to have all the comments in one data type, I will create a list of data frames 

library(rvest)
library(stringi)

episodes <- list(41)
seasons <- c("http://www.tv.com/shows/game-of-thrones/season-1/", 
             "http://www.tv.com/shows/game-of-thrones/season-2/", 
             "http://www.tv.com/shows/game-of-thrones/season-3/", 
             "http://www.tv.com/shows/game-of-thrones/season-4/")

for(i in seq_along(seasons)){
   html_seasons <- html(seasons[i])
   episodes[[i]] <- html_nodes(html_seasons, "._inline_navigation li:nth-child(2) a") %>% html_attr("href")
}

episodes[[2]] <- episodes[[2]][-length(episodes[[2]])] #I remove an episode 
#which was a pre-season special

episodes <- lapply(episodes, rev)
episodes <- unlist(episodes)
episodes <- lapply(episodes, function(x) stri_paste("http://www.tv.com", x, collapse = ""))
episodes <- unlist(episodes)

n <- length(episodes)
everything <- list(n)
#the elements of the above list will be data frames

#each data frame consists of lists of unknown length which depends on the number of comments for each episode

for(i in seq_along(episodes)){
   episode_html <- html(episodes[i])
   
   comments <- html_nodes(episode_html, ".text")
   comments <- html_text(comments)
   comments <- comments[-length(comments)]
   comments <- stri_replace_all_regex(comments, "\r|\n|\t", "")
   comments <- unlist(comments)
   
   users <- html_nodes(episode_html, ".author")
   users <- html_text(users)
   users <- unlist(users)
   
   dates <- html_nodes(episode_html, ".byline")
   dates <- html_text(dates)
   dates <- as.character(dates)
   dates <- stri_extract_all_regex(dates, "[A-Z]{1}[a-z]{2}[[:space:]][0-9]{2},[[:space:]][0-9]{4}")
   dates <- unlist(dates)
   dates <- stri_replace_all_regex(dates, ",", "")
   mylocale <- Sys.setlocale("LC_TIME")
   Sys.setlocale("LC_TIME", "English")
   dates <- as.Date(dates, "%b %d %Y")
   Sys.setlocale("LC_TIME", mylocale)
   
   everything[[i]] <- data.frame(Nick = users, Date = dates, 
                                 Comment = comments)
}   
everything


