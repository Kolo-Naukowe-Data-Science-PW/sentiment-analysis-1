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

#for(i in 1:4){
#   episode_i <- character(10)
#   episode <- character(10)
#   episode3 <- character(10)
#   episode4 <- character(10)
#}
#
#n <- length(episode1)
#for(i in seq_along(names)){
#   names[[i]][10] <- stri_paste("tvc_s0", i, "e10")
#   for(j in 1:9){
#      names[[i]][j] <- stri_paste("tvc_s0", i, "e0", j)
#   }
#}
#unlist(names)
#length(names)

names <- c("tvc_s01e01","tvc_s01e02","tvc_s01e03","tvc_s01e04","tvc_s01e05",
           "tvc_s01e06","tvc_s01e07","tvc_s01e08","tvc_s01e09","tvc_s01e10",
           "tvc_s02e01","tvc_s02e02","tvc_s02e03","tvc_s02e04","tvc_s02e05",
           "tvc_s02e06","tvc_s02e07","tvc_s02e08","tvc_s02e09","tvc_s02e10",
           "tvc_s03e01","tvc_s03e02","tvc_s03e03","tvc_s03e04","tvc_s03e05",
           "tvc_s03e06","tvc_s03e07","tvc_s03e08","tvc_s03e09","tvc_s03e10",
           "tvc_s04e01","tvc_s04e02","tvc_s04e03","tvc_s04e04","tvc_s04e05",
           "tvc_s04e06","tvc_s04e07","tvc_s04e08","tvc_s04e09","tvc_s04e10")

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
   
   everything[[i]] <- data.frame(nick = users, date = dates, 
                                 comment = comments)

   main_path <- "C:/Dane/Pawel_2/PW/Data_Science"
   write.table(everything[[i]], file = stri_paste(main_path, "/", names[i], ".txt"))
}   
everything


