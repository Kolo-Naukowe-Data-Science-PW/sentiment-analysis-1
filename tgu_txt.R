# saving data.frame for each episode to .txt files
# page with links to each episodes:
# http://www.theguardian.com/tv-and-radio/series/game-of-thrones-episode-by-episode


library(rvest)
library(stringi)
library(tm)

names<-c("TGU_s01e01","TGU_s01e02","TGU_s01e03","TGU_s01e04","TGU_s01e05",
  "TGU_s01e06","TGU_s01e07","TGU_s01e08","TGU_s01e09","TGU_s01e10","TGU_s02e00",
  "TGU_s02e01","TGU_s02e02","TGU_s02e03","TGU_s02e04","TGU_s02e05",
  "TGU_s02e06","TGU_s02e07","TGU_s02e08","TGU_s02e09","TGU_s02e10",
  "TGU_s03e01","TGU_s03e02","TGU_s03e03","TGU_s03e04","TGU_s03e05",
  "TGU_s03e06","TGU_s03e07","TGU_s03e08","TGU_s03e09","TGU_s03e10",
  "TGU_s04e01","TGU_s04e02","TGU_s04e03","TGU_s04e04","TGU_s04e05",
  "TGU_s04e06","TGU_s04e07","TGU_s04e08","TGU_s04e09","TGU_s04e10")
names<-rev(names)

#url for episodes
url_episodes1<-"http://www.theguardian.com/tv-and-radio/series/game-of-thrones-episode-by-episode"
url_episodes2<-"http://www.theguardian.com/tv-and-radio/series/game-of-thrones-episode-by-episode?page=2"
html1 <- html(url_episodes1)
epsiode1 <- html_nodes(html1,".u-faux-block-link__overlay")%>% html_attr("href")
html2 <- html(url_episodes2)
epsiode2 <- html_nodes(html2,".u-faux-block-link__overlay")%>% html_attr("href")
epsiode<-c(epsiode1,epsiode2)
n<-length(epsiode)

path<-"C:/Users/Miko³aj/Documents/GitHub/sentiment-analysis-1/data/tgu-txt-files"
path2<-stri_paste(path,"/",names,".txt")
i<-1
for (i in 1:n){
   write.table(TGUepisodeDataFrame(epsiode[i]),path2[i])
}

