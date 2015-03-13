library(RSQLite)

#set your working directory to "sentiment-analysis-1/data/aso-txt_files"
setwd(...)

#url for seasons 
urlSeason1 <- "http://asoiaf.westeros.org/index.php/forum/40-season-1/"
urlSeason2 <- "http://asoiaf.westeros.org/index.php/forum/59-season-2/"
urlSeason3 <- "http://asoiaf.westeros.org/index.php/forum/74-season-3/"
urlSeason4 <- "http://asoiaf.westeros.org/index.php/forum/86-season-4/"

#for every season we create list of data frames - one data frame for one episode
#function makeSeaonDataFrameList is in aso-webscraping.R file
listSeason1 <- makeSeasonDataFrameList(urlSeason1)
listSeason2 <- makeSeasonDataFrameList(urlSeason2)
listSeason3 <- makeSeasonDataFrameList(urlSeason3)
listSeason4 <- makeSeasonDataFrameList(urlSeason4)


#saving data frames to ".txt" files
saveToFile(listSeason1, 1)
saveToFile(listSeason2, 2)
saveToFile(listSeason3, 3)
saveToFile(listSeason4, 4)


## !!! this function is not finish! i have some problems to make it work
saveInDataBase <- function(dataFrameList, numOfSeason){
   con <- dbConnect(SQLite(), "database.db")
   names <- makeNames(dataFrameList, numOfSeason)
   n <- length(dataFrameList)
   for (i in 1:n){
      dbWriteTable(con, names[i], dataFrameList[[i]])
   }
   dbDisconnect(con)
}
dbListTables(con)


#function for saving dataframes to ".txt" file with appropriate name
saveToFile <- function(dataFrameList, numOfSeason){
   names <- paste(makeNames(dataFrameList, numOfSeason), ".txt", sep="")
   n <- length(dataFrameList)
   for (i in 1:n){
      write.table(dataFrameList[[i]], names[i], quote=FALSE, sep="\t")  
   }
}

#function for creating names (depending on number od season and episode)
makeNames <- function(dataFrameList, numOfSeason){
   n <- length(dataFrameList)
   names <- character(n)
   for (i in 1:n){
      if (i <= 9){
         names[i] <- paste("aso-s0", numOfSeason, "e0", i, sep="")
      } else {
         names[i] <- paste("aso-s0", numOfSeason, "e", i, sep="")
      }
   }
   return(names)
}