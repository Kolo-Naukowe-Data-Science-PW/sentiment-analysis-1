setwd("C:\\Users/Miko³aj/Documents/SMAD/kolo data science/sentiment analysis")
load("komentarze.rda" )


dane <- dane[sample(1:nrow(dane),10000,replace = FALSE),]
dane <- dane[order(dane$i),]
affin <- read.csv2("C:\\Users/Miko³aj/Documents/SMAD/kolo data science/sentiment analysis/slowniki/AFINN-111.txt",
                   sep="\t")

postacie <- c("Cersei",
              "Tyrion",
              "Daenerys",
              "Arya",
              "Jon",
              "Sansa",
              "Jaime",
              "Tywin",
              "Joffrey",
              "Robb",
              "Ned",
              "Stannis",
              "Margaery")

rody <- c("stark","Lannister","Targaryen")

library(stringi)
library("RSQLite")
con <- dbConnect(SQLite(),"F:/Norbert/studia/KoloNaukoweDS/Projekty/Analiza sentymentu/sentiment-analysis-2/baza_seriali.sql")
dbListTables(con)
head(dbReadTable(con,"Serials"))
komentarze_wszystkie <- dbGetQuery(con,"select Comment from Serials")[[1]]


##############################

library(stringi)


#funkcja sentyment zwraca wektor sentymentow dla wektora tekstow
sentyment <- function(teksty,slownik){
   teksty <- stri_trans_tolower(teksty)
   sapply(teksty,function(x) sum(stri_count_fixed(x,slownik[[1]])*slownik[[2]]),USE.NAMES = FALSE)
}
sentiment <- sentyment(komentarze[,3],affin)


#sentyment_komentarzy <- sentyment(dane[,3],affin)
# 
# 
# library(ggplot2)
# 
# ggplot()
plot(
  sentiment*stri_detect_fixed(komentarze[,3],"tyrion")
)

komentarze <- cbind(komentarze, sentiment)
library(data.table)
dim(komentarze)
save(komentarze,file="kokomentarze.rda")

plot(
   komentarze[ ,4]*stri_detect_fixed(komentarze[,3]," tyrion ")
)
head(komentarze)

library(ggplot2)
komentarze <- cbind(komentarze, i=1:nrow(komentarze))
komentarze <- komentarze[order(komentarze$episode), ]



rody <- c("stark","lannister","Targaryen", "baratheon")


d <- komentarze[stri_detect_fixed(komentarze[, 3], " lannister "), ]
dim(d)
dt <- data.table(d)
d2 <- dt[, sum(sentiment<=0), by=episode]

library(data.table)

#########
# liczba pozytywnych komentarzy dla rodow 

d <- data.table(komentarze)
d2 <- d[stri_detect_fixed(comment, " lannister "), sum(sentiment>0), by=episode ]
d2 <- cbind(d2, Rod="Lannister")
d3 <- d[stri_detect_fixed(comment, " stark "), sum(sentiment>0), by=episode ]
d3 <- cbind(d3, Rod="Stark")
d4 <- d[stri_detect_fixed(comment, " targaryen "), sum(sentiment>0), by=episode ]
d4 <- cbind(d4, Rod="Targaryen")
d4

dd <- rbind(d2,d3,d4)
ggplot(dd, aes(x=episode, y=V1, col=Rod)) + geom_line() +
   ggtitle("Liczba pozytywnych komentarzy") +
   xlab("Numer odcineka") +
   ylab("")
names(d2)

ggsave("rody.png")

#  frakcja pozytywnych komentarzy

d <- data.table(komentarze)
d2 <- d[stri_detect_fixed(comment, " lannister "), sum(sentiment>0)/.N, by=episode ]
d2 <- cbind(d2, Rod="Lannister")
d3 <- d[stri_detect_fixed(comment, " stark "), sum(sentiment>0)/.N, by=episode ]
d3 <- cbind(d3, Rod="Stark")
d4 <- d[stri_detect_fixed(comment, " targaryen "), sum(sentiment>0)/.N, by=episode ]
d4 <- cbind(d4, Rod="Targaryen")
d4

dd <- rbind(d2,d3,d4)
g2<-ggplot(dd, aes(x=episode, y=V1, col=Rod)) + geom_line() +
   ggtitle("Frakcja pozytywnych komentarzy") +
   xlab("Numer odcinka") +
   ylab("")
ggsave("frakcja.png", g2)


# frakcja na sezon


d <- data.table(komentarze)
d[, season:= ceiling(episode/10)]

d2 <- d[stri_detect_fixed(comment, " lannister "), sum(sentiment>0)/.N, by=season ]
d2 <- cbind(d2, Rod="Lannister")
d3 <- d[stri_detect_fixed(comment, " stark "), sum(sentiment>0)/.N, by=season ]
d3 <- cbind(d3, Rod="Stark")
d4 <- d[stri_detect_fixed(comment, " targaryen "), sum(sentiment>0)/.N, by=season ]
d4 <- cbind(d4, Rod="Targaryen")
d4

dd <- rbind(d2,d3,d4)
ggplot(dd, aes(x=season, y=V1, col=Rod)) + geom_line() +
   ggtitle("Frakcja pozytywnych komentarzy w sezonie") +
   xlab("Numer sezonu") + 
   ylab("")

ggsave("frakcja_sezon.png")








#########
# srednia komentarzy dla rodów
# nic ciekawego nie widaæ 

d <- data.table(komentarze)
d2 <- d[stri_detect_fixed(comment, " lannister "), mean(sentiment), by=episode ]
d2 <- cbind(d2, Rod="Lannister")
d3 <- d[stri_detect_fixed(comment, " stark "), mean(sentiment), by=episode ]
d3 <- cbind(d3, Rod="Stark")
d4 <- d[stri_detect_fixed(comment, " targaryen "), mean(sentiment), by=episode ]
d4 <- cbind(d4, Rod="Targaryen")
d4

dd <- rbind(d2,d3,d4)
ggplot(dd, aes(x=episode, y=V1, col=Rod)) + geom_line() +
   ylab("liczba pozytywnych komentarzy") +
   xlab("numer odcinka")
names(d2)



# liczba pozytywnych komentarzy dla wybranych aktoróW 

d <- data.table(komentarze)
d2 <- d[stri_detect_fixed(comment, " tyrion"), sum(sentiment>0), by=episode ]
d2 <- cbind(d2, Postac="Tyrion")
d3 <- d[stri_detect_fixed(comment, " robb "), sum(sentiment>0), by=episode ]
d3 <- cbind(d3, Postac="Robb")
d4 <- d[stri_detect_fixed(comment, " jon "), sum(sentiment>0), by=episode ]
d4 <- cbind(d4, Postac="Jon")


dd <- rbind(d2,d3,d4)
ggplot(dd, aes(x=episode, y=V1, col=Postac)) + geom_line() +
   ggtitle("Liczba pozytywnych komentarzy") +
   xlab("Numer odcinka")+
   ylab("")

ggsave("postacie.png")


# frakcja pozytywnych komentarzy dla postaci
d <- data.table(komentarze)
d2 <- d[stri_detect_fixed(comment, " tyrion"), sum(sentiment>0)/.N, by=episode ]
d2 <- cbind(d2, Postac="Tyrion")
d3 <- d[stri_detect_fixed(comment, " robb "), sum(sentiment>0)/.N, by=episode ]
d3 <- cbind(d3, Postac="Robb")
d4 <- d[stri_detect_fixed(comment, " jon "), sum(sentiment>0)/.N, by=episode ]
d4 <- cbind(d4, Postac="Jon")


dd <- rbind(d2,d3,d4)
ggplot(dd, aes(x=episode, y=V1, col=Postac)) + geom_line() +
   ggtitle("Frakcja pozytywnych komentarzy") +
   xlab("Numer odcinka")+
   ylab("")

ggsave("frakcja_postacie.png")


# frakcja pozytywnych komentarzy dla postaci w sezonie
d <- data.table(komentarze)
d[, season:= ceiling(episode/10)]
d2 <- d[stri_detect_fixed(comment, " tyrion"), sum(sentiment>0)/.N, by=season ]
d2 <- cbind(d2, Postac="Tyrion")
d3 <- d[stri_detect_fixed(comment, " robb "), sum(sentiment>0)/.N, by=season ]
d3 <- cbind(d3, Postac="Robb")
d4 <- d[stri_detect_fixed(comment, " jon "), sum(sentiment>0)/.N, by=season ]
d4 <- cbind(d4, Postac="Jon")
d5 <- d[stri_detect_fixed(comment, " cersei "), sum(sentiment>0)/.N, by=season ]
d5 <- cbind(d5, Postac="Cersei")


dd <- rbind(d2,d3,d4, d5)
ggplot(dd, aes(x=season, y=V1, col=Postac)) + geom_line() +
   ggtitle("Frakcja pozytywnych komentarzy w sezonie") +
   xlab("Numer odcinka")+
   ylab("")

ggsave("frakcja_postacie_sezon.png")
