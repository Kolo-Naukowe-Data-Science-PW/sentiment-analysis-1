library(stringi)

sentyment <- function(teksty,slownik){
   teksty <- stri_trans_tolower(teksty)
   sapply(teksty,function(x) sum(stri_count_fixed(x,slownik[[1]])*slownik[[2]]),USE.NAMES = FALSE)
}

slownikAFINN <- read.csv2("C:\\Dane\\Pawel\\PW\\GitHub\\sentiment-analysis-2\\AFINN - slownik\\AFINN-111.txt", 
                          sep="\t", header = FALSE)
load("C:\\Dane\\Pawel\\PW\\Data_Science\\komentarze.rda")

podzial <- split(komentarze$comment, komentarze$episode)

n <- length(podzial)
wydzwiek <- numeric(40)
for(i in 1:n){
   wydzwiek[i] <- mean(sentyment(podzial[[i]], slownikAFINN))
}

