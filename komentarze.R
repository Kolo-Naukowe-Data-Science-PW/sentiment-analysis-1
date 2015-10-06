library(stringi)

setwd("C:\\Dane\\Pawel\\PW\\Data_Science\\Prezentacja_3\\aso_pliki")

lista <- list.files("C:\\Dane\\Pawel\\PW\\Data_Science\\Prezentacja_3\\aso_pliki")

komentarze <- data.frame()
for (i in 1:40){
   d <- read.table(lista[i], sep=",", header = TRUE)
   s <- stri_extract_first_regex(lista[i],"[0-9]{2}")
   e <- stri_extract_last_regex(lista[i],"[0-9]{2,3}")
   episode <- 10*as.numeric(s) + as.numeric(e) - 10
   d <- cbind(d,episode) 
   komentarze <- rbind(komentarze, d)
}

setwd("C:\\Dane\\Pawel\\PW\\Data_Science")
save(komentarze, file="komentarze.rda")

