#Import libraries

library(httr)
library(rvest)

#Get tables from web pages

get_dataframe <- function(url, table) {
    table <- read_html(url) %>% html_table %>% .[[table]]
    return (table)
}

top20 <- get_dataframe("https://www.tiobe.com/tiobe-index/", 1)
others <- get_dataframe("https://www.tiobe.com/tiobe-index/", 2)

#Print info about the dataframes

cat("#####  TOP 20  #####\n\n")
cat("-- SUMMARY:\n")
print(summary(top20))
cat("\n-- COLUMN NAMES:\n")
print(colnames(top20))
cat("\n-- FIRST ROWS:\n")
print(head(top20))

cat("\n\n#####  TOP 21-50  #####\n\n")
cat("-- SUMMARY:\n")
print(summary(others))
cat("\n-- COLUMN NAMES:\n")
print(colnames(others))
cat("\n-- FIRST ROWS:\n")
print(head(others))

#Drop unwanted columns from the top20 dataframe

top20 = top20[, -c(3, 4)]

#Rename some of the columns of the top20 dataframe

colnames(top20)[1] <- "Position"
colnames(top20)[2] <- "Previous"

#Add missing columns in the others dataframe and set the missing values to NA

for (i in colnames(top20)){
    if (!(i %in% colnames(others))){
        others[,i] <- NA
    }
} 

#Create a top50 dataframe by joining the top20 dataframe (1-20) with others dataframe (21-50)

top50 <- rbind(top20, others)

#Remove % symbol from Ratings column and cast is to Numeric datatype

new_ratings <- c()
for (i in top50$Ratings){
    new_ratings <- append(new_ratings, gsub("%", "", i))
    } 
top50$Ratings <- as.numeric(as.character(new_ratings))

#Remove % symbol from Change column for the first 20 rows
#The other rows come from the others dataframe, which doesn't have a Change column, so the values in this column from row 21 to row 50 are all NA

new_changes <- c()
for (i in top50$Change[1:20]){
    new_changes <- append(new_changes, gsub("%", "", i))
    } 
top50$Change[1:20] <- as.numeric(as.character(new_changes))

#Reorder columns and change column name

top50 <- top50[, c(1,3,4,2,5)]
colnames(top50)[2] <- "Language"

#Print top 5 programming languages

cat("\nIn 2022, the top 5 programming languages according to TIOBE index are:", top50$Language[1:5], sep = "\n-- ")


#Sort dataframe by Change column

top50[1:20,] <- top50[order(top50$Change[1:20], decreasing = T),]

#Print languages in ascention, decline and with unknown change from 2021 to 2022

cat("\nLanguages in ascension: ", top50[1:20,][top50[1:20,]$Change > 0,]$Language, sep = "\n-- ")
cat("\nLanguages in decline: ", top50[1:20,][top50[1:20,]$Change <= 0,]$Language, sep = "\n-- ")
cat("\nChange unknown: ", top50[21:50,]$Language, sep = "\n-- ")
