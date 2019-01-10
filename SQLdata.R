

library(DBI)
library(odbc)
library(tidyr)
library(tibble)
library(glue)
library(keyring)
library(googledrive)

conn <- dbConnect(odbc::odbc(), 
                 Driver="SQL Server",
                 Server="localhost\\SQLEXPRESS",
                 Database="master",
                 UID="",
                 PWD="",
                 DBMSencoding = "UTF-8",
                 Port=1433)

refreshSQLDatabase <- function(conn){
  vaaliData <- read.csv("CleanData.csv")
  
  # convertToUTF16 <- function(s){
  #   lapply(s, function(x) unlist(iconv(x,from="UTF-8",to="UTF-16LE",toRaw=TRUE)))
  # }
  # 
  # vaaliData <- vaaliData %>% mutate(
  #   NAME=convertToUTF16(NAME),
  #   PROVINCE = convertToUTF16(PROVINCE))
  
  if (dbExistsTable(conn, "Vaalidata"))
    dbRemoveTable(conn, "Vaalidata")
  
  dbWriteTable(conn, 
               name = "Vaalidata", value = vaaliData, row.names = FALSE,
               field.types = c(NAME = "NVARCHAR(MAX)", 
                               PROVINCE = "NVARCHAR(MAX)"))
  print("Database refreshed")
}


vaaliData <- dbReadTable(conn, name = "Vaalidata")

sql <- "SELECT * FROm WHERE "
#results <- dbGetQuery(con, sql)


#dbDisconnect(conn)