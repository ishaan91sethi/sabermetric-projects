# Webscraping practice
library(rvest)
library(RSelenium)

rD <- rsDriver(browser = "firefox", port = 4401L, verbose = F)
remDr <- rD[["client"]]

remDr$navigate("https://www.wunderground.com/history/daily/KPHL/date/2016-5-28")
html <- remDr$getPageSource()[[1]]
webpage <- read_html(html) # parse HTML

# Change names

rank_data_html <- html_nodes(webpage, css = ".cdk-cell.mat-table-sticky")


rank_data_html <- html_nodes(webpage, css = ".mat-column-temperature .wu-value-to")
rank_data <- html_text(rank_data_html)
rank_data

remDr$close()
rD$server$stop()
rm(rD)
rm(remDr)
gc()
# Kills java instances inside RStudio
system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)










#Specifying the url for desired website to be scraped
url <- 'http://www.imdb.com/search/title?count=100&release_date=2016,2016&title_type=feature'

#Reading the HTML code from the website
webpage <- read_html(url)

#Using CSS selectors to scrape the rankings section
rank_data_html <- html_nodes(webpage,'.text-primary')

#Converting the ranking data to text
rank_data <- html_text(rank_data_html)

#Let's have a look at the rankings
head(rank_data)

#Data-Preprocessing: Converting rankings to numerical
rank_data<-as.numeric(rank_data)

#Let's have another look at the rankings
head(rank_data)

#Using CSS selectors to scrape the title section
title_data_html <- html_nodes(webpage,'.lister-item-header a')

#Converting the title data to text
title_data <- html_text(title_data_html)

#Let's have a look at the title
head(title_data)



