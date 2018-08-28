devtools::install_github("johndharrison/binman")
devtools::install_github("johndharrison/wdman")
devtools::install_github("ropensci/RSelenium")

library(jsonlite)
library(dplyr)
library(httr)
library(RSelenium)
library(rvest)

ch=wdman::chrome(port=4567L) 
remDr=remoteDriver(port=4567L, browserName='chrome')
remDr$open()


raw <- NULL

key<-" "
for(i in 1:length(a$director)){
 url<-paste0("http://www.kobis.or.kr/kobisopenapi/webservice/rest/people/searchPeopleList.json?key=",key,"&peopleNm=",a$director[i])
 remDr$navigate(url)
 url <- remDr$getCurrentUrl()[[1]] 
 tmp<-fromJSON(url)
 tmp <- tmp$`peopleListResult`$peopleList$filmoNames
 tmp <- tmp[grep('\\|',tmp)]
 val <- strsplit(tmp,split="\\|")[[1]][1:10]
 movie <- NULL
  for(j in 1:10){
   movie <- cbind(movie,val[j])
  }
 raw <- rbind(raw,movie)
}


total <- NULL

for(i in 1:nrow(raw)){
value <- NULL
 for(j in 1:10){
  url2 <- paste0("https://search.naver.com/search.naver?where=nexearch&sm=top_hty&fbm=0&ie=utf8&query=",raw[i,j])
  remDr$navigate(url2)
  url2 <- remDr$getCurrentUrl()[[1]] 
  number <- read_html(url2)
  numberinfos <- html_nodes(number,css='.desc_detail')
  val2 <- numberinfos%>%html_nodes('span')%>%html_text()
  val2 <- val2[grep('Έν',val2)]
  val2 <- val2[grep('[0-9]',val2)]
  val2 <- gsub('Έν','',val2)
  val2 <- gsub(',','',val2)
  val2 <- as.numeric(val2)
  if(!identical(val2,numeric(0))) {value <- c(value,val2)} else {value <- c(value,NA)}
 }
total <- rbind(total,value)
}

