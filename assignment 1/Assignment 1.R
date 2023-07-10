## Asignment 1 ##

install.packages("rvest")
library(rvest)

## Part <a> ##

html<-read_html("https://www.moneyworks4me.com/best-index/nse-stocks/top-nifty50-companies-list/")
table<- html %>% html_table(fill=TRUE)
table<-table[[1]]
table <- table[,-13]
table <- table[,-14]
table <- table[,-1]



## Part <b> ##

html<-read_html("https://www.moneyworks4me.com/best-index/nse-stocks/top-nifty50-companies-list/")
url<- html %>% html_elements(".company-ellipses a") %>% html_attr("href")
for(i in 12:16)
  url[i] <- paste("https://www.moneyworks4me.com/", url[i], sep = '')

html=read_html(url[12]) ## Change value of 12 to 13, 14, 15 and 16 to get the other 4 tables
table<- html %>% html_table()
table1<-table[[1]]
table1 <- table1[-c(1,2,3,4,5),]
table1 <- table1[,-c(12,13,14)]
colnames(table1) <- c("","Mar'13","Mar'14","Mar'15","Mar'16","Mar'17","Mar'18","Mar'19","Mar'20","Mar'21","Mar'22")
table2 <- table[[3]]
table2 <- table2[-1,]
table2 <- table2[,-c(12,13)]
colnames(table2) <- c("","Mar'13","Mar'14","Mar'15","Mar'16","Mar'17","Mar'18","Mar'19","Mar'20","Mar'21","Mar'22")
table1<-rbind(table1,table2)



## Part <c> ##

tennis<-function(p)
{
  a<-0
  b<-0
  for(i in 1:5)
  {
    if(rbinom(n=1,size=1,prob=p)==1)
      a=a+1
    else
      b=b+1
    if(a==3|b==3)
      return(i)
  }
  return(5)
}
matches <- numeric(length = 1000)
for(i in 1:1000)
  matches[i]=tennis(0.70)
ans <- mean(matches)



## Part <d> ##

MontyHall <- function() {
  prize <- sample(1:3,1)
  choice <- sample(1:3,1)
  if(prize!=choice)
    return(1)
  else
    return(0)
}
MontyHall()
new <- numeric(length = 1000)
for(i in 1:1000)
  new[i]=MontyHall()
prob_win_if_switch=sum(new==1)/1000



## Part <e> ##

html<-read_html("https://editorial.rottentomatoes.com/guide/best-netflix-movies-to-watch-right-now/")
ranking<-html %>% html_elements(".countdown-index") %>% html_text()
name<-html %>% html_elements(".article_movie_title a") %>% html_text()
tomato_score<-html %>% html_elements(".tMeterScore") %>% html_text()
year<-html %>% html_elements(".start-year") %>% html_text() %>% substring(2,5)
M=data.frame("Name"=name,"Year"=year,"Ranking"=ranking,"Tomato_score"=tomato_score)
