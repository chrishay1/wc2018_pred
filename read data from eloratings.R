##PREDICTIVE MODELLING FOR THE 2018 WORLD CUP 
##read some data from the eloratings.net website
##rvest seems to be a package that may be of some use in reading the data
##install.packages("rvest")
library(rvest)
library(jsonlite)
library(tidyverse)



#determine the continent of each country
#this lists all the current countries, but might be missing old countries. We may have to manually fill.

assoc <- c("AFC","CAF","CONCACAF","CONMEBOL","OFC","UEFA")

url <- paste0("http://www.international-football.net/countries")
test_results <-read_html(url)
conf_x <- lapply(assoc,function(x){
    scrape <- paste0("#",x," div")
    conf_data1 <- html_nodes(test_results,scrape)
    conf_data2 <- html_text(conf_data1)
    
    conf_data  <- unique(conf_data2[conf_data2!=""])
    conf_data  <- as.data.frame(conf_data)
    conf_data$assoc <-x
    return(conf_data)
})

conferences <- bind_rows(conf_x)
colnames(conferences) <-c("country","conference")

##here is a simple function that will return a list of ranks for a certain point in time,
##we can then use this to determine team ranks
ranking_date <- function(year_rank,month_rank,day_rank){
url <- paste0("http://www.international-football.net/elo-ratings-table?year=",year_rank,"&month=",month_rank,"&day=",day_rank)
test_2014 <-read_html(url)
test_20142 <- html_nodes(test_2014,"#contents td")
test_20143 <- html_text(test_20142)
exclude_index <- c(0:300)*3+1
country_names_index <- c(0:300)*3+2
country_ranks_index <- c(0:300)*3+3

country_names_x <- (test_20143[country_names_index])
country_names <-as.data.frame(country_names_x[is.na(country_names_x)==FALSE])

country_ranks_x <- (test_20143[country_ranks_index])
country_ranks <-as.data.frame(as.numeric(country_ranks_x[is.na(country_ranks_x)==FALSE]))

country_name_ranks <- cbind(country_names,country_ranks)
colnames(country_name_ranks) <-c("country","rank")
country_name_ranks <- left_join(country_name_ranks,conferences)
country_name_ranks$year <- year_rank
return(country_name_ranks)
}
ranks_test <- ranking_date(1980,01,01)
##join on the ranking

#next step; get the world cup results

wc_results <-  function(year){
url <- paste0("http://www.international-football.net/tournament?compet-id=World%20Cup&year=",year)
results <-read_html(url)
results2 <- html_nodes(results,".survol .opensans")
results3 <- html_text(results2)

team_one_index <- c(0:300)*3+1
score_index <- c(0:300)*3+2
team_two_index <- c(0:300)*3+3

team_one <- as.data.frame(results3[team_one_index])
score_one <- as.data.frame(as.numeric(substr(results3[score_index],1,1)))
score_two <- as.data.frame(as.numeric(substr(results3[score_index],5,6)))
team_two <- as.data.frame(results3[team_two_index])
results <- cbind(team_one,team_two,score_one,score_two)
colnames(results) <- c("team_one","team_two","score_one","score_two")
results <- results[is.na(results$team_one)==FALSE,]
results$year <- year
results$team_one <- as.character(results$team_one)
results$team_two <- as.character(results$team_two)
return(results)
}

##problem; the results here don't differentiate between wins after extra time or regular wins (although 
##it does seem to list games that go to penalties as draws) may want to find a way to augment this. 

##ok, now we want to bring it all together, for a collection of years.

years <- c(1978,1982,1986,1990,1994,1998,2002,2006,2010,2014)
wc_host_conf <- c("CONMEBOL","UEFA","CONCACAF","UEFA","CONCACAF",
                  "UEFA","AFC","UEFA","CAF","CONMEBOL")
wc_hist <- as.data.frame(years)
wc_hist$conf <- wc_host_conf
wc_results2 <- list(0)
for (i in 1:nrow(wc_hist)){
year <- wc_hist[i,1]
wc_results_out <- wc_results(year) %>% select (-year)
wc_ranks_out <-ranking_date(year,06,01) %>% select (-year)

wc_results_with_rank <- left_join(wc_results_out,wc_ranks_out,by=c("team_one"="country")) %>%
                        rename(rank_team_one = rank,conf_team_one = conference) %>%
                        left_join(wc_ranks_out,by=c("team_two"="country")) %>%
                        rename(rank_team_two = rank,conf_team_two = conference)

wc_results_with_rank$year <- year

wc_results_with_rank$host_region <- wc_hist[i,2]
wc_results2[[i]] <- wc_results_with_rank
}
wc_results3 <- bind_rows(wc_results2)

##some countries wont have conferences; we'll need to fill them in manually
unique(wc_results3$team_one[is.na(wc_results3$conf_team_one)==TRUE])

##luckily, all the countries that don't exist in the current list are from europe, 
##so we can just be a bit lazy and call them UEFA 

wc_results3$conf_team_one[is.na(wc_results3$conf_team_one)==TRUE] <- "UEFA"
wc_results3$conf_team_two[is.na(wc_results3$conf_team_two)==TRUE] <- "UEFA"

##add some additional data items based on which team was playing in their host continent

wc_results3$home_cont_one <- ifelse(wc_results3$host_region == wc_results3$conf_team_one,1,0)

wc_results3$home_cont_two <- ifelse(wc_results3$host_region == wc_results3$conf_team_two,1,0)
##need this to be a factor
wc_results3$home_cont_one <- as.factor(wc_results3$home_cont_one)
wc_results3$home_cont_two <- as.factor(wc_results3$home_cont_two)

#the data extraction assumes the score is one-digit long; there is a game that messes this up
#(hungary v el salvador 1982) so we'll fill this one in manually

wc_results3$score_one[is.na(wc_results3$score_two)==TRUE] = 10
wc_results3$score_two[is.na(wc_results3$score_two)==TRUE] = 1

#reference;
#https://en.wikipedia.org/wiki/Hungary_v_El_Salvador_(1982_FIFA_World_Cup)

#add some flags to show the result
wc_results3$team_one_win <- ifelse(wc_results3$score_one >  wc_results3$score_two,1,0)
wc_results3$team_two_win <- ifelse(wc_results3$score_two >  wc_results3$score_one,1,0)
wc_results3$draw <- ifelse(wc_results3$score_two ==  wc_results3$score_one,1,0)

wc_results3$team_one_win <- as.factor(wc_results3$team_one_win)
wc_results3$team_two_win <- as.factor(wc_results3$team_two_win)
wc_results3$draw <- as.factor(wc_results3$draw)
##save down the file (in case we lose it!)
write.csv(wc_results3,"C:/Users/pc/Documents/Data science/World cup 2018/wc_results.csv")
wc_results_x <- wc_results3[,c("team_two","team_one","score_two","score_one","rank_team_two",
                               "conf_team_two","rank_team_one","conf_team_one","year",
                               "host_region","home_cont_two","home_cont_one","team_two_win",
                               "team_one_win","draw")]

colnames(wc_results_x) <- colnames(wc_results3)
wc_results4 <- rbind(wc_results3,wc_results_x)
wc_results4$rating_diff_amt <- wc_results4$rank_team_one - wc_results4$rank_team_two

wc_results4$rating_diff_percent <- wc_results4$rank_team_one / wc_results4$rank_team_two
summary(wc_results4$rating_diff_percent)

wc_results4$result <- ""
for( i in 1:nrow(wc_results4)){
    wc_results4$result[i] <-if(wc_results4$score_one[i] >wc_results4$score_two[i]) {("win")}
    else if (wc_results4$score_one[i] ==wc_results4$score_two[i]){("draw")}
    else {("loss")}
    
}


                                   
    