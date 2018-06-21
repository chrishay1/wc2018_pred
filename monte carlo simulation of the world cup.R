##ok, so we've got a model that gives reasonable predictions of the outcome of games.
##we can then use that to simulate the results of the tournament. 
##Firstly, we need to get a list of th teams playing & the fixtures.
##Source; https://en.wikipedia.org/wiki/2018_FIFA_World_Cup

library(rvest)
library(tidyverse)

logit2prob <- function(x){exp(x)/(1+exp(x))}
groups <- list(0)

groups[[1]] <- c("Russia","Saudi Arabia","Egypt","Uruguay")
groups[[2]] <- c("Portugal","Spain","Morocco","Iran")
groups[[3]] <- c("France","Australia","Peru","Denmark")
groups[[4]] <- c("Argentina","Iceland","Croatia","Nigeria")
groups[[5]] <- c("Brazil","Switzerland","Costa Rica","Serbia")
groups[[6]] <- c("Germany","Mexico","Sweden","South Korea")
groups[[7]] <- c("Belgium","Panama","Tunisia","England")
groups[[8]] <- c("Poland","Senegal","Colombia","Japan")
##reload our function to read the ELO ratings at a certain point in time

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

ranks <- ranking_date(2018,05,20)
ranks <- dplyr::select(ranks,-year)
ranks$home_cont_flag <- ifelse(ranks$conference=="UEFA",1,0)
groups_with_rank <- lapply(groups,function(x){
    x1 <- as.data.frame(x)
    colnames(x1) <- c("country")
    x2 <- inner_join(x1,ranks)
    return(x2)})

##ok, now I want to generate for each group a set of games, and then
##we can apply the model to these games

group_games <- lapply(groups_with_rank,function(x){
        games_temp <- as.data.frame((matrix(nrow=6,ncol=6)))
        for (i in (1:4)){
        games_temp[,i]   <- c(x[1,i],x[1,i],x[1,i],x[2,i],x[2,i],x[3,i])
        games_temp[,i+4] <- c(x[2,i],x[3,i],x[4,i],x[3,i],x[4,i],x[4,i])
        }
        colnames(games_temp) <- c("team_one","rank_team_one","conf_team_one","home_cont_one",
                                  "team_two","rank_team_two","conf_team_two","home_cont_two")
        games_temp$rating_diff_percent_one <- games_temp$rank_team_one / games_temp$rank_team_two
        games_temp$rating_diff_percent_two <- games_temp$rank_team_two / games_temp$rank_team_one
        
        return(games_temp)
        }
        )

##some further manipulation and then apply the model
group_games_with_pred <- lapply(group_games,function(x){
    team_one_pred_temp <- select(x,conf_team_one,home_cont_one,home_cont_two,rating_diff_percent_one)
    colnames(team_one_pred_temp) <- c("conf_team_one","home_cont_one","home_cont_two","rating_diff_percent")
    team_one_pred_temp$home_cont_two <- as.factor(team_one_pred_temp$home_cont_two)
    team_one_pred_temp$home_cont_one <- as.factor(team_one_pred_temp$home_cont_one)
    team_one_win_pred <- predict(wc_glm_conf4b,newdata=team_one_pred_temp)
    
    team_two_pred_temp <- select(x,conf_team_two,home_cont_two,home_cont_one,rating_diff_percent_two)
    colnames(team_two_pred_temp) <- c("conf_team_one","home_cont_one","home_cont_two","rating_diff_percent")
    team_two_pred_temp$home_cont_two <- as.factor(team_two_pred_temp$home_cont_two)
    team_two_pred_temp$home_cont_one <- as.factor(team_two_pred_temp$home_cont_one)
    
    team_two_win_pred <- predict(wc_glm_conf4b,newdata=team_two_pred_temp)
    x$team_one_win_pred <- logit2prob(team_one_win_pred)
    x$team_two_win_pred <- logit2prob(team_two_win_pred)
    x$draw_pred <- 1-x$team_one_win_pred - x$team_two_win_pred 
    x$draw_pred[x$draw_pred<0]  <- 0
    ##add some return statistics
    x$team_one_win_return = 1/x$team_one_win_pred
    x$team_two_win_return = 1/x$team_two_win_pred
    x$draw_return = 1/x$draw_pred
    return(x)
})
##ok, so now we have a list of each group game, and the likelihood of each game taking place
##We can then use this to simulate the results.
games <- bind_rows(group_games_with_pred)

iter <- 2 ##number of iterations

wc_quarterfinals <- list(0)
group_winners <- list(0)
group_runners_up <- list(0)
for (i in c(1:iter)){
    set.seed(i)
    games$rngs <- runif(n=48)

    games$win_one <- 0
    games$draw <- 0
    games$win_two <- 0
    
    ##determine the result of each game
    for(j in c(1:48)){
    games$win_one[j] <- ifelse(games$rngs[j] < games$team_one_win_pred[j],1,0)
    games$draw[j] <- ifelse(games$rngs[j] > games$team_one_win_pred[j]& 
                             games$rngs[j] < (games$team_one_win_pred[j] + games$draw_pred[j]) ,1,0)
    games$win_two[j] <- ifelse(games$win_one[j] ==0 & games$draw[j]==0,1,0)
    }
    ##determine number of team_wins
    team_wins <- games %>% group_by(team_one) %>% summarise(win_ones=sum(win_one),draw_ones=sum(draw))
    colnames(team_wins)[1] <- c("country")
    team_wins2 <- games %>% group_by(team_two) %>% summarise(win_twos=sum(win_two),draw_twos=sum(draw))
    colnames(team_wins2)[1] <- c("country")
     countries <- as.data.frame(unique(c(as.character(team_wins$country),as.character(team_wins2$country))))
     colnames(countries) <- c("country")
    countries2 <- left_join(countries,team_wins) %>% left_join(team_wins2) 
    countries2[is.na(countries2)] <-0
    countries3 <- mutate(countries2,wins=win_ones+win_twos,draws=draw_ones+draw_twos,losses=3-wins-draws)
                                                         
    team_wins_out <- countries3[,c("country","wins","draws","losses")]
    ##create a new list with the rankings
    set.seed(i+1000) 
        groups_with_res <- lapply(groups_with_rank,function(x) {
        groups_temp <- left_join(x,team_wins_out)
        groups_temp$points <- 3*groups_temp$wins + groups_temp$draws

        groups_temp$rand <- runif(4) 
        groups_temp2 <- arrange(groups_temp,desc(points),rand)
        return(groups_temp2)
    })
        r16_games <- list(0)
    #ok, now we have the group games. Lets set up the round of 16 games.
        r16_games_one <- c(1,3,5,7,2,4,6,8)
        r16_games_two <- c(2,4,6,8,1,3,5,7)
        r16_games_one_rank <- c(1,1,1,1,1,1,1,1)
        r16_games_two_rank <- c(2,2,2,2,2,2,2,2)
        

        for(j in (1:8)){
        t1 <- r16_games_one[j]
        t2 <- r16_games_two[j]
        r1 <- r16_games_one_rank[j]
        r2 <- r16_games_two_rank[j]
        t1_data <- groups_with_res[[ t1 ]][r1,c("country","rank","conference","home_cont_flag")]
        t2_data <- groups_with_res[[ t2 ]][r2,c("country","rank","conference","home_cont_flag")]
        r16_game_temp <- cbind(t1_data,t2_data)
        colnames(r16_game_temp) <- c("team_one","rank_team_one","conf_team_one","home_cont_one",
                                  "team_two","rank_team_two","conf_team_two","home_cont_two")
        r16_game_temp$rating_diff_percent_one <- r16_game_temp$rank_team_one / r16_game_temp$rank_team_two
        r16_game_temp$rating_diff_percent_two <- r16_game_temp$rank_team_two / r16_game_temp$rank_team_one
        
        r16_games[[j]] <- r16_game_temp
        }
     
##determine the result of each r16 game
        
        r16_games_with_pred <- lapply(r16_games,function(x){
            
            team_one_pred_temp <- select(x,conf_team_one,home_cont_one,home_cont_two,rating_diff_percent_one)
            colnames(team_one_pred_temp) <- c("conf_team_one","home_cont_one","home_cont_two","rating_diff_percent")
            team_one_pred_temp$home_cont_two <- as.factor(team_one_pred_temp$home_cont_two)
            team_one_pred_temp$home_cont_one <- as.factor(team_one_pred_temp$home_cont_one)
            
            team_one_win_pred <- predict(wc_glm_conf4b,newdata=team_one_pred_temp)
            
            team_two_pred_temp <- select(x,conf_team_two,home_cont_two,home_cont_one,rating_diff_percent_two)
            colnames(team_two_pred_temp) <- c("conf_team_one","home_cont_one","home_cont_two","rating_diff_percent")
            team_two_pred_temp$home_cont_two <- as.factor(team_two_pred_temp$home_cont_two)
            team_two_pred_temp$home_cont_one <- as.factor(team_two_pred_temp$home_cont_one)
                    
            team_two_win_pred <- predict(wc_glm_conf4b,newdata=team_two_pred_temp)
            x$team_one_win_pred <- logit2prob(team_one_win_pred)
            x$team_two_win_pred <- logit2prob(team_two_win_pred)
            x$draw_pred <- 1-x$team_one_win_pred - x$team_two_win_pred 
            x$draw_pred[x$draw_pred<0]  <- 0
            return(x)
        })
        r16_game_table <- bind_rows(r16_games_with_pred)
        set.seed(i+3000)
        r16_game_table$rngs <- runif(8)
        set.seed(i+30000)
        r16_game_table$rngs2 <- runif(8) ##this one will be used if any game ends in a draw 
        
        r16_game_table$win_one <- 0
        r16_game_table$draw <- 0
        r16_game_table$win_two <- 0
        r16_game_table$advance_team <- ""
        r16_game_table$advance_conf <- ""
        r16_game_table$advance_home_cont <- 0
        r16_game_table$advance_rank_team <- 0
        ##determine the result of each game
        for(j in c(1:8)){
            r16_game_table$win_one[j] <- ifelse(r16_game_table$rngs[j] < r16_game_table$team_one_win_pred[j],1,0)
            r16_game_table$draw[j] <- ifelse(r16_game_table$rngs[j] > r16_game_table$team_one_win_pred[j]& 
                                                 r16_game_table$rngs[j] < (r16_game_table$team_one_win_pred[j] + r16_game_table$draw_pred[j]),1,0)
            r16_game_table$win_two[j] <- ifelse(r16_game_table$win_one[j] ==0 & r16_game_table$draw[j]==0,1,0)
        
            if (r16_game_table$draw[j]==1 & r16_game_table$rngs2[j] < 0.5) {r16_game_table$win_one[j] <- 1}
            else if(r16_game_table$draw[j]==1 & r16_game_table$rngs2[j] > 0.5) {r16_game_table$win_two[j] <- 1}
            
            ##who advances?
            if (r16_game_table$win_one[j]==1 ){
                r16_game_table$advance_team[j] <- r16_game_table$team_one[j]
                r16_game_table$advance_conf[j] <- r16_game_table$conf_team_one[j]
                r16_game_table$advance_home_cont[j] <- r16_game_table$home_cont_one[j]
                r16_game_table$advance_rank_team[j] <- r16_game_table$rank_team_one[j]
            }
            else {
                r16_game_table$advance_team[j] <- r16_game_table$team_two[j]
                r16_game_table$advance_conf[j] <- r16_game_table$conf_team_two[j]
                r16_game_table$advance_home_cont[j] <- r16_game_table$home_cont_two[j]
                r16_game_table$advance_rank_team[j] <- r16_game_table$rank_team_two[j]
            }
         }
        ##ok, so now we need to do the same thing for the round of 8
        
        r8_games <- list(0)
        for(j in (1:4)){
            t1_data <-  r16_game_table[j*2-1,c("advance_team","advance_rank_team","advance_conf","advance_home_cont")]
            t2_data <-  r16_game_table[j*2,c("advance_team","advance_rank_team","advance_conf","advance_home_cont")]
            r8_game_temp <- cbind(t1_data,t2_data)
            colnames(r8_game_temp) <- c("team_one","rank_team_one","conf_team_one","home_cont_one",
                                         "team_two","rank_team_two","conf_team_two","home_cont_two")
            r8_game_temp$rating_diff_percent_one <- r8_game_temp$rank_team_one / r8_game_temp$rank_team_two
            r8_game_temp$rating_diff_percent_two <- r8_game_temp$rank_team_two / r8_game_temp$rank_team_one
            
            r8_games[[j]] <- r8_game_temp
        }
        
        r8_games_with_pred <- lapply(r8_games,function(x){
            
            team_one_pred_temp <- select(x,conf_team_one,home_cont_one,home_cont_two,rating_diff_percent_one)
            colnames(team_one_pred_temp) <- c("conf_team_one","home_cont_one","home_cont_two","rating_diff_percent")
            team_one_pred_temp$home_cont_two <- as.factor(team_one_pred_temp$home_cont_two)
            team_one_pred_temp$home_cont_one <- as.factor(team_one_pred_temp$home_cont_one)
            
            team_one_win_pred <- predict(wc_glm_conf4b,newdata=team_one_pred_temp)
            
            team_two_pred_temp <- select(x,conf_team_two,home_cont_two,home_cont_one,rating_diff_percent_two)
            colnames(team_two_pred_temp) <- c("conf_team_one","home_cont_one","home_cont_two","rating_diff_percent")
            team_two_pred_temp$home_cont_two <- as.factor(team_two_pred_temp$home_cont_two)
            team_two_pred_temp$home_cont_one <- as.factor(team_two_pred_temp$home_cont_one)
            
            team_two_win_pred <- predict(wc_glm_conf4b,newdata=team_two_pred_temp)
            x$team_one_win_pred <- logit2prob(team_one_win_pred)
            x$team_two_win_pred <- logit2prob(team_two_win_pred)
            x$draw_pred <- 1-x$team_one_win_pred - x$team_two_win_pred 
            x$draw_pred[x$draw_pred<0]  <- 0
            return(x)
        })
        

        r8_game_table <- bind_rows(r8_games_with_pred)
        set.seed(i+5000)
        r8_game_table$rngs <- runif(4)
        set.seed(i+50000)
        r8_game_table$rngs2 <- runif(4) ##this one will be used if any game ends in a draw 
        
        r8_game_table$win_one <- 0
        r8_game_table$draw <- 0
        r8_game_table$win_two <- 0
        r8_game_table$advance_team <- ""
        r8_game_table$advance_conf <- ""
        r8_game_table$advance_home_cont <- 0
        r8_game_table$advance_rank_team <- 0
        ##determine the result of each game
        for(j in c(1:4)){
            r8_game_table$win_one[j] <- ifelse(r8_game_table$rngs[j] < r8_game_table$team_one_win_pred[j],1,0)
            r8_game_table$draw[j] <- ifelse(r8_game_table$rngs[j] > r8_game_table$team_one_win_pred[j]& 
                                                 r8_game_table$rngs[j] < (r8_game_table$team_one_win_pred[j] + r8_game_table$draw_pred[j]),1,0)
            r8_game_table$win_two[j] <- ifelse(r8_game_table$win_one[j] ==0 & r8_game_table$draw[j]==0,1,0)
            
            if (r8_game_table$draw[j]==1 & r8_game_table$rngs2[j] < 0.5) {r8_game_table$win_one[j] <- 1}
            else if(r8_game_table$draw[j]==1 & r8_game_table$rngs2[j] > 0.5) {r8_game_table$win_two[j] <- 1}
            
            ##who advances?
            if (r8_game_table$win_one[j]==1 ){
                r8_game_table$advance_team[j] <- r8_game_table$team_one[j]
                r8_game_table$advance_conf[j] <- r8_game_table$conf_team_one[j]
                r8_game_table$advance_home_cont[j] <- r8_game_table$home_cont_one[j]
                r8_game_table$advance_rank_team[j] <- r8_game_table$rank_team_one[j]
            }
            else {
                r8_game_table$advance_team[j] <- r8_game_table$team_two[j]
                r8_game_table$advance_conf[j] <- r8_game_table$conf_team_two[j]
                r8_game_table$advance_home_cont[j] <- r8_game_table$home_cont_two[j]
                r8_game_table$advance_rank_team[j] <- r8_game_table$rank_team_two[j]
            }
        }
        ##ok, so now we need to do the same thing for the round of 4
        
        r4_games <- list(0)
        for(j in (1:2)){
            t1_data <-  r8_game_table[j*2-1,c("advance_team","advance_rank_team","advance_conf","advance_home_cont")]
            t2_data <-  r8_game_table[j*2,c("advance_team","advance_rank_team","advance_conf","advance_home_cont")]
            r4_game_temp <- cbind(t1_data,t2_data)
            colnames(r4_game_temp) <- c("team_one","rank_team_one","conf_team_one","home_cont_one",
                                        "team_two","rank_team_two","conf_team_two","home_cont_two")
            r4_game_temp$rating_diff_percent_one <- r4_game_temp$rank_team_one / r4_game_temp$rank_team_two
            r4_game_temp$rating_diff_percent_two <- r4_game_temp$rank_team_two / r4_game_temp$rank_team_one
            
            r4_games[[j]] <- r4_game_temp
        }
        
        r4_games_with_pred <- lapply(r4_games,function(x){
            
            team_one_pred_temp <- select(x,conf_team_one,home_cont_one,home_cont_two,rating_diff_percent_one)
            colnames(team_one_pred_temp) <- c("conf_team_one","home_cont_one","home_cont_two","rating_diff_percent")
            team_one_pred_temp$home_cont_two <- as.factor(team_one_pred_temp$home_cont_two)
            team_one_pred_temp$home_cont_one <- as.factor(team_one_pred_temp$home_cont_one)
            
            team_one_win_pred <- predict(wc_glm_conf4b,newdata=team_one_pred_temp)
            
            team_two_pred_temp <- select(x,conf_team_two,home_cont_two,home_cont_one,rating_diff_percent_two)
            colnames(team_two_pred_temp) <- c("conf_team_one","home_cont_one","home_cont_two","rating_diff_percent")
            team_two_pred_temp$home_cont_two <- as.factor(team_two_pred_temp$home_cont_two)
            team_two_pred_temp$home_cont_one <- as.factor(team_two_pred_temp$home_cont_one)
            
            team_two_win_pred <- predict(wc_glm_conf4b,newdata=team_two_pred_temp)
            
            x$team_one_win_pred <- logit2prob(team_one_win_pred)
            x$team_two_win_pred <- logit2prob(team_two_win_pred)
            x$draw_pred <- 1-x$team_one_win_pred - x$team_two_win_pred 
            x$draw_pred[x$draw_pred<0]  <- 0
            return(x)
        })
        
        
        r4_game_table <- bind_rows(r4_games_with_pred)
        set.seed(i+6000)
        r4_game_table$rngs <- runif(2)
        set.seed(i+60000)
        r4_game_table$rngs2 <- runif(2) ##this one will be used if any game ends in a draw 
        
        r4_game_table$win_one <- 0
        r4_game_table$draw <- 0
        r4_game_table$win_two <- 0
        r4_game_table$advance_team <- ""
        r4_game_table$advance_conf <- ""
        r4_game_table$advance_home_cont <- 0
        r4_game_table$advance_rank_team <- 0
        ##determine the result of each game
        for(j in c(1:2)){
            r4_game_table$win_one[j] <- ifelse(r4_game_table$rngs[j] < r4_game_table$team_one_win_pred[j],1,0)
            r4_game_table$draw[j] <- ifelse(r4_game_table$rngs[j] > r4_game_table$team_one_win_pred[j]& 
                                                r4_game_table$rngs[j] < (r4_game_table$team_one_win_pred[j] + r4_game_table$draw_pred[j]),1,0)
            r4_game_table$win_two[j] <- ifelse(r4_game_table$win_one[j] ==0 & r4_game_table$draw[j]==0,1,0)
            
            if (r4_game_table$draw[j]==1 & r4_game_table$rngs2[j] < 0.5) {r4_game_table$win_one[j] <- 1}
            else if(r4_game_table$draw[j]==1 & r4_game_table$rngs2[j] > 0.5) {r4_game_table$win_two[j] <- 1}
            
            ##who advances?
            if (r4_game_table$win_one[j]==1 ){
                r4_game_table$advance_team[j] <- r4_game_table$team_one[j]
                r4_game_table$advance_conf[j] <- r4_game_table$conf_team_one[j]
                r4_game_table$advance_home_cont[j] <- r4_game_table$home_cont_one[j]
                r4_game_table$advance_rank_team[j] <- r4_game_table$rank_team_one[j]
            }
            else {
                r4_game_table$advance_team[j] <- r4_game_table$team_two[j]
                r4_game_table$advance_conf[j] <- r4_game_table$conf_team_two[j]
                r4_game_table$advance_home_cont[j] <- r4_game_table$home_cont_two[j]
                r4_game_table$advance_rank_team[j] <- r4_game_table$rank_team_two[j]
            }
        }  
        
        
        ##ok, now for the final
t1_data <-  r4_game_table[1,c("advance_team","advance_rank_team","advance_conf","advance_home_cont")]
t2_data <-  r4_game_table[2,c("advance_team","advance_rank_team","advance_conf","advance_home_cont")]
r2_game_table <- cbind(t1_data,t2_data)
colnames(r2_game_table) <- c("team_one","rank_team_one","conf_team_one","home_cont_one",
                            "team_two","rank_team_two","conf_team_two","home_cont_two")
r2_game_table$rating_diff_percent_one <- r2_game_table$rank_team_one / r2_game_table$rank_team_two
r2_game_table$rating_diff_percent_two <- r2_game_table$rank_team_two / r2_game_table$rank_team_one
        
team_one_pred_temp <- select(r2_game_table,conf_team_one,home_cont_one,home_cont_two,rating_diff_percent_one)
colnames(team_one_pred_temp) <- c("conf_team_one","home_cont_one","home_cont_two","rating_diff_percent")
team_one_pred_temp$home_cont_two <- as.factor(team_one_pred_temp$home_cont_two)
team_one_pred_temp$home_cont_one <- as.factor(team_one_pred_temp$home_cont_one)

team_one_win_pred <- predict(wc_glm_conf4b,newdata=team_one_pred_temp)
        
team_two_pred_temp <- select(r2_game_table,conf_team_two,home_cont_two,home_cont_one,rating_diff_percent_two)
colnames(team_two_pred_temp) <- c("conf_team_one","home_cont_one","home_cont_two","rating_diff_percent")
team_two_pred_temp$home_cont_two <- as.factor(team_two_pred_temp$home_cont_two)
team_two_pred_temp$home_cont_one <- as.factor(team_two_pred_temp$home_cont_one)
team_two_win_pred <- predict(wc_glm_conf4b,newdata=team_two_pred_temp)

r2_game_table$team_one_win_pred <- logit2prob(team_one_win_pred)
r2_game_table$team_two_win_pred <- logit2prob(team_two_win_pred)
r2_game_table$draw_pred <- 1-r2_game_table$team_one_win_pred - r2_game_table$team_two_win_pred 
r2_game_table$draw_pred[r2_game_table$draw_pred<0]  <- 0

set.seed(i+7000)
r2_game_table$rngs <- runif(1)
set.seed(i+70000)
r2_game_table$rngs2 <- runif(1) ##this one will be used if any game ends in a draw 


r2_game_table$win_one <- 0
r2_game_table$draw <- 0
r2_game_table$win_two <- 0
r2_game_table$advance_team <- ""

r2_game_table$win_one <- ifelse(r2_game_table$rngs < r2_game_table$team_one_win_pred,1,0)
r2_game_table$draw <- ifelse(r2_game_table$rngs > r2_game_table$team_one_win_pred& 
                                    r2_game_table$rngs < (r2_game_table$team_one_win_pred + r2_game_table$draw_pred),1,0)
r2_game_table$win_two <- ifelse(r2_game_table$win_one ==0 & r2_game_table$draw==0,1,0)

if (r2_game_table$draw==1 & r2_game_table$rngs2 < 0.5) {r2_game_table$win_one <- 1} else
    if(r2_game_table$draw==1 & r2_game_table$rngs2 > 0.5) {r2_game_table$win_two <- 1}

##who advances?
if (r2_game_table$win_one==1 ){
    r2_game_table$advance_team <- r2_game_table$team_one
} else {
    r2_game_table$advance_team <- r2_game_table$team_two

}
##final rankings
country_final_rank <- ""
country_final_rank[1] <- r2_game_table$advance_team
country_final_rank[2] <- ifelse(r2_game_table$win_one==1,r2_game_table$team_two,r2_game_table$team_one)

country_final_rank[3] <- ifelse(r4_game_table$win_one[1]==1,r4_game_table$team_two[1],r4_game_table$team_one[1])
country_final_rank[4] <- ifelse(r4_game_table$win_one[2]==1,r4_game_table$team_two[2],r4_game_table$team_one[2])

country_final_rank[5] <- ifelse(r8_game_table$win_one[1]==1,r8_game_table$team_two[1],r8_game_table$team_one[1])
country_final_rank[6] <- ifelse(r8_game_table$win_one[2]==1,r8_game_table$team_two[2],r8_game_table$team_one[2])
country_final_rank[7] <- ifelse(r8_game_table$win_one[3]==1,r8_game_table$team_two[3],r8_game_table$team_one[3])
country_final_rank[8] <- ifelse(r8_game_table$win_one[4]==1,r8_game_table$team_two[4],r8_game_table$team_one[4])

group_winners_temp <- lapply(groups_with_res,function(x){return(x$country[1])})
group_winners_temp2 <- unlist(group_winners_temp)

group_rups_temp <- lapply(groups_with_res,function(x){return(x$country[2])})
group_rups_temp2 <- unlist(group_rups_temp)


country_final_rank <- as.data.frame(cbind(country_final_rank,c(1,2,4,4,8,8,8,8)))
colnames(country_final_rank) <- c("country","rank")
wc_quarterfinals[[i]] <- country_final_rank
group_winners[[i]] <- group_winners_temp2
group_runners_up[[i]] <- group_rups_temp2
}
wc_quarterfinals_out <- bind_rows(wc_quarterfinals)
wc_winners <- wc_quarterfinals_out %>%filter(rank ==1) %>% group_by(country) %>% summarise(wins=n(),
                                                                                          percent=n()/iter,
                                                                                          return=1/percent) %>%
                                                                arrange(desc(percent))

wc_finalists <- wc_quarterfinals_out %>%filter(rank %in% c(1,2)) %>% group_by(country) %>% summarise(wins=n(),
                                                                                           percent=n()/iter,
                                                                                           return=1/percent) %>%
    arrange(desc(percent))

wc_semis <- wc_quarterfinals_out %>%filter(rank %in% c(1,2,3,4)) %>% group_by(country) %>% summarise(wins=n(),
                                                                                                     percent=n()/iter,
                                                                                                     return=1/percent) %>%
    arrange(desc(percent))

wc_qfs <- wc_quarterfinals_out %>% group_by(country) %>% summarise(qfs=n(),
                                                                                           percent=n()/iter) %>%
    arrange(desc(percent))


group_winners2 <- unlist(group_winners)
group_win_count <- arrange(as.data.frame(table(group_winners2)),desc(Freq)) %>% mutate(win_pct = Freq/1000,
                                                                                return=(1/win_pct))

head(games)
##save some output in CSVs
write.csv(games,"C:/Users/pc/Documents/Data science/World cup 2018/group game preds.csv")
write.csv(group_win_count,"C:/Users/pc/Documents/Data science/World cup 2018/group win count preds.csv")
write.csv(wc_qfs,"C:/Users/pc/Documents/Data science/World cup 2018/quarter finalist preds.csv")
write.csv(wc_semis,"C:/Users/pc/Documents/Data science/World cup 2018/semi finalist preds.csv")
write.csv(wc_finalists,"C:/Users/pc/Documents/Data science/World cup 2018/finalist preds.csv")
write.csv(wc_winners,"C:/Users/pc/Documents/Data science/World cup 2018/winner preds.csv")
library(dplyr)
wc_finalists_spec <- wc_quarterfinals_out %>%filter(rank %in% c(1))

wc_finalists_spec2 <- wc_quarterfinals_out %>%filter(rank %in% c(2))
wc_finalists_det <- paste0(wc_finalists_spec[,1]," ",wc_finalists_spec2[,1]) 
wc_finalists_table <- table(wc_finalists_det)/1000 

write.csv(wc_finalists_table,"C:/Users/pc/Documents/Data science/World cup 2018/specific finalists.csv")
