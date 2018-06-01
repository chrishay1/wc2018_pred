##exploratory analysis of the world cup results data set.

##This r code follows on from the "read data from eloratings" data set; here we assume that
##the file has been downloaded, but you can also get it from the wc_results.csv file


##ok, so let's have a look at distribution of team one wins and team two wins
g <- ggplot(data=wc_results3,aes(x=team_one_win))+geom_bar()
g

g <- ggplot(data=wc_results3,aes(x=team_two_win))+geom_bar()
g



#as suspected, nearly all of the winning teams are listed as "team one". So we'll need to
#correct for this. One way would be to duplicate the data with team one and team two swapped;
#alternatively one could randomly alternate team one and team two information.
#i'm going to go with the first one, this might produce overfit (particularly for draws)
colnames(wc_results3)
##hm, the best way to do this;
wc_results_x <- wc_results3[,c("team_two","team_one","score_two","score_one","rank_team_two",
                              "conf_team_two","rank_team_one","conf_team_one","year",
                              "host_region","home_cont_two","home_cont_one","team_two_win",
                              "team_one_win","draw")]

colnames(wc_results_x) <- colnames(wc_results3)
wc_results4 <- rbind(wc_results3,wc_results_x)
wc_results4$result <- ""
for( i in 1:nrow(wc_results4)){
    wc_results4$result[i] <-if(wc_results4$score_one[i] >wc_results4$score_two[i]) {("win")}
    else if (wc_results4$score_one[i] ==wc_results4$score_two[i]){("draw")}
    else {("loss")}
    
}

##add a feature of rating difference, and rating difference %

wc_results4$rating_diff_amt <- wc_results4$rank_team_one - wc_results4$rank_team_two

wc_results4$rating_diff_percent <- wc_results4$rank_team_one / wc_results4$rank_team_two


##a flag to say whether both teams are either home or away 
wc_results4$home_stus <- ""
wc_results4$home_stus[wc_results4$home_cont_one==1&wc_results4$home_cont_two==1] <-"both home"

wc_results4$home_stus[wc_results4$home_cont_one==1&wc_results4$home_cont_two==0] <- "home v away"
wc_results4$home_stus[wc_results4$home_cont_one==0&wc_results4$home_cont_two==1] <-"away v home"
wc_results4$home_stus[wc_results4$home_cont_one==0&wc_results4$home_cont_two==0]<- "away v away"

##ok, now we have a data set where we can analyse everything in the context of "team one".

g1 <- ggplot(data=wc_results4,aes(x=team_one_win))+geom_bar()
g1

g2 <- ggplot(data=wc_results4,aes(x=team_two_win))+geom_bar()
g2

str(wc_results4)
##ok, lets see what we can glean from the rank variable
g3 <- ggplot(data=wc_results4,aes(x=team_one_win,y=rank_team_one)) + geom_boxplot()
g3 <- g3 + xlab("team won? (1=yes,0=no)")+ ylab("ELO rating") +ggtitle("Distribution of ELO rating based on team win")
g3

g3a <- ggplot(data=wc_results4,aes(x=result,y=rank_team_one)) + geom_boxplot()
g3a <- g3a + xlab("game result")+ ylab("Team one ELO rating") +ggtitle("Distribution of ELO rating based on team win")
g3a


g3b <- ggplot(data=wc_results4,aes(x=result,y=rating_diff_percent)) + geom_boxplot()
g3b <- g3b + xlab("game result")+ ylab("Ratio of team differences") +ggtitle("Box plot distribution of ELO rating based on team win")
g3b

g3c <- ggplot(data=wc_results4,aes(rating_diff_percent,group=result,colour=result)) + geom_density()
g3c <- g3c + xlab("Ratio of team differences")+ ylab("Distribution") +ggtitle("Density of ELO rating based on team win")
g3c

g3d <- ggplot(data=wc_results4,aes(x=result,y=rating_diff_percent)) + geom_boxplot()
g3d <- g3d + xlab("game result")+ ylab("Ratio of team differences") +ggtitle("Box plot distribution of ELO rating based on team win")
g3d <- g3d + facet_grid(.~conf_team_one)
g3d

##there does seem to be a relationship there. Now rank and conference;

g4 <- ggplot(data=wc_results4,aes(x=conf_team_one,y=rank_team_one)) + geom_boxplot()
g4
##there is some correlation between rank and conference. Need to consider if that's a problem
##now lets see how difference in rank performs, also home_cont_one
g5 <- ggplot(data=wc_results4,aes(x=result,y=rating_diff_percent)) + geom_boxplot()+
    facet_grid(.~home_stus)
g5

home_stus1 <- wc_results4 %>% group_by(home_stus) %>% summarise(count_all=n())

home_stus2 <- wc_results4 %>% group_by(home_stus) %>%filter(team_one_win==1) %>% summarise(count_wins=n())
home_stus3 <- wc_results4 %>% group_by(home_stus) %>%filter(draw==1) %>% summarise(count_draws=n())

home_stus <-  inner_join(home_stus1,home_stus2) %>%  inner_join (home_stus3) %>%
              mutate(win_pct = count_wins/count_all,draw_pct = count_draws/count_all,loss_pct = 1-win_pct-draw_pct)
#there does seemt o be some relationship, home v away. And away teams draw less often
#than home teams. Interesting ... 

cont_stus1 <- wc_results4 %>% group_by(conf_team_one) %>% summarise(count_all=n())

cont_stus2 <- wc_results4 %>% group_by(conf_team_one) %>%filter(team_one_win==1) %>% summarise(count_wins=n())
cont_stus3 <- wc_results4 %>% group_by(conf_team_one) %>%filter(draw==1) %>% summarise(count_draws=n())

cont_stus <-  left_join(cont_stus1,cont_stus2) %>%  inner_join (cont_stus3) %>% replace_na(list(count_wins=0)) %>%
    mutate(win_pct = count_wins/count_all,draw_pct = count_draws/count_all,loss_pct = 1-win_pct-draw_pct)


continent <- as.data.frame(c("Asia","Africa","North America","South America","Oceania","Europe"))
continent$conf_team_one <- c("AFC","CAF","CONCACAF","CONMEBOL","OFC","UEFA")
colnames(continent) <- c("Continent","conf_team_one")
cont_stus <- inner_join(cont_stus,continent)
cont_stus$count_losses <- with(cont_stus,count_all-count_draws-count_wins)
cont_stus <- cont_stus[,c("conf_team_one","Continent","win_pct","draw_pct","loss_pct","count_wins","count_draws",
                          "count_losses","count_all")]

colnames(cont_stus) <- c("Conference","Continent","Win percentage","Draw percentage","Loss percentage","Win count",
                         "Draw count","Loss count","Total games")

##overall win loss draw percentage

stus1 <- wc_results4  %>% summarise(count_all=n())

stus2 <- wc_results4 %>% filter(team_one_win==1) %>% summarise(count_wins=n())
stus3 <- wc_results4 %>% filter(draw==1) %>% summarise(count_draws=n())

stus <-  cbind(stus1,stus2) %>%  cbind (stus3) %>% replace_na(list(count_wins=0)) %>%
    mutate(win_pct = count_wins/count_all,draw_pct = count_draws/count_all,loss_pct = 1-win_pct-draw_pct)

stus$win_pct <- round(stus$win_pct,3)
stus$draw_pct <- round(stus$draw_pct,3)
stus$loss_pct <- round(stus$loss_pct,3)
stus$count_losses <- with(stus,count_all-count_draws-count_wins)

stus <- stus[,c("win_pct","draw_pct","loss_pct","count_wins","count_draws",
                          "count_losses","count_all")]

colnames(stus) <- c("Win percentage","Draw percentage","Loss percentage","Win count",
                         "Draw count","Loss count","Total games")


kable(stus)
##next step; splitting the data into test and train data sets and starting some modelling
