##OK, we've read in and tidied the data, and had a look at some of the
##relationships; lets now try and build some models on the data.

##split off a test data set. Arguably at this point it could be worth considering splitting
##off the first and lasst years; i'm not going to do this, but it'll be worth keeping a 
#close eye on year as a facgtor.
library(caret)
set.seed(101)
train_data <- createDataPartition(wc_results4$team_one_win,p=0.8,list=FALSE)
wc_train <-wc_results4[train_data,]
wc_test <-wc_results4[-train_data,]

wc_glm_conf4b <- glm(data=wc_train,team_one_win~rating_diff_percent+home_cont_two,
                     family=binomial(link="logit"))
wc_glm_conf4b_predict <- ifelse(predict(wc_glm_conf4b,newdata=wc_train)>0,1,0)
wc_glm_conf4b_predict_tab <- table(wc_train$team_one_win,wc_glm_conf4b_predict)
confusionMatrix(wc_glm_conf4b_predict_tab)


wc_glm_conf4b_predict_test <- ifelse(predict(wc_glm_conf4b,newdata=wc_test)>0,1,0)
wc_glm_conf4b_predict_tab_test <- table(wc_test$team_one_win,wc_glm_conf4b_predict_test)
confusionMatrix(wc_glm_conf4b_predict_tab_test)

##and apply to the test data sets;
wc_glm_conf4_predict_test <- ifelse(predict(wc_glm_conf4,newdata=wc_test)>0,1,0)
wc_glm_conf4_predict_tab_test <- table(wc_test$team_one_win,wc_glm_conf4_predict_test)
confusionMatrix(wc_glm_conf4_predict_tab_test)
str(wc_train)
##this was the final model we settled on before
wc_glm_conf4 <- glm(data=wc_train,team_one_win~rating_diff_percent+conf_team_one+home_cont_two,
                    family=binomial(link="logit"))
wc_glm_conf4_predict <- ifelse(predict(wc_glm_conf4,newdata=wc_train)>0,1,0)
wc_glm_conf4_predict_tab <- table(wc_train$team_one_win,wc_glm_conf4_predict)
confusionMatrix(wc_glm_conf4_predict_tab)

##lets just test a model just with rating diff percent
wc_glm_conf4a <- glm(data=wc_train,team_one_win~rating_diff_percent,
                    family=binomial(link="logit"))
wc_glm_conf4a_predict <- ifelse(predict(wc_glm_conf4a,newdata=wc_train)>0,1,0)
wc_glm_conf4a_predict_tab <- table(wc_train$team_one_win,wc_glm_conf4a_predict)
confusionMatrix(wc_glm_conf4a_predict_tab)



##lets  test a model just with rating diff percent and home_away status
####THIS IS THE MODEL TO USE #### 
wc_glm_conf4b <- glm(data=wc_train,team_one_win~rating_diff_percent+home_cont_two,
                     family=binomial(link="logit"))
wc_glm_conf4b_predict <- ifelse(predict(wc_glm_conf4b,newdata=wc_train)>0,1,0)
wc_glm_conf4b_predict_tab <- table(wc_train$team_one_win,wc_glm_conf4b_predict)
confusionMatrix(wc_glm_conf4b_predict_tab)


##and apply to the test data sets;
wc_glm_conf4_predict_test <- ifelse(predict(wc_glm_conf4,newdata=wc_test)>0,1,0)
wc_glm_conf4_predict_tab_test <- table(wc_test$team_one_win,wc_glm_conf4_predict_test)
confusionMatrix(wc_glm_conf4_predict_tab_test)

wc_glm_conf4a_predict_test <- ifelse(predict(wc_glm_conf4a,newdata=wc_test)>0,1,0)
wc_glm_conf4a_predict_tab_test <- table(wc_test$team_one_win,wc_glm_conf4a_predict_test)
confusionMatrix(wc_glm_conf4a_predict_tab_test)


wc_glm_conf4b_predict_test <- ifelse(predict(wc_glm_conf4b,newdata=wc_test)>0,1,0)
wc_glm_conf4b_predict_tab_test <- table(wc_test$team_one_win,wc_glm_conf4b_predict_test)
confusionMatrix(wc_glm_conf4b_predict_tab_test)

##another test; transform rating_diff_percent
wc_train$ln_rating <- log(wc_train$rating_diff_percent)

wc_test$ln_rating <- log(wc_test$rating_diff_percent)


wc_glm_conf4c <- glm(data=wc_train,team_one_win~ln_rating+home_cont_two,
                     family=binomial(link="logit"))
wc_glm_conf4c_predict <- ifelse(predict(wc_glm_conf4c,newdata=wc_train)>0,1,0)
wc_glm_conf4c_predict_tab <- table(wc_train$team_one_win,wc_glm_conf4c_predict)
confusionMatrix(wc_glm_conf4c_predict_tab)
confusionMatrix(wc_glm_conf4b_predict_tab)


wc_glm_conf4c_predict_test <- ifelse(predict(wc_glm_conf4c,newdata=wc_test)>0,1,0)
wc_glm_conf4c_predict_tab_test <- table(wc_test$team_one_win,wc_glm_conf4c_predict_test)
confusionMatrix(wc_glm_conf4c_predict_tab_test)

##lets start with some simple glms

predict_cols <- c("rank_team_one","conf_team_one","rank_team_two","conf_team_two",
                "rating_diff_amt","rating_diff_percent","home_stus","both_home_cont_stus")

predictive_value <- sapply(predict_cols,function(x) {
    wc_model_1way <- glm(data=wc_train,as.formula(paste0("team_one_win~",x)),family=binomial(link="logit"))
    return(summary(wc_model_1way)$aic)}
                        )

## so the best individual predictive (based on AIC) appears to be rating diff percent
#lets try some collection of factors and see whats best
#all our candidates
wc_glm_sink <- glm(data=wc_train,team_one_win~rank_team_one+rank_team_two+conf_team_one+
                       conf_team_two+rating_diff_amt+rating_diff_percent+home_stus+both_home_cont_stus,
                   family=binomial(link="logit"))

wc_glm_sink_predict <- ifelse(predict(wc_glm_sink,newdata=wc_train)>0.5,1,0)
wc_glm_sink_predict_tab <- table(wc_train$team_one_win,wc_glm_sink_predict)
confusionMatrix(wc_glm_sink_predict_tab)

#this model seems to perform very poorly!
#just ranks
wc_glm_ranks <- glm(data=wc_train,team_one_win~rank_team_one+rank_team_two,
                    family=binomial(link="logit"))
wc_glm_ranks_predict <- ifelse(predict(wc_glm_ranks,newdata=wc_train)>0,1,0)
wc_glm_ranks_predict_tab <- table(wc_train$team_one_win,wc_glm_ranks_predict)
confusionMatrix(wc_glm_ranks_predict_tab)
#this model performs a little better, lets see if we can introduce some conference data in

wc_glm_ranks2 <- glm(data=wc_train,team_one_win~rank_team_one+rank_team_two+home_stus,
                    family=binomial(link="logit"))
wc_glm_ranks2_predict <- ifelse(predict(wc_glm_ranks2,newdata=wc_train)>0,1,0)
wc_glm_ranks2_predict_tab <- table(wc_train$team_one_win,wc_glm_ranks2_predict)
confusionMatrix(wc_glm_ranks2_predict_tab)
##adding in ranks improves the accuracy a tiny fraction, but well within the confidence
#intervals, and the p values are quite high.

##an alternate approach; just use ranking_diff_percent
wc_glm_diff <- glm(data=wc_train,team_one_win~rating_diff_percent,
                     family=binomial(link="logit"))
wc_glm_diff_predict <- ifelse(predict(wc_glm_diff,newdata=wc_train)>0,1,0)
wc_glm_diff_predict_tab <- table(wc_train$team_one_win,wc_glm_diff_predict)
confusionMatrix(wc_glm_diff_predict_tab)
##also slightly better; i'm a little more confident in this as it is predictive
##ok, so home/away conference status wasnt great; lets introduce team one/two conferences
wc_glm_conf <- glm(data=wc_train,team_one_win~rating_diff_percent+conf_team_one+conf_team_two,
                   family=binomial(link="logit"))
wc_glm_conf_predict <- ifelse(predict(wc_glm_conf,newdata=wc_train)>0,1,0)
wc_glm_conf_predict_tab <- table(wc_train$team_one_win,wc_glm_conf_predict)
confusionMatrix(wc_glm_conf_predict_tab)
#team one is somewhat predictive, team two less so. Lets try a combination of the two
wc_glm_conf2 <- glm(data=wc_train,team_one_win~rating_diff_percent+conf_team_one*conf_team_two,
                   family=binomial(link="logit"))
wc_glm_conf2_predict <- ifelse(predict(wc_glm_conf2,newdata=wc_train)>0,1,0)
wc_glm_conf2_predict_tab <- table(wc_train$team_one_win,wc_glm_conf2_predict)
confusionMatrix(wc_glm_conf2_predict_tab)
##ok, so that's not encouraging. Lets go back to just team one and see what happens them. 

wc_glm_conf2 <- glm(data=wc_train,team_one_win~rating_diff_percent+conf_team_one+conf_team_two,
                    family=binomial(link="logit"))
wc_glm_conf2_predict <- ifelse(predict(wc_glm_conf2,newdata=wc_train)>0,1,0)
wc_glm_conf2_predict_tab <- table(wc_train$team_one_win,wc_glm_conf2_predict)
confusionMatrix(wc_glm_conf2_predict_tab)

##lets add whether either team is playing at home or not

wc_glm_conf3 <- glm(data=wc_train,team_one_win~rating_diff_percent+conf_team_one+conf_team_two+
                        home_cont_one+home_cont_two,
                    family=binomial(link="logit"))
wc_glm_conf3_predict <- ifelse(predict(wc_glm_conf3,newdata=wc_train)>0,1,0)
wc_glm_conf3_predict_tab <- table(wc_train$team_one_win,wc_glm_conf3_predict)
confusionMatrix(wc_glm_conf3_predict_tab)
##interesting that team2 conference is not predicive but home_cont_two is... lets finally try a
##model using those factors specifically
wc_glm_conf4 <- glm(data=wc_train,team_one_win~rating_diff_percent+conf_team_one++home_cont_two,
                    family=binomial(link="logit"))
wc_glm_conf4_predict <- ifelse(predict(wc_glm_conf4,newdata=wc_train)>0,1,0)
wc_glm_conf4_predict_tab <- table(wc_train$team_one_win,wc_glm_conf4_predict)
confusionMatrix(wc_glm_conf4_predict_tab)
##ok, that seems to be the best. let's see how it goes on the test data set
wc_glm_test <- ifelse(predict(wc_glm_conf4,newdata=wc_test)>0,1,0)
wc_glm_test_tab <- table(wc_test$team_one_win,wc_glm_test)
confusionMatrix(wc_glm_test_tab)
##and how much better it is than just the elo difference model
wc_glm_test2 <- ifelse(predict(wc_glm_diff,newdata=wc_test)>0,1,0)
wc_glm_test2_tab <- table(wc_test$team_one_win,wc_glm_test2)
confusionMatrix(wc_glm_test2_tab)
##so there is a bit of model performance uplift with the new "new" model. 

##now, an alternate approach. The result of a game is a three-factor result; win,
##loss or draw. So lets see what a random forest model against those.
##need to build a factor first
wc_train$result <- ""
for( i in 1:nrow(wc_train)){
 wc_train$result[i] <-if(wc_train$score_one[i] >wc_train$score_two[i]) {("win")}
       else if (wc_train$score_one[i] ==wc_train$score_two[i]){("draw")}
       else {("loss")}
    
}

wc_rf <- train(data=wc_train,result~rating_diff_percent+rank_team_one+rank_team_two+
                   conf_team_one+conf_team_two+home_cont_one+home_cont_two+
                   home_stus,method="rf")

wc_rf_predict <- predict(wc_rf,newdata=wc_train)
wc_rf_predict2 <- ifelse(wc_rf_predict=="win",1,0)
wc_rf_predict2_tab <- table(wc_train$team_one_win,wc_rf_predict2)
confusionMatrix(wc_glm_test2_tab)

wc_rf_predict2d <- ifelse(wc_rf_predict=="draw",1,0)
wc_rf_predict2d_tab <- table(wc_train$draw,wc_rf_predict2d)
confusionMatrix(wc_rf_predict2d_tab)
##the random forest doesnt seem to have added much. No, what I think we want to do is use
##the original model to predict a team two win,and then the games with neither team winning
##are draws

#wc_glm_conf4 is our best model so far
summary(wc_glm_conf4)

str(wc_train)
wc_train_alt <- wc_train[,c("team_two","team_one","score_two","score_one","rank_team_two",
                            "conf_team_two","rank_team_one","conf_team_one","year",
                            "host_region","home_cont_two","home_cont_one")]
colnames(wc_train_alt) <- colnames(wc_train[1:12])

wc_train_alt$rating_diff_percent <- wc_train_alt$rank_team_one / wc_train_alt$rank_team_two

team_two_win <- ifelse(predict(wc_glm_conf4,newdata=wc_train_alt)>0,1,0)
team_two_win_table <- table(team_two_win,wc_train$team_two_win)
confusionMatrix(team_two_win_table)

wc_train$team_two_win_pred <- team_two_win
wc_train$team_one_win_pred <- wc_glm_conf4_predict
#if neither team is predicted to win, then we predict a draw
wc_train$draw_pred <- ifelse((wc_train$team_two_win_pred==0 & wc_train$team_one_win_pred==0),1,0)
draw_table <- table(wc_train$draw_pred,wc_train$draw)
confusionMatrix(draw_table)
team_two_win_table <- table(wc_train$team_two_win_pred,wc_train$team_two_win)
confusionMatrix(team_two_win_table)

team_one_win_table <- table(wc_train$team_one_win_pred,wc_train$team_one_win)
confusionMatrix(team_one_win_table)

##this doesn't produce amazing results. I wonder if we can model a draw explicitly.
##I'm tempted to use a random forest, lets try that again;
##firstly lets give a random forest a chance at modelling a win.

wc_rf_t1 <- train(data=wc_train,team_one_win~rating_diff_percent+rank_team_one+rank_team_two+
                   conf_team_one+conf_team_two+home_cont_one+home_cont_two+
                   home_stus,method="rf")

team_one_win_pred <- predict(wc_rf_t1,newdata=wc_train)
team_one_win_pred_tab <- table(team_one_win_pred,wc_train$team_one_win)
confusionMatrix(team_one_win_pred_tab)
#oooh, this produces a better outcome... 
#lets try it against the test data set
team_one_win_predt<- predict(wc_rf_t1,newdata=wc_test)
team_one_win_predt_tab <- table(team_one_win_predt,wc_test$team_one_win)
confusionMatrix(team_one_win_predt_tab)
##hm, not a significant boost based on the results on the training data set.
##still, lets try a draw and see what happens with that.

wc_rf_draw <- train(data=wc_train,draw~rating_diff_percent+conf_team_one+conf_team_two+home_cont_one+home_cont_two,
                      method="rf")

draw_pred <- predict(wc_rf_draw,newdata=wc_train)
draw_tab <- table(draw_pred,wc_train$draw)
confusionMatrix(draw_tab)
##the bloody model just puts everything to 0! boo

##ok, so we'll stick with the thing we have for now.
wc_train$pred_result <- ""
for( i in 1:nrow(wc_train)){
    wc_train$pred_result[i] <-if(wc_train$team_one_win_pred[i]==1) {("win")}
    else if (wc_train$team_two_win_pred[i]==1){("loss")}
    else {("draw")}
    
}
preds <- table(wc_train$result,wc_train$pred_result)
str(wc_test)
preds
table(wc_train$result)
##ok, so the model is currently over predicting draws; might want to play around with that a bit
##next steps; play around with the model to see if we can get less draw predictions
##try and turn these into "expected" results somehow
##poisson modelling of goals scored
##expected competition winners