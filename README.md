# Monte carlo simulation of the 2018 FIFA World Cup

This repository contains a number of R scripts used to create a monte carlo simulation of the 2018 FIFA World Cup. A write-up of this work can be found at http://rpubs.com/chrishay1/wc2018. In summmary these files download some data, fit a regression model, and then run 1000 simulated world cups based on that model.

The specific purpose of each script is listed below.
* Read data from eloratings.R - This script reads a set of rankings for each team from an external website, and results of games from previous World Cups. 
* exploratory analysis.R - Some initial workings on the relationships (messy)
* modelling.R - Attempts at modelling a logistic regression model to the data (messy)
* monte carlo simulation of the world cup.R - Simulating the outcome of the tournament based on the logistic regression model developed above
* markdown.rmd - the R markdown documenting the output. 

I have also uploaded a number of csv files to this repository;
* wc_results.csv - this is a listing of results from the 10 previous world cups.
* finalist preds.csv - this is an estimate of the probability of each team making the final based on the monte carlo simulation
* group game preds.csv - this is an estimate of the probability of each game in the group stages based on the simulation
* group win count preds.csv - this is an estimate of the probability of each team winning their group based on the simulation.
* quarter finalist preds.csv - this is an estimate of the probability of each team making the quarter finals based on the simulation.
* semi finalist preds.csv - this is an estimate of the probability of each team making the semi finals based on the simulation.
* winner preds.csv - this is an estimate of the probabilty of each team winning the world cup based on the simulation..
