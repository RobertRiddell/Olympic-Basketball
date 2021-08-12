# Olympic-Basketball
## Overview
This repository contains the information and data to apply predictive algorithms to Men's FIBA World Cup and Olympic games from 2000 onwards. 

## File Conetents and Description
Basketball_Exploratory_Data_Analysis
* rsconnect: file for the shinyapps 
* app.R: The shiny app script for the relationship plots
* game_total.csv: Data that is used by the app.R script

data
* Contains the fixtures and box scores from the 2000,2004,2008,2012 and 2016 Men's Olympic Basketball and 2010,2014 and 2019 FIBA World Cup.

docs
* Olympics Basketball.pptx : Presentation on what it takes to win
* Top 4 finishes.xlsx: Table to aggregate the coutries and how many top four finishes they have had.
* Variable description.txt: Contains the definitions and more information about the statistics inluded in the analysis.

figs
* plots and pictures that have been used in the docs/Olympics Basketball.pptx

funs
* data_cleaner.R: Script to bind all the individual boxscores into a single dataframe and then summarise the data by game id and country so overall output of each team can be analysed. More statistcis have been created based on the orignal data provided. The script will then write the cleaned data to the out/ folder which can then read in by the analysis scripts. 

out
* all_data.csv: The complete player by player data set
* game_total.csv: The cleaned and summarised data including the original statistics and some additional stats.

* clean.Rproj: project that all R scripts have been opened in
* Final_round_comparison.Rmd: rmarkdown that contains some ad hoc analysis and graphs included in the Olympics Basketball.pptx
* fixtures.py: Python script to scrape the fixture page for the World Cup and Olympics
* pts_analysis.Rmd: rmarkdown that works through the full modelling process to predict points.
* result_analysis: rmarkdown that works through the full modelling process to predict results.
* stats.py: python script that uses the fixture data to create the file heirachy and scrape the box scores then stores all as csv's.
