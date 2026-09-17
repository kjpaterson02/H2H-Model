# Head-to-Head Information in Football Match Prediction

This repository contains the R code used for my MSc research project investigating whether head-to-head (H2H) match information can improve football match outcome prediction.

The project uses English Premier League match data from the 2014/15 season onwards. A simple expected-goals (xG) based Poisson model, based on the approach of Wilkens, is used as the benchmark. The model is extended by incorporating historical head-to-head information and applying exponential weighting to give greater importance to more recent H2H fixtures.

The predictive performance of the models is evaluated using Brier score, log loss, accuracy and probability calibration. The final model is also evaluated through an out-of-sample betting strategy using bookmaker odds.

## Repository Structure

The analysis is divided into five sections:

### 1. Data Extraction

Contains scripts used to obtain, clean and combine the match, expected-goals and bookmaker odds data.

### 2. Assumptions

Tests the assumptions underlying the Poisson modelling framework, including the Poisson assumption for goals and the independence of home and away scores.

### 3. Model Building

Contains the benchmark model, H2H model specifications, parameter tuning and matched random-history control experiment.

### 4. Model Evaluation

Evaluates predictive performance using Brier score, log loss and accuracy. This section also examines probability calibration and the effect of isotonic regression.

### 5. Betting Strategy

Implements the rolling out-of-sample betting strategy and evaluates profitability using profit and loss (P&L), return on investment (ROI), Sharpe ratio and maximum drawdown.

## Data Sources

Match results and expected-goals (xG) data were obtained from Understat.

Historical bookmaker odds were obtained from Football-Data.co.uk.

The analysis covers English Premier League matches from the 2014/15
season onwards.

Raw and processed data files are not included in this repository.
The scripts in `Scripts/1. Data Extraction/` contain the procedures
used to obtain, clean and combine the data used in the analysis.

Football-Data files can be obtained directly from:
https://www.football-data.co.uk/englandm.php

## Running the Analysis

The main analysis scripts are intended to be run sequentially. Helper functions used by each stage are contained within the corresponding script folders.

The main workflow is:

1. Data extraction and preparation
2. Assessment of modelling assumptions
3. Model construction and parameter tuning
4. Model evaluation and calibration
5. Betting strategy evaluation

The project was developed in R and RStudio.

## Figures

The `Figures` directory contains the principal figures generated during the analysis, including model calibration, assumption checks, random-history control results and cumulative betting performance.

## Research Project

This repository accompanies my MSc research project. It is intended to provide the code required to understand and reproduce the analysis presented in the project.