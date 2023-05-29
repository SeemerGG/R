install.packages(packages)
install.packages('ggplot2')
install.packages('dplyr')
install.packages('tidyr')
install.packages('tibble')
install.packages('vctrs')
install.packages('tibble')
install.packages("vctrs")
install.packages('tibble', repos = 'http://cran.rstudio.com/', type = 'source')
install.packages('klaR')
install.packages('party')
install.packages('randomForest')
install.packages('lattice')

install.packages('MASS')
install.packages('grid')
install.packages('mvnorm')
install.packages('modeltools')
install.packages('stats4')
install.packages('strucchange')
install.packages('zoo')

library (lattice)
library("scatterplot3d")
library(klaR)
library(party)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)
library(randomForest) 
#Part 1

df <- read.csv('dataset.csv')
df <- df[,c("year", "city", "state", "urbanrural", "race", "killed", "injured", "victims", "type")]
count_regions <- 
summary(df)
#Количество событий по регионам 
boxplot(df)

