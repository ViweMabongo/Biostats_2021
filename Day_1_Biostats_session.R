#Basic Biostats
# Author : Viwe Mabongo
# Date : 19 April 2021
# Day 1

load(tidyverse)
datasets::BOD
str(BOD) # Shows the structure of the data
summary(BOD)

datasets:: InsectSprays
ls("package:datasets")
mtcars <- mtcars
str(mtcars)
summary(mtcars)

#Calculate sample size
library(tidyverse)
chicks <- as_tibble(ChickWeight)
nrow(chicks)

#homany weights are  
chicks %>%
  summarise(length = n())

unique(chicks$Chick)

#calculate the mean final mass of the chicks at 20.
tidyverse
chicks %>% 
filter(Time == 20) %>% 
group_by(Diet) %>% 
summarise(mean_weight = mean(weight))
#or
tidyverse
chicks %>% 
  filter(Time == 20) %>% 
  group_by(Diet) %>% 
  summarise(mean_weight = sum(weight) /n()) %>% 
  summarise(mean_wt = mean(weight))


# Calculate Kurtosis at 20 days for different diets
library(e1071)
chicks %>% 
  filter(Time == 20) %>% 
  group_by(Diet) 

  kurtosis(chicks$weight)
 
chicks %>% 
  group_by(Diet) %>% 
  summarise(min_wt = min(weight),
            qrt1_wt = quantile(weight, p = 0.25),
            med_wt = median(weight),
            qrt3_wt = median(weight, p = 0.75),
            max_wt = max(weight))
 # Calculate range 
chicks %>% 
  group_by %>% 
  summarise(lower_wt = range(weight)[1],
            upper_wt = range(weight)[2])



