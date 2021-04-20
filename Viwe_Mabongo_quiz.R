#Author: Viwe Mabongo
#Task : Quiz
#20 April 2021

library(tidyverse)

#Question 1
#LIST DATA CLASSES AND EXPLANATION
        
# Numerical data-Numerical data are quantitative in nature, things that can be conted or calculated.
#Example- Number of boys and girls in a classroom.

#Nominal (discrete) data- used to label variables without providing any quantitative value
# Example- when determining Gender, Race

#Continuous data- Continuous data is data that can take any value
# Example measuring Height, weight, temperature

#Qualitative data- it is data used to describe qualities and characteristics.
#Examples-collecting data on quesinarres, one on one interviews

#LIST SOME FUNCTIONS USED TO VIEW DATA IN R

#library(tidyverse)
# dimensions- dim()
#structure- str()
#column names- col()


#DISCUSS SKEWNESS AND KURTOIS
#Skewness essentially measures the relative size of the two tails. Kurtosis is a measure of the combined sizes of the two tails
# 

#QUESTION 2
Orange <- datasets::Orange



# This is a Nominal(discrete) data

Orange <- datasets::Orange
head(Orange)

tail(Orange)


Orange <- datasets::Orange %>% 
  summary(Orange)


Orange_tibble <- as_tibble(Orange)
Orange

  Orange %>% 
  filter("circumference") %>% 
  summarise(min_cir= min(circumference),
            median_cir = median(circumference),
            max_cir = max(circumference),
            ave_cir = mean(circumference),
            sd_cir = sd(circumference))
  
  
#Question 3 


 #mutate() adds new variables and preserves existing ones





























