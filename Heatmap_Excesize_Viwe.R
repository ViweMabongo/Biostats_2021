#Author: Viwe_Mabongo
#Date: 21 April 2021
#Heatmap_Excercise




library(tidyverse)
library(corrplot)
library(ggpubr)
library(reshape)
library(dplyr)



#load data ecklonia data

ecklonia <- read_csv("ecklonia.csv")

#Selecting columns that must be excluded in the dataset.
ecklonia_sub1 <- ecklonia %>% 
  select(-species, -site, -ID)

#Perfom Pearson test
data <- round(cor(ecklonia_sub1), 2)

#melting to organize the data
Melted_data <- melt(data)

#Create ggplot for Heatmap, the closer the values are to 1 the stronge the correlation.
ggplot(data = Melted_data, aes(x = X1, y = X2, fill = value)) +
  geom_tile() +
  theme(axis.text = element_text(angle = 45))




