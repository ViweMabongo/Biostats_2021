#Author : Viwe_Mabongo
#Date   : 22 April 2021
#Biostats quiz 2

library(tidyverse)
library(dplyr)
library(dslabs)
library(tidyr)
library(plotly)

Orange <- datasets::Orange
str(Orange)
summary(Orange)
group_by(age) %>% 
  summarise(mean_age = mean(age),
            sd_age = sd(age)) %>% 
  ungroup()



Orange <- data.frame(dat = c(rnorm(n = 35, mean = 118.0, sd = ),
                            rnorm(n = 35, mean = 30.0, sd = )),
                    sample = c(rep("A", 35), rep("B", 35)))

datasets::ToothGrowth
#Hypothesis: HO- Vitamin C have no influence on toothgrowth.
            #H1- Vitamin C have influence on toothgrowth.
ToothGrowth$dose <- as.factor(ToothGrowth$dose)

levels(ToothGrowth$supp) <- c("OrangeJuice", "Vitamin C")
library(ggplot2)
#Plot showing the line graph
ggplot(ToothGrowth, aes(x = dose, y = len)) + geom_line(aes(fill = dose)) + xlab("Dose in milligrams/day") + 
  ylab("Tooth length") + facet_grid(. ~ supp) + theme(plot.title = element_text(size = 16, face = "bold")) +
  theme(axis.title.x = element_text(size = 12, face = "bold"), axis.text.x = element_text(size = 10)) + 
  theme(axis.title.y = element_text(size = 12, face = "bold"), axis.text.y = element_text(size = 10)) + 
  theme(strip.text = element_text(size = 12, color = "blue", face = "bold.italic")) + 
  ggtitle("Tooth length vs. Dose levels:\nComparison by supplement type")

#4
datasets::ToothGrowth

ToothGrowth$dose <- as.factor(ToothGrowth$dose)

levels(ToothGrowth$supp) <- c("OrangeJuice", "Vitamin C")
library(ggplot2)
#Ploting the box and whisker graph 
ggplot(ToothGrowth, aes(x = dose, y = len)) + geom_boxplot(aes(fill = dose)) + xlab("Dose in milligrams/day") + 
  ylab("Tooth length") + facet_grid(. ~ supp) + theme(plot.title = element_text(size = 16, face = "bold")) +
  theme(axis.title.x = element_text(size = 12, face = "bold"), axis.text.x = element_text(size = 10)) + 
  theme(axis.title.y = element_text(size = 12, face = "bold"), axis.text.y = element_text(size = 10)) + 
  theme(strip.text = element_text(size = 12, color = "blue", face = "bold.italic")) + 
  ggtitle("Tooth length vs. Dose levels:\nComparison by supplement type")




# Question 2 --------------------------------------------------------------

#Objective: to compare if monthly differences exist in Sea Surface Temperature (SST) between each of
#the sites and source.

library(tidyverse)
library(lubridate)

#Do stuff
library(readr)
SACTN_data <- SACTN_daily_v4.2
View(SACTN_data)

SACTN_grp <- SACTN_data %>%
  mutate(yr = year(date),
         mo= month(date)) %>%
  group_by(index, mo) %>% #index = site
  summarise(mean_temp = mean(temp, na.rm = TRUE))
  
#Creating a plot representing differences in Sea Surface Temperatures(SST).
 ggplot(SACTN_grp, aes(x = mo, y = mean_temp)) +
  geom_line(aes(group =index), colour = "salmon") +
  facet_wrap(~index, nrow = 3) +
  labs(x ="Year", y ="Temperature (degree celcius)",     
       title = "Monthly mean temperature") #index = site
 


 
 

