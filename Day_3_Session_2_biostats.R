#Authior: Viwe_Mabongo
#Date: 21 April 2021
#Correlations

#loading libraries
library(tidyverse)
library(corrplot)
library(ggpubr)

ecklonia <- read_csv("data/ecklonia.csv")

ecklonia_sub <- ecklonia %>% 
  select(-species, - site, - ID)

# Perform correlation analysis on two specific variables
# Note that we do not need the final two arguments in this function to be stated
# as they are the defaut settings.
# They are only shown here to illustrate that they exist.

cor.test(x = ecklonia$stipe_length, ecklonia$frond_length,
         use = "everything", method = "pearson")

ecklonia_pearson <- cor(ecklonia_sub)
ecklonia_pearson

# Creating ordinal data
ecklonia$length <- as.numeric(cut((ecklonia$stipe_length+ecklonia$frond_length), breaks = 3))

# Runni test on any variable
cor.test(ecklonia$length, ecklonia$digits)


ecklonia_norm <- ecklonia_sub %>% 
  gather(key = "variable") %>% 
  group_by(variable) %>% 
  summarise(variable_norm = as.numeric(shapiro.test(value)[2]))
ecklonia_norm

# Calculate Pearson r beforehand for plotting
r_print <- paste0("r = ", 
                  round(cor(x = ecklonia$stipe_length, ecklonia$frond_length),2))

# Creating a single panel showing one correlation
ggplot(data = ecklonia, aes(x = stipe_length, y = frond_length)) +
  geom_smooth(method = "lm", colour = "grey90", se = F) +
  geom_point(colour = "mediumorchid4") +
  geom_label(x = 300, y = 240, label = r_print) +
  labs(x = "Stipe length (cm)", y = "Frond length (cm)") +
  theme_pubclean()

corrplot(ecklonia_pearson, method = "circle")

ggplot(data = ecklonia_pearson , aes(x= stipe_length, y= frond_length, fill=value)) + 
  geom_tile()


heatmap(ecklonia_pearson, scale = "none")# Creating a heatmap

r_print <- paste0("r = ", 
                  round(cor(x = ecklonia$stipe_length, ecklonia$frond_length),2))

Melt_data <- melt(r_print)




