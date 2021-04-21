# Author: Viwe Mabongo
# Biostats_assignment_1
#Date: 21 April 2021


# Section 1 ---------------------------------------------------------------



data <- BOD
view(BOD)
# C is correct


# Section 2 ---------------------------------------------------------------


#load libraries
library(dplyr)
library(dslabs)
library(tidyr)
library(ggplot2)
#load data

Murder_data <- murders
glimpse(Murder_data)#This makes it possible to see every column in a data frame.
head(Murder_data)# To explore the first six rows of the dataset.
tail(Murder_data)# To explore the last six rows of the dataset.
str(Murder_data)# To display the internal structure of a dataset.
summary(Murder_data)#To produce result summaries of the results of various model fitting functions.
?murders

#The murder dataset shows gun murder US gun murders in 4 regions which are Northeast, South,
# North Central and West. In each of the regions the are various states that are located within.
# More murders occur in the Northeast and South where there is higher population.

SLCT_S_PS <- Murder_data %>% 
  select(state, population)

Remv_Flo <- Murder_data[Murder_data$state != "Florida",]

no_south <- Murder_data[Murder_data$region != "South",]
# There are 34 states when the South region is removed.

# Calculate the population size of the south and west regionally

S_Pop <- Murder_data %>% 
  filter(region == "South") %>% 
  summarise(S_Pop = sum(population))

West_Pop <- Murder_data %>% 
  filter(region == "West") %>% 
  summarise(West_Pop = sum(population))

NE <- Murder_data %>% 
filter(region == "Northeast")

# Illustrating relationship between population and number gun murders.
ggplot(Murder_data, aes(x = population, y = total)) +
  geom_point(aes(color = region)) +
  geom_smooth() +
  theme(legend.position = "right") +
  labs(x = "population", y = "Total murders by gun") +
  ggtitle("Gun murders in United States")

# The North East and South region have higher number of gun murders because of higher populatios.

#Illustrating the relationship between gun murders and population.
ggplot(Murder_data, aes(x = population, y = total)) +
  geom_line(aes(color = region)) +
  geom_smooth() +
  theme(legend.position = "right") +
  labs(x = "population", y = "Total murders by gun") +
  ggtitle("Gun murders in United States")



#Total population of is 115674434, then West population is 71945553
#The South population is bigger by 43,7 million people.
Tot <- Murder_data %>% 
filter(total > 20) %>% 
  filter(total < 100)

# Slicing 
Create_Obj <- Murder_data %>% 
  slice(10:24, 26)

murders_tibble <- as_tibble(Murder_data)

tibble_region <- as_tibble(Murder_data) %>% 
  group_by(region)



# Section 3 ---------------------------------------------------------------

 
#load data

# The minimum height of females is greater than that of males, and the median height of females,
#is less than than of males with the maximum height of males also greater than than of females and the average,
# of males is greater than that of females, which gives to the conclusion that males students are taller than females,
# students in this class.

head(heights)# To explore the first six rows of the dataset.

tail(heights)# To explore the last six rows of the dataset.

glimpse(heights)#This makes it possible to see every column in a data frame, more
# like str() function.



heights %>% 
  filter(sex == "Female") %>% 
  summarise(min_fem = min(height),
            median_fem = median(height),
            max_fem = max(height),
            ave_fem = mean(height),
            sd_fem = sd(height))
            

heights %>% 
filter(sex == "Male") %>% 
  summarise(min_mal = min(height),
            med_mal = median(height),
            max_mal = max(height),
            ave_mal = mean(height),
            sd_mal = sd(height))

# The minimum height of females is greater than that of males, and the median height of females,
#is less than than of males with the maximum height of males also greater than than of females and the average,
# of males is greater than that of females, which gives to the conclusion that males students are taller than females,
# students in this class.




# section 4 ---------------------------------------------------------------




x <- c(1, 6, 21, 19, NA, 73, NA)
y <- c(NA, NA, 3, NA, 13, 24, NA )

summary(x)
# 2 missing elements
summary(y)
# 4 missing elements

x <- data.frame(x = c(1, 6, 21, 19, 73, NA),            
                y = c(NA, NA, 3, NA, 13, 24, NA))

# Apply transform function
x <- transform(data, x = x + 10)  





# Section 5 ---------------------------------------------------------------


Seasonal_data <- data.frame(year = c(2015, 2016, 2017, 2018),
                            winter = c(41, 39, 47, 40),
                            spring = c(41, 46, 57, 45),
                            summer = c(75, 52, 85, 66),
                            Autumn = c(57, 66, 52, 56))

#Formulating the hypothesis for average summer temperature over the period from 2015-2018.
#H0: There is no difference in average summer temperature from 2015-2018.
#H1: There is difference in average summer temperature from 2015-2018.

# The average summer temperatures were at 75 degrees fahrentheit(F) in 2015, then decreased to 52 degrees F  in 2016,
# then again increased in 2017 to 85 degrees  F, then in 2018 the temperatures decreased to 66 degrees F.
#Therefore, we reject null hypothesis.

ggplot() +
  geom_line(data = Seasonal_data, aes(x = year, y = summer), colour = "green") +
  labs(x = "Year", y = "Temperature (degrees Fahrenheit)") +
  ggtitle("Average summer temperatures from 2015-2018")

ggplot(data = Seasonal_data, aes(x = year, y = summer)) +
  geom_bar(stat = "identity", color = "black", fill = "green") +
  labs(x = "Year", y = "Temperature (degrees fahrenheit)") +
  ggtitle("Average summer temperatures from 2015 to 2018")



cats_data<- tibble(cats = c("A", "B", "C"),
                   position = c("1-2-3", "3-1-2", "2-3-1"),
                   minutes = c(3, 3, 3),
                   seconds = c(12, 44, 15))
cats_data




# Separating the position column into three different columns.
Sep_cats <- cats_data %>%
  separate(col = "position", into = c("first_place", "second_place", "third_place"), sep = "-")

#Uniting the minutes and seconds column into single column(total_time)
uni_cats <- Sep_cats %>%
  unite(minutes, seconds, col = "total_time", sep = ":")






#Section 6 -------------------------------------------------


mtcars <- mtcars
str(mtcars)
summary(mtcars)

#Filtering the mtcars gear column focus on cars with gear 3
Fil <- mtcars %>% 
  filter(gear == 3)

#Group mtcars by weight from the least heavy to the most heavy, also calculating the mean.
Grp <- mtcars %>% 
  group_by(wt) %>% 
  summarise(mean = mean(wt))

#Arrange mtcars from the lowest miles per gallon to the most miles in a descending order.
Arr_1 <- mtcars %>% 
  arrange(mtcars, mpg)

#Selecting the different columns to explore them from the rest of the data
Sel <- mtcars %>% 
  select(gear, mpg, hp, cyl, am)

#Uniting the column vs(Engine of cars) and am(car transmission)
Unite_mt <- mtcars %>%
  unite(vs_am, vs, am)

#Separating the column of transmission and engine(vs_am) into to different columns
Sep_mt <- Unite_mt %>% 
separate(col = vs_am,into = c("vs","am"))

  
  
  
  
  
  
  
  










