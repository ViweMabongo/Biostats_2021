#Athor: Viwe_Mabongo
#Date: 22 April 2021
#Day_4_Session

library(tidyverse)
str(data)
Input <- ("
Student  Sex     Teacher  Steps  Rating
a        female  Jacob    8000   7
b        female  Jacob    9000  10
c        female  Jacob   10000   9
d        female  Jacob    7000   5
e        female  Jacob    6000   4
f        female  Jacob    8000   8
g        male    Jacob    7000   6
h        male    Jacob    5000   5
i        male    Jacob    9000  10
j        male    Jacob    7000   8
k        female  Sadam    8000   7
l        female  Sadam    9000   8
m        female  Sadam    9000   8
n        female  Sadam    8000   9
o        male    Sadam    6000   5
p        male    Sadam    8000   9
q        male    Sadam    7000   6
r        female  Donald   10000  10
s        female  Donald    9000  10
t        female  Donald    8000   8
u        female  Donald    8000   7
v        female  Donald    6000   7
w        male    Donald    6000   8
x        male    Donald    8000  10
y        male    Donald    7000   7
z        male    Donald    7000   7
")
#Summary of the data showing measures of cental tendency
data <- read.table(textConnection(Input),header = TRUE)
summary(data)

library(rcompanion)
# ungrouped data is indicated with a 1 on the right side of the formula, or the group = NULL argument.
groupwiseMean(Steps ~ Sex,data = data, conf = 0.95, digits = 3)


out <- groupwiseMean(Steps ~ Sex,data = data, conf = 0.95, digits = 3)

#Plotting a graph showing steps of males and females
ggplot(data = out) +
  geom_col(aes(x = Sex, y = Mean), fill = "red", col = "black") +
  geom_errorbar(aes(ymin = Trad.lower,
                   ymax = Trad.upper,
                   x = Sex),
               col ="black",
               width = 0.2) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Sex") + ylab("Steps")



out2 <- groupwiseMean(Steps ~ Teacher + Sex, data = data, conf = 0.95,digits = 3)

#Creating a plot of steps which is splitted into matrix of panels
ggplot(data = out2) +
  geom_col(aes(x = Sex, y = Mean), fill = "red", col = "black") +
  geom_errorbar(aes(ymin = Trad.lower,
                   ymax = Trad.upper,
                   x = Sex),
               col ="black",
               width = 0.2) +
  facet_wrap(~Teacher, ncol = 3)
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Sex") + ylab("Steps")
  
  
  #Bootstraping
  groupwiseMean(Steps ~ Sex,
                data = data,
                conf = 0.95,
                digits = 3,
                R = 10000,
                boot = TRUE,
                traditional = FALSE,
                normal = FALSE,
                basic = FALSE,
                percentile = FALSE,
                bca = TRUE)

# Perfoming anova two test  
anova <- aov(Steps~Sex*Teacher, data = data)
summary(anova)

anova_Tukey <- TukeyHSD(anova)

#Plot Turkey showing differences in mean levels of Sex: Teacher
plot(anova_Tukey)

  
  
  