#Author: Viwe Mabongo
#Date: 30 June 2021
#Task: Dissimilarity matrix_excercise


library(tidyverse)
library(vegan)
library(BiodiversityR)
library(ggplot2)
library(dplyr)
library(readr)



# 1 -----------------------------------------------------------------------

#The matrix is square because there is the same number of rows and columns in the dataset,
#Number of rows and columns is determined by the dimensions or order of the matrix.


# 2 -----------------------------------------------------------------------

#Diagonal Matrix is when a site is compared to it self giving a zero dissimilarities,
# such as comparing site 1 to site 1, site 2 to site 2 etc, and this is represented in a diagonal way 
# in a square matrix. It can also be defined as straight path that connects elements whose rows and 
#columns are the same in a square matrix.


# 3 -----------------------------------------------------------------------

# Non-diagonal elements are those elementswhich do not lie on the leading diagonal of a square matrix,
# these are the elements that show differences among different sites.


# 4 -----------------------------------------------------------------------

#Loading the CSV file that will be used.
SeaSpp <- read_csv("SeaweedSpp_dis_matrix.csv")

#Dimension show how many rows and columns we have.
dim(SeaSpp)

#Viewing the first 10 rows and 10 columns
SeaSpp[1:10, 1:10]

#Removing a the first column that is not relevant in our data
SeaSpp <- dplyr::select(SeaSpp, -1)


sor <- vegdist(SeaSpp, binary = TRUE) # binary = TRUE sets to presence/absence data
sor_df <- round(as.matrix(sor), 4)
sor_df[1:1, ] # select row 1 and  all the columns


#Create a plot for row number 1 agaisnt all the columns
ggplot(data = SeaSpp, (aes(x =1:58, y = sor_df[1:1,]))) +
  geom_line() + xlab("Coastal section, west to east") + ylab("Disimilarities(Sorenson index)")




# 5 -----------------------------------------------------------------------

#From West  to East there was an increase a dissimilarity index meaning that the communities are 
#totally different when moving more to the East side, this can be supported with the fact that 
# the numbers are increasing to more than 0.9.




