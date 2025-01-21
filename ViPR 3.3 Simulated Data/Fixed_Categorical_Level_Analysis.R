#Analysis of relationship between input variables and responses for fixed categorical levels.
#William Fisher
#09/04/2024

#Load libraries
library(car) #For using variance inflation factor.
library(MASS) #For using Box-cox

#Import the response_simulator function.
source("Surrogate_Project_Custom_Functions.R")

#We need to analyze the simulated dataset to see constraints on variables
simulated_data_old <- read.csv("FinalDataCleaned.csv")

fixed_cat_data_inspect <- simulated_data_old[simulated_data_old$Running.Gear.Type == 6 & simulated_data_old$Frame.Shape == 4,]


min_values <- apply(fixed_cat_data_inspect,2,min)

max_values <- apply(fixed_cat_data_inspect,2,max)

