# Simulate Bean Beetle experiment -----------------------------------------
# 
# This function creates a dataset for a single student that contains data 
# from a specific number of dishes, beetles and with whatever observations
# you want (but it defaults to 60, since that was the original set up of
# one observation per minute).
# 
# To simulate the data I used a multinomial distribution that takes a vector
# of probabilities for each of the regions in the petri dish, that defines
# how likely a beetle would go to that region at each time.
# 
# As you can see in the function arguments, it defaults to create a dataset
# with two dishes with 3 regions, each with 1 female, that has no preference 
# for any region in particular. This vector needs to add up to 1 (the 
# function rounds up values if necessary, but it cannot exceed 1)
# 
# The number of regions is defined by the length of the vector you create 
# for the probabilities associated to each region. For example, to create 
# a dish with 4 regions, you need to supply the function with a vector like:
# 
# c(0.2, 0.1, 0.2, 0.5)
# 
# The function creates a column for each region that contains the number
# of females present in each observation. If you assign more females, each
# row of the dataset will add up to whatever number you have there. 
# 
# It also needs you to specify the student name, this should be a 
# character vector, which will be appended at the end of the spreadsheet 
# name, e.g.:
# 
# bean_beetles_RamiroRea
# 
# Avoid using spaces here, since that can create conflicts sometimes when
# writing the file.
# 
# Finally, I included two options to save the data. You could either save
# all as a .csv and then upload it to google sheets and open it with that,
# which will create a copy of the data as a .gsheet file. 
# 
# Alternatively, if you feel adventurous like me, you can save this data
# straight to your google drive in that format. For this you need to install
# the package "googlesheets4"
# 
# Additionally, you will need to authenticate once per session to link R to 
# the Sheets API using the command:
# 
# gs_auth() 
# 
# And then uncomment the line:
# 
# gs4_create(name = paste("bean_beetles_", student, sep = ""), sheets = experiment_data)
# 
# This will save the sheet to your drive root folder, and you can just move
# it later to the directory you use for your class. Hope you find this useful!
# 
# Written by Ramiro Eduardo Rea Reyes
# https://github.com/rreareyes
# BIO153
# February 2021 

# Examples ----------------------------------------------------------------
#
# 2 dishes with 3 regions, 3 females, high preference for 2nd region
# 
# bean_beetle_sim(n_females = 3, observations = 60, n_dishes = 2, probs = c(0.1, 0.8, 0.1), student = "Ramiro")
#  
# 3 dishes with 4 regions, 1 female, high preference for 1st region
# 
# bean_beetle_sim(n_females = 1, observations = 60, n_dishes = 3, probs = c(0.6, 0.1, 0.2, 0.1), student = "Ramiro")

# -------------------------------------------------------------------------

require(tidyverse)
#require(googlesheets4)

bean_beetle_sim <- function(n_females = 1, observations = 60, n_dishes = 2, probs = c(0.3, 0.3, 0.3), student = ""){
  
  dish_samples <- list()
  dish_data <- list()
  
  for (iDish in 1:n_dishes) {
    
    dish_samples[[iDish]] <- rmultinom(observations, n_females, probs)
    
    dish_data[[iDish]] <- as.data.frame(t(dish_samples[[iDish]])) %>% 
      rename_with(~ tolower(gsub("V", paste("dish", iDish, "_region", sep = ""), .x, fixed = TRUE))) %>% 
      mutate(observation = 1:observations)
    
  }
  
  experiment_data <- reduce(dish_data, full_join, by = "observation") %>% 
    relocate(observation)
  
  #gs4_create(name = paste("bean_beetles_", student, sep = ""), sheets = experiment_data)
  
  write.csv(experiment_data, file = paste("bean_beetles_", student, ".csv", sep = ""), row.names = F)
  
}