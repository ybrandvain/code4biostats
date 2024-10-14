# Load necessary libraries
library(readr)   # For reading CSV files
library(ggplot2) # For data visualization (though not used in this script)
library(dplyr)   # For data manipulation

# Load the newts dataset from the provided URL
newts <- read_csv("https://whitlockschluter3e.zoology.ubc.ca/Data/chapter13/chap13q08Newts.csv")

# Calculate the observed difference in mean whole animal resistance between localities
obs_diff <- newts %>%
  group_by(locality) %>%                      # Group the data by 'locality'
  summarise(means = mean(wholeAnimalResistance)) %>% # Calculate mean resistance per locality
  summarise(difference = diff(means),         # Find the difference between the two locality means
            abs_difference = abs(difference)) %>%  # Calculate the absolute difference
  pull(abs_difference)  # Extract the absolute difference

# Set the number of permutations for the permutation test
n_perms <- 1000

# Perform the permutation test
permuted_dist <- replicate(n = n_perms, simplify = FALSE,  # Replicate the following procedure n_perms times
    newts %>%
    mutate(perm_locality = sample(locality, replace = FALSE)) %>%  # Randomly shuffle the 'locality' labels
    group_by(perm_locality) %>%  # Group by the permuted locality labels
    summarise(mean_perm_resist = mean(wholeAnimalResistance)) %>%  # Calculate mean resistance for permuted groups
    summarise(abs_mean_perm_diff = abs(diff(mean_perm_resist)))) %>% # Find the absolute difference in permuted means
  bind_rows()  # Combine the results into a single dataframe

# Calculate the p-value by comparing the observed difference to the permutation distribution
permuted_dist %>%
  mutate(obs_diff = obs_diff,  # Add the observed difference to the dataset
         as_or_more_extreme = abs_mean_perm_diff >= obs_diff) %>%  # Identify permutations as extreme as the observed
  summarise(p_value = mean(as_or_more_extreme))  # Calculate the p-value as the proportion of extreme permutations
