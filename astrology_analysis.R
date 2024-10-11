# Yaniv Brandvain
# Sept 9
# Exploring astrology data

# Load necessary libraries
library(readr)      # For reading CSV files
library(dplyr)      # For data manipulation using the pipe operator and functions like rename()
library(janitor)    # For cleaning data, such as renaming columns to more manageable names
library(ggplot2)    # For creating static plots
library(plotly)     # For creating interactive plots from ggplot2 objects


###### YOU DONT HAVETHIS INSTALLED YET
library(mosaic)
######


# Read in the dataset from a CSV file
# Replace the file path with the correct path to the CSV file on your system
astrology <- read_csv("https://raw.githubusercontent.com/ybrandvain/datasets/refs/heads/master/astrology.csv")

# Clean up the column names to make them more manageable and readable
# The rename() function from dplyr allows us to rename columns in a more convenient way
astrology <- astrology %>%
  rename(run_num = `Run number`,  # Rename 'Run number' to 'run_num'
         n_correct = correctAnswersOnChallengeOutOf12,  # Shorten column name for number of correct answers
         pre_challenge = preChallengeNumberTheyThoughtTheyWouldGetCorrect,  # Expected correct answers before the challenge
         post_challenge = postChallengeNumberTheyThoughtTheyGotCorrect,  # Expected correct answers after the challenge
         experience_num = astrologicalExperienceNumerical,  # Numerical measure of astrological experience
         experience_qual = astrologicalExperienceLabel,  # Qualitative measure of astrological experience
         astrologer = classificationBasedOnWhetherHasAstrologicalExperienceAndBelievesWillDoBetterThanChance)  # Classification based on experience and belief in astrology


# Make a plot 
# Here I'm showing off an alternative approach to dealing with overplotiting
# Make data points big,
# Weight them in your line
astrology %>%
  group_by(astrologer,pre_challenge,post_challenge)%>%
  dplyr::tally() %>%
  ggplot(aes(x = pre_challenge, y= post_challenge, weight = n))+
  geom_point(aes(size=n))+
  geom_smooth(method = "lm")+
  facet_wrap(~astrologer)
  

# Run a linear model
lm(post_challenge ~ pre_challenge, data = astrology)

