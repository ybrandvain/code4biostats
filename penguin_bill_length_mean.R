# Load necessary libraries
library(dplyr)          # For data manipulation
library(ggplot2)        # For plotting
library(palmerpenguins) # For the penguins dataset
library(patchwork)      # For combining multiple ggplots

# Create the first plot 'a': A boxplot of bill lengths for all penguins
a <- ggplot(penguins, aes(x = "all penguins", y = bill_length_mm, group = "x")) +
  geom_boxplot() +                                        # Add a boxplot to show distribution
  geom_jitter(width = .1, height = 0, size = 2, alpha = .4) + # Add jittered points for individual data visibility
  stat_summary(fun = "mean", color = "red", size = 3, alpha = .5) + # Add a red dot representing the mean
  labs(title = "Mean", subtitle ="")                      # Set the title of the plot

# Create the second plot 'b': A scatter plot of bill lengths with lines connecting each point to the mean
b <- penguins %>% 
    mutate(id = 1:n()) %>%                                # Add an ID column to identify each point
  ggplot(aes(x = id, y = bill_length_mm)) +
  geom_point() +                                          # Add points for each penguin's bill length
  geom_segment(aes(xend = id, yend = mean(bill_length_mm, na.rm=TRUE)), linewidth = .2) + # Add lines connecting points to the mean
  labs(y = "Bill length (mm)", title = "Data points", subtitle = "Lines connect points to predictions") # Set labels

# Create the third plot 'c': Residuals plot showing deviations from the mean
c <- penguins %>% 
    mutate(id = 1:n(),
           residuals = bill_length_mm - mean(bill_length_mm, na.rm=TRUE)) %>% # Calculate residuals
  ggplot(aes(x = id, y = residuals)) +
  geom_point() +                                          # Plot residuals as points
  labs(y = "Bill length (mm)", title = "Residuals", subtitle ="") # Set labels

# Combine the three plots into one layout
library(patchwork)
a + b + c + plot_layout(widths = c(2, 5, 5))              # Arrange plots 'a', 'b', and 'c' with specified widths
