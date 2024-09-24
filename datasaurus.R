# Using the datasauraus data set to motivate exploratory plots
# Yaniv Brandvain
# Applied Biostats

# Load necessary packages
library(ggplot2)       # For creating plots
library(dplyr)         # For data manipulation
library(datasauRus)    # For the datasaurus dataset
library(patchwork)     # For combining plots

# 1. Summary statistics
# We'll calculate summary statistics for each dataset in the 'datasaurus_dozen' group
# This shows that even datasets with similar summary statistics can look very different when visualized.

summary_stats <- datasaurus_dozen %>% 
  group_by(dataset) %>% 
  summarize(
    mean_x    = mean(x),
    mean_y    = mean(y),
    std_dev_x = sd(x),
    std_dev_y = sd(y),
    corr_x_y  = cor(x, y)
  ) 

# 2. Plot Histograms for 'x'
# Histograms can help us understand the distribution of 'x' values in each dataset.
# Here we expect some datasets to have a normal distribution and others to deviate significantly.

plot_x_hist <- ggplot(datasaurus_dozen, aes(x = x)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  facet_wrap(~ dataset, ncol = 7) +
  ggtitle("Histogram of x") +
  theme_minimal()

# 3. Plot Histograms for 'y'
# Similar to 'x', visualizing the distribution of 'y' values can reveal patterns or anomalies.
# This complements the 'x' distribution by showing how the other variable behaves.

plot_y_hist <- ggplot(datasaurus_dozen, aes(x = y)) +
  geom_histogram(binwidth = 5, fill = "lightgreen", color = "black") +
  facet_wrap(~ dataset, ncol = 7) +
  ggtitle("Histogram of y") +
  theme_minimal()

# 4. Scatter Plot of 'x' vs 'y'
# Scatter plots are crucial for revealing relationships between two variables.
# Even though the summary statistics (mean, standard deviation, and correlation) are similar,
# the actual relationship between 'x' and 'y' can be dramatically different, as seen in the "Datasaurus Dozen."

plot_scatter <- ggplot(datasaurus_dozen, aes(x = x, y = y)) +
  geom_point(color = "darkorange") +
  facet_wrap(~ dataset, ncol = 7) +
  ggtitle("Scatter Plot of x vs y") +
  theme_minimal()

# 5. Combine the plots vertically using the 'patchwork' package
# Patchwork allows us to arrange plots in a more organized manner.
# Here, we will combine the histograms of 'x', 'y', and the scatter plot into a single vertical figure.

combined_plot <- (plot_x_hist / plot_y_hist / plot_scatter) +
  plot_annotation(title = "Visualizing Datasaurus Dozen: Why Visualizing Data Matters")

# Display the combined plot
print(combined_plot)
