# YB Example code for proect one

library(knitr)     # For creating well-formatted tables
library(dplyr)     # For data manipulation and summarization
library(ggplot2)   # For data visualization
library(patchwork) # For combining ggplot objects into one figure
library(tidyr)     # For reshaping data frames, particularly pivoting data

##################################
##################################
### Summary stats
##################################
##################################

# Calculate the correlation between petal length and petal width for each species in the iris dataset
iris %>%
  group_by(Species) %>%
  summarise(cor_petal_WL = cor(Petal.Width, Petal.Length))

# Generate summary statistics (mean, standard deviation, coefficient of variation, and correlations)
# for petal width and length within each species
iris_summaries <- iris %>%
  group_by(Species) %>%
  summarise(mean_petal_W       = mean(Petal.Width),                   # Mean of petal width
            sd_petal_W         = sd(Petal.Width),                     # Standard deviation of petal width
            coef_var_petal_W   = 100 * sd_petal_W / mean_petal_W,     # Coefficient of variation for petal width (as %)
            mean_petal_L       = mean(Petal.Length),                  # Mean of petal length
            sd_petal_L         = sd(Petal.Length),                    # Standard deviation of petal length
            coef_var_petal_L   = sd_petal_L / mean_petal_W,           # Coefficient of variation for petal length
            cor_petal_WL       = cor(Petal.Width, Petal.Length))      # Correlation between petal width and length

# Print the summary statistics in a formatted table
kable(iris_summaries, digits = 2)

##################################
##################################
### Linear models
##################################
##################################

# Model 1: Fit a linear model predicting petal width as a function of petal length for the entire dataset

## 1a: Fit a simple linear regression model with petal width as the response and petal length as the predictor
lm(Petal.Width ~ Petal.Length, data = iris) %>% coef()

### Calculate R-squared for the model with only petal length as the predictor
iris %>%
  mutate(mean_petal_width = mean(Petal.Width),                             # Calculate the mean petal width
         predicted_petal_width = -0.363 + 0.416 * Petal.Length,            # Predict petal width using the model
         squared_diff_model_pred = (predicted_petal_width - mean_petal_width)^2,  # Squared differences for model predictions
         squared_diff_total = (Petal.Width - mean_petal_width)^2) %>%      # Total squared differences from the mean
  summarise(r2 = sum(squared_diff_model_pred) / sum(squared_diff_total))   # R-squared calculation

## 1b: Fit a linear regression model with petal width as the response, and petal length and species as predictors
lm(Petal.Width ~ Petal.Length + Species, data = iris) %>% coef()

### Calculate R-squared for the model with both petal length and species as predictors
iris %>%
  mutate(mean_petal_width = mean(Petal.Width),                                        # Calculate the mean petal width
         predicted_petal_width = -0.091 + 0.23 * Petal.Length +                      # Model predicted values
           0.43537 * as.numeric(Species == "versicolor") +                           # Add species effect for "versicolor"
           0.8377 * as.numeric(Species == "virginica"),                              # Add species effect for "virginica"
         squared_diff_model_pred = (predicted_petal_width - mean_petal_width)^2,     # Squared differences for model predictions
         squared_diff_total = (Petal.Width - mean_petal_width)^2) %>%                # Total squared differences from the mean
  summarise(r2 = sum(squared_diff_model_pred) / sum(squared_diff_total))             # R-squared calculation

# Model 2: Fit linear models for each species separately, predicting petal width as a function of petal length
lm(Petal.Width ~ Petal.Length, data = filter(iris, Species == "setosa")) %>% coef()      # Model for setosa species
lm(Petal.Width ~ Petal.Length, data = filter(iris, Species == "versicolor")) %>% coef()  # Model for versicolor species
lm(Petal.Width ~ Petal.Length, data = filter(iris, Species == "virginica")) %>% coef()   # Model for virginica species

##################################
##################################
### Figures
##################################
##################################

# Figure 1: Histograms showing the distribution of petal width and length for each species

iris %>%
  select(Species, contains("Petal")) %>%
  pivot_longer(cols = contains("Petal"), names_to = "trait", values_to = "size") %>%  # Reshape the data for plotting
  ggplot(aes(x = size, fill = Species)) +                                             # Plot histograms
  geom_histogram(color = "White") +                                                   # Add white border around bars
  facet_grid(Species ~ trait)                                                         # Facet by species and trait

# Figure 2: Scatter plot of petal width as a function of petal length, colored and shaped by species

ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Species, label = Species, shape = Species)) +
  geom_point(size = 3, alpha = .7) +                                                   # Plot points with species-specific colors and shapes
  geom_label(data = . %>%
               group_by(Species) %>%
               summarise(across(contains("Petal"), mean, na.rm = TRUE)),               # Add labels for mean petal sizes by species
             nudge_x = c(0, -1, -1.5), nudge_y = c(.5, 0, 0),                          # Adjust label positions
             size = 5, show.legend = FALSE, fontface = "bold") +
  theme_grey() +                                                                       # Apply grey theme
  scale_shape(guide = guide_legend(reverse = TRUE)) +                                  # Reverse the legend order for shape
  scale_colour_viridis_d(guide = guide_legend(reverse = TRUE)) +                       # Reverse the legend order for color
  geom_smooth(method = "lm", se = FALSE, linewidth = 2, show.legend = FALSE) +         # Add a species-specific regression line
  geom_smooth(aes(group = 1), method = "lm", se = FALSE, lty = 2, color = "black")     # Add a dashed regression line for all species combined
