# Load necessary libraries
library(ggplot2)
library(dplyr)
library(patchwork)

# Prepare the data
penguin_SS <- penguins %>%  
  filter(!is.na(sex) & !is.na(bill_length_mm)) %>%  # Remove rows with missing values for sex or bill length
  mutate(id = 1:n()) %>%                            # Create an 'id' column to uniquely identify each row
  select(id, sex, bill_length_mm) %>%               # Select only the necessary columns
  mutate(
    grand_mean     = mean(bill_length_mm),          # Calculate the grand mean of bill length
    total_error    = bill_length_mm - grand_mean,   # Calculate the total deviation from the grand mean
    prediction     = 42.097 + case_when(            # Create predictions based on sex (simple linear model)
      sex == "male"   ~ 3.758,
      sex == "female" ~ 0
    ),
    model_explains = prediction - grand_mean,       # Calculate the deviation explained by the model
    residual_error = bill_length_mm - prediction    # Calculate the residual error (unexplained by the model)
  )

# Create the first plot: Total Sum of Squares (SS_total)
a <- ggplot(penguin_SS, aes(x = id, xend = id, y = bill_length_mm, yend = grand_mean)) +
  geom_point() +                                       # Plot the actual bill length values
  geom_segment(linewidth = .2, color = "brown4") +     # Draw lines representing deviations from the grand mean
  labs(y = "Bill length(mm)", title = "(A) SS_total =") +
  facet_wrap(~sex, ncol=2) +                           # Separate plots by sex
  theme(plot.title = element_text(size= 20, color = "brown4"), 
        axis.title = element_text(size=20), 
        strip.text = element_text(size=20))

# Create the second plot: Model Sum of Squares (SS_model)
b <- ggplot(penguin_SS, aes(x = id, xend = id, y = bill_length_mm, yend = grand_mean)) +
  geom_point() +                                       # Plot the actual bill length values
  geom_segment(aes(y = prediction), linewidth = .2, color = "blue") +  # Draw lines from grand mean to predictions
  labs(y = "Bill length(mm)", title = "(B) SS_model +") +
  facet_wrap(~sex, ncol=2) +                           # Separate plots by sex
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        plot.title = element_text(size= 20, color = "blue"), 
        axis.title = element_text(size=20), 
        strip.text = element_text(size=20))

# Create the third plot: Error Sum of Squares (SS_error)
c <- ggplot(penguin_SS, aes(x = id, xend = id, y = bill_length_mm, yend = prediction)) +
  geom_point() +                                       # Plot the actual bill length values
  geom_segment(linewidth = .2, color = "orange") +     # Draw lines from predictions to actual values (residuals)
  labs(y = "Bill length(mm)", title = "(C) SS_error") +
  facet_wrap(~sex, ncol=2) +                           # Separate plots by sex
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        plot.title = element_text(size= 20, color = "orange"), 
        axis.title = element_text(size=20), 
        strip.text = element_text(size=20))

# Combine the three plots to visualize SS_total = SS_model + SS_error
a + b + c  # Assume we have the patchwork package loaded
