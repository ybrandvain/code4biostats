library(dplyr)
library(readr)
library(janitor)
library(ggplot2)
birds <- read_csv("https://raw.githubusercontent.com/ybrandvain/datasets/refs/heads/master/bird-diversity.csv") %>%
  clean_names() %>%
  mutate(log10_range_size = log10(breeding_range_size),
         log10_body_mass = log10(body_mass),
         big_range = log10_range_size > 2,
         big_mass  = log10_body_mass > 2)

group_by(birds, big_range, big_mass) %>% #het for big&small range&body
  summarise(mean_heterozygosity = mean(heterozygosity)) %>%
  arrange(desc(mean_heterozygosity))

filter(birds, migratory_status == "Migratory")%>% # Just for migrants 
  group_by(big_range, big_mass) %>%
  summarise(mean_heterozygosity = mean(heterozygosity)) %>%
  arrange(desc(mean_heterozygosity))

filter(birds, migratory_status == "Resident")%>%  # Just for nonmigrants
  group_by(big_range, big_mass) %>%
  summarise(mean_heterozygosity = mean(heterozygosity)) %>%
  arrange(desc(mean_heterozygosity))
