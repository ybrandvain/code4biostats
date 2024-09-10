# Load packages
library(ggplot2)
library(dplyr)
library(ggrepel)
library(patchwork)

# Make figure a
a <-ggplot(filter(penguins, !is.na(sex)), aes(x = species, y = flipper_length_mm,color = species))+
  geom_jitter(size = 3, alpha = .4, show.legend = FALSE,height = 0, width = .3)+
  theme(axis.text = element_text(),
        axis.title.x= element_text(color = "white",face = "bold"),
        axis.text.x = element_text(color = "white",face = "bold",angle = 20,vjust = 0.65),
        axis.title.y= element_text(color = "white",face = "bold"),
        axis.text.y = element_text(color = "white",face = "bold"),
        axis.ticks = element_line(color = "white"),
        plot.title = element_text(color = "white",face = "bold"),
        plot.background = element_rect(fill= "black",color = "black"))+
  labs(title = "A) geom_jitter()")

# Make figure b
b <- ggplot(filter(penguins, !is.na(sex)), 
       aes(x =  flipper_length_mm,fill = species))+
  geom_histogram(show.legend = FALSE)+
  theme(axis.title.x= element_text(color = "white",face = "bold"),
        axis.text.x = element_text(color = "white",face = "bold",angle = 20,vjust = 0.65),
        axis.title.y= element_text(color = "white",face = "bold"),
        axis.text.y = element_text(color = "white",face = "bold"),
        plot.title = element_text(color = "white",face = "bold"),
        axis.ticks = element_line(color = "white"),
        plot.background = element_rect(fill= "black",color = "black"))+
  geom_label_repel(data = . %>% group_by(species) %>% summarise(flipper_length_mm = median(flipper_length_mm )),
                   aes(y = 33, label = species), show.legend = FALSE)+
  labs(y = "count")+
  labs(title = "B) geom_histogram()")

# Make figure c
c <- ggplot(filter(penguins, !is.na(sex)), 
       aes(x =  flipper_length_mm,fill = species))+
  geom_label_repel(data = . %>% group_by(species) %>% summarise(flipper_length_mm = median(flipper_length_mm )),
                   aes(y = .07, label = species), show.legend = FALSE)+
  geom_density(alpha = .85, show.legend = FALSE)+
  theme(axis.title.x= element_text(color = "white",face = "bold"),
        axis.text.x = element_text(color = "white",face = "bold",angle = 20,vjust = 0.65),
        axis.title.y= element_text(color = "white",face = "bold"),
        axis.text.y = element_text(color = "white",face = "bold"),
        axis.ticks = element_line(color = "white"),
        plot.title = element_text(color = "white",face = "bold"),
        plot.background = element_rect(fill= "black",color = "black"))+
  labs(y = "density")+
  labs(title = "C) geom_density()")

# Put them together with syntax from the patchwork package
a/(b+c)
