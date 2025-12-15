library(tidyverse)
library(ggplot2)
library(ggpubr)

#-------------------------------------------------------------------------------
# Histogram with chi-square or fisher exact test
#-------------------------------------------------------------------------------

data %>% 
  dplyr::group_by(x_variable,
                  color_variable) %>% 
  dplyr::summarise(freq = n()) %>% 
  ggplot(aes(x = x_variable, 
             fill = color_variable,
             weight = freq, by = x_variable))+
  geom_bar(color = "white")+
  geom_text(aes(y = freq, label = freq), position = position_stack(vjust = 0.5), size = 5) +
  theme_minimal()+
  theme(strip.text = element_text(size = 15))+
  theme(
    axis.text=element_text(size = 11, color = "black"),
    axis.title=element_text(size = 12, color = "black"))+
  annotate(geom = "text", x = 1, y = 15, label = "Fisher p = ", size = 6)+
  labs(fill = "",
       x = "",
       y = "")+
  scale_fill_manual(values = c("#5F9EA0", "#E1B378"))
