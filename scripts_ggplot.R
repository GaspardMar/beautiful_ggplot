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

#-------------------------------------------------------------------------------
# cercle des corrélations
#-------------------------------------------------------------------------------

circleFun <- function(center = c(-1,1), diameter = 1, npoints = 100) {
  r = diameter/2
  tt <- seq(0, 2*pi,length.out = npoints)
  xx <- center[1] + r*cos(tt)
  yy <- center[2] + r*sin(tt)
  return(data.frame(x = xx, y = yy))
}

circle <- rbind(circleFun(c(0,0), 2, npoints = 100), circleFun(c(0,0), 1, npoints = 100))

sp.cor <- data.frame(t(round(cor(df[,c("comp1","comp2")], X, use = "pairwise.complete.obs", method = "pearson"), 3)))
sp.cor$label <- rownames(sp.cor)

ggplot(sp.cor, aes(x = comp1, y = comp2)) +
  geom_path(aes(x, y), data = circle, col = "gray60", size = .2) + 
  theme_minimal() +
  geom_vline(xintercept = 0, col = "gray60", size = .3) + 
  geom_hline(yintercept = 0, col = "gray60", size = .3) +
  ggtitle("correlation circle") +
  geom_point(size = 1.6, shape = 21, color = "white", stroke = .3) +
  geom_text_repel(aes(label = label), vjust = 0.4, hjust = 0.4, nudge_y = 0.02,
                  size = 2.3, segment.size  = 0.2,
                  segment.color = "grey50", show.legend = FALSE) +
  xlab("Comp 1 : xxx % explained veriance") + 
  ylab("Comp 2 : xxx % explained variance") + 
  theme(axis.text = element_text(color = "black", size = 12), 
        axis.title = element_text(size = 12),
        legend.title = element_blank(), legend.text = element_text(size = 9),
        legend.spacing.x = unit(.1, 'cm'),
        legend.position = "bottom", legend.margin = margin(t = -8),
        plot.title = element_text(hjust = 0.5, size = 12),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = .3, color = "gray92")
  ) +
  scale_color_manual(values=c("darkred", "dodgerblue4", "springgreen4")) +
  scale_fill_manual(values=c("darkred", "dodgerblue4", "springgreen4")) +
  guides(color = guide_legend(override.aes = list(size = 3)))
