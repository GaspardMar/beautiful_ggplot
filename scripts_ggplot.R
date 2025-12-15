library(tidyverse)
library(ggplot2)
library(ggpubr)
library(ggrepel)

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
# correlations circle
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

#-------------------------------------------------------------------------------
# Forest Plot
#-------------------------------------------------------------------------------

cm1 <- coxph(Surv(time.reach6.y, event) ~  
               Sex + Onset age (years) + First_relapse + OCB + Rituximab, 
             data = xx1)

# Tidy the model results using broom
model_res1 <- tidy(cm1, conf.int = TRUE)

# Add a new column for HR and round for readability
model_res1$HR <- exp(model_res1$estimate)
model_res1$HR <- round(model_res1$HR, 3)
model_res1$conf.low <- round(exp(model_res1$conf.low), 3)
model_res1$conf.high <- round(exp(model_res1$conf.high), 3)
model_res1$p_value <- summary(cm1)$coefficients[, "Pr(>|z|)"] 

# Create the forest plot using ggplot2 and add HR and CI text annotations
model_res1$significance <- ifelse(model_res1 $p_value < 0.001, "*",
                                  ifelse(model_res1$p_value < 0.01, "", 
                                         ifelse(model_res1$p_value < 0.05, "*", "")))

model_res1$HRstar <- paste0(model_res1$HR, model_res1$significance)

FP1 <- ggplot(model_res1, aes(x = term, y = HR, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange(size = .1, linewidth = .3) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", linewidth = .3) +
  coord_flip() +
  xlab("Variable") +
  ylab("Hazard Ratio (95% CI)") +
  ggtitle("Cox PH model for time to reach EDSS 6 in AQP4+") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 5, color = "black"), 
    axis.text.y = element_text(size = 5, color = "black"), 
    axis.title.y = element_blank(),
    plot.title = element_text(size = 5),
    axis.title.x = element_text(size = 5),
    panel.grid = element_line(linewidth = .2)
  ) +
  geom_text(aes(label = paste0("HR = ", HRstar, "\n(", conf.low, ", ", conf.high, ")")),
            position = position_dodge(width = 1), 
            hjust = -0.2, size = 1.6)  +
  scale_y_continuous(breaks = seq(-1, 12, 1), minor_breaks = seq(-1, 12, 1))

#-------------------------------------------------------------------------------
# to transform all categorical variables to factor
#-------------------------------------------------------------------------------

col_names <- c(
  "agreement_silver_MRI_Stephane", "agreement_silver_MRIandV1_Stephane", "agreement_silver_MRI_Goncalo",
  "agreement_silver_MRIandV1_Goncalo", "Interrateragreement_MRI", "Interrateragreement_MRIandV1",
  "agreement_silver_ML", "agreement_MRI_ML"
)
# replace by the categorical variables you want to trnafrom into factors

data[, col_names] <- lapply(data[, col_names], factor)

#-------------------------------------------------------------------------------
# ggalluvial
#-------------------------------------------------------------------------------

dat_traj %>% 
  dplyr::filter(Study == "study5") %>% 
  ggplot(aes(x = Genotype, y = percent, fill = trajectories))+
  geom_flow(aes(alluvium = trajectories), alpha= .9, 
            lty = 2, fill = "white", color = "black",
            curve_type = "linear", 
            width = .6) +
  geom_bar(stat = "identity", col = "black", width = 0.6)+
  #geom_col(aes(fill = trajectories), width = .5, color = "black") +
  geom_text(data = dat_traj %>% 
              dplyr::filter(Study == "study5" & percent!=0),
            aes(label = paste0(round(percent,0),"%")), 
            position = position_stack(vjust =  0.5),
            size = 4, col = "black")+
  theme_minimal()+
  theme(axis.text = element_text(color = "black"),
        panel.grid.major.x = element_blank(),
        legend.position = "bottom")+
  facet_wrap(~ Stage, nrow = 2)+
  labs(x = "",
       y = "Overall search strategies (%)")+
  scale_y_continuous(breaks = seq(0, 100, by = 10),
                     minor_breaks = seq(0, 100, by = 10))+
  scale_fill_manual(values = c(Chaining = "#BDD7E7",
                               Scanning = "#6BAED6",
                               Thigmotaxis = "#2171B5",
                               Random.search = "#08519C",
                               Direct.swim = "#BAE4B3",
                               Focal.search = "#74C476",
                               Direct.search = "#31A354",
                               Not.rec = "grey75"))

#-------------------------------------------------------------------------------
# area plot
#-------------------------------------------------------------------------------

dat_traj %>% 
  dplyr::filter(Study == "study5") %>%
  dplyr::group_by(Genotype, Stage, trajectories) %>%
  dplyr::summarise(percent = sum(percent, na.rm = TRUE)) %>%
  ggplot(aes(x = Stage, y = percent, fill = trajectories, group = trajectories)) +
  geom_area(position = "stack", show.legend = TRUE, col = "black")+
  facet_wrap(~ Genotype)+
  # geom_vline(data = dat_traj %>% dplyr::filter(Study == "study1" & Genotype == "Ctrl"), aes(xintercept = 5), col = "black", linetype = "dashed")+
  theme_minimal()+
  theme(axis.text = element_text(color = "black"),
        panel.grid = element_blank(),
        panel.margin = unit(1, "lines"))+
  labs(x = "",
       y = "Overall search strategies (%)")+
  scale_y_continuous(breaks = seq(0, 100, by = 10),
                     minor_breaks = seq(0, 100, by = 10),
                     expand = c(0,0))+
  scale_x_discrete(breaks = c("J1", "J2", "J3", "J4", "J5",
                              "J1(R)", "J2(R)", "J3(R)", "J4(R)", "J5(R)"),
                   expand = c(0,0))+
  scale_fill_manual(values = c(Chaining = "#BDD7E7",
                               Scanning = "#6BAED6",
                               Thigmotaxis = "#2171B5",
                               Random.search = "#08519C",
                               Direct.swim = "#BAE4B3",
                               Focal.search = "#74C476",
                               Direct.search = "#31A354",
                               Not.rec = "grey75"))
