install.packages("ggdark")
install.packages("ggplot2")
install.packages("readxl")
install.packages("reshape2")
install.packages("matrixStats")
install.packages("dplyr")
install.packages("AICcmodavg")
install.packages("tidyverse")
install.packages("ggpubr")
install.packages("rstatix")
install.packages("emmeans")
install.packages("lme4")
# Data Read In

library(ggplot2)
library(readxl)
library(reshape2)
library(matrixStats)
library(dplyr)
library(AICcmodavg)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(ggdark)
library(emmeans)
library(lme4)

### Locally Estimated Scatterplot Smoothing (LOESS) ###
smooth.beef <- read.csv("./Data/smooth.beef.csv")

#Add facet labels#

smooth.beef$series <- paste(smooth.beef$group," ",smooth.beef$sex,"", sep = "")

## Calculate weekly Mean and Median Mass ##
sum_smooth.beef <- smooth.beef %>%
  group_by(sex, group, week) %>%
  summarise("mean" = mean(weight), "median" = median(weight), "sd" = sd(weight), n = n())

sum_smooth.beef
sum_smooth.beef$series <- paste(sum_smooth.beef$group, " ", sum_smooth.beef$sex, "", sep = "")

sum_smooth.beef <- data.frame(sum_smooth.beef)

sum_smooth.beef$mean <- round(sum_smooth.beef$mean, digits = 1)

## Facet Titles ##
sex.label <- c("Female", "Male")
names(sex.label) <- c("female", "male")

## LOESS Plot ##
ggplot(data = smooth.beef, aes(week, weight, group = series, col = group)) +
  geom_smooth(linewidth = .8) +
  facet_wrap(facets = "sex", labeller = labeller(sex = sex.label), ncol = 1) +
  geom_point(data = sum_smooth.beef, aes(y = mean, x = week, group = series), size = .5) +
  scale_x_continuous(limits = c(0, 72),
                     breaks = c(4, 24, 48, 72)) +
  scale_y_continuous(limits = c(14, 45)) +
  geom_vline(xintercept = 64, col = "red", linetype = 2, linewidth = 1) +
  scale_color_manual(label = c("HFBN", "HFB"),
                     values = c("purple", "darkgoldenrod2")) +
  labs(title = "Weekly Total Mass",
       y = "Mean Mass (g)", x = "Age (weeks)",
       color = "Diets:"       ) +
  theme(plot.title = element_text(family = "Fira Sans Condensed", hjust = 0.5, size = 20),
        plot.background = element_rect(fill = "white"),
        panel.border = element_rect(linetype = "solid", fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_blank(),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 18),
        axis.title.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.text = element_text(size = 15, color = "black"),
        axis.ticks = element_blank(),
        legend.key = element_blank(),
        legend.direction = "horizontal",
        legend.key.size = unit(1, "cm"),
        legend.position = "bottom")
