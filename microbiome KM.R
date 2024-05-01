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

library(janitor) # clean_names
library(readxl) # read excel, duh!
library(data.table) # magical data frames
library(magrittr) # pipes
library(stringr) # string functions
library(forcats) # factor functions


#install.packages(c("lubridate", "ggsurvfit", "gtsummary", "tidycmprsk"))
library(lubridate)
library(ggsurvfit)
library(gtsummary)
library(tidycmprsk)

#devtools::install_github("zabore/condsurv")
#library(condsurv)

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
library(survival)
library(knitr)
library(tibble)

getwd()

## Call ggsurvfit ready .csv ##
#Event data (Censored[0] and Deaths[1])
hfb.km <- read.csv("./Data/HFBmicrobiome KM.csv")

names(hfb.km)

hfb.km[which(hfb.km$week == max(hfb.km$week)),]

#Data Tables#

km.model <- survfit(data = hfb.km, Surv(week, death) ~ group)
kmf <- survfit(data = hfb.km[which(hfb.km$sex == "female"),], Surv(week, death) ~ group)
kmm <- survfit(data = hfb.km[which(hfb.km$sex == "male"),], Surv(week, death) ~ group)

## Order Controls then Exp ##
hfb.km$group <- factor(hfb.km$group, levels = c("group 4", "group 3"))

## Plot Females ##

fmbiome.fit <- survfit2(Surv(week, death) ~ group, data = hfb.km[which(hfb.km$sex == "female"),]) %>%
  ggsurvfit(linewidth = 2) +
  scale_color_manual(values = c("darkgoldenrod2", "purple"), 
                     labels = c("HFB", "HFBN")) +
  geom_vline(xintercept = c(26, 52, 73), color = "black", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = 64, col = "red", linewidth = 1, linetype  = 2) +
  labs(
    x = "Weeks",
    y = "Overall Survival Probability",
    title = "Females") +
  scale_ggsurvfit(y_scales = list(breaks = seq(.4, 1, by = .2),
                                  limits = c(.4, 1)),
                  x_scales = list(limits = c(0, 73),
                                  breaks = c(4, 24, 48, 72))) +
  add_pvalue(location = "annotation", y = .6, x = 10,
             size = 5) +
  theme(axis.title.x = element_text(size = 24),
        axis.text.x = element_text(size = 16),
        axis.title.y = element_text(size = 24),
        axis.text.y = element_text(size = 16),
        legend.text = element_text(size = 18),
        title = element_text(size = 28),
        plot.title = element_text(hjust = .5))
fmbiome.fit

## Plot Males ##

mmbiome.fit <- survfit2(Surv(week, death) ~ group, data = hfb.km[which(hfb.km$sex == "male"),]) %>%
  ggsurvfit(linewidth = 2) +
  scale_color_manual(values = c("darkgoldenrod2", "purple"),
                     labels = c("HFB", "HFBN")) +
  geom_vline(xintercept = c(26, 52, 73), color = "black", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = 64, col = "red", linewidth = 1, linetype  = 2) +
  labs(
    x = "Weeks",
    y = element_blank(),
    title = "Males") +
  scale_ggsurvfit(y_scales = list(breaks = seq(.4, 1, by = .2),
                                  limits = c(.4, 1)),
                  x_scales = list(limits = c(0, 73),
                                  breaks = c(4, 24, 48, 72))) +
  add_pvalue(location = "annotation", y = .6, x = 10,
             size = 5) +
  theme(axis.title.x = element_text(size = 24),
        axis.text.x = element_text(size = 16),
        axis.title.y = element_text(size = 24),
        axis.text.y = element_text(size = 16),
        legend.text = element_text(size = 18),
        title = element_text(size = 28),
        plot.title = element_text(hjust = .5))
mmbiome.fit

## Merge Figures

fmbiome.fit + mmbiome.fit +
  patchwork::plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        axis.title = element_text(size = 15))

## Log-Rank Test ##
#Females
survdiff(formula = Surv(week, death) ~ group, data = subset(hfb.km, sex == "female"))

#Males
survdiff(formula = Surv(week, death) ~ group, data = subset(hfb.km, sex == "male"))

