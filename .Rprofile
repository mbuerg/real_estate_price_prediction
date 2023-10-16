source("renv/activate.R")
library(ggplot2)
library(docstring)

# Theme für plots:
# https://bookdown.org/content/2015/figures.html
apatheme=theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        text=element_text(family='Times'),
        legend.title=element_blank(),
        axis.text.y=element_text(size = 12),
        axis.text.x=element_text(size = 12))


path_data_train <- here::here("data", "train.csv")
path_data_test <- here::here("data", "test.csv")
path_data_sample_submission <- here::here("data", "sample_submission.csv")
path_data_complete_imputated <- here::here("data", "data_complete_imputated.csv")
