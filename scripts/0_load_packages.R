if (!require("pacman")) install.packages("pacman")

pkgs <- c("here",
          "tidyverse",
          "lubridate",
          "ggplot2",
          "cowplot",
          "ggdist",
          "readxl",
          "gtsummary",
          "tidymodels",
          "caret",
          "modeldata",
          "vip",
          "glmnet",
          "xgboost",
          "janitor",
          "corrplot",
          "themis",
          "ranger",
          "kernlab",
          "pROC",
          "ROCR",
          "ggridges")

pacman::p_load(pkgs, character.only=T)
