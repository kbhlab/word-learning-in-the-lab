#------------//------------
# author: Rodrigo Dal Ben
# date: "March, 16, 2021"
#
# This script loads the packages used in the analyses scripts of:
# Gonzalez-Barrero, A. M., Dal Ben, R., Killam, H., & Byers-Heinlein, K. (under review). **ADD TITLE!!!**
#  
# Feedback and suggestions: 
#   <dalbenwork@gmail.com>, 
#   <kbh.coordinator@concordia.ca>, 
#   <anamaria.gonzalezbarrero@mail.concordia.ca> 
#   <k.byers@concordia.ca>
#
#------------//------------
  
# load groundhog
#install.packages("groundhog")
library(groundhog) # version: 1.4.0

# list packages
gh_pkgs <- 
  c("here", 
    "tidyverse", 
    "tidylog", 
    "patchwork", 
    "eyetrackingR", 
    "lme4",
    "car", 
    "sjPlot", 
    "metafor",
    "janitor",
    "lubridate",
    "data.table",
    "readxl",
    "weights",
    "broom",
    "corrr",
    "ez",
    "ggpubr",
    "effectsize",
    "lmerTest",
    "pwr",
    "simr",
    "TOSTER",
    "parameters",
    "sjstats",
    "see")

# last update
gh_date <- "2021-08-01" # R-4.1.0

# load packages via groundhog
groundhog::groundhog.library(pkg = gh_pkgs, date = gh_date)

# avoid scientific notation
options(scipen = 999)

#------------//------------
# THE END
#------------//------------