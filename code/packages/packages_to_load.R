############ 1. Packages on CRAN ############

# list of packages to use (in alphabetical order and 5 per row)
list.of.packages = c('PCPhelpers', 'pcpr', 'ggridges', 'foreach', 'tictoc', 'progressr', 'httr', 'RCurl', 'vtable',
                    'haven', 'RColorBrewer','data.table','dplyr','gamm4','mgcv', 'rvest', 'qdapRegex', 'stringi',
                    'naniar','rpart','rpart.plot','pROC', 'ggfortify', 'psych', 'GPArotation',
                     'devtools','ggplot2','grid','gridExtra','lubridate', 'multiApply',
                     'plyr', 'psych','raster','readr','reshape','reshape2','rgdal',
                     'scales','stringr','splines','tidyverse', 'tidyr', 'tidycensus', 
                     'viridis', 'zoo', 'lme4', 'ICC', 'corrplot', 'janitor', 'purrr')
                     

# check if list of packages is installed. If not, it will install ones not yet installed
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,repos = "https://cloud.r-project.org")

# load packages
lapply(list.of.packages, require, character.only = TRUE)


