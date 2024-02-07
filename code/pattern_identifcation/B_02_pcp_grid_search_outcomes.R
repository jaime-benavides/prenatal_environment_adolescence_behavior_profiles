# First step to load packages etc.
rm(list=ls())
library(rvest)
library(qdapRegex)
library(stringi)
library(rexposome)
# for PCP
library(PCPhelpers)
library(gridExtra)
library(pcpr)
library(foreach)
library(tictoc) # for timing
library(plotly)    # for visualizing the gridsearches
library(progressr) # needed for progress bars with the new PCP gridsearch
library(tidyverse)

# 1a Declare root directory, folder locations and load essential stuff
project.folder = paste0(print(here::here()),'/')
source(paste0(project.folder,'0_01_init_directory_structure.R'))
source(paste0(functions.folder,'script_initiate.R'))


# PCP 
# second vanilla search
mon <- 192 # 16 years old
na_level <- 75 #  inclusion criterion of percent missingness in behavioral data
scale <- "TRUE"
# read behavioral outcomes (generated at A_06_put_together_outcomes.R)
data <- readRDS(paste0(generated.data.folder, "data_outc_",mon/12, "_yrs_na_", na_level ,"_scale", "_", scale,".rds"))
# read description
outc_desc_loc <- readRDS(paste0(generated.data.folder, "desc_outc_",mon/12, "_yrs_na_", na_level ,".rds"))

# run pcp grid search to discover optimal hyperparameters (both rank and eta levels)
etas <- seq(0.05,0.15, length.out=11) # this is hyperparameter eta
rank <- 11 # this is the maximum number of ranks the grid search will be searching
rrmc_grid <- expand.grid(eta = etas, r = rank) # this creates the grid for different configurations to search
runs = 35 # number of iterations
LOD = rep(0, ncol(data$M)) # lods in this case all are equal to zero; we add them because are needed for the algorithm to run
perc_test = 0.10 # percent to leave out for testing 
cores = parallel::detectCores(logical = F) /2 # number of cores
# 3b. Run gridsearch:
with_progress(expr = {
  rrmc_results <- vanilla_search(
    cores = cores,
    mat = data$M, 
    pcp_func = RRMC, 
    grid = rrmc_grid,
    LOD = LOD,
    perc_test = perc_test,
    runs = runs,
    save_as = paste0(generated.data.folder,"pcp_outcomes_vanilla_", mon/12, "_yrs_na_", na_level ,"_scale", "_", scale, "rev_scs")
  )
})
# # read results
rrmc_results <- readRDS(paste0(generated.data.folder,"pcp_outcomes_vanilla_", mon/12, "_yrs_na_", na_level ,"_scale", "_", scale, "rev_scs.rds"))

# # # 3c. The best parameter setting according to relative error (the lower the better) and sparsity (about 99)...  r = 3, eta = 0.11
rrmc_results$summary_stats %>% slice_min(rel_err)
# # 
# # # 3d. Visualizing the whole gridsearch:
plot_ly(data = rrmc_results$summary_stats, x = ~eta, y = ~r, z = ~rel_err, type = "heatmap")
# # sparsities
plot_ly(data = rrmc_results$summary_stats, x = ~eta, y = ~r, z = ~S_sparsity, type = "heatmap")
# the optimal configuration is the one below (rank 3 and eta level 0.11)
# # run PCP 
pcp_outs <- RRMC( data$M, r = 3, eta = 0.11, LOD = LOD)
saveRDS(pcp_outs, file = paste0(generated.data.folder, "pcp_rrmc_outcome_", mon/12, "_yrs_na_", na_level, "scale", scale, "_rev_scs.rds"))
