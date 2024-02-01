# First step to load packages etc.
rm(list=ls())
library(rvest)
library(qdapRegex)
library(stringi)
library(rexposome)
# for PCP
library(PCPhelpers)
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

# load prepared exposure data
exposures <- readRDS(paste0(generated.data.folder,"exposures.rds"))

# filter only those moms enrolled in the MN cohort
mn_cohort_participants_sids <- readRDS(paste0(generated.data.folder, "mn_cohort_participants_sids.rds"))
exposures <- exposures[which(exposures$SID %in% mn_cohort_participants_sids),]

# save exposures data for later use as predictors in inputation dataset
saveRDS(exposures, paste0(generated.data.folder, "exposures_for_profiles_rev_shs_rev_valid_part.rds"))
exposures <- readRDS(paste0(generated.data.folder, "exposures_for_profiles_rev_shs_rev_valid_part.rds"))
### eda 
## prenatal period
ms <- unique(exposures$month)
m <- 1 # prenatal

expo_per_raw <- exposures[exposures$month == ms[m],]

# obtain sid for visit 16 years old (for age range paper)
rownames(expo_per_raw) <- expo_per_raw$SID
expo_per_raw_vars <- expo_per_raw[,-c(1,2)]

# delete empty variables (those with no subjects prenatal visit)
expo_per_raw <- expo_per_raw[,-c(as.numeric(which((colSums(is.na(expo_per_raw))/nrow(expo_per_raw)) == 1)))]
# apply criteria of missing data per row and column
tot_subj <- nrow(expo_per_raw)

# create the exposure social stress variables
expo_per_raw$demoralization <- rowSums(expo_per_raw[,c(paste0("L", sprintf('%0.2d', 1:27)))], na.rm = T)
expo_per_raw$material_hardship <- rowSums(expo_per_raw[,c(paste0("A", 10:17))], na.rm = T)
# add to description the new variables 
add_desc <- data.frame(variable_name = c("demoralization", "material_hardship", "E10"), 
                       variable_description = c("total demoralization", "total material_hardship", "questionnaire second-hand smoke"), 
                       exposure = c("demoralization", "material_hardship", "tobacco_exposure"), 
                       month = c(0, 0, 0), 
                       family =  c("demoralization", "material_hardship", "second_hand_smoke"),
                         table_name = c("prenatal", "prenatal", "tobacco / cotinine"), 
                       table_description = c("prenatal questionnaire", "prenatal questionnaire", "tobacco questionnaire and cotinine measurements"))

# delete variables of items for demoralization and material hardship
expo_per_raw <- expo_per_raw[,-which(grepl('^A', colnames(expo_per_raw)) | grepl('^L', colnames(expo_per_raw)))] # starts with AB?
exposure_description <- exposure_description[-which(grepl('^A', exposure_description$variable_name) | grepl('^L', exposure_description$variable_name)), ]

# check if any smokers in the cohort
sid_smokers <- expo_per_raw[which(expo_per_raw$E01 == 1), "SID"] # 14 participants have reported to be active smokers and + 27 have cotinine levels higher than 1 
# delete unused tobacco smoke variables
expo_per_raw <- expo_per_raw[,-c(4,5)]
# plot nas (used to inform ##Figure 1)
nas_perc <- (colMeans(is.na(expo_per_raw)))*100
nas_perc <- nas_perc[-c(1,2)]
x <- nas_perc
x <- x[order(x, decreasing = TRUE)]
x <- as.data.frame(round(x, 2))
pdf(paste0(output.folder, "prenatal_exposure_na_orig_data_matrix_n_727_rev_grav_corr_rev_shs_rev_valid_part.pdf"), height=15, width=20)
grid.table(x)
dev.off()

na_tol <- 50 # cut-off of data missingness
expo_per_row_comp <- expo_per_raw[-which((rowMeans(is.na(expo_per_raw)))*100 > na_tol),]
nrow(expo_per_row_comp) / tot_subj
if(any(which((colMeans(is.na(expo_per_row_comp)))*100 > na_tol))){
expo_per_row_comp <- expo_per_row_comp[,-which((colMeans(is.na(expo_per_row_comp)))*100 > na_tol)]
}
expo_per <- expo_per_row_comp
expo_per_id_month <- expo_per[,c("SID", "month")]

# reduced 50% max na / row and column 438 subjects

# update data description (this is an object to keep track of naming and description for variables)
expo_desc_loc <- exposure_description[
  which(exposure_description$month == unique(expo_per$month) & # todo: generalize to sid and month any order
          exposure_description$exposure %in% colnames(expo_per[,-which(colnames(expo_per) %in% c("SID", "month"))])),]
expo_desc_loc <- expo_desc_loc[!duplicated(expo_desc_loc$variable_name), ]
expo_desc_loc <- rbind(expo_desc_loc, add_desc)


# restructure exposure data matrix for pcp ingest
expo_per <- expo_per[,-c(which(colnames(expo_per)=="month"))]
colnames(expo_per)[which(colnames(expo_per)=="SID")] <- "id"
rownames(expo_desc_loc) <- expo_desc_loc[, 3]
expo_desc_loc <- expo_desc_loc[ , -3]
rownames(expo_per) <- expo_per[ , which(colnames(expo_per)=="id")]
expo_per <- expo_per[ , -c(which(colnames(expo_per)=="id"))]


## Table S3
# build matrix LODs (from MN codebook) lod values are needed for pcp
# lod matrix values are zero for missing lod and higher than zero for reported lod
lods_names <- c("madducts", "MEHHP","MECPP","MEOHP","MEHP","MCPP","MIBP","MBP","MBZP","MEP",
                "UBPA", "cadducts")
lods_values <- c(0.25, 0.7, 0.6, 0.7, 1.2, 0.2, 0.3, 0.6, 0.216, 0.528,
                 0.4, 0.25) 

# assign lod values to vector in the same order as exposure matrix
all_var_names <- colnames(expo_per)
all_lod_values <- c(rep(0, length(all_var_names)))
names(all_lod_values) <- all_var_names
all_lod_values[match(lods_names,names(all_lod_values))] <- lods_values

# scale using standard deviation
my.min <- unlist(lapply(expo_per,function(x) min(x,na.rm=T))) 
my.max <- unlist(lapply(expo_per,function(x) max(x,na.rm=T))) 
non_scale <- which(my.min == 0 & my.max == 1)
# scale by standard deviation of same variable (exclude variables ranging from 0 to 1)
# make <LOD NA so they dont affect the scaling
expo_per_no_lod <- expo_per
for(p in 1:ncol(expo_per_no_lod)){
 lod_loc <- all_lod_values[which(names(all_lod_values) == colnames(expo_per_no_lod)[p])]
 expo_per_no_lod[,colnames(expo_per_no_lod)[p]][expo_per_no_lod[,colnames(expo_per_no_lod)[p]] < lod_loc] <- NA
}


# Get stand dev of values > LOD
non_scale_pos <- which(colnames(expo_per) %in% names(non_scale))
denoms <- apply(expo_per_no_lod[,-non_scale_pos], 2, function(a) sd(a, na.rm = T))

scal_vars <-colnames(expo_per)[-non_scale_pos]
expo_per_scaled = mapply(`/`, expo_per[,-non_scale_pos], denoms)

expo_prep <- cbind(expo_per_id_month, expo_per[,non_scale], expo_per_scaled)
colnames(expo_prep)[3] <- "SHS" # this variable was missing the name, so I add it manually
# scale lods
all_lod_values[match(lods_names,names(all_lod_values))] <- all_lod_values[match(lods_names,names(all_lod_values))] / 
  denoms[match(names(all_lod_values[match(lods_names,names(all_lod_values))]),names(denoms))]

# save exposure matrix ready for pcp running
saveRDS(expo_prep, paste0(generated.data.folder, "data_row_comp_na_", na_tol, "_reduced_rev_grav_corr_rev_shs_rev_valid_part.rds"))
# run pcp grid search 
# prepare data matrix
data <- list("M" = expo_prep[,-c(which(colnames(expo_prep) %in% c("SID", "month")))]) %>% purrr::map(as.matrix)
# second vanilla search
etas <- seq(0,0.3, length.out=11) # this is hyperparameter eta
rank <- 11 # this is the maximum number of ranks the grid search will be searching
rrmc_grid <- expand.grid(eta = etas, r = rank) # this creates the grid for different configurations to search
runs = 35 # number of iterations
perc_test = 0.10 # percent to leave out for testing 
LOD = all_lod_values # lods
cores = parallel::detectCores(logical = F) /2 # number of cores for computation
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
    save_as = paste0(generated.data.folder, "rrmc_vanilla_results_na_", na_tol, "_scale_TRUE_grav_corr_rev_SHS_rev_valid_part")
  )
})
# read results
rrmc_results <- readRDS(paste0(generated.data.folder, "rrmc_vanilla_results_na_", na_tol, "_scale_TRUE_grav_corr_rev_SHS_rev_valid_part.rds"))
# 
# # 
# # # 3c. The best parameter setting according to relative error (the lower the better) and sparsity (about 99)...  r = 3, eta = 0.12
rrmc_results$summary_stats %>% slice_min(rel_err)
# # 
# # # 3d. Visualizing the whole gridsearch:
plot_ly(data = rrmc_results$summary_stats, x = ~eta, y = ~r, z = ~rel_err, type = "heatmap")
# # sparsities
plot_ly(data = rrmc_results$summary_stats, x = ~eta, y = ~r, z = ~S_sparsity, type = "heatmap")
# the optimal configuration is the one below (rank 3 and eta level 0.12)
pcp_outs <- RRMC(data$M, r = 3, eta = 0.12, LOD = LOD)
saveRDS(pcp_outs, file = paste0(generated.data.folder, "pren_exposures_pcp_rrmc_na_", na_tol, "_scale_TRUE_grav_rev_SHS_rev_valid_part.rds"))


