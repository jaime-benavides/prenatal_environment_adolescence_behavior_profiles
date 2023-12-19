# First step to load packages etc.
rm(list=ls())
.libPaths(c(.libPaths(), "/home/jbenavides/R/x86_64-pc-linux-gnu-library/4.1"))
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
df_mh <- readRDS(paste0(generated.data.folder, "material_hardship.rds"))
df_els <- readRDS(paste0(generated.data.folder, "mother_perceived_stress.rds"))
df_nq <- readRDS(paste0(generated.data.folder, "neighborhood_quality.rds"))
df_ipv <- readRDS(paste0(generated.data.folder, "intimate_partner_violence.rds"))
df_ss <- readRDS(paste0(generated.data.folder, "social_support.rds"))
df_dem <- readRDS(paste0(generated.data.folder, "demoralization.rds"))
df_disc <- readRDS(paste0(generated.data.folder, "discrimination.rds"))
df_smk <- readRDS(paste0(generated.data.folder, "smoke_cot_derivative_self_report.rds"))
df_bpa <- readRDS(paste0(generated.data.folder, "bisphenols_specific_gravity_corr.rds"))
df_bpa <- df_bpa[,-which(colnames(df_bpa) %in% c("M1SG","specific_gravity_corr"))]
df_phthal <- readRDS(paste0(generated.data.folder, "phthalates_specific_gravity_corr.rds")) # todo: rerun profiles with corrected
df_phthal <- df_phthal[,-which(colnames(df_phthal) %in% c("M1SG","specific_gravity_corr"))]
df_phthal_lod <- readRDS(paste0(generated.data.folder, "phthalates_below_lod.rds"))
df_smk <- df_smk[,c("SID", "month", "E10", "E01", "smk_self")]
colnames(df_smk)[3] <- "SHS"
df_pah <- readRDS(paste0(generated.data.folder, "pah_total.rds"))
df_pah <- df_pah[,c("SID", "month", "totalpah")]
df_pbde <- readRDS(paste0(generated.data.folder, "pbde_imp.rds"))
df_pah_adducts <- readRDS(paste0(generated.data.folder, "pah_adducts.rds"))
df_pah_adducts <- df_pah_adducts[,-which(colnames(df_pah_adducts) == "madducts_below_lod")]
exposure_description <- readRDS(paste0(generated.data.folder, "exposure_description.rds"))
# create a dataframe that includes all the combinations of SID and month contained in the above datasets
sids <- unique(c(df_mh$SID, df_els$SID, df_nq$SID, df_ipv$SID, df_ss$SID, df_dem$SID, df_disc$SID, df_smk$SID,df_pah$SID, df_bpa$SID, df_phthal$SID, df_pbde$SID))
sids <- sids[order(sids)]
months <- unique(c(df_mh$month, df_els$month, df_nq$month, df_ipv$month, df_ss$month, df_dem$month, df_disc$month, df_smk$month,df_pah$month, df_bpa$month, df_phthal$month, df_pbde$month))
months <- months[order(months)]
exposures <- data.frame()
for(i in 1:length(sids)){
  df_loc <- data.frame(SID = sids[i], month = months)
  exposures <- rbind(exposures, df_loc)
}
exposures <- exposures %>%
  dplyr::left_join(df_mh, by = c("SID", "month")) %>%
  dplyr::left_join(df_els, by = c("SID", "month"))  %>%
  dplyr::left_join(df_nq, by = c("SID", "month")) %>%
  dplyr::left_join(df_ipv, by = c("SID", "month"))  %>%
  dplyr::left_join(df_ss, by = c("SID", "month")) %>%
  dplyr::left_join(df_dem, by = c("SID", "month"))  %>%
  dplyr::left_join(df_disc, by = c("SID", "month"))  %>%
  dplyr::left_join(df_smk, by = c("SID", "month")) %>%
  dplyr::left_join(df_pah, by = c("SID", "month")) %>%
  dplyr::left_join(df_bpa, by = c("SID", "month")) %>%
  dplyr::left_join(df_phthal, by = c("SID", "month")) %>%
  dplyr::left_join(df_pbde, by = c("SID", "month")) %>%
  dplyr::left_join(df_pah_adducts, by = c("SID", "month"))

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

# todo: add next steps that led to generate summary variables for both demoralization and material hardship to exposure data preparation
expo_per_raw$demoralization <- rowSums(expo_per_raw[,c(paste0("L", sprintf('%0.2d', 1:27)))], na.rm = T)
expo_per_raw$material_hardship <- rowSums(expo_per_raw[,c(paste0("A", 10:17))], na.rm = T)

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
# plot nas 
nas_perc <- (colMeans(is.na(expo_per_raw)))*100
nas_perc <- nas_perc[-c(1,2)]
x <- nas_perc
x <- x[order(x, decreasing = TRUE)]
x <- as.data.frame(round(x, 2))
pdf(paste0(output.folder, "prenatal_exposure_na_orig_data_matrix_n_727_rev_grav_corr_rev_shs_rev_valid_part.pdf"), height=15, width=20)
grid.table(x)
dev.off()

x.max <- max(x, na.rm = T)
if(x.max > 100) x.max <- 100
if(x.max < 0) x.max <- 1
# var_names <- names(nas_perc)[-c(1,2)]

# dt <- data.frame(var_names = var_names, nas_perc = nas_perc)
png(paste0(output.folder, "prenatal_exposure_missinges_reduced_grav_corr_rev_shs_rev_valid_part.png"), 900, 460)
plot <- ggplot2::ggplot(data.frame(x),
                        ggplot2::aes(seq_along(x), x, fill = x)) +
  ggplot2::geom_bar(stat = "identity", width = 1)
plot <- plot + ggplot2::theme_bw() + ggplot2::xlim(names(x))
plot <- plot + ggplot2::scale_fill_continuous(name = "%",
                                              breaks = seq(0, 100, 20),
                                              limits = c(0, 100), low="violet", high="violetred4")
plot <- plot + ggplot2::geom_hline(yintercept=25, linetype="dashed", color = "grey", size = 1.5)
plot <- plot + ggplot2::geom_hline(yintercept=50, linetype="dashed", color = "blue", size = 1.5)
plot <- plot + ggplot2::geom_hline(yintercept=75, linetype="dashed", color = "green", size = 1.5)
plot <- plot + ggplot2::ylab("% Missing Data")
plot <- plot + ggplot2::xlab("Exposures")
plot <- plot + ggplot2::coord_flip()
plot <- plot + ggplot2::scale_y_continuous(limits = c(0, x.max))
plot
dev.off()

png(paste0(output.folder, "prenatal_exposure_neuromixtures_prep_set_missinges_rev_shs_rev_valid_part.png"), 900, 460)
ggmice::plot_pattern(expo_per_raw, 
                     square = FALSE, 
                     rotate = TRUE)
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

png(paste0(output.folder, "prenatal_exposure_neuromixtures_prep_set_missinges_rev_shs_438_rev_valid_part.png"), 900, 460)
ggmice::plot_pattern(expo_per_row_comp, 
                     square = FALSE, 
                     rotate = TRUE)
dev.off()

nas_perc <- (colMeans(is.na(expo_per_row_comp)))*100
nas_perc <- nas_perc[-c(1,2)]
x <- nas_perc
x <- x[order(x, decreasing = TRUE)]
x <- as.data.frame(round(x, 2))
pdf(paste0(output.folder, "prenatal_exposure_na_res_data_matrix_n_438_rev_grav_corr_rev_shs_rev_valid_part.pdf"), height=15, width=20)
grid.table(x)
dev.off()

# todo: extend scaling approach to all analysis
expo_desc_loc <- exposure_description[
  which(exposure_description$month == unique(expo_per$month) & # todo: generalize to sid and month any order
          exposure_description$exposure %in% colnames(expo_per[,-which(colnames(expo_per) %in% c("SID", "month"))])),]
expo_desc_loc <- expo_desc_loc[!duplicated(expo_desc_loc$variable_name), ]
expo_desc_loc <- rbind(expo_desc_loc, add_desc)

# save img of description variable

png(paste0(output.folder, "exposure_na_50_variable_description_rev_grav_corr_rev_shs_rev_valid_part.png"), 1300, 800)
grid.table(expo_desc_loc[,-c(1,4)], rows = NULL)
dev.off()

# # making phenotype
# phenotype = data.frame(id = expo_per$SID, var = 1)
# rownames(phenotype) <- phenotype[ , 1]
expo_per <- expo_per[,-c(which(colnames(expo_per)=="month"))]
colnames(expo_per)[which(colnames(expo_per)=="SID")] <- "id"
rownames(expo_desc_loc) <- expo_desc_loc[, 3]
expo_desc_loc <- expo_desc_loc[ , -3]
rownames(expo_per) <- expo_per[ , which(colnames(expo_per)=="id")]
expo_per <- expo_per[ , -c(which(colnames(expo_per)=="id"))]

pdf(paste0(output.folder, "prenatal_exposure_description_reduced_na_tol", na_tol, "_rev_grav_corr_rev_shs_rev_valid_part.pdf"), height=15, width=20)
grid.table(expo_desc_loc[,-3])
dev.off()

# build matrix LODs (from MN codebook)
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
colnames(expo_prep)[3] <- "SHS"
# scale lods
all_lod_values[match(lods_names,names(all_lod_values))] <- all_lod_values[match(lods_names,names(all_lod_values))] / 
  denoms[match(names(all_lod_values[match(lods_names,names(all_lod_values))]),names(denoms))]

# PCP
saveRDS(expo_prep, paste0(generated.data.folder, "data_row_comp_na_", na_tol, "_reduced_rev_grav_corr_rev_shs_rev_valid_part.rds"))
data <- list("M" = expo_prep[,-c(which(colnames(expo_prep) %in% c("SID", "month")))]) %>% purrr::map(as.matrix)
raw.hm <- heatmaply::heatmaply(data$M, Colv = F, Rowv = F, labRow = NULL,
                                        cexRow = 100,
                                        showticklabels = c(T, F), 
                               file = paste0(output.folder, "heatmap_exposure_","prenatal_na_", na_tol, "_reduced_rev_grav_corr_rev_shs_rev_valid_part.html"), 
                               main = "Raw matrix")

# second vanilla search
etas <- seq(0,0.3, length.out=11)
rank <- 11
rrmc_grid <- expand.grid(eta = etas, r = rank) # RRMC will search up to rank 6
runs = 35
perc_test = 0.10
LOD = all_lod_values
cores = parallel::detectCores(logical = F) /2
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

pcp_outs <- RRMC(data$M, r = 3, eta = 0.12, LOD = LOD)
# 
sum(pcp_outs$L<0)/prod(dim(pcp_outs$L)) # 0.1 % below 0 in L matrix
sum(pcp_outs$L<(-1/2))/prod(dim(pcp_outs$L)) # 0.1% below -1/2
saveRDS(pcp_outs, file = paste0(generated.data.folder, "pren_exposures_pcp_rrmc_na_", na_tol, "_scale_TRUE_grav_rev_SHS_rev_valid_part.rds"))
# 
pcp_outs <- readRDS(file = paste0(generated.data.folder, "pren_exposures_pcp_rrmc_na_", na_tol, "_scale_TRUE_grav_rev_SHS_rev_valid_part.rds"))

colnames(pcp_outs$L) <- colnames(pcp_outs$S)


raw.hm <- heatmaply::heatmaply(data$M, Colv = F, Rowv = F, labRow = NULL,
                               cexRow = 100,
                               showticklabels = c(TRUE, FALSE), main = "Raw exposures matrix",
)

raw.hm

L.hm <- heatmaply::heatmaply(pcp_outs$L, Colv = F, Rowv = F, labRow = NULL,
                             cexRow = 100,
                             showticklabels = c(TRUE, FALSE), main = "L matrix",
)
# 
L.hm
# # # 
S.hm <- heatmaply::heatmaply(pcp_outs$S, Colv = F, Rowv = F, labRow = NULL,
                             cexRow = 100,
                             showticklabels = c(TRUE, FALSE), main = "S matrix",
)

S.hm
# # # 
# # # 
# # # 
# # raw matrix correlations:
na_level <- 50
scale <- TRUE
graph_title <- "Raw matrix correlations"
png(paste0(output.folder, "raw_exposure_mat_corr_L_", "pren_na_", na_level,  "_scale_", scale,  "_grav_corr_rev_SHS_rev_valid_part.png"), 900, 460)
data$M %>% GGally::ggcorr(., method = c("pairwise.complete.obs", "pearson"),
                          label = T, label_size = 3, label_alpha = T,
                          hjust = 1, nbreaks = 10, limits = TRUE,
                          size = 4, layout.exp = 5) + ggtitle(graph_title)
dev.off()
# # L matrix correlations:
graph_title <- "L matrix correlations"
png(paste0(output.folder, "pcp_rrmc_exposure_mat_corr_L_", "pren_na_", na_level,  "_scale_", scale,  "_grav_corr_rev_SHS_rev_valid_part.png"), 900, 460)
pcp_outs$L %>% GGally::ggcorr(., method = c("pairwise.complete.obs", "pearson"),
                              label = T, label_size = 3, label_alpha = T,
                              hjust = 1, nbreaks = 10, limits = TRUE,
                              size = 4, layout.exp = 5) + ggtitle(graph_title)
dev.off()

# # S matrix correlations:
graph_title <- "S matrix correlations"
png(paste0(output.folder, "pcp_rrmc_exposure_mat_corr_S_", "pren_na_", na_level,  "_scale_", scale,  "_grav_corr_rev_SHS.png"), 900, 460)
pcp_outs$S %>% GGally::ggcorr(., method = c("pairwise.complete.obs", "pearson"),
                              label = T, label_size = 3, label_alpha = T,
                              hjust = 1, nbreaks = 10, limits = TRUE,
                              size = 4, layout.exp = 5) + ggtitle(graph_title)
dev.off()

