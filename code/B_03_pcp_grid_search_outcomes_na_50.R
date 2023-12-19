# First step to load packages etc.
rm(list=ls())
.libPaths(c(.libPaths(), "/home/jbenavides/R/x86_64-pc-linux-gnu-library/4.1"))
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

# load prepared outcome data
df_adhd16 <- readRDS(paste0(generated.data.folder, "adhd16.rds"))
df_adhd16 <- df_adhd16[,c("SID", "month", "ADHDCTOT", "ADHDCIA", "ADHDCHI", "ADHDWTOT", "ADHDWIA", "ADHDWHI")]
df_time16 <- readRDS(paste0(generated.data.folder, "time16.rds"))
df_ksads16 <- readRDS(paste0(generated.data.folder, "ksads16.rds"))
df_yrbss16 <- readRDS(paste0(generated.data.folder, "yrbss16.rds"))
df_yrbss16 <- df_yrbss16[,c("SID", "month", "risky_drug_use")]
df_yrbss16 <- df_yrbss16[-which(duplicated(df_yrbss16$SID)),]
df_yrs14_16 <- readRDS(paste0(generated.data.folder, "yrs_14_16.rds"))
df_conners <- readRDS(paste0(generated.data.folder, "conners.rds")) #
df_scs16 <- readRDS(paste0(generated.data.folder, "scs16_rev.rds"))
df_scs16 <- df_scs16[,c("SID", "month", "total_self_control_prob")]
outcome_description <- readRDS(paste0(generated.data.folder, "outcome_description.rds"))
# create a dataframe that includes all the combinations of SID and month contained in the above datasets
sids <- unique(c(df_adhd16$SID, df_time16$SID, df_ksads16$SID, df_yrs14_16$SID,df_conners$SID, df_scs16$SID, df_yrbss16$SID))
sids <- sids[order(sids)]
months <- unique(c(df_adhd16$month, df_time16$month, df_ksads16$month, df_yrs14_16$month, df_conners$month, df_scs16$month))
months <- months[order(months)]
outcomes <- data.frame()
for(i in 1:length(sids)){
  df_loc <- data.frame(SID = sids[i], month = months)
  outcomes <- rbind(outcomes, df_loc)
}
outcomes <- outcomes %>%
  dplyr::left_join(df_adhd16, by = c("SID", "month")) %>%
  dplyr::left_join(df_time16, by = c("SID", "month"))  %>%
  dplyr::left_join(df_ksads16, by = c("SID", "month")) %>%
  dplyr::left_join(df_yrs14_16, by = c("SID", "month"))  %>%
  dplyr::left_join(df_conners, by = c("SID", "month"))  %>%
  dplyr::left_join(df_scs16, by = c("SID", "month")) %>%
  dplyr::left_join(df_yrbss16, by = c("SID", "month"))

# filter only those moms enrolled in the MN cohort
mn_cohort_participants_sids <- readRDS(paste0(generated.data.folder, "mn_cohort_participants_sids.rds"))
outcomes <- outcomes[which(outcomes$SID %in% mn_cohort_participants_sids),]


# # obtain sid for visit 16 years old (for age range paper)
# outc_16_yrs <- outcomes[which(outcomes$month == 192), ]
# rownames(outc_16_yrs) <- outc_16_yrs$SID
# outc_16_yrs <- outc_16_yrs[,-c(1,2)]
# outc_16_yrs_any <- outc_16_yrs[-c(as.numeric(which((rowSums(is.na(outc_16_yrs))/ncol(outc_16_yrs)) == 1))),]
# nrow(outc_16_yrs_any)
# sids_visit_16 <- as.numeric(rownames(outc_16_yrs_any))
# mn_cohort_participants_sids <- readRDS(paste0(generated.data.folder, "mn_cohort_participants_sids.rds"))
# sids_visit_16[which(!sids_visit_16 %in% mn_cohort_participants_sids)] # SID 348 not part of the MN cohort?
# saveRDS(sids_visit_16, paste0(generated.data.folder, "sids_any_neurobehavioral_data.rds"))
# ### eda 
# ## for each period
na_maxs <- c(25, 50, 75)
collect_n <- numeric()
collect_na_max <- numeric()
collect_fams <- character()
collect_visit <- numeric()
collect_var_n <- numeric()
ms <- unique(outcomes$month)
scale <- T
for(n in 1:length(na_maxs)){
for(m in 1:length(ms)){
outc_per_raw <- outcomes[outcomes$month == ms[m],]
# delete empty variables
outc_per_raw <- outc_per_raw[,-c(as.numeric(which((colSums(is.na(outc_per_raw))/nrow(outc_per_raw)) == 1)))]
# get missingness / column
nas_perc <- (colMeans(is.na(outc_per_raw)))*100
nas_perc <- nas_perc[-c(1,2)]
x <- nas_perc
x <- x[order(x, decreasing = TRUE)]
# summary(x)
# apply criteria of missing data per row and column
tot_subj <- nrow(outc_per_raw)
outc_per_raw_row_comp <- outc_per_raw[-which((rowMeans(is.na(outc_per_raw)))*100 > na_maxs[n]),]
nrow(outc_per_raw_row_comp) / tot_subj
if(nrow(outc_per_raw_row_comp) / tot_subj > 0){
if(length(which((colMeans(is.na(outc_per_raw_row_comp)))*100 > na_maxs[n])) > 0){
outc_per_raw_row_comp <- outc_per_raw_row_comp[,-which((colMeans(is.na(outc_per_raw_row_comp)))*100 > na_maxs[n])] # todo: check this 25
}
outc_per <- outc_per_raw_row_comp
outc_desc_loc <- outcome_description[
  which(outcome_description$month == unique(outc_per$month) &
          outcome_description$variable_name %in% colnames(outc_per[,-c(which(colnames(outc_per) %in% c("SID", "month")))])),]
outc_desc_loc <- outc_desc_loc[!duplicated(outc_desc_loc$variable_name), ]

outc_per_id_month <- outc_per[,c("SID", "month")]
outc_per <- outc_per[,-which(colnames(outc_per) == "month")]
colnames(outc_per)[which(colnames(outc_per) == "SID")] <- "id"
rownames(outc_desc_loc) <- outc_desc_loc[, 1]
outc_desc_loc <- outc_desc_loc[ , -1]
rownames(outc_per) <- outc_per[ , which(colnames(outc_per) == "id")]
outc_per <- outc_per[ , -c(which(colnames(outc_per) == "id"))]
if(scale){
# scale excluding variables that range from 0 to 1
my.min <- unlist(lapply(outc_per,function(x) min(x,na.rm=T)))
my.max <- unlist(lapply(outc_per,function(x) max(x,na.rm=T)))
non_scale <- which(my.max - my.min == 1 | my.min == my.max)

if(length(non_scale) > 0){
  non_scale_pos <- which(colnames(outc_per) %in% names(non_scale))
  denoms <- apply(outc_per[,-non_scale_pos], 2, function(a) sd(a, na.rm = T))

  scal_vars <-colnames(outc_per)[!colnames(outc_per) %in% names(non_scale)]
  outc_per_scaled = apply(outc_per[,-non_scale_pos], 2, function(a) a/sd(a, na.rm = T))

  
  outc_prep <- cbind(outc_per_id_month, outc_per[,non_scale], outc_per_scaled)
} else {
  outc_per_scaled = apply(outc_per, 2, function(a) a/sd(a, na.rm = T))
  outc_prep <- cbind(outc_per_id_month, outc_per_scaled)
}
data <- list("M" = outc_prep[,-c(which(colnames(outc_prep) %in% c("SID", "month")))]) %>% purrr::map(as.matrix)
} else {
data <- list("M" = outc_per) %>% purrr::map(as.matrix)
}
saveRDS(outc_prep, paste0(generated.data.folder, "data_with_ids_outc_",ms[m]/12, "_yrs_na_", na_maxs[n] ,"_scale", "_", scale, ".rds"))
saveRDS(data, paste0(generated.data.folder, "data_outc_",ms[m]/12, "_yrs_na_", na_maxs[n] ,"_scale", "_", scale, ".rds"))
saveRDS(outc_desc_loc, paste0(generated.data.folder, "desc_outc_",ms[m]/12, "_yrs_na_", na_maxs[n] ,".rds"))

# hm.raw <- heatmaply::heatmaply(data$M, Colv = F, Rowv = F,
#                                # ylab = params$rowvar_name, labRow = as.character(rowlabs),
#                                cexRow = 100, #row_side_colors = data.frame("cohort" = cohorts),
#                                #col_side_colors = data.frame("exposure family" = as.factor(params$colgroupings)),
#                                showticklabels = c(T, F), #main = plot.title,
#                                file = paste0(output.folder, "heatmap_outc_",ms[m]/12, "_yrs_na_", na_maxs[n], ".html")
#                                #label_names = c(params$rowvar_name, "exposure", "value"
# )
# 
# 
# # hm.raw # todo save heatmap
collect_visit <- append(collect_visit, ms[m])
collect_n <- append(collect_n, nrow(data$M))
collect_na_max <- append(collect_na_max, na_maxs[n])
collect_fams <- append(collect_fams, paste(unique(outc_desc_loc$family), collapse = " | "))
collect_var_n <- append(collect_var_n, ncol(outc_prep)-2)
rm(data)
} else {
  print("no sufficient data for this na threshold level")
}
}
}
# res_filters <- data.frame(visit = collect_visit/12, NA_perc = collect_na_max,
#                           n_subjects = collect_n,
#                           n_vars = collect_var_n,
#                           outcome_families = collect_fams)
# 
# pdf(paste0(output.folder, "outcomes_summary_table_n.pdf"), height=8, width=20)
# grid.table(res_filters)
# dev.off()

# PCP 
# second vanilla search
mon <- 192
na_level <- 50
scale <- "TRUE"
data <- readRDS(paste0(generated.data.folder, "data_outc_",mon/12, "_yrs_na_", na_level ,"_scale", "_", scale,".rds"))
outc_desc_loc <- readRDS(paste0(generated.data.folder, "desc_outc_",mon/12, "_yrs_na_", na_level ,".rds"))

# pdf(paste0(output.folder, "outcomes_description_table_", mon/12, "yrs.pdf"), height=8, width=20)
# grid.table(outc_desc_loc)
# dev.off()


etas <- seq(0.05,0.15, length.out=11)
rank <- 11
rrmc_grid <- expand.grid(eta = etas, r = rank) # RRMC will search up to rank 6
runs = 35
LOD = rep(0, ncol(data$M))
perc_test = 0.10
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
    save_as = paste0(generated.data.folder,"pcp_outcomes_vanilla_", mon/12, "_yrs_na_", na_level ,"_scale", "_", scale, "rev_scs")
  )
})
# # read results
rrmc_results <- readRDS(paste0(generated.data.folder,"pcp_outcomes_vanilla_", mon/12, "_yrs_na_", na_level ,"_scale", "_", scale, "rev_scs.rds"))
# # 
# # # 
# # # # 3c. The best parameter setting according to relative error...
# # # saveRDS(rrmc_results$summary_stats, paste0(generated.data.folder, "vanilla_search_res_rrmc.RDS"))
rrmc_results$summary_stats %>% slice_min(rel_err)
# 
# 
# # # 
# # # # 3d. Visualizing the whole gridsearch:
plot_ly(data = rrmc_results$summary_stats, x = ~eta, y = ~r, z = ~rel_err, type = "heatmap")
# # # sparsities
plot_ly(data = rrmc_results$summary_stats, x = ~eta, y = ~r, z = ~S_sparsity, type = "heatmap")
# 
# # run PCP 
LOD = rep(0, ncol(data$M))
# # na 75 eta 0.11 r 3
# # na 50 eta 0.11 r 3
# # na 25 eta 0.14 r 3 rel.err 0.293
pcp_outs <- RRMC( data$M, r = 3, eta = 0.11, LOD = LOD)
# # 
sum(pcp_outs$L<0)/prod(dim(pcp_outs$L)) # 13 % below 0 in L matrix
sum(pcp_outs$L<(-1/2))/prod(dim(pcp_outs$L)) # 0% below -1/2
saveRDS(pcp_outs, file = paste0(generated.data.folder, "pcp_rrmc_outcome_", mon/12, "_yrs_na_", na_level, "scale", scale, "_rev_scs.rds"))
# # 
# pcp_outs <- readRDS(paste0(generated.data.folder, "pcp_rrmc_outcome_", mon/12, "_yrs_na_", na_level,  "scale", scale, ".rds"))
# # 
# # png(paste0(output.folder, "pre_exposures_gridsearch.png"), 900, 460)
# # grid.table(stats, rows = NULL)
# # dev.off()
# # # # 
# colnames(pcp_outs$L) <- colnames(pcp_outs$S)
# 
# raw.hm <- heatmaply::heatmaply(data$M, Colv = F, Rowv = F, labRow = NULL,
#                              cexRow = 100,
#                              showticklabels = c(TRUE, FALSE), main = "Raw exposures matrix",
# )
# 
# raw.hm
# 
# L.hm <- heatmaply::heatmaply(pcp_outs$L, Colv = F, Rowv = F, labRow = NULL,
#                   cexRow = 100,
#                   showticklabels = c(TRUE, FALSE), main = "L matrix",
# )
# # 
# L.hm
# # # # 
# S.hm <- heatmaply::heatmaply(pcp_outs$S, Colv = F, Rowv = F, labRow = NULL,
#                              cexRow = 100,
#                              showticklabels = c(TRUE, FALSE), main = "S matrix",
# )
# 
# S.hm
# # # # 
# # # # 
# # # # 
# # # raw matrix correlations:
# png(paste0(output.folder, "raw_outcome_mat_corr_L_", mon/12, "_yrs_na_", na_level,  "_scale", scale,  ".png"), 900, 460)
# data$M %>% GGally::ggcorr(., method = c("pairwise.complete.obs", "pearson"),
#                               limits = F, label = T, label_size = 3, label_alpha = T, hjust = 0.75,
#                               size = 3, layout.exp = 1) + ggtitle(paste0("raw_outcome_", mon/12, "_yrs_na_", na_level))
# dev.off()
# # # L matrix correlations:
# png(paste0(output.folder, "pcp_rrmc_outcome_mat_corr_L_", mon/12, "_yrs_na_", na_level,  "_scale", scale,  ".png"), 900, 460)
# pcp_outs$L %>% GGally::ggcorr(., method = c("pairwise.complete.obs", "pearson"),
#                               limits = F, label = T, label_size = 3, label_alpha = T, hjust = 0.75,
#                               size = 3, layout.exp = 1) + ggtitle(paste0("Outcome L matrix: Pearson correlation ", mon/12, "_yrs_na_", na_level))
# dev.off()
# 
# # # S matrix correlations:
# png(paste0(output.folder, "pcp_rrmc_outcome_mat_corr_S_", mon/12, "_yrs_na_", na_level,  "_scale", scale,  ".png"), 900, 460)
# pcp_outs$S %>% GGally::ggcorr(., method = c("pairwise.complete.obs", "pearson"),
#                                    limits = F, label = T, label_size = 3, label_alpha = T, hjust = 0.75,
#                                    size = 3, layout.exp = 1) + ggtitle(paste0("Outcome S matrix: Pearson correlation ", mon/12, "_yrs_na_", na_level))
# dev.off()
# # 
# # 
# # # what if i run nonconvex pcp
# # default <- mat %>% purrr::map(PCPhelpers::get_pcp_defaults)
# # lambdas <- seq(default[["pre"]]$lambda - .03, by = .03, length.out =3)
# # mus <- seq(default[["pre"]]$mu - 2, by = 2, length.out = 3)
# # values <- data.frame(r=3:5, mu = mus, lambda = lambdas)
# # grid <- expand.grid(values)
# # pre_gs_scaled <- PCPhelpers::grid_search_cv(mat[["pre"]], pcp_func = pcpr::root_pcp_noncvx_na, # another search w/RRMC
# #                                             grid_df = grid,
# #                                             cores = 4, runs = 4,
# #                                             file = paste0(generated.data.folder, "pre_exp_gridsearch_root_pcp_noncvx_na_v1.Rda"))
# # load(paste0(generated.data.folder, "pre_exp_gridsearch_root_pcp_noncvx_na.Rda"))
