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


# load outcome data
df_adhd16 <- readRDS(paste0(generated.data.folder, "adhd16.rds"))
df_adhd16 <- df_adhd16[,c("SID", "month", "ADHDCTOT", "ADHDCIA", "ADHDCHI", "ADHDWTOT", "ADHDWIA", "ADHDWHI")]
df_time16 <- readRDS(paste0(generated.data.folder, "time16.rds"))
df_ksads16 <- readRDS(paste0(generated.data.folder, "ksads16.rds"))
df_yrbss16 <- readRDS(paste0(generated.data.folder, "yrbss16.rds"))
df_yrbss16 <- df_yrbss16[,c("SID", "month", "risky_drug_use")]
df_yrbss16 <- df_yrbss16[-which(duplicated(df_yrbss16$SID)),]
df_yrs14_16 <- readRDS(paste0(generated.data.folder, "yrs_14_16.rds"))
df_conners <- readRDS(paste0(generated.data.folder, "conners.rds")) # # GC: a redundant hashtag sign
df_scs16 <- readRDS(paste0(generated.data.folder, "scs16_rev.rds"))
df_scs16 <- df_scs16[,c("SID", "month", "total_self_control_prob")]
outcome_description <- readRDS(paste0(generated.data.folder, "outcome_description.rds"))
# create a dataframe that includes all the combinations of SID and month contained in the above datasets
sids <- unique(c(df_adhd16$SID, df_time16$SID, df_ksads16$SID, df_yrs14_16$SID,df_conners$SID, df_scs16$SID, df_yrbss16$SID))
sids <- sids[order(sids)]
months <- unique(c(df_adhd16$month, df_time16$month, df_ksads16$month, df_yrs14_16$month, df_conners$month, df_scs16$month)) # GC: the 'df_yrbss16' data frame is missing (while appears in line 36 for creating 'sids').
months <- months[order(months)]
outcomes <- data.frame()
# create a dataframe containing subject id and month for temporal reference
for(i in 1:length(sids)){
  df_loc <- data.frame(SID = sids[i], month = months)
  outcomes <- rbind(outcomes, df_loc)
}
# add columns for the outcome data to the previous dataframe
outcomes <- outcomes %>%
  dplyr::left_join(df_adhd16, by = c("SID", "month")) %>%
  dplyr::left_join(df_time16, by = c("SID", "month"))  %>%
  dplyr::left_join(df_ksads16, by = c("SID", "month")) %>%
  dplyr::left_join(df_yrs14_16, by = c("SID", "month"))  %>%
  dplyr::left_join(df_conners, by = c("SID", "month"))  %>%
  dplyr::left_join(df_scs16, by = c("SID", "month")) %>%
  dplyr::left_join(df_yrbss16, by = c("SID", "month"))

# obtain sid for visit 16 years old (for age range paper)
outc_16_yrs <- outcomes[which(outcomes$month == 192), ]
outc_16_yrs_out <- outc_16_yrs[,-c(1,2)]
# delete empty subjects
outc_16_yrs <- outc_16_yrs[-c(as.numeric(which((rowSums(is.na(outc_16_yrs_out))/ncol(outc_16_yrs_out)) == 1))),]
# subset to subjects that completed the visit
mn_cohort_participants_sids <- readRDS(paste0(generated.data.folder, "mn_cohort_participants_sids.rds"))
outc_16_yrs <- outc_16_yrs[which(outc_16_yrs$SID %in% mn_cohort_participants_sids),] 

# for different conditions of allowance of data missingness, generate outcome datasets
# ### eda 
# ## for each period
na_maxs <- c(25, 50, 75) # max percent of data missingness 25,50 or 75
collect_n <- numeric()
collect_na_max <- numeric()
collect_fams <- character()
collect_visit <- numeric()
collect_var_n <- numeric()
ms <- unique(outcomes$month)
scale <- T
for(n in 1:length(na_maxs)){ # for each threshold level of data missingness
  for(m in 1:length(ms)){ # for each visit (in this project we only have visit at 16 years old, I created this script to be able to handle more visits)
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
    # delete subjects who have more data missingness than allowed by missingness threshold
    outc_per_raw_row_comp <- outc_per_raw[-which((rowMeans(is.na(outc_per_raw)))*100 > na_maxs[n]),]

    if(nrow(outc_per_raw_row_comp) / tot_subj > 0){
      # delete columns that have more data missingness than allowed by missingness threshold
      if(length(which((colMeans(is.na(outc_per_raw_row_comp)))*100 > na_maxs[n])) > 0){
        outc_per_raw_row_comp <- outc_per_raw_row_comp[,-which((colMeans(is.na(outc_per_raw_row_comp)))*100 > na_maxs[n])] # todo: check this 25 # GC: please remove comment if task is done.
      }
      # subset data description
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
      # scale data
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
      # save data
      saveRDS(outc_prep, paste0(generated.data.folder, "data_with_ids_outc_",ms[m]/12, "_yrs_na_", na_maxs[n] ,"_scale", "_", scale, ".rds"))
      saveRDS(data, paste0(generated.data.folder, "data_outc_",ms[m]/12, "_yrs_na_", na_maxs[n] ,"_scale", "_", scale, ".rds"))
      saveRDS(outc_desc_loc, paste0(generated.data.folder, "desc_outc_",ms[m]/12, "_yrs_na_", na_maxs[n] ,".rds"))
      # fill objects counting number of subjects per data missingness threshold
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
# save objects counting number of subjects per data missingness threshold
res_filters <- data.frame(visit = collect_visit/12, NA_perc = collect_na_max,
                          n_subjects = collect_n,
                          n_vars = collect_var_n,
                          outcome_families = collect_fams)

pdf(paste0(output.folder, "outcomes_summary_table_n.pdf"), height=8, width=20)
grid.table(res_filters)
dev.off()
