library("haven")
data <- read_sav("/home/jbenavides/maklab/scratch/data/health/amy_r01_aim1/Typesamp_only.sav")  
mn_cohort_participants_sids <- data$SID[(which(!is.na(data$ENRLSTAT)))]
saveRDS(mn_cohort_participants_sids, paste0(generated.data.folder, "mn_cohort_participants_sids.rds"))
