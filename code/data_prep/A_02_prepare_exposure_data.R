# create a dataframe containing all the exposures for each subject
# Jaime Benavides 6/8/22
# First step to load packages etc.
rm(list=ls())
# 1a Declare root directory, folder locations and load essential stuff
project.folder = paste0(print(here::here()),'/')
source(paste0(project.folder,'init_directory_structure.R'))
source(paste0(functions.folder,'script_initiate.R'))

# data paths
data_path <- "/home/jbenavides/maklab/scratch/data/health/amy_r01_aim1/raw_data/"
ccceh_data_path <- paste0(data_path, "CCCEH_Data/")
ysr_data_path <- paste0(data_path, "/YSR_foranalysis/AR_EK_YSRs/")

file_names <- system(paste0("ls ", ccceh_data_path), intern = TRUE)
file_names <- gsub(".csv", "",file_names)
table_names <- file_names[-which(grepl( ".sav", file_names, fixed = TRUE))]
table_names <- c(table_names, "PAH")
file_names_phthal <- system(paste0("ls ", data_path, "additional_phthalate_data/"), intern = TRUE)
file_names_phthal <- gsub(".csv", "",file_names_phthal)
table_names_phthal <- file_names_phthal[-which(grepl( ".sav", file_names_phthal, fixed = TRUE))]
table_names <- c(table_names, table_names_phthal)
# load ccceh_data
bchart <- read_csv(paste0(ccceh_data_path, "BCHART.csv")) # 710 x 30   
exp48hr <- read_csv(paste0(ccceh_data_path, "EXP48HR.csv"))  # 756 x 468
month12 <- read_csv(paste0(ccceh_data_path, "MONTH12.csv"))  # 609 x 779
month84 <- read_csv(paste0(ccceh_data_path, "MONTH84.csv"))  # 522 x 694
scs16 <- read_csv(paste0(ccceh_data_path, "SCS16.csv"))  # 303 x 46
sgm1 <- read_csv(paste0(ccceh_data_path, "SGM1.csv"))  # 496 x 5
sgm6 <- read_csv(paste0(ccceh_data_path, "SGM6.csv"))  # 496 x 5
wisc16 <- read_csv(paste0(ccceh_data_path, "WISC16.csv"))  # 317 x 30
wppsi_sw <- read_csv(paste0(ccceh_data_path, "WPPSI_SW.csv")) # 391 x 68 / sibid 111 sid 368?
bpa <- read_csv(paste0(ccceh_data_path, "BPA.csv"))  # 568 x 17
home <- read_csv(paste0(ccceh_data_path, "HOME.csv"))  # 545 x 19
month24 <- read_csv(paste0(ccceh_data_path, "MONTH24.csv")) # 566 x 779
uphthm1 <- read_csv(paste0(data_path, "additional_phthalate_data/", "UPHTHM1.csv"))
uphthm5 <- read_csv(paste0(data_path, "additional_phthalate_data/", "UPHTHM5.csv")) # 381
uphthm6 <- read_csv(paste0(data_path, "additional_phthalate_data/", "UPHTHM6.csv")) # 486
uphthm7 <- read_csv(paste0(data_path, "additional_phthalate_data/", "UPHTHM7.csv")) # 320
ses16 <- read_csv(paste0(ccceh_data_path, "SES16.csv"))  # 288 x 179
tanner <- read_csv(paste0(ccceh_data_path, "TANNER.csv"))  # 473 x 21
wisc <- read_csv(paste0(ccceh_data_path, "WISC.csv"))  # 1,086 x 69
year09 <- read_csv(paste0(ccceh_data_path, "YEAR09.csv"))  # 476 x 711
caars <- read_csv(paste0(ccceh_data_path, "CAARS.csv"))  # 628 x 37
ksads16 <- read_csv(paste0(ccceh_data_path, "KSADS16.csv"))  # 221 x 164
month36 <- read_csv(paste0(ccceh_data_path, "MONTH36.csv"))  # 562 x 779
prenatal <- read_csv(paste0(ccceh_data_path, "PRENATAL.csv"))  # 845 x 975
sgm1 <- read_csv(paste0(ccceh_data_path, "SGM1.csv"))  # 562 x 5
time16 <- read_csv(paste0(ccceh_data_path, "TIME16.csv"))  # 1,192 x 42
wppsi <- read_csv(paste0(ccceh_data_path, "WPPSI.csv"))  # 543 x 15
year11 <- read_csv(paste0(ccceh_data_path, "YEAR11.csv"))  #  378 x 748
cotinine <- read_csv(paste0(ccceh_data_path, "COTININE.csv"))  # 720 x 14
mfi_mom <- read_csv(paste0(ccceh_data_path, "MFI_MOM.csv"))  # 995 x 29
month60 <- read_csv(paste0(ccceh_data_path, "MONTH60.csv"))  # 551 x 680
puberty <- read_csv(paste0(ccceh_data_path, "PUBERTY.csv"))  # 578 x 18
sgm5 <- read_csv(paste0(ccceh_data_path, "SGM5.csv"))  # 429 x 5
wasi <- read_csv(paste0(ccceh_data_path, "WASI.csv"))  # 364 x 55
wppsi_iii <- read_csv(paste0(ccceh_data_path, "WPPSI_III.csv"))  
year14 <- read_csv(paste0(ccceh_data_path, "YEAR14.csv"))  # 355 x 844
pah <- read_csv(paste0(ccceh_data_path, "PAHtot.csv")) 
pah_all <- read_csv(paste0(ccceh_data_path, "PAH.csv")) 
adducts_month0 <- read_csv(paste0(ccceh_data_path, "ADDUCTS.csv")) # this is in the codebook
pbde_month0 <- read_csv(paste0(ccceh_data_path, "MN_PBDE_longformat.csv"))
              
# read codebook
simple <- rvest::read_html(paste0(data_path, "ceh_codebook.html"))

# read table description

table_description <- read_csv(paste0(data_path, "table_description.csv"))
table_description <- table_description[which(table_description$Name %in% table_names),]
colnames(table_description) <- c("dta_table_number", "dta_table_name", "data_table_description")

# transform code book into text
text_cbook <- simple %>%
  rvest::html_nodes("p") %>%
  rvest::html_text()

# for each data table
df <- data.frame()
for(i in 1:length(table_description$dta_table_name)){
  tab_n <- table_description$dta_table_name[i]
  tab_desc <- table_description$data_table_description[i]
  # get names of variables and descriptions from this table
  tab_n_lines <- text_cbook[which(stringr::str_detect(text_cbook, tab_n))]
  tab_f_lines <- text_cbook[which(stringr::str_detect(text_cbook, tab_n))+1]
  var_nams <- character()
  var_descs <- character()
  for(j in 1:length(tab_n_lines)){
  # extract variable names
  var_nam <- stringi::stri_replace_all_charclass(qdapRegex::ex_between(tab_n_lines[j], "ble)", "\"")[[1]], "\\p{WHITE_SPACE}", "")
  # extract variable description
  var_desc <- qdapRegex::ex_between(tab_n_lines[j], "\"", "\"")[[1]]
  # append vectors
  var_nams <- append(var_nams, var_nam, after = length(var_nams))
  var_descs <- append(var_descs, var_desc, after = length(var_descs))
  }
  local_df <- data.frame(variable_name = var_nams, variable_description = var_descs, dta_table_name = tab_n, data_table_description = tab_desc)
  df <- rbind(df, local_df)
}
codebook <- df
saveRDS(codebook, paste0(generated.data.folder, "codebook_mn.rds"))
codebook <- readRDS(paste0(generated.data.folder, "codebook_mn.rds"))
# create columns in the codebook to describe variables as domain (exposures, covariates, neurobehavior)	family (exposures: ETS, PAH, Phthalates, BP, ELS),	subfamily	(No2, pm2.5) period	(prenatal, postnatal), location (home, school, na),	period_postnatal (12 months, 24...)

# for exposures, create a dataframe containing all the exposures for each subject

# Material hardship (8 questions)
codebook[which(grepl("A10_", codebook$variable_name, fixed=TRUE)),]

mh_prenatal_vars <- c("A10_0","A11_0","A12_0","A13_0","A14_0","A15_0","A16_0","A17_0")
mh_12_vars <- c("A10_12","A11_12","A12_12","A13_12","A14_12","A15_12","A16_12","A17_12")
mh_24_vars <- c("A10_24","A11_24","A12_24","A13_24","A14_24","A15_24","A16_24","A17_24")
mh_36_vars <- c("A10_36","A11_36","A12_36","A13_36","A14_36","A15_36","A16_36","A17_36")
mh_60_vars <- c("A10_60","A11_60","A12_60","A13_60","A14_60","A15_60","A16_60","A17_60")
mh_84_vars <- c("A10_84","A11_84","A12_84","A13_84","A14_84","A15_84","A16_84","A17_84")
mh_Y9_vars <- c("A10_Y9","A11_Y9","A12_Y9","A13_Y9","A14_Y9","A15_Y9","A16_Y9","A17_Y9")


mh_time_points <- qdapRegex::ex_between(ls(pattern = "mh"), "_", "_")
mh_time_points <- as.character(mh_time_points)
mh_items <- gsub("_0", "", mh_prenatal_vars)

# ELS (early life stress) from questions from the standard maternal interview from CCCEH cohort studies - mother perceived stress - 14 questions
codebook[which(grepl("R01", codebook$variable_name, fixed=TRUE)),]
els_12_vars <- c("R01_12", "R02_12", "R03_12", "R04_12", "R05_12", "R06_12", "R07_12", "R08_12", "R09_12", "R10_12" , "R11_12" , "R12_12", "R13_12", "R14_12")
els_24_vars <- c("R01_24", "R02_24", "R03_24", "R04_24", "R05_24", "R06_24", "R07_24", "R08_24", "R09_24", "R10_24" , "R11_24" , "R12_24", "R13_24", "R14_24")
els_36_vars <- c("R01_36", "R02_36", "R03_36", "R04_36", "R05_36", "R06_36", "R07_36", "R08_36", "R09_36", "R10_36" , "R11_36" , "R12_36", "R13_36", "R14_36")
els_60_vars <- c("R01_60", "R02_60", "R03_60", "R04_60", "R05_60", "R06_60", "R07_60", "R08_60", "R09_60", "R10_60" , "R11_60" , "R12_60", "R13_60", "R14_60")
els_84_vars <- c("R01_84", "R02_84", "R03_84", "R04_84", "R05_84", "R06_84", "R07_84", "R08_84", "R09_84", "R10_84" , "R11_84" , "R12_84", "R13_84", "R14_84")
els_Y9_vars <- c("R01_Y9", "R02_Y9", "R03_Y9", "R04_Y9", "R05_Y9", "R06_Y9", "R07_Y9", "R08_Y9", "R09_Y9", "R10_Y9" , "R11_Y9" , "R12_Y9", "R13_Y9", "R14_Y9")
els_Y11_vars <- c("R01_Y11", "R02_Y11", "R03_Y11", "R04_Y11", "R05_Y11", "R06_Y11", "R07_Y11", "R08_Y11", "R09_Y11", "R10_Y11" , "R11_Y11" , "R12_Y11", "R13_Y11", "R14_Y11")
els_Y14_vars <- c("R01_Y11", "R02_Y11", "R03_Y11", "R04_Y11", "R05_Y11", "R06_Y11", "R07_Y11", "R08_Y11", "R09_Y11", "R10_Y11" , "R11_Y11" , "R12_Y11", "R13_Y11", "R14_Y11") # name is wrong in table YEAR14

els_time_points <- qdapRegex::ex_between(ls(pattern = "els"), "_", "_")
els_time_points <- as.character(els_time_points)
els_items <- gsub("_12", "", els_12_vars)

# intimate partner violence (IPV) - 12 questions
codebook[which(grepl("X12", codebook$variable_name, fixed=TRUE)),]
ipv_60_vars <- c("X01_60", "X02_60", "X03_60", "X04_60", "X05_60", "X06_60", "X07_60", "X08_60", "X09_60", "X10_60" , "X11_60" , "X12_60")
ipv_84_vars <- c("X01_84", "X02_84", "X03_84", "X04_84", "X05_84", "X06_84", "X07_84", "X08_84", "X09_84", "X10_84" , "X11_84" , "X12_84")
ipv_Y9_vars <- c("X01_Y9", "X02_Y9", "X03_Y9", "X04_Y9", "X05_Y9", "X06_Y9", "X07_Y9", "X08_Y9", "X09_Y9", "X10_Y9" , "X11_Y9" , "X12_Y9")
ipv_Y11_vars <- c("X01_Y11", "X02_Y11", "X03_Y11", "X04_Y11", "X05_Y11", "X06_Y11", "X07_Y11", "X08_Y11", "X09_Y11", "X10_Y11" , "X11_Y11" , "X12_Y11")
ipv_Y11_vars <- c("X01_Y11", "X02_Y11", "X03_Y11", "X04_Y11", "X05_Y11", "X06_Y11", "X07_Y11", "X08_Y11", "X09_Y11", "X10_Y11" , "X11_Y11" , "X12_Y11")
ipv_time_points <- qdapRegex::ex_between(ls(pattern = "ipv"), "_", "_")
ipv_time_points <- as.character(ipv_time_points)
ipv_items <- gsub("_60", "", ipv_60_vars)

# social support (8 questions)
codebook[which(grepl("Y05Y11", codebook$variable_name, fixed=TRUE)),]
ss_60_vars <- c("Y01_60" , "Y03_60" , "Y05_60" , "Y06A_60" , "Y06B_60" , "Y06C_60" , "Y06D_60" , "Y06E_60")
ss_84_vars <- c("Y01_84" , "Y03_84" , "Y05_84" , "Y06A_84" , "Y06B_84" , "Y06C_84" , "Y06D_84" , "Y06E_84")
ss_Y9_vars <- c("Y01_Y9" , "Y03_Y9" , "Y05_Y9" , "Y06A_Y9" , "Y06B_Y9" , "Y06C_Y9" , "Y06D_Y9" , "Y06E_Y9")
ss_Y11_vars <- c("Y01Y11" , "Y03Y11" , "Y05Y11" , "Y06AY11" , "Y06BY11" , "Y06CY11" , "Y06DY11" , "Y06EY11")
ss_Y14_vars <- c("Y01Y11" , "Y03Y11" , "Y05Y11" , "Y06AY11" , "Y06BY11" , "Y06CY11" , "Y06DY11" , "Y06EY11")
ss_time_points <- qdapRegex::ex_between(ls(pattern = "ss"), "_", "_")
ss_time_points <- as.character(ss_time_points)
ss_items <- gsub("_60", "", ss_60_vars)

# neighborhood quality (32 questions)
codebook[which(grepl("U02A", codebook$variable_name, fixed=TRUE)),]
nq_60_vars <- c("U02A_60", "U02B_60", "U02C_60", "U02D_60", "U02E_60", "U02F_60", "U02G_60", "U02H_60", "U02I_60", "U02J_60", "U02K_60", "U02L_60", "U03_1_60",
"U03_2_60", "U03_3_60", "U03_4_60", "U03_5_60", "U03_6_60", "U0401_60", "U0402_60", "U0403_60", "U0404_60", "U0405_60", "U0405A60",
"U0406_60", "U0407_60", "U0408_60", "U0409_60", "U0410_60" , "U0411_60" , "U0412_60" , "U0413_60")

nq_84_vars <- c("U02A_84", "U02B_84", "U02C_84", "U02D_84", "U02E_84", "U02F_84", "U02G_84", "U02H_84", "U02I_84", "U02J_84", "U02K_84", "U02L_84", "U03_1_84",
                "U03_2_84", "U03_3_84", "U03_4_84", "U03_5_84", "U03_6_84", "U0401_84", "U0402_84", "U0403_84", "U0404_84", "U0405_84", "U0405A84",
                "U0406_84", "U0407_84", "U0408_84", "U0409_84", "U0410_84" , "U0411_84" , "U0412_84" , "U0413_84")

nq_Y9_vars <- c("U02A_Y9", "U02B_Y9", "U02C_Y9", "U02D_Y9", "U02E_Y9", "U02F_Y9", "U02G_Y9", "U02H_Y9", "U02I_Y9", "U02J_Y9", "U02K_Y9", "U02L_Y9", "U03_1_Y9",
                "U03_2_Y9", "U03_3_Y9", "U03_4_Y9", "U03_5_Y9", "U03_6_Y9", "U0401_Y9", "U0402_Y9", "U0403_Y9", "U0404_Y9", "U0405_Y9", "U0405AY9",
                "U0406_Y9", "U0407_Y9", "U0408_Y9", "U0409_Y9", "U0410_Y9" , "U0411_Y9" , "U0412_Y9" , "U0413_Y9")

nq_Y11_vars <- c("U02AY11", "U02BY11", "U02CY11", "U02DY11", "U02EY11", "U02FY11", "U02GY11", "U02HY11", "U02IY11", "U02JY11", "U02KY11", "U02LY11", "U03_1Y11",
                "U03_2Y11", "U03_3Y11", "U03_4Y11", "U03_5Y11", "U03_6Y11", "U0401Y11", "U0402Y11", "U0403Y11", "U0404Y11",  "U0405Y11", "U0405AY11",
                "U0406Y11", "U0407Y11", "U0408Y11", "U0409Y11", "U0410Y11" , "U0411Y11" , "U0412Y11" , "U0413Y11")

nq_Y14_vars <- c("U02AY11", "U02BY11", "U02CY11", "U02DY11", "U02EY11", "U02FY11", "U02GY11", "U02HY11", "U02IY11", "U02JY11", "U02KY11", "U02LY11", "U03_1Y11",
                "U03_2Y11", "U03_3Y11", "U03_4Y11", "U03_5Y11", "U03_6Y11", "U0401Y11", "U0402Y11", "U0403Y11", "U0404Y11", "U0405Y11", "U0405AY11",
                "U0406Y11", "U0407Y11", "U0408Y11", "U0409Y11", "U0410Y11" , "U0411Y11" , "U0412Y11" , "U0413Y11")

nq_time_points <- qdapRegex::ex_between(ls(pattern = "nq"), "_", "_")
nq_time_points <- as.character(nq_time_points)
nq_items <- gsub("60","", gsub("_60", "", nq_60_vars)) 

# maternal distress or demoralization (27 questions)
codebook[which(grepl("L01", codebook$variable_name, fixed=TRUE)),]

dem_prenatal_vars <- c("L01_0", "L02_0", "L03_0", "L04_0", "L05_0","L06_0","L07_0","L08_0","L09_0","L10_0","L11_0",
                "L12_0", "L13_0", "L14_0", "L15_0", "L16_0","L17_0","L18_0","L19_0","L20_0","L21_0","L22_0",
                "L23_0","L24_0","L25_0", "L26_0","L27_0")

dem_12_vars <- c("L01_12", "L02_12", "L03_12", "L04_12", "L05_12","L06_12","L07_12","L08_12","L09_12","L10_12","L11_12",
                "L12_12", "L13_12", "L14_12", "L15_12", "L16_12","L17_12","L18_12","L19_12","L20_12","L21_12","L22_12",
                "L23_12","L24_12","L25_12", "L26_12","L27_12")

dem_24_vars <- c("L01_24", "L02_24", "L03_24", "L04_24", "L05_24","L06_24","L07_24","L08_24","L09_24","L10_24","L11_24",
                 "L12_24", "L13_24", "L14_24", "L15_24", "L16_24","L17_24","L18_24","L19_24","L20_24","L21_24","L22_24",
                 "L23_24","L24_24","L25_24", "L26_24","L27_24")

dem_36_vars <- c("L01_36", "L02_36", "L03_36", "L04_36", "L05_36","L06_36","L07_36","L08_36","L09_36","L10_36","L11_36",
                 "L12_36", "L13_36", "L14_36", "L15_36", "L16_36","L17_36","L18_36","L19_36","L20_36","L21_36","L22_36",
                 "L23_36","L24_36","L25_36", "L26_36","L27_36")

dem_60_vars <- c("L01_60", "L02_60", "L03_60", "L04_60", "L05_60","L06_60","L07_60","L08_60","L09_60","L10_60","L11_60",
                 "L12_60", "L13_60", "L14_60", "L15_60", "L16_60","L17_60","L18_60","L19_60","L20_60","L21_60","L22_60",
                 "L23_60","L24_60","L25_60", "L26_60","L27_60")

dem_84_vars <- c("L01_84", "L02_84", "L03_84", "L04_84", "L05_84","L06_84","L07_84","L08_84","L09_84","L10_84","L11_84",
                 "L12_84", "L13_84", "L14_84", "L15_84", "L16_84","L17_84","L18_84","L19_84","L20_84","L21_84","L22_84",
                 "L23_84","L24_84","L25_84", "L26_84","L27_84")

dem_Y9_vars <- c("L01_Y9", "L02_Y9", "L03_Y9", "L04_Y9", "L05_Y9","L06_Y9","L07_Y9","L08_Y9","L09_Y9","L10_Y9","L11_Y9",
                 "L12_Y9", "L13_Y9", "L14_Y9", "L15_Y9", "L16_Y9","L17_Y9","L18_Y9","L19_Y9","L20_Y9","L21_Y9","L22_Y9",
                 "L23_Y9","L24_Y9","L25_Y9", "L26_Y9","L27_Y9")

dem_Y11_vars <- c("L01Y11", "L02Y11", "L03Y11", "L04Y11", "L05Y11","L06Y11","L07Y11","L08Y11","L09Y11","L10Y11","L11Y11",
                 "L12Y11", "L13Y11", "L14Y11", "L15Y11", "L16Y11","L17Y11","L18Y11","L19Y11","L20Y11","L21Y11","L22Y11",
                 "L23Y11","L24Y11","L25Y11", "L26Y11","L27Y11")

dem_Y14_vars <- c("L01Y11", "L02Y11", "L03Y11", "L04Y11", "L05Y11","L06Y11","L07Y11","L08Y11","L09Y11","L10Y11","L11Y11",
                  "L12Y11", "L13Y11", "L14Y11", "L15Y11", "L16Y11","L17Y11","L18Y11","L19Y11","L20Y11","L21Y11","L22Y11",
                  "L23Y11","L24Y11","L25Y11", "L26Y11","L27Y11")

dem_time_points <- qdapRegex::ex_between(ls(pattern = "dem"), "_", "_")
dem_time_points <- as.character(dem_time_points)
dem_items <- gsub("60","", gsub("_60", "", dem_60_vars)) 

# discrimination # left out V03 because I can't normalize it and put it in the same context as other variables
codebook[which(grepl("V04", codebook$variable_name, fixed=TRUE)),]
disc_60_vars <- c("V01A_60", "V01B_60", "V01C_60", "V01D_60", "V01E_60", "V01F_60", "V01G_60", "V01H_60", "V01I_60", "V02_60", "V04_60")
disc_84_vars <- c("V01A_84", "V01B_84", "V01C_84", "V01D_84", "V01E_84", "V01F_84", "V01G_84", "V01H_84", "V01I_84", "V02_84", "V04_84")
disc_Y9_vars <- c("V01A_Y9", "V01B_Y9", "V01C_Y9", "V01D_Y9", "V01E_Y9", "V01F_Y9", "V01G_Y9", "V01H_Y9", "V01I_Y9", "V02_Y9", "V04_Y9")
disc_Y11_vars <- c("V01AY11", "V01BY11", "V01CY11", "V01DY11", "V01EY11", "V01FY11", "V01GY11", "V01HY11", "V01IY11", "V02Y11",  "V04Y11")
disc_Y14_vars <- c("V01AY11", "V01BY11", "V01CY11", "V01DY11", "V01EY11", "V01FY11", "V01GY11", "V01HY11", "V01IY11", "V02Y11",  "V04Y11")

disc_time_points <- qdapRegex::ex_between(ls(pattern = "disc"), "_", "_")
disc_time_points <- as.character(disc_time_points)
disc_items <- gsub("_60", "", disc_60_vars)

## ETS (environmental tobacco smoke)

# cotinine
codebook[which(grepl("MOMCOT", codebook$variable_name, fixed=TRUE)),]
cot_0_vars <- c("MOMCOT", "MOMFLG") # available also "CORDCOT", "CORDFLG" use them if needed
cot_24_vars <- c("M24COT", "M24FLG")
cot_36_vars <- c("M36COT", "M36FLG")
cot_60_vars <- c("M60COT", "M60FLG")
cot_Y9_vars <- c("Y9COT", "Y9FLG")

cot_time_points <- qdapRegex::ex_between(ls(pattern = "cot_"), "_", "_")
cot_time_points <- as.character(cot_time_points)
cot_items <- gsub("M60", "", cot_60_vars)

# self-report smoke
codebook[which(grepl("E01_0", codebook$variable_name, fixed=TRUE)),]
smk_0_vars <- c("E01_0", "E01A_0", "E01B_0", "E02_0", "E03_0", "E04_0", "E05_0", "E06_0", "E07_0", "E07A_0", "E07B_0", "E08_0", "E09A_0",
                "E09AA_0", "E09AB_0", "E09AC_0", "E09B_0", "E09BA_0", "E09BB_0", "E09BC_0", "E09C_0", "E09CA_0", "E09CB_0", 
                "E09CC_0", "E10_0", "E10A_0", "E11B_0", "E11C_0", "E11D_0", "E11E_0", "E11F_0", "E11G_0") # there are more...


smk_12_vars <- c("E01_12", "E01A_12", "E01B_12", "E02_12", "E03_12", "E04_12", "E05_12", "E06_12", "E07_12", "E07A_12", "E07B_12", "E08_12", "E09A_12",
                "E09AA_12", "E09AB_12", "E09AC_12", "E09B_12", "E09BA_12", "E09BB_12", "E09BC_12", "E09C_12", "E09CA_12", "E09CB_12", 
                "E09CC_12", "E10_12", "E10A_12", "E11B_12", "E11C_12", "E11D_12", "E11E_12", "E11F_12", "E11G_12") # there are more...

smk_24_vars <- c("E01_24", "E01A_24", "E01B_24", "E02_24", "E03_24", "E04_24", "E05_24", "E06_24", "E07_24", "E07A_24", "E07B_24", "E08_24", "E09A_24",
                "E09AA_24", "E09AB_24", "E09AC_24", "E09B_24", "E09BA_24", "E09BB_24", "E09BC_24", "E09C_24", "E09CA_24", "E09CB_24", 
                "E09CC_24", "E10_24", "E10A_24", "E11B_24", "E11C_24", "E11D_24", "E11E_24", "E11F_24", "E11G_24") # there are more...

smk_36_vars <- c("E01_36", "E01A_36", "E01B_36", "E02_36", "E03_36", "E04_36", "E05_36", "E06_36", "E07_36", "E07A_36", "E07B_36", "E08_36", "E09A_36",
                "E09AA_36", "E09AB_36", "E09AC_36", "E09B_36", "E09BA_36", "E09BB_36", "E09BC_36", "E09C_36", "E09CA_36", "E09CB_36", 
                "E09CC_36", "E10_36", "E10A_36", "E11B_36", "E11C_36", "E11D_36", "E11E_36", "E11F_36", "E11G_36") # there are more...

smk_60_vars <- c("E01_60", "E01A_60", "E01B_60", "E02_60", "E03_60", "E04_60", "E05_60", "E06_60", "E07_60", "E07A_60", "E07B_60", "E08_60", "E09A_60",
                "E09AA_60", "E09AB_60", "E09AC_60", "E09B_60", "E09BA_60", "E09BB_60", "E09BC_60", "E09C_60", "E09CA_60", "E09CB_60", 
                "E09CC_60", "E10_60", "E10A_60", "E11B_60", "E11C_60", "E11D_60", "E11E_60", "E11F_60", "E11G_60") # there are more...

smk_84_vars <- c("E01_84", "E01A_84", "E01B_84", "E02_84", "E03_84", "E04_84", "E05_84", "E06_84", "E07_84", "E07A_84", "E07B_84", "E08_84", "E09A_84",
                "E09AA_84", "E09AB_84", "E09AC_84", "E09B_84", "E09BA_84", "E09BB_84", "E09BC_84", "E09C_84", "E09CA_84", "E09CB_84", 
                "E09CC_84", "E10_84", "E10A_84", "E11B_84", "E11C_84", "E11D_84", "E11E_84", "E11F_84", "E11G_84") # there are more...

smk_Y9_vars <- c("E01_Y9", "E01A_Y9", "E01B_Y9", "E02_Y9", "E03_Y9", "E04_Y9", "E05_Y9", "E06_Y9", "E07_Y9", "E07A_Y9", "E07B_Y9", "E08_Y9", "E09A_Y9",
                "E09AA_Y9", "E09AB_Y9", "E09AC_Y9", "E09B_Y9", "E09BA_Y9", "E09BB_Y9", "E09BC_Y9", "E09C_Y9", "E09CA_Y9", "E09CB_Y9", 
                "E09CC_Y9", "E10_Y9", "E10A_Y9", "E11B_Y9", "E11C_Y9", "E11D_Y9", "E11E_Y9", "E11F_Y9", "E11G_Y9") # there are more...

smk_time_points <- qdapRegex::ex_between(ls(pattern = "smk_"), "_", "_")
smk_time_points <- as.character(smk_time_points)
smk_items <- gsub("_0", "", smk_0_vars)

## PAH (polycyclic aromatic hydrocarbons)
codebook[which(grepl("BAA", codebook$variable_name, fixed=TRUE)),]


pah_time_points <-"0"
pah_items <- colnames(pah)[3:length(colnames(pah))]


## Phthalates

uphthm1_vars <- gsub("R1U", "", colnames(uphthm1)[which(grepl("R1U", colnames(uphthm1), fixed = TRUE))])
phthal_tab <- codebook[which(grepl("R1U", codebook$variable_name, fixed=TRUE)),]
phthal_time_points <- c("0")
phthal_items <- unique(c(uphthm1_vars))

df_phthal <- data.frame()
desc_phthal <- data.frame()
family <- "phthalates"

for (p in 1:length(phthal_time_points)){
  tab_n <- p
  per_loc <- phthal_time_points[tab_n] 
  if(per_loc == 0){num <-1} else {num <- as.numeric(per_loc) / 12}
  tab_obj <- paste0("uphthm", num)
  tab <- get(tab_obj)
  ids <- tab$SID
  per_month <- as.numeric(per_loc)
  df_loc <- data.frame(SID = ids, month = per_month)
  items <- get(paste0(tab_obj, "_vars")) 
  for(t in 1:length(items)){
    item_n <- t
    name_it <- items[item_n]
    col_n <- paste0("R", num, "U", name_it)
    if(col_n %in% names(tab)){
     desc_phthal <- rbind(desc_phthal, data.frame(variable_name = col_n, variable_description = tolower(codebook[which(codebook$variable_name == col_n), "variable_description"]), exposure = items[t], month = per_month, 
                                           family = family, table_name = tolower(codebook[which(codebook$variable_name == col_n), "dta_table_name"]), 
                                           table_description = tolower(codebook[which(codebook$variable_name == col_n), "data_table_description"])))
    df_loc <- dplyr::left_join(df_loc, tab[,c("SID", col_n)], by = "SID")
    colnames(df_loc)[length(colnames(df_loc))] <- items[t]
    if(any(df_loc[,length(colnames(df_loc))] %in% c(-9))){
      df_loc[,length(colnames(df_loc))][which(df_loc[,length(colnames(df_loc))] %in% c(-9))] <- NA
    }
      } else {
      print(paste0("column name ", col_n, " is not present in ", tab_obj, " dataset."))
    }
  }
  df_phthal <-   dplyr::bind_rows(df_phthal, df_loc)
}
# specific gravity correction
df_phthal_corr <- dplyr::left_join(df_phthal, sgm1[,c('SID', 'M1SG')], by = 'SID')
df_phthal_corr$specific_gravity_corr <- ifelse(!is.na(df_phthal_corr$M1SG), "yes", "no")
df_phthal_corr[which(df_phthal_corr$month != 0), "specific_gravity_corr"] <- "no"
out <- which(colnames(df_phthal_corr) %in% c("SID", "month", "M1SG", "specific_gravity_corr"))
df_phthal_corr_orig <- df_phthal_corr
median_sg <- median(df_phthal_corr$M1SG, na.rm = T)
# following correction by Cantonwine et al. (2015)
df_phthal_corr[,-out] <- df_phthal_corr[,-out] * ((median_sg - 1)/(df_phthal_corr$M1SG - 1))
df_phthal_corr$M1SG[which(df_phthal_corr$specific_gravity_corr == "no")] <- NA
saveRDS(df_phthal_corr, paste0(generated.data.folder, "phthalates_specific_gravity_corr.rds"))

# find out which values are below the lod
df_phthal_lod <- data.frame()
for (p in 1:length(phthal_time_points)){
  tab_n <- p
  per_loc <- phthal_time_points[tab_n] 
  if(per_loc == 0){num <-1} else {num <- as.numeric(per_loc) / 12}
  tab_obj <- paste0("uphthm", num)
  tab <- get(tab_obj)
  ids <- tab$SID
  per_month <- as.numeric(per_loc)
  df_loc <- data.frame(SID = ids, month = per_month)
  items <- get(paste0(tab_obj, "_vars")) 
  for(t in 1:length(items)){
    item_n <- t
    name_it <- items[item_n]
    col_n <- paste0("F", num, "U", name_it)
    if(col_n %in% names(tab)){
      df_loc <- dplyr::left_join(df_loc, tab[,c("SID", col_n)], by = "SID")
      colnames(df_loc)[length(colnames(df_loc))] <- items[t]
      if(any(df_loc[,length(colnames(df_loc))] %in% c(-9))){
        df_loc[,length(colnames(df_loc))][which(df_loc[,length(colnames(df_loc))] %in% c(-9))] <- NA
      }
    } else {
      print(paste0("column name ", col_n, " is not present in ", tab_obj, " dataset."))
    }
  }
  df_phthal_lod <-   dplyr::bind_rows(df_phthal_lod, df_loc)
}
saveRDS(df_phthal_lod, paste0(generated.data.folder, "phthalates_below_lod.rds")) # 1 = below lod
# check if lod > 50%
n <- numeric()
for(p in 3:14){
n[p] <- 100*(length(which(df_phthal_lod[,p] == 1))/nrow(df_phthal_lod))
}

## BPA bisphenols
bpa_tab <- codebook[which(grepl("BPA", codebook$variable_name, fixed=TRUE)),]
#kylie: The BPA file that was sent has BPA measured in urine from the prenatal period through age 7. (M1UBPA though M7UBPA) There are also flag and date variables included in the file.
bpa_0_vars <- c("M1UBPA", "FM1UBPA") # maternal, well Maternal Urine BPA result ng/ml
bpa_time_points <- qdapRegex::ex_between(ls(pattern = "bpa_"), "_", "_vars")[1:4]
bpa_time_points <- as.character(bpa_time_points)
bpa_items <- gsub("M1", "", bpa_0_vars)

df_bpa <- data.frame()
desc_bpa <- data.frame()
family <- "bisphenols"

for (p in 1:length(bpa_time_points)){
  tab_n <- p
  per_loc <- bpa_time_points[tab_n] 
  tab_obj <- "bpa"
  tab <- get(tab_obj)
  ids <- tab$SID
  per_month <- as.numeric(per_loc)
  df_loc <- data.frame(SID = ids, month = per_month)
    if(per_month == 0){col_n = "M1UBPA"}
  if(col_n %in% names(tab)){
    df_loc <- dplyr::left_join(df_loc, tab[,c("SID", col_n)], by = "SID")
    colnames(df_loc)[length(colnames(df_loc))] <- bpa_items[1]
    if(any(df_loc[,length(colnames(df_loc))] %in% c(-9))){
      df_loc[,length(colnames(df_loc))][which(df_loc[,length(colnames(df_loc))] %in% c(-9))] <- NA
    }
    } else {
      print(paste0("column name ", col_n, " is not present in ", tab_obj, " dataset."))
    }
  df_bpa <-   dplyr::bind_rows(df_bpa, df_loc)
  }

# specific gravity correction

df_bpa_corr <- dplyr::left_join(df_bpa, sgm1[,c('SID', 'M1SG')], by = 'SID')
df_bpa_corr$specific_gravity_corr <- ifelse(!is.na(df_bpa_corr$M1SG), "yes", "no")
df_bpa_corr[which(df_bpa_corr$month != 0), "specific_gravity_corr"] <- "no"
out <- which(colnames(df_bpa_corr) %in% c("SID", "month", "M1SG", "specific_gravity_corr"))
df_bpa_corr_orig <- df_bpa_corr
median_sg <- median(df_bpa_corr$M1SG, na.rm = T)
df_bpa_corr[,-out] <- df_bpa_corr[,-out] * ((median_sg - 1)/(df_bpa_corr$M1SG - 1))
df_bpa_corr$M1SG[which(df_bpa_corr$specific_gravity_corr == "no")] <- NA
saveRDS(df_bpa_corr, paste0(generated.data.folder, "bisphenols_specific_gravity_corr.rds"))

# get values below lod
df_bpa_lod <- data.frame()
for (p in 1:length(bpa_time_points)){
  tab_n <- p
  per_loc <- bpa_time_points[tab_n] 
  tab_obj <- "bpa"
  tab <- get(tab_obj)
  ids <- tab$SID
  per_month <- as.numeric(per_loc)
  df_loc <- data.frame(SID = ids, month = per_month)
  # todo: what do we do with values below limit of detection - given 0.2
  if(per_month == 0){col_n = "FM1UBPA"}
  if(col_n %in% names(tab)){
    df_loc <- dplyr::left_join(df_loc, tab[,c("SID", col_n)], by = "SID")
    colnames(df_loc)[length(colnames(df_loc))] <- bpa_items[1]
    if(any(df_loc[,length(colnames(df_loc))] %in% c(-9))){
      df_loc[,length(colnames(df_loc))][which(df_loc[,length(colnames(df_loc))] %in% c(-9))] <- NA
    }
  } else {
    print(paste0("column name ", col_n, " is not present in ", tab_obj, " dataset."))
  }
  df_bpa_lod <-   dplyr::bind_rows(df_bpa_lod, df_loc)
}

saveRDS(df_bpa_lod, paste0(generated.data.folder, "bisphenols_below_lod.rds"))
# check if lod > 50%
length(which(df_bpa_lod$UBPA ==1)) / length(df_bpa_lod$UBPA)

# initiative dataframe with subject ids
tab_order <- c("PRENATAL")
period <- c("0")
df_mh <- data.frame()
desc_mh <- data.frame()
family <- "material_hardship"
for (p in 1:length(mh_time_points)){
  if(exists("tab")){rm(tab)}
  tab_n <- p
  per_loc <- mh_time_points[tab_n] 
  if(per_loc == "prenatal"){per_loc <- "0"}
  tab_obj <- tolower(tab_order[which(period == per_loc)])
  tab <- get(tab_obj)
  ids <- tab$SID
  if(is.na(as.numeric(per_loc))){
  } else {
    per_month <- as.numeric(per_loc)
  }
  df_loc <- data.frame(SID = ids, month = per_month)
  for(t in 1:length(mh_items)){
    item_n <- t
    name_it <- mh_items[item_n]
    col_n <- paste0(name_it, "_", per_loc)
    if(col_n %in% names(tab)){
      desc_mh <- rbind(desc_mh, data.frame(variable_name = col_n, variable_description = tolower(codebook[which(codebook$variable_name == col_n), "variable_description"]), exposure = name_it, month = per_month, 
                                           family = family, table_name = tolower(codebook[which(codebook$variable_name == col_n), "dta_table_name"]), 
                                           table_description = tolower(codebook[which(codebook$variable_name == col_n), "data_table_description"])))
    df_loc <- dplyr::left_join(df_loc, tab[,c("SID", col_n)], by = "SID")
    colnames(df_loc)[length(colnames(df_loc))] <- mh_items[item_n]
    if(any(df_loc[,length(colnames(df_loc))] %in% c(8,9))){
      df_loc[,length(colnames(df_loc))][which(df_loc[,length(colnames(df_loc))] %in% c(8,9))] <- NA
    }
    # rescale All stress related item responses to 0-1 where higher scores indicating more stress. 
    # Material hardship
    if(name_it %in% c("A10")){
      df_loc[,length(colnames(df_loc))] <-  (df_loc[,length(colnames(df_loc))] -1) / 4
    } else if (name_it %in% c("A11", "A12", "A13", "A14", "A15", "A16", "A17")){
      df_loc[,length(colnames(df_loc))] <-  (2-df_loc[,length(colnames(df_loc))])
    }
    } else {
      print(paste0("column name ", col_n, " is not present in ", tab_obj, " dataset."))
    }
  }
  df_mh <-   dplyr::bind_rows(df_mh, df_loc)
}
saveRDS(df_mh, paste0(generated.data.folder, "material_hardship.rds"))

# demoralization
df_dem <- data.frame()
desc_dem<- data.frame()
family <- "demoralization"
for (p in 1:length(dem_time_points)){
  if(exists("tab")){rm(tab)}
  tab_n <- p
  per_loc <- dem_time_points[tab_n] 
  if(per_loc == "prenatal"){per_loc <- "0"}
  tab_obj <- tolower(tab_order[which(period == per_loc)])
  tab <- get(tab_obj)
  ids <- tab$SID
  if(is.na(as.numeric(per_loc))){
  } else {
    per_month <- as.numeric(per_loc)
  }
  df_loc <- data.frame(SID = ids, month = per_month)
  for(t in 1:length(dem_items)){
    item_n <- t
    name_it <- dem_items[item_n]
    col_n <- paste0(name_it, "_", per_loc)      
    if(col_n %in% names(tab)){
      desc_dem <- rbind(desc_dem, data.frame(variable_name = col_n, variable_description = tolower(codebook[which(codebook$variable_name == col_n), "variable_description"]), exposure = name_it, month = per_month, 
                                           family = family, table_name = tolower(codebook[which(codebook$variable_name == col_n), "dta_table_name"]), 
                                           table_description = tolower(codebook[which(codebook$variable_name == col_n), "data_table_description"])))
      df_loc <- dplyr::left_join(df_loc, tab[,c("SID", col_n)], by = "SID")
      colnames(df_loc)[length(colnames(df_loc))] <- dem_items[item_n]
      if(any(df_loc[,length(colnames(df_loc))] %in% c(8,9))){
        df_loc[,length(colnames(df_loc))][which(df_loc[,length(colnames(df_loc))] %in% c(8,9))] <- NA
      }
      if(name_it %in% c("L01", "L02", "L03", "L04", "L05","L06","L07","L08","L09","L10","L11",
                        "L12", "L13", "L14", "L15", "L16","L17","L18","L19","L20","L21","L22",
                        "L23","L24","L25", "L26","L27")){
        df_loc[,length(colnames(df_loc))] <-  df_loc[,length(colnames(df_loc))] / 4
      }
    } else {
      print(paste0("column name ", col_n, " is not present in ", tab_obj, " dataset."))
    }
  }
  df_dem <-   dplyr::bind_rows(df_dem, df_loc)
}
saveRDS(df_dem, paste0(generated.data.folder, "demoralization.rds"))

## ETS (environmental tobacco smoke)

# questionnaire
df_smk <- data.frame()
desc_ets<- data.frame()
family <- "env_tobacco_smoke"
for (p in 1:length(smk_time_points)){
  if(exists("tab")){rm(tab)}
  tab_n <- p
  per_loc <- smk_time_points[tab_n] 
  if(per_loc == "prenatal"){per_loc <- "0"}
  tab_obj <- tolower(tab_order[which(period == per_loc)])
  tab <- get(tab_obj)
  ids <- tab$SID
  if(is.na(as.numeric(per_loc))){
    if(per_loc == "Y9"){per_month <- 108} else if(per_loc == "Y11"){per_month <- 132} else if(per_loc == "Y14"){per_month <- 168}
  } else {
    per_month <- as.numeric(per_loc)
  }
  df_loc <- data.frame(SID = ids, month = per_month)
  for(t in 1:length(smk_items)){
    item_n <- t
    name_it <- smk_items[item_n]
    if(per_loc == "Y11"){
      col_n <- paste0(name_it, per_loc)  
    } else if (per_loc == "Y14") {
      col_n <- paste0(name_it, "Y11")       
    } else {
      col_n <- paste0(name_it, "_", per_loc)      
    }
    if(col_n %in% names(tab)){
      desc_ets <- rbind(desc_ets, data.frame(variable_name = col_n, variable_description = tolower(codebook[which(codebook$variable_name == col_n), "variable_description"]), exposure = name_it, month = per_month, 
                                               family = family, table_name = tolower(codebook[which(codebook$variable_name == col_n), "dta_table_name"]), 
                                               table_description = tolower(codebook[which(codebook$variable_name == col_n), "data_table_description"])))
      df_loc <- dplyr::left_join(df_loc, tab[,c("SID", col_n)], by = "SID")
      colnames(df_loc)[length(colnames(df_loc))] <- smk_items[item_n]
      if(any(df_loc[,length(colnames(df_loc))] %in% c(8,9))){
        df_loc[,length(colnames(df_loc))][which(df_loc[,length(colnames(df_loc))] %in% c(8,9))] <- NA
      }
      if(name_it %in% c("V01A", "V01B", "V01C", "V01D", "V01E", "V01F", "V01G", "V01H", "V01I")){
        df_loc[,length(colnames(df_loc))] <-  df_loc[,length(colnames(df_loc))] / 4
      } else if (name_it %in% c("V02", "V04")){
        df_loc[,length(colnames(df_loc))] <-  2 - df_loc[,length(colnames(df_loc))] 
      }
    } else {
      print(paste0("column name ", col_n, " is not present in ", tab_obj, " dataset."))
    }
  }
  df_smk <-   dplyr::bind_rows(df_smk, df_loc)
}
#saveRDS(df_smk, paste0(generated.data.folder, "smoke_self_report.rds"))
df_smk <- readRDS(paste0(generated.data.folder, "smoke_self_report.rds"))
# cotinine
family <- "env_tobacco_smoke"
tab_obj <- "cotinine"
df_cot <- data.frame()
for (p in 1:length(cot_time_points)){
  if(exists("tab")){rm(tab)}
  tab_n <- p
  per_loc <- cot_time_points[tab_n] 
  if(per_loc == "prenatal"){per_loc <- "0"}
  tab <- get(tab_obj)
  ids <- tab$SID
  if(is.na(as.numeric(per_loc))){
    if(per_loc == "Y9"){per_month <- 108} else if(per_loc == "Y11"){per_month <- 132} else if(per_loc == "Y14"){per_month <- 168}
  } else {
    per_month <- as.numeric(per_loc)
  }
  df_loc <- data.frame(SID = ids, month = per_month)
  for(t in 1:length(cot_items[1])){ # only interested in cotinine value because we care about if cotinine higher than 1 for exposed, which is higher than LOD
    item_n <- t
    name_it <- cot_items[item_n]
    if(per_loc == "Y9"){
      col_n <- paste0(per_loc, name_it)  
    } else if (per_loc == "0") { 
      col_n <- paste0("MOM", name_it)       
    } else {
      col_n <- paste0("M", per_loc, name_it)      
    }
    if(col_n %in% names(tab)){
      desc_ets <- rbind(desc_ets, data.frame(variable_name = col_n, variable_description = tolower(codebook[which(codebook$variable_name == col_n), "variable_description"]), exposure = name_it, month = per_month, 
                                             family = family, table_name = tolower(codebook[which(codebook$variable_name == col_n), "dta_table_name"]), 
                                             table_description = tolower(codebook[which(codebook$variable_name == col_n), "data_table_description"])))
      df_loc <- dplyr::left_join(df_loc, tab[,c("SID", col_n)], by = "SID")
      colnames(df_loc)[length(colnames(df_loc))] <- cot_items[item_n]
      if(any(df_loc[,length(colnames(df_loc))] %in% c(-7,-9))){
        df_loc[,length(colnames(df_loc))][which(df_loc[,length(colnames(df_loc))] %in% c(-7,-9))] <- NA
      }
    } else {
      print(paste0("column name ", col_n, " is not present in ", tab_obj, " dataset."))
    }
  }
  df_cot <-   dplyr::bind_rows(df_cot, df_loc)
}
df_cot$smk_exp_cot <- ifelse(df_cot$COT >= 1, 1, 0)

df_cot <- dplyr::left_join(df_cot, df_smk[,c("SID", "month", "E01", "E10")], by = c("SID", "month"))

# change direction to E01 and E10
df_cot$E01 <- 2 - df_cot$E01
df_cot$E10 <- 2 - df_cot$E10

df_cot$smk_self <- ifelse((df_cot$E01 > 0 | df_cot$smk_exp_cot > 0), 1, 0)
df_cot$smk_exp <- ifelse((df_cot$E10 > 0 | df_cot$smk_exp_cot > 0), 1, 0)
desc_ets_der <- data.frame(variable_name = c("COT", "smk_exp_cot", "E01", "E10", "smk_self", "smk_exp"), 
                           variable_description = c("maternal cotinine (ng/ml)", "cotinine exposure", "questionnaire smoke", "questionnaire second-hand smoke", "tobacco smoke self", "tobacco smoke SHS"),
                           exposure = c("COT", "smk_exp_cot", "E01", "E10", "smk_self", "smk_exp"), 
           family = "environmental_tobacco_smoke", 
           table_name = "tobacco / cotinine", 
           table_description = "tobacco questionnaire and cotinine measurements")
desc_ets_der_m <- data.frame()
for(i in 1:length(unique(df_cot$month))){
  desc_ets_der_loc <- data.frame(desc_ets_der, month = unique(df_cot$month)[i])
  desc_ets_der_m <- rbind(desc_ets_der_m, desc_ets_der_loc)
  }
saveRDS(df_cot, paste0(generated.data.folder, "smoke_cot_derivative_self_report.rds"))



# PAH
pah$month <- 0
saveRDS(pah, paste0(generated.data.folder, "pah_total.rds"))
desc_pah <- data.frame(variable_name = "totalpah", variable_description = "total PAH in ng/m3 ", exposure = "totalpah", month = 0, 
                                       family = "air pollution", table_name = "pah", 
                                       table_description = "pah total")

# PAH adducts

adducts <- adducts_month0
# pah adducts not available
adducts$madducts_avail <- ifelse((adducts$MADDUCTS == -6 | 
                                     adducts$MADDUCTS == -8 | 
                                     adducts$MADDUCTS == -9
                                   ), 1, 0)
# pah adducts not available as na
adducts$MADDUCTS[which(adducts$madducts_avail == 1)] <- NA

adducts$cadducts_avail <- ifelse((adducts$CADDUCTS == -6 | 
                                           adducts$CADDUCTS == -8 | 
                                           adducts$CADDUCTS == -9
), 1, 0)
adducts$CADDUCTS[which(adducts$cadducts_avail == 1)] <- NA

adducts <- adducts[c("SID", "MADDUCTS", "CADDUCTS")]
colnames(adducts)[c(2,3)] <- tolower(colnames(adducts)[c(2,3)])
adducts$month <- 0
adducts$madducts_below_lod <- ifelse((adducts$madducts == 0.125), 1, 0)
adducts$cadducts_below_lod <- ifelse((adducts$cadducts == 0.125), 1, 0)
# check if lod > 50% 
length(which(adducts$madducts_below_lod == 1)) / length(adducts$madducts_below_lod)
length(which(adducts$cadducts_below_lod == 1)) / length(adducts$cadducts_below_lod)
saveRDS(adducts, paste0(generated.data.folder, "pah_adducts.rds"))
desc_pah_adducts <- data.frame(variable_name = 
                                 c("madducts", "cadducts"), 
                               variable_description = 
                                 c("maternal PAH-DNA adducts", 
                                   "cord PAH-DNA adducts"), 
                               exposure = 
                                 c("madducts", "cadducts"), month = 0, 
                       family = "PAH-DNA adducts", table_name = "PAH-DNA Adducts", 
                       table_description = "PAH-DNA Adducts")


# PBDE 
pbde <- pbde_month0
pbde$month <- pbde$bloodage*12
# get percentage of each variable without values
variables <- c("sid", "month", colnames(pbde)[which(grepl("lw", colnames(pbde), fixed = TRUE))])
variables_detect <- c("sid", "month", colnames(pbde)[which(grepl("detect", colnames(pbde), fixed = TRUE))])
variables_lod <- c("sid", "month", colnames(pbde)[which(grepl("lwlod", colnames(pbde), fixed = TRUE))])

pbde_detect <- pbde[,variables_detect]
pbde_lod <- pbde[,variables_lod]
pbde <- pbde[,variables]


# create an imputed version of the PBDEs
# from Herbstman et al. (2010) We used the LOD divided by the square root of 2 for concentrations below the LOD
pbde_imp <- pbde
for(i in which(grepl("lw_", colnames(pbde_imp), fixed = TRUE))){
  for(nas in which(is.na(pbde_imp[,colnames(pbde_imp)[i]]))){
    pbde_imp[nas,colnames(pbde_imp)[i]] <- pbde_imp[nas,colnames(pbde_imp)[i+1]] / sqrt(2)
  }
}

pbde_imp <- pbde_imp[,c("sid", "month", colnames(pbde_imp)[which(grepl("lw_", colnames(pbde_imp), fixed = TRUE))])]
colnames(pbde_imp)[1] <- "SID"
pbde <- pbde[,c("sid", "month", colnames(pbde)[which(grepl("lw_", colnames(pbde), fixed = TRUE))])]
colnames(pbde)[1] <- "SID"
# restore na's in pbde_imp for values not reported
for (col in c(3:14)){
  if(any(pbde_detect[,col] == "NOTREPOR")){
    pbde_imp[which(pbde_detect[,col] == "NOTREPOR"), col] <- NA
  }
}
# delete pbde variables that have more than 50% of values < lod

pbde_lod_perc <- (colMeans(pbde_detect == "<LOD"))*100
x <- pbde_lod_perc
x <- x[order(x, decreasing = TRUE)]

whichs <- numeric()
for(i in 1:length(gsub("detect_", "", names(which(x > 50))))){
whichs[i] <- which(stringr::str_detect(colnames(pbde_imp), gsub("detect_", "", names(which(x > 50)))[i]))
}
pbde_imp <- pbde_imp[,-whichs]
pbde_lod <- pbde_lod[,-whichs]
saveRDS(pbde_imp, paste0(generated.data.folder, "pbde_imp.rds"))
saveRDS(pbde, paste0(generated.data.folder, "pbde.rds"))
saveRDS(pbde_lod, paste0(generated.data.folder, "pbde_lod_values.rds"))
#
pbde_lod <- pbde[,-c(1,2)] %>% replace(!is.na(.), 0) %>% replace(is.na(.), 1)
pbde_lod <- cbind(pbde[,c(1,2)],pbde_lod)
saveRDS(pbde_lod, paste0(generated.data.folder, "pbde_lod.rds"))
saveRDS(pbde_detect, paste0(generated.data.folder, "pbde_detect.rds"))

pbde_imp <- readRDS(paste0(generated.data.folder, "pbde_imp.rds"))


pbde_names <- colnames(pbde)[-c(1,2)] # substr(colnames(pbde)[-c(1,2)],4, nchar(colnames(pbde)[-c(1,2)])) 
desc_pbde_der <- data.frame(
  variable_name = colnames(pbde)[-c(1,2)], 
  variable_description = paste0("analyte concentration ", pbde_names," in pg/g lipid"), 
  exposure = pbde_names, 
  family = "PBDEs", 
  table_name = "MN_PBDE_longformat", 
  table_description = "pbdes analyte concentration"
  )
desc_pbde_der_m <- data.frame()
for(i in 1:length(unique(pbde$month))){
  desc_pbde_der_loc <- data.frame(desc_pbde_der, month = unique(pbde$month)[i])
  desc_pbde_der_m <- rbind(desc_pbde_der_m, desc_pbde_der_loc)
}


# save description
exposure_description <- rbind(desc_mh, desc_dem, desc_pah,desc_ets_der_m, desc_bpa, desc_phthal, desc_pbde_der_m, desc_pah_adducts)
saveRDS(exposure_description, paste0(generated.data.folder, "exposure_description.rds"))
