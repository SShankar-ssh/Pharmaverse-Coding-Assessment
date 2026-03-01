###-----------------------------Header---------------------------------------###
# Program : 01_create_DS.R
# Author  : S Shankar
# Location: ~/question_1_sdtm/01_create_ds_domain.R
# Purpose : Create SDTM for disposition data
#------------------------------------------------------------------------------#
# Input Data : pharmaverseraw::ds_raw
#              pharmaversesdtm::dm
# Output Data: ds.csv
#------------------------------------------------------------------------------#
# Modification History
#  01MAR2026 S Shankar
#  Initiated program
###--------------------------------------------------------------------------###

## ----r setup, message=FALSE, warning=FALSE, results='hold'--------------------
source("functions.R")

pkgs <- c("dplyr", "sdtm.oak", "pharmaverseraw", "janitor", "xlsx", "readr")
install_and_load(pkgs)

ds_raw <- pharmaverseraw::ds_raw

## ----r------------------------------------------------------------------------
dm <- pharmaversesdtm::dm

## ----r------------------------------------------------------------------------
ds_raw <- ds_raw |>
  generate_oak_id_vars(
    pat_var = "PATNUM",
    raw_src = "ds_raw"
  )

## ----r, echo = TRUE-----------------------------------------------------------
study_ct <- read.csv("metadata/sdtm_ct.csv")

## ----r------------------------------------------------------------------------
ds <-
  # Map DSSTDTC using assign_datetime, raw_var=IT.DSSTDAT
  assign_datetime(
    raw_dat = ds_raw,
    raw_var = "IT.DSSTDAT",
    tgt_var = "DSSTDTC",
    raw_fmt = c("m-d-y"),
    id_vars = oak_id_vars(),
    raw_unk = c("UN", "UNK")
  ) |>
  # Map DSDTC using assign_datetime, raw_var=c("DSDTCOL", "DSTMCOL")
  assign_datetime(
    raw_dat = ds_raw,
    raw_var = c("DSDTCOL", "DSTMCOL"),
    tgt_var = "DSDTC",
    raw_fmt = c("m-d-y", "H:M"),
    id_vars = oak_id_vars(),
    raw_unk = c("UN", "UNK")
  ) 

## ----r------------------------------------------------------------------------
ds2 <- ds_raw %>%mutate(  
  #Copy IT.DSTERM or OTHERSP to DSTERM  
  #Copy IT.DSDECOD or OTHERSP to DSDECOD  
  DSDECOD = if_else(    is.na(OTHERSP),    IT.DSDECOD,    OTHERSP  ),  
  DSTERM = if_else(    is.na(OTHERSP),    IT.DSTERM,    OTHERSP  ),  
  DSCAT = case_when(    !is.na(OTHERSP)             ~ "OTHER EVENT",    
                        IT.DSDECOD == "Randomized"  ~ "PROTOCOL MILESTONE",    
                        TRUE                       ~ "DISPOSITION EVENT"  ),  
  VISITNUM = case_when( INSTANCE == "Screening"            ~ 1,    
                        INSTANCE == "Visit 1"              ~ 2,    
                        INSTANCE == "Screening 1"          ~ 1,    
                        INSTANCE == "Screening 2"          ~ 2,    
                        INSTANCE == "Baseline"             ~ 3,    
                        INSTANCE == "Week 2"               ~ 4,    
                        INSTANCE == "Week 4"               ~ 5,   
                        INSTANCE == "Week 6"               ~ 7,    
                        INSTANCE == "Week 8"               ~ 8,    
                        INSTANCE == "Week 12"              ~ 9,    
                        INSTANCE == "Week 16"              ~ 10,    
                        INSTANCE == "Week 20"              ~ 11,    
                        INSTANCE == "Week 24"              ~ 12,    
                        INSTANCE == "Week 26"              ~ 13,    
                        INSTANCE == "Retrieval"            ~ 201,    
                        INSTANCE == "Unscheduled 3.1"      ~ 3.1,    
                        INSTANCE == "Ambul ECG Placement"  ~ 3.5,    
                        INSTANCE == "Ambul ECG Removal"    ~ 6,    
                        TRUE                               ~ NA_real_  ), 
  VISIT = case_when(    INSTANCE == "Screening"            ~ "SCREENING",    
                        INSTANCE == "Visit 1"              ~ "VISIT 1",    
                        INSTANCE == "Screening 1"          ~ "SCREENING 1",    
                        INSTANCE == "Screening 2"          ~ "SCREENING 2",    
                        INSTANCE == "Baseline"             ~ "BASELINE",    
                        INSTANCE == "Week 2"               ~ "WEEK 2",    
                        INSTANCE == "Week 4"               ~ "WEEK 4",   
                        INSTANCE == "Week 6"               ~ "WEEK 6",    
                        INSTANCE == "Week 8"               ~ "WEEK 8",   
                        INSTANCE == "Week 12"              ~ "WEEK 12",   
                        INSTANCE == "Week 16"              ~ "WEEK 16",   
                        INSTANCE == "Week 20"              ~ "WEEK 20",    
                        INSTANCE == "Week 24"              ~ "WEEK 24",    
                        INSTANCE == "Week 26"              ~ "WEEK 26",    
                        INSTANCE == "Retrieval"            ~ "RETRIEVAL",   
                        INSTANCE == "Unscheduled 3.1"      ~ "UNSCHEDULED 3.1",   
                        INSTANCE == "Ambul ECG Placement"  ~ "AMBUL ECG PLACEMENT",   
                        INSTANCE == "Ambul ECG Removal"    ~ "AMBUL ECG REMOVAL",   
                        TRUE                               ~ NA_character_  ),    
  STUDYID = ds_raw$STUDY,    
  DOMAIN = "DS",    
  USUBJID = paste0("01-", ds_raw$PATNUM),    
  DSDECOD = toupper(DSDECOD),    
  DSTERM  = toupper(DSTERM)  ) |>  
  select(oak_id, patient_number, DSTERM, DSDECOD, DSCAT, STUDYID, DOMAIN,
         USUBJID, VISIT, VISITNUM)
## ----r------------------------------------------------------------------------
ds3 <- left_join(ds, ds2, by = c("oak_id", "patient_number"))
 
## ----r------------------------------------------------------------------------
ds4 <- ds3 |>
    derive_seq(
      tgt_var  = "DSSEQ",
      rec_vars = c("USUBJID", "DSTERM")
    ) |>
    derive_study_day(
      dm_domain = dm,
      tgdt      = "DSSTDTC",
      refdt     = "RFXSTDTC",
      study_day_var = "DSSTDY"
    ) |>
    select(
      "STUDYID", "DOMAIN", "USUBJID", "DSSEQ", "DSTERM", "DSDECOD", 
      "DSCAT", "VISITNUM", "VISIT", "DSDTC", "DSSTDTC", "DSSTDY"
    )

## ----r------------------------------------------------------------------------
#Export SDTM dataset
write_csv(ds4, "/cloud/project/question_1_sdtm/ds.csv")
