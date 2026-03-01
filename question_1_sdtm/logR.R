###-----------------------------Header---------------------------------------###
# Program : logR.R
# Author  : S Shankar
# Location: ~/question_1_sdtm/logR.R
# Purpose : Wrapper program to create log for 01_create_ds_domain.R with logrx.
#------------------------------------------------------------------------------#
# Modification History
#  01MAR2026 S Shankar
#  Initiated program
###--------------------------------------------------------------------------###

source("functions.R")

pkgs <- c("logrx")
install_and_load(pkgs)

log_remove()

programs <- c("/cloud/project/question_1_sdtm/01_create_ds_domain.R")

for (prog in programs) {
  axecute(file = prog)
  log_remove()
}
