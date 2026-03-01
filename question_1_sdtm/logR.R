library(logrx)

log_remove()

programs <- c("/cloud/project/question_1_sdtm/01_create_ds_domain.R")

for (prog in programs) {
  axecute(file = prog)
  log_remove()
}
