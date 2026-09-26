# LIBRARIES ----
source("R/libraries.R")

# FUNCTIONS ----
source("R/helpers.R")

# PARAMS ----
source("R/params.R")

# stk <- c("nw", "ap", "sp", "ao", "ao_grp", "st")
stk <- c("st")

table_name <- paste0("RECORD_", toupper(stk))

# add the day with data that you want
# 'LastVersion' or day in format "%Y%m%d"
# backup_version <- 'LastVersion'
backup_version <- "20260922"


restore_backup(stk, backup_version)
