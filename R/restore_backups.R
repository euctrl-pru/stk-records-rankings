# LIBRARIES ----
source("R/libraries.R")

# FUNCTIONS ----
source("R/helpers.R")

# PARAMS ----
source("R/params.R")

# stk <- c("nw", "ap", "sp", "ao", "ao_grp", "st")
stk <- c("nw")

table_name <- paste0("RECORD_", toupper(stk))

# add the day with data that you want
# 'LastVersion' or day in format "%Y%m%d"
# backup_version <- 'LastVersion'
backup_version <- "20260821"


if (backup_version == 'LastVersion') {
  backup_file <- here(
    backup_folder,
    'LastVersion',
    paste0(tolower(table_name), ".csv")
  )
} else {
  backup_file <- here(
    backup_folder,
    paste0(
      backup_version,
      "_",
      tolower(table_name),
      ".csv"
    )
  )
}

backup_data <- read_csv(backup_file)
# backup_data2 <- read_csv(backup_file)
#
# backup_data_kpi <- backup_data %>%
#   mutate(KPI = 'flt') %>%
#   relocate(KPI, .before = AVG_FLT)%>%
#   rename(AVG_VALUE = AVG_FLT)
#
# backup_data2_kpi <- backup_data2 %>%
#   mutate(KPI = 'total_dly') %>%
#   relocate(KPI, .before = AVG_FLT) %>%
#   rename(AVG_VALUE = AVG_FLT)
#
# backup_data_kpi_all <- backup_data_kpi %>%
#   rbind(backup_data2_kpi)

# write_table_oracle(backup_data_kpi_all, "RECORD_NW", append = FALSE, overwrite = TRUE)

db_data <- export_query(glue("select * from {table_name}"))

# write backup of the bad data
table_name_bad_data <- paste0("ZZ_BAD_", table_name)
write_table_oracle(
  db_data,
  table_name_bad_data,
  append = FALSE,
  overwrite = TRUE
)

# restore backup
write_table_oracle(backup_data, table_name, append = FALSE, overwrite = TRUE)
