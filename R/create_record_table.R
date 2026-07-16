# stk <- c("nw", "ap", "sp", "ao", "ao_grp", "st_dai")
stk <- c("nw")
# kpi <- c("flt")

### import source table
source_table <- paste0("RECORD_", toupper(stk))
# source_table2 <- paste0("RECORD_", toupper(stk), "_", toupper("dly"))

data_source <- export_query(glue(
  "select * from {source_table}"
)) %>%
  mutate(INIT_DATE_PERIOD = as.Date(INIT_DATE_PERIOD))

# data_source2 <- export_query(glue(
#   "select * from {source_table2}"
# )) %>%
#   mutate(INIT_DATE_PERIOD = as.Date(INIT_DATE_PERIOD))

data_norm <- data_source %>%
  mutate(
    FLT_UID = NA
  ) %>%
  select(
    STK_ID,
    STK_TYPE,
    INIT_DATE_PERIOD,
    KPA,
    KPI,
    KPI_AVG_VALUE,
    RANK,
    PERIOD,
    DAYS_PERIOD,
    FLT_UID,
    LAST_UPDATED
  )

# data_norm2 <- data_source2 %>%
#   mutate(
#     STK_TYPE = 'nw',
#     KPA = 'atfm_dly',
#     KPI = 'total_atfm_dly'
#   ) %>%
#   select(
#     STK_ID = AREA,
#     STK_TYPE,
#     INIT_DATE_PERIOD,
#     KPA,
#     KPI,
#     KPI_AVG_VALUE = AVG_FLT,
#     RANK,
#     PERIOD,
#     DAYS_PERIOD,
#     LAST_UPDATED
#   )

new_data <- data_norm

new_source_table <- paste0("RECORD_", toupper(stk))

write_table_oracle(new_data, new_source_table, append = FALSE)


new_data_source <- export_query(glue(
  "select * from {new_source_table}"
)) %>%
  mutate(INIT_DATE_PERIOD = as.Date(INIT_DATE_PERIOD))

write_table_oracle(
  new_data,
  paste0("ZZ_BAD_", new_source_table),
  append = FALSE
)
