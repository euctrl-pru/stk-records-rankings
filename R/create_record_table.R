library(eurocontrol)
library(readxl)
library(dplyr)
library(dbplyr)
library(lubridate)
library(here)
library(stringr)
library(DBI)
library(sendmailR)
library(glue)
library(eurocontrol)
library(readr)
library(purrr)

# FUNCTIONS ----
export_query <- function(query, schema = "PRU_READ") {
  withr::local_envvar(c(
    "TZ" = "UTC",
    "ORA_SDTZ" = "UTC",
    "NLS_LANG" = ".AL32UTF8"
  ))

  con <- withr::local_db_connection(
    eurocontrol::db_connection(schema = schema)
  )

  dplyr::tbl(con, dplyr::sql(query)) |>
    collect()
}

write_table_oracle <- function(
  data,
  table_name,
  schema = "PRU_READ",
  append = TRUE
) {
  withr::local_envvar(c(
    TZ = "UTC",
    ORA_SDTZ = "UTC",
    NLS_LANG = ".AL32UTF8"
  ))

  con <- eurocontrol::db_connection(schema = schema)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbWriteTable(
    con,
    name = table_name,
    value = data,
    append = append,
    row.names = FALSE
  )
}

# DIMENSIONS ----
## Airport ----
dim_ap <- export_query(
  "
                            select
                            BK_AP_ID,
                            CFMU_AP_CODE,
                            VALID_FROM,
                            VALID_TO
                            from pruread.v_aiu_dim_airport
                            "
)

list_ap <- export_query(
  'SELECT BK_AP_ID, EC_AP_NAME FROM pruread.v_aiu_app_list_airport'
)

list_id_ap <- list_ap |> pull(BK_AP_ID)
# list_airport_ids <- 5410

## ANSP ----
dim_sp <- "
SELECT
ANSP_ID, ANSP_NAME
FROM PRUDEV.V_PRU_REL_CFMU_AUA_ANSP
--WHERE ANSP_ID  in (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,26,27,28,29,30,31,32,33,39,42,44,45,46,50,53,56,57)
group by  ANSP_ID, ANSP_NAME
"

list_sp <- dim_sp

# QUERIES ----
## NOTE: Whatever the stakeholder, the output of the query should be,  in this order, stakeholder Id, date, flights. The name of the fields doesn't matter, but the order is important.

## Airport ----
ap_traffic_query <- "
WITH all_data AS (
  SELECT
  TRUNC(lobt) AS flt_date,
  bk_adep_id AS bk_ap_id,
  COUNT(flt_uid) AS mvt,
  'dep' AS flow_type
  FROM swh_fct.fac_flight
  WHERE bk_adep_id IN ({list_id_ap*})
  AND lobt < TO_DATE('2007-01-01', 'yyyy-mm-dd')
  AND flt_status IN ('TE','TA','AA')
  GROUP BY
  TRUNC(lobt),
  bk_adep_id

  UNION ALL

  SELECT
  TRUNC(lobt) AS flt_date,
  bk_ades_id AS bk_ap_id,
  COUNT(flt_uid) AS mvt,
  'arr' AS flow_type
  FROM swh_fct.fac_flight
  WHERE bk_ades_id IN ({list_id_ap*})
  AND lobt < TO_DATE('2007-01-01', 'yyyy-mm-dd')
  AND flt_status IN ('TE','TA','AA')
  GROUP BY
  TRUNC(lobt),
  bk_ades_id

  UNION ALL

  SELECT
  a.ADEP_DAY_FLT_DATE AS flt_date,
  b.bk_ap_id,
  SUM(a.ADEP_DAY_ALL_TRF) AS mvt,
  'dep' AS flow_type
  FROM aru_syn.AGG_dep_DAY a
  LEFT JOIN pruread.v_aiu_dim_airport b
  ON a.adep_day_adep = b.CFMU_AP_CODE
  AND a.ADEP_DAY_FLT_DATE >= b.valid_from
  AND a.ADEP_DAY_FLT_DATE <= b.valid_to
  WHERE EXTRACT(YEAR FROM a.ADEP_DAY_FLT_DATE) >= 2007
  AND b.bk_ap_id IN ({list_id_ap*})
  GROUP BY
  a.ADEP_DAY_FLT_DATE,
  b.bk_ap_id

  UNION ALL

  SELECT
  a.ADES_DAY_FLT_DATE AS flt_date,
  b.bk_ap_id,
  SUM(a.ADES_DAY_ALL_TRF) AS mvt,
  'arr' AS flow_type
  FROM aru_syn.AGG_arr_DAY a
  LEFT JOIN pruread.v_aiu_dim_airport b
  ON a.ADES_DAY_ADES_CTFM = b.CFMU_AP_CODE
  AND a.ADES_DAY_FLT_DATE >= b.valid_from
  AND a.ADES_DAY_FLT_DATE <= b.valid_to
  WHERE EXTRACT(YEAR FROM a.ADES_DAY_FLT_DATE) >= 2007
  AND b.bk_ap_id IN ({list_id_ap*})
  GROUP BY
  a.ADES_DAY_FLT_DATE,
  b.bk_ap_id
)
SELECT
bk_ap_id,
flt_date,
SUM(mvt) AS DEP_ARR
FROM all_data
GROUP BY
bk_ap_id,
flt_date
"
## ANSP ----
sp_traffic_query <- "
SELECT
	 unit_id AS ansp_id,
     FLIGHT_DATE,
     TTF_FLT AS flt
FROM PRUDEV.V_PRU_FAC_TD_DD
WHERE unit_kind = 'ANSP'
ORDER BY ansp_id, FLIGHT_DATE
"

# COLUMN NAMES ----
## Airport ----
colnames_ap <- c(
  "BK_AP_ID",
  "INIT_DATE_PERIOD",
  "AVG_DEP_ARR",
  "RANK",
  "PERIOD",
  "LAST_UPDATED"
)

## ANSP ----
colnames_sp <- c(
  "ANSP_ID",
  "INIT_DATE_PERIOD",
  "AVG_FLT",
  "RANK",
  "PERIOD",
  "LAST_UPDATED"
)

# GET BASIC DATA ----
### set stakeholder
stk <- "ap"

### build query ----
con <- eurocontrol::db_connection(schema = "PRU_READ")

sql_template <- get(paste0(stk, "_traffic_query"))

base_query <- as.character(
  glue::glue_sql(
    sql_template,
    .con = con
  )
)

DBI::dbDisconnect(con)

### execute query ----
# careful, the ap it takes hours to run
data_raw <- export_query(base_query)

# data_raw %>%
#   write_csv(
#     'G:/HQ/dgof-pru/Project/DDP/Projects/DDP-25-020_Data_snapshot_top_airports_max_flights_days/apt_traffic_all_days.csv'
#   )

# data_raw <- read_csv('G:/HQ/dgof-pru/Project/DDP/Projects/DDP-25-020_Data_snapshot_top_airports_max_flights_days/apt_traffic_all_days.csv')

# BUILD PERIOD LIST ----
# rank_period <- "DAY"

fake_dates <- as.Date("2026-05-04") + 0:6
rank_period <- toupper(wday(
  fake_dates,
  label = TRUE,
  abbr = FALSE,
  week_start = 1
))
rank_period <- append(rank_period, c("DAY", "WEEK"))

period_month <- paste0("MONTH_", sprintf("%02d", 1:12))
rank_period <- append(rank_period, c(period_month, "MONTH"))

period_quarter <- paste0("QUARTER_", sprintf("%02d", 1:4))
rank_period <- append(rank_period, c(period_quarter, "QUARTER", "YEAR"))


## process data ----
### normalise dataset
norm_colnames <- c("ID", "FLT_DATE", "FLT")

colnames(data_raw) <- norm_colnames

data_norm <- data_raw %>%
  mutate(FLT_DATE = as.Date(FLT_DATE))

### find cutoff dates
cutoff_date_month <- data_norm %>%
  filter(day(FLT_DATE) == days_in_month(FLT_DATE)) %>%
  summarise(cutoff_date = max(FLT_DATE, na.rm = TRUE)) %>%
  pull(cutoff_date)

cutoff_date_quarter <- data_norm %>%
  filter(FLT_DATE == ceiling_date(FLT_DATE, "quarter") - days(1)) %>%
  summarise(cutoff_date = max(FLT_DATE, na.rm = TRUE)) %>%
  pull(cutoff_date)

cutoff_date_week <- data_norm %>%
  filter(wday(FLT_DATE, week_start = 1) == 7) %>%
  summarise(cutoff_date = max(FLT_DATE, na.rm = TRUE)) %>%
  pull(cutoff_date)

cutoff_date_year <- data_norm %>%
  filter(FLT_DATE == ceiling_date(FLT_DATE, "year") - days(1)) %>%
  summarise(cutoff_date = max(FLT_DATE, na.rm = TRUE)) %>%
  pull(cutoff_date)

period_ranking <- function(rank_period) {
  if (str_detect(rank_period, "DAY")) {
    data_filtered <- data_norm %>%
      rename(
        INIT_DATE_PERIOD = FLT_DATE,
        AVG_FLT = FLT
      )
    if (rank_period != "DAY") {
      data_filtered <- data_filtered %>%
        filter(
          toupper(wday(
            INIT_DATE_PERIOD,
            label = TRUE,
            abbr = FALSE,
            week_start = 1
          )) ==
            rank_period
        )
    }
  } else if (rank_period == "WEEK") {
    data_filtered <- data_norm %>%
      mutate(
        WEEK = isoweek(FLT_DATE),
        WEEK_YEAR = isoyear(FLT_DATE)
      ) %>%
      group_by(ID, WEEK_YEAR, WEEK) %>%
      summarise(FLT = sum(FLT, na.rm = TRUE), .groups = "drop") %>%
      mutate(
        AVG_FLT = FLT / 7,
        # reconstruct first day of the week - chatgpt
        INIT_DATE_PERIOD = as.Date(paste0(WEEK_YEAR, "-01-04")) +
          7 * (WEEK - 1) -
          (wday(as.Date(paste0(WEEK_YEAR, "-01-04")), week_start = 1) - 1)
      )
  } else if (str_detect(rank_period, "MONTH")) {
    data_filtered <- data_norm %>%
      filter(FLT_DATE <= cutoff_date_month) %>%
      mutate(
        FLT_DATE = as.Date(FLT_DATE),
        YEAR = year(FLT_DATE),
        MONTH = month(FLT_DATE),
        DAYS = days_in_month(FLT_DATE)
      ) %>%
      group_by(ID, YEAR, MONTH, DAYS) %>%
      summarise(FLT = sum(FLT, na.rm = TRUE), .groups = "drop") %>%
      mutate(
        AVG_FLT = FLT / DAYS,
        INIT_DATE_PERIOD = ymd(paste0(YEAR, sprintf("%02d", MONTH), "01"))
      )
    if (rank_period != "MONTH") {
      data_filtered <- data_filtered %>%
        filter(sprintf("%02d", MONTH) == str_sub(rank_period, -2, -1))
    }
  } else if (str_detect(rank_period, "QUARTER")) {
    data_filtered <- data_norm %>%
      filter(FLT_DATE <= cutoff_date_quarter) %>%
      mutate(
        FLT_DATE = as.Date(FLT_DATE),
        YEAR = year(FLT_DATE),
        QUARTER = quarter(FLT_DATE),
        DAYS = days_in_month(FLT_DATE)
      ) %>%
      group_by(ID, YEAR, QUARTER, DAYS) %>%
      summarise(
        FLT = sum(FLT, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      group_by(ID, YEAR, QUARTER) %>%
      summarise(
        FLT = sum(FLT, na.rm = TRUE),
        DAYS = sum(DAYS, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        AVG_FLT = FLT / DAYS,
        INIT_DATE_PERIOD = ymd(paste0(
          YEAR,
          sprintf("%02d", (QUARTER - 1) * 3 + 1),
          "01"
        ))
      )
    if (rank_period != "QUARTER") {
      data_filtered <- data_filtered %>%
        filter(sprintf("%02d", QUARTER) == str_sub(rank_period, -2, -1))
    }
  } else if (rank_period == "YEAR") {
    data_filtered <- data_norm %>%
      filter(FLT_DATE <= cutoff_date_year) %>%
      mutate(
        YEAR = year(FLT_DATE),
        DAYS = if_else(leap_year(YEAR), 366, 365)
      ) %>%
      group_by(ID, YEAR, DAYS) %>%
      summarise(FLT = sum(FLT, na.rm = TRUE), .groups = "drop") %>%
      mutate(
        AVG_FLT = FLT / DAYS,
        # reconstruct first day of the week - chatgpt
        INIT_DATE_PERIOD = ymd(paste0(YEAR, "0101"))
      )
  }

  data_ranking <- data_filtered %>%
    select(
      ID,
      INIT_DATE_PERIOD,
      AVG_FLT
    ) %>%
    group_by(ID) %>%
    mutate(RANK = dense_rank(-AVG_FLT)) %>%
    filter(RANK <= 10) %>%
    arrange(ID, RANK, desc(INIT_DATE_PERIOD)) %>%
    ungroup() %>%
    mutate(
      PERIOD = rank_period,
      LAST_UPDATED = now()
    )

  colnames(data_ranking) <- get(paste0("colnames_", stk))

  return(data_ranking)
}

## write table ----
data_ranking <- map_dfr(rank_period, period_ranking)


table_name <- paste0("RECORD_", toupper(stk), "_FLT")

## set append to TRUE/FALSE depending on whether you want to add entries to an existing table or (re)create the table.
## It's commented out to force you to purposefully activate the line only whenever needed

# write_table_oracle(data_ranking, table_name, append = TRUE)
