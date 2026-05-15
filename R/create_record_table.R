# LIBRARIES ----
source("R/libraries.R")

# FUNCTIONS ----
source("R/helpers.R")

# DIMENSIONS ----
source("R/dimensions.R")

# PARAMS ----
source("R/params.R")

# QUERIES ----
## NOTE: Whatever the stakeholder, the output of the query should be,  in this order, stakeholder Id, date, flights. The name of the fields doesn't matter, but the order is important.

## Airport ----
ap_traffic_query <- "
WITH all_data AS (
  SELECT
  TRUNC(lobt) AS flt_date,
  bk_adep_id AS STK_ID,
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
  bk_ades_id AS STK_ID,
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
  b.bk_ap_id as STK_ID,
  SUM(a.ADEP_DAY_ALL_TRF) AS mvt,
  'dep' AS flow_type
  FROM aru_syn.AGG_dep_DAY a
  LEFT JOIN pruread.v_aiu_dim_airport b
  ON a.adep_day_adep = b.STK_CODE
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
  b.bk_ap_id as STK_ID,
  SUM(a.ADES_DAY_ALL_TRF) AS mvt,
  'arr' AS flow_type
  FROM aru_syn.AGG_arr_DAY a
  LEFT JOIN pruread.v_aiu_dim_airport b
  ON a.ADES_DAY_ADES_CTFM = b.STK_CODE
  AND a.ADES_DAY_FLT_DATE >= b.valid_from
  AND a.ADES_DAY_FLT_DATE <= b.valid_to
  WHERE EXTRACT(YEAR FROM a.ADES_DAY_FLT_DATE) >= 2007
  AND b.bk_ap_id IN ({list_id_ap*})
  GROUP BY
  a.ADES_DAY_FLT_DATE,
  b.bk_ap_id
)
SELECT
STK_ID,
flt_date,
SUM(mvt) AS DEP_ARR
FROM all_data
GROUP BY
STK_ID,
flt_date
"

## ANSP ----
sp_traffic_query <- "
SELECT
	 unit_id AS STK_ID,
     FLIGHT_DATE,
     TTF_FLT AS flt
FROM PRUDEV.V_PRU_FAC_TD_DD
WHERE unit_kind = 'ANSP'
ORDER BY STK_ID, FLIGHT_DATE
"

## COUNTRY AUA DAIO ----
st_aua_daio_traffic_query <- "
    SELECT
        case when agg_asp_id = '
        agg_asp_id as EC_ICAO_COUNTRY_CODE,
        agg_asp_entry_date AS flt_date,
        SUM(COALESCE(a.agg_asp_a_traffic_asp, 0)) AS FLT
    FROM aru_syn.agg_asp a
    WHERE
        agg_asp_ty = 'COUNTRY_AUA' AND a.agg_asp_unit_ty <> 'REGION'
        AND agg_asp_id in ({list_st_icao_code*})
    GROUP BY
        agg_asp_entry_date,
        agg_asp_id,
        agg_asp_ty,
        agg_asp_name
"

## Aircraft operator ----
ao_traffic_query <- "
WITH
list_ao AS (
	SELECT 	ao_id,
	    ao_code,
			ao_name,
			wef,
			til
--	  	, ao_grp_code,
--			ao_grp_name
	FROM pruread.v_aiu_app_list_ao_grp
	ORDER BY ao_id

),
FLIGHTS AS (
  SELECT
    a.flt_uid,
    TRUNC(a.flt_a_asp_prof_time_entry) AS entry_date,
    a.ao_icao_id
  FROM prudev.v_aiu_flt a
  WHERE a.ao_icao_id IN ({list_icao_ao*})
    AND a.flt_lobt >= TO_DATE('2019-01-01', 'yyyy-mm-dd') - 2
    AND a.flt_lobt <  TRUNC(SYSDATE) + 2
    AND a.flt_a_asp_prof_time_entry >= TO_DATE('2019-01-01', 'yyyy-mm-dd')
    AND a.flt_a_asp_prof_time_entry <  TRUNC(SYSDATE)
    AND a.flt_state IN ('TE','TA','AA')
),

AO_MAP AS (
  SELECT
    f.flt_uid,
    d.ao_id,
    ROW_NUMBER() OVER (
      PARTITION BY f.flt_uid
      ORDER BY d.wef DESC NULLS LAST, d.til DESC NULLS LAST
    ) AS rn
  FROM FLIGHTS f
  LEFT JOIN list_AO d
    ON d.ao_code = f.ao_icao_id
   AND f.entry_date BETWEEN d.wef AND d.til
),

DATA_FLIGHT AS (
  SELECT
    NVL(m.ao_id,   99999) AS ao_id,
    f.entry_date,
    f.flt_uid
  FROM FLIGHTS f
  LEFT JOIN AO_MAP m
    ON m.flt_uid = f.flt_uid
   AND m.rn = 1
)

  SELECT
    ao_id,
    entry_date AS flight_date,
    COUNT(flt_uid)        AS day_tfc
  FROM DATA_FLIGHT
  WHERE ao_id IN ({list_id_ao*})
  GROUP BY ao_id,  entry_date
"

ao_old_traffic_query <- "
WITH

FLIGHTS AS (
  SELECT
    a.flt_uid,
    TRUNC(a.Ifpz_entry_time_act) AS flight_date,
    a.bk_op_id
  FROM swh_fct.fac_flight a
  WHERE a.bk_op_id IN ({list_old_id_ao*})
    AND a.lobt <  TO_DATE('2019-01-01', 'yyyy-mm-dd') + 2
    AND a.Ifpz_entry_time_act < TO_DATE('2019-01-01', 'yyyy-mm-dd')
    AND a.flt_status IN ('TE','TA','AA')
)

  SELECT
    bk_op_id,
    flight_date,
    COUNT(flt_uid)        AS day_tfc
  FROM FLIGHTS
  GROUP BY bk_op_id,  flight_date

"

# GET BASIC DATA ----
### set stakeholder
stk <- "ao_grp"

### build query ----
con <- eurocontrol::db_connection(schema = "PRU_READ")

sql_template <- get(paste0(
  if_else(stk == "ao_grp", "ao", stk),
  "_traffic_query"
))

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
#     'G:/HQ/dgof-pru/Project/DDP/Projects/DDP-25-020_Data_snapshot_top_airports_max_flights_days/ao_traffic_pre_2019.csv'
#   )

# data_raw <- read_csv('G:/HQ/dgof-pru/Project/DDP/Projects/DDP-25-020_Data_snapshot_top_airports_max_flights_days/apt_traffic_all_days.csv')

# data_raw <- data_raw_new

if (stk == 'ao' | stk == 'ao_grp') {
  data_raw_old <- read_csv(
    'G:/HQ/dgof-pru/Project/DDP/Projects/DDP-25-020_Data_snapshot_top_airports_max_flights_days/ao_traffic_pre_2019.csv'
  )

  data_raw_old_new_id <- data_raw_old %>%
    left_join(rel_list_ao_old_dim_ao, by = "BK_OP_ID") %>%
    select(AO_ID, FLIGHT_DATE, DAY_TFC)

  data_raw <- data_raw %>%
    rbind(data_raw_old_new_id)

  # test <- data_all %>% filter(AO_ID == 1754) %>% select(FLIGHT_DATE, DAY_TFC)
  #
  # plot(test)
  if (stk == 'ao_grp') {
    data_grp <- data_raw %>%
      left_join(rel_ao_id_ao_grp, by = "AO_ID")

    data_stk <- data_grp %>%
      mutate(FLIGHT_DATE = as.Date(FLIGHT_DATE)) %>%
      group_by(AO_GRP_NAME, FLIGHT_DATE) %>%
      summarise(DAY_TFC = sum(DAY_TFC, na.rm = TRUE), .groups = "drop") %>%
      select(AO_GRP_NAME, FLIGHT_DATE, DAY_TFC)

    # test_day <- data_stk %>% filter(FLIGHT_DATE == ymd(20260514))
  }
} else {
  data_stk <- data_raw
}


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

colnames(data_stk) <- norm_colnames

data_norm <- data_stk %>%
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

# write_table_oracle(data_ranking, table_name, append = FALSE)
