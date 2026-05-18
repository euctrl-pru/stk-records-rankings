# LIBRARIES ----
source("R/libraries.R")

# FUNCTIONS ----
source("R/helpers.R")

# DIMENSIONS ----
source("R/dimensions.R")

# PARAMS ----
source("R/params.R")

# QUERIES ----
## Airport ----
ap_traffic_update_query <- "
  WITH
  all_data AS (
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
      WHERE a.ADEP_DAY_FLT_DATE >= TO_DATE({from_date_str}, 'YYYY-MM-DD')
        AND a.ADEP_DAY_FLT_DATE < TO_DATE({to_date_str}, 'YYYY-MM-DD')
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
      WHERE a.ADES_DAY_FLT_DATE >= TO_DATE({from_date_str}, 'YYYY-MM-DD')
      AND a.ADES_DAY_FLT_DATE < TO_DATE({to_date_str}, 'YYYY-MM-DD')        AND b.bk_ap_id IN ({list_id_ap*})
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
sp_traffic_update_query <- "
WITH
-- flights
OP_AUA_DATA as
(
SELECT
FLT_MODEL_FLT_UID flight_sk, c.ansp_id,  c.ansp_name,
ASP_PROF_TIME_ENTRY as asp_time_entry
FROM prudev.aiu_Asp_Prof_calc a
inner join prudev.v_aiu_flt b on (a.FLT_MODEL_FLT_UID = b.flt_uid)
inner join PRUDEV.V_PRU_REL_CFMU_AUA_ANSP C ON (a.ASP_PROF_ID = c.aua_code)
where a.FLT_MODEL_TY =3
 and a.flt_model_lobt >= trunc(sysdate)-7-1 and a.flt_model_lobt < trunc(sysdate)+1
   and b.flt_lobt >= trunc(sysdate)-7-1 and b.flt_lobt < trunc(sysdate)+1
and a.asp_prof_time_entry >= trunc(sysdate)-7-1  and a.asp_prof_time_entry < trunc(sysdate)+1
and asp_prof_ty = 'AUA'
and b.flt_state IN ('TE', 'TA', 'AA')
and a.ASP_PROF_ID = c.aua_code and  a.ASP_PROF_TIME_ENTRY >= c.wef and  ASP_PROF_TIME_ENTRY <= c.till
),

 OP_ANSP_DATA as (
SELECT flight_sk, ansp_id, ansp_name,asp_time_entry as first_entry_time,
     row_number() OVER ( PARTITION BY  ansp_id,flight_sk ORDER BY  asp_time_entry) row_num
FROM OP_AUA_DATA
 ),

OP_ANSP_DATA2  as (
SELECT  ansp_id,  ansp_name
       , trunc(first_entry_time) as ENTRY_DATE
       , count(*) as FLT_DAIO
FROM  OP_ANSP_DATA
WHERE
       row_num = 1
GROUP BY trunc(first_entry_time), ansp_name, ansp_id
),

all_flt as(
SELECT ansp_id,
       ENTRY_DATE  AS flt_date,
       flt_daio
FROM op_ansp_data2
where entry_date >=trunc(sysdate)-7 and entry_date < trunc(sysdate)

UNION

SELECT unit_id AS ansp_id,
       FLIGHT_DATE as flt_date,
       TTF_FLT as FLT_DAIO
FROM  PRUDEV.V_PRU_FAC_TD_DD
WHERE  unit_kind = 'ANSP' and
      EXTRACT (YEAR FROM flight_date) >= extract (YEAR FROM (trunc(sysdate) -1)) -1
      AND flight_date < trunc(sysdate)-7
)

SELECT * FROM
all_flt
WHERE flt_date >= TO_DATE({from_date_str}, 'YYYY-MM-DD')
  AND flt_date < TO_DATE({to_date_str}, 'YYYY-MM-DD')

"


## AO ----
ao_traffic_update_query <- "
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
    AND a.flt_lobt >= TO_DATE({from_date_str}, 'YYYY-MM-DD') - 2
    AND a.flt_lobt <  TO_DATE({to_date_str}, 'YYYY-MM-DD') + 2
    AND a.flt_a_asp_prof_time_entry >= TO_DATE({from_date_str}, 'YYYY-MM-DD')
    AND a.flt_a_asp_prof_time_entry <  TO_DATE({to_date_str}, 'YYYY-MM-DD')
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

## AO GRP ----
ao_grp_traffic_update_query <- "
WITH
list_ao AS (
	SELECT 	ao_id,
	    ao_code,
			ao_name,
			wef,
			til,
			ao_grp_name
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
    AND a.flt_lobt >= TO_DATE({from_date_str}, 'YYYY-MM-DD') - 2
    AND a.flt_lobt <  TO_DATE({to_date_str}, 'YYYY-MM-DD') + 2
    AND a.flt_a_asp_prof_time_entry >= TO_DATE({from_date_str}, 'YYYY-MM-DD')
    AND a.flt_a_asp_prof_time_entry <  TO_DATE({to_date_str}, 'YYYY-MM-DD')
    AND a.flt_state IN ('TE','TA','AA')
),

AO_MAP AS (
  SELECT
    f.flt_uid,
    d.ao_id,
    d.ao_grp_name,
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
    nvl(m.ao_grp_name, 'Undefined') AS ao_grp_name,
    f.entry_date,
    f.flt_uid
  FROM FLIGHTS f
  LEFT JOIN AO_MAP m
    ON m.flt_uid = f.flt_uid
   AND m.rn = 1
)

  SELECT
    ao_grp_name,
    entry_date AS flight_date,
    COUNT(flt_uid)        AS day_tfc
  FROM DATA_FLIGHT
  WHERE ao_id <> 99999
  GROUP BY ao_grp_name,  entry_date
"

## ST DAI ----
st_dai_traffic_update_query <- "
WITH

COUNTRY_ICAO2LETTER  as (
 select distinct
       ec_icao_country_code  ICAO2LETTER,
       CASE
             WHEN ec_icao_country_code = 'GE' then 'LE'
             WHEN ec_icao_country_code = 'ET' then 'ED'
             ELSE ec_icao_country_code
        END  COUNTRY_code
  from SWH_FCT.dim_icao_country a
  WHERE Valid_to > trunc(sysdate) - 1
  AND ec_icao_country_code in ({list_st_icao_code_dai*})
  ORDER BY COUNTRY_code
 ) ,

LIST_COUNTRY as (
select  COUNTRY_code
FROM COUNTRY_ICAO2LETTER
group by  COUNTRY_code),

 CTRY_DAY AS (
SELECT a.COUNTRY_code,
        t.day_date
FROM LIST_COUNTRY a, prudev.pru_time_references t
WHERE
   t.day_date >= TO_DATE({from_date_str}, 'YYYY-MM-DD')
  	AND t.DAY_date < TO_DATE({to_date_str}, 'YYYY-MM-DD')
       ),


DATA_DEP AS (
(SELECT
        B.COUNTRY_code ,
        TRUNC(A.flt_a_asp_prof_time_entry) flight_DATE,
        COUNT(a.flt_uid) DAY_TFC
FROM prudev.v_aiu_flt a,
     COUNTRY_ICAO2LETTER b
WHERE  SUBSTR(A.flt_dep_ad,1,2) =  b.ICAO2LETTER
    AND A.flt_lobt >= TO_DATE({from_date_str}, 'YYYY-MM-DD') -2
    AND A.flt_lobt <  TO_DATE({to_date_str}, 'YYYY-MM-DD') + 2
    AND A.flt_a_asp_prof_time_entry >= TO_DATE({from_date_str}, 'YYYY-MM-DD')
    AND A.flt_a_asp_prof_time_entry <  TO_DATE({to_date_str}, 'YYYY-MM-DD')
    AND A.flt_state IN ('TE','TA','AA')
GROUP BY  B.COUNTRY_code  ,
        TRUNC(A.flt_a_asp_prof_time_entry)
)
),

DATA_ARR AS (
SELECT
        C.COUNTRY_code ,
        TRUNC(A.flt_a_asp_prof_time_entry) flight_DATE,
        COUNT(a.flt_uid) DAY_TFC
FROM prudev.v_aiu_flt a,
     COUNTRY_ICAO2LETTER c
WHERE
     SUBSTR(A.flt_ctfm_ades,1,2) = C.ICAO2LETTER
    AND A.flt_lobt >= TO_DATE({from_date_str}, 'YYYY-MM-DD') -2
    AND A.flt_lobt <  TO_DATE({to_date_str}, 'YYYY-MM-DD') + 2
    AND A.flt_a_asp_prof_time_entry >= TO_DATE({from_date_str}, 'YYYY-MM-DD')
    AND A.flt_a_asp_prof_time_entry <  TO_DATE({to_date_str}, 'YYYY-MM-DD')
    AND A.flt_state IN ('TE','TA','AA')
GROUP BY  C.COUNTRY_code  ,
        TRUNC(A.flt_a_asp_prof_time_entry)
),

DATA_DOMESTIC as
(SELECT
        B.COUNTRY_code ,
        TRUNC(A.flt_a_asp_prof_time_entry) FLIGHT_DATE,
        COUNT(a.flt_uid) DAY_TFC
FROM prudev.v_aiu_flt a,
     COUNTRY_ICAO2LETTER b,
     COUNTRY_ICAO2LETTER c
WHERE  SUBSTR(A.flt_dep_ad,1,2) =  b.ICAO2LETTER   AND
       SUBSTR(A.flt_ctfm_ades,1,2) = C.ICAO2LETTER
    AND  B.COUNTRY_code =C.COUNTRY_code
    AND A.flt_lobt >= TO_DATE({from_date_str}, 'YYYY-MM-DD') -2
    AND A.flt_lobt <  TO_DATE({to_date_str}, 'YYYY-MM-DD') + 2
    AND A.flt_a_asp_prof_time_entry >= TO_DATE({from_date_str}, 'YYYY-MM-DD')
    AND A.flt_a_asp_prof_time_entry <  TO_DATE({to_date_str}, 'YYYY-MM-DD')
    AND A.flt_state IN ('TE','TA','AA')
GROUP BY  B.COUNTRY_code  ,
        TRUNC(A.flt_a_asp_prof_time_entry)
),

DATA_SPAIN_SEPARATED AS (
SELECT
          a.COUNTRY_code,
          a.day_date as flight_date,
--          coalesce(b.DAY_TFC,0) as DEP,
--          coalesce( c.DAY_TFC,0) as ARR ,
--          coalesce( d.DAY_TFC,0) as DOM ,
         coalesce(b.DAY_TFC,0)  + coalesce( c.DAY_TFC,0) - coalesce( d.DAY_TFC,0) as DAY_TFC
FROM CTRY_DAY A
LEFT JOIN DATA_DEP b on a.COUNTRY_code = B.COUNTRY_code and a.day_date = b.FLIGHT_date
LEFT JOIN DATA_ARR c on a.COUNTRY_code = c.COUNTRY_code and a.day_date = c.FLIGHT_date
LEFT JOIN DATA_DOMESTIC d on a.COUNTRY_code = d.COUNTRY_code and a.day_date = d.FLIGHT_date
),


 CTRY_DAY_SPAIN AS (
SELECT 'LEGC' AS COUNTRY_code,
        t.day_date
FROM prudev.pru_time_references t
WHERE
   t.day_date >= TO_DATE({from_date_str}, 'YYYY-MM-DD')
  	AND t.DAY_date < TO_DATE({to_date_str}, 'YYYY-MM-DD')
       ),


DATA_DEP_SPAIN AS (
(SELECT
		'LEGC' AS country_code,
        TRUNC(A.flt_a_asp_prof_time_entry) flight_DATE,
        COUNT(a.flt_uid) DAY_TFC
FROM prudev.v_aiu_flt a
WHERE  SUBSTR(A.flt_dep_ad,1,2) IN ('GE', 'GC', 'LE')
    AND A.flt_lobt >= TO_DATE({from_date_str}, 'YYYY-MM-DD') -2
    AND A.flt_lobt <  TO_DATE({to_date_str}, 'YYYY-MM-DD') + 2
    AND A.flt_a_asp_prof_time_entry >= TO_DATE({from_date_str}, 'YYYY-MM-DD')
    AND A.flt_a_asp_prof_time_entry <  TO_DATE({to_date_str}, 'YYYY-MM-DD')
    AND A.flt_state IN ('TE','TA','AA')
GROUP BY  TRUNC(A.flt_a_asp_prof_time_entry)
)
),

DATA_ARR_SPAIN AS (
SELECT
		'LEGC' AS country_code,
        TRUNC(A.flt_a_asp_prof_time_entry) flight_DATE,
        COUNT(a.flt_uid) DAY_TFC
FROM prudev.v_aiu_flt a
WHERE  SUBSTR(A.flt_ctfm_ades,1,2) IN ('GE', 'GC', 'LE')
    AND A.flt_lobt >= TO_DATE({from_date_str}, 'YYYY-MM-DD') -2
    AND A.flt_lobt <  TO_DATE({to_date_str}, 'YYYY-MM-DD') + 2
    AND A.flt_a_asp_prof_time_entry >= TO_DATE({from_date_str}, 'YYYY-MM-DD')
    AND A.flt_a_asp_prof_time_entry <  TO_DATE({to_date_str}, 'YYYY-MM-DD')
    AND A.flt_state IN ('TE','TA','AA')
GROUP BY  TRUNC(A.flt_a_asp_prof_time_entry)
),


DATA_DOMESTIC_SPAIN as
(SELECT
        'LEGC' AS country_code,
        TRUNC(A.flt_a_asp_prof_time_entry) FLIGHT_DATE,
        COUNT(a.flt_uid) DAY_TFC
FROM prudev.v_aiu_flt a
WHERE  SUBSTR(A.flt_dep_ad,1,2) IN ('GE', 'GC', 'LE')  AND
       SUBSTR(A.flt_ctfm_ades,1,2) IN ('GE', 'GC', 'LE')
    AND A.flt_lobt >= TO_DATE({from_date_str}, 'YYYY-MM-DD') -2
    AND A.flt_lobt <  TO_DATE({to_date_str}, 'YYYY-MM-DD') + 2
    AND A.flt_a_asp_prof_time_entry >= TO_DATE({from_date_str}, 'YYYY-MM-DD')
    AND A.flt_a_asp_prof_time_entry <  TO_DATE({to_date_str}, 'YYYY-MM-DD')
    AND A.flt_state IN ('TE','TA','AA')
GROUP BY  TRUNC(A.flt_a_asp_prof_time_entry)
),

DATA_SPAIN_TOGETHER AS (
SELECT
          a.COUNTRY_code,
          a.day_date as flight_date,
--          coalesce(b.DAY_TFC,0) as DEP,
--          coalesce( c.DAY_TFC,0) as ARR ,
--          coalesce( d.DAY_TFC,0) as DOM ,
         coalesce(b.DAY_TFC,0)  + coalesce( c.DAY_TFC,0) - coalesce( d.DAY_TFC,0) as DAY_TFC
FROM CTRY_DAY_SPAIN A
LEFT JOIN DATA_DEP_SPAIN b on a.day_date = b.FLIGHT_date
LEFT JOIN DATA_ARR_SPAIN c on a.day_date = c.FLIGHT_date
LEFT JOIN DATA_DOMESTIC_SPAIN d on a.day_date = d.FLIGHT_date
)

SELECT * FROM DATA_SPAIN_SEPARATED
UNION ALL
SELECT * FROM DATA_SPAIN_TOGETHER

"

# CALCS ----
## set params & toggles ----
backup_folder <- 'G:/HQ/dgof-pru/Data/DataProcessing/Covid19/Archive'
toggle_write_db <- TRUE
agg_period <- "day"
# agg_period <- "month"
# current date not included in the dataset. Max day + 1
current_date <- today()
# current_date <- seq.Date(ymd("20260425"), ymd("20260501"))

stk <- c("ap", "sp", "ao", "ao_grp", "st_dai")
# stk <- c("st_dai")

run_for_day <- function(current_date) {
  if (day(current_date) == 1) {
    agg_period <- append(agg_period, "month")
  } else if (wday(current_date, week_start = 1) == 1) {
    agg_period <- append(agg_period, "week")
  } else if (day(current_date) == 1 & month(current_date) %in% c(1, 4, 7, 10)) {
    agg_period <- append(agg_period, "quarter")
  } else if (yday(current_date) == 1) {
    agg_period <- append(agg_period, "year")
  }

  run_for_stk <- function(stk) {
    message(paste("running", stk))

    run_for_agg_period <- function(agg_period) {
      # agg_period <- "day"
      if (agg_period == "day") {
        # from_date <- ymd("20260515")
        from_date <- current_date - days(1)
      } else if (agg_period == "week") {
        from_date <- current_date - days(7)
      } else if (agg_period == "month") {
        from_date <- current_date - months(1)
      } else if (agg_period == "quarter") {
        from_date <- current_date - months(3)
      } else if (agg_period == "year") {
        from_date <- current_date - months(12)
      }

      days_period <- as.numeric(current_date - from_date)
      from_date_str <- format(from_date, "%Y-%m-%d")
      to_date_str <- format(current_date, "%Y-%m-%d")

      ### build query ----
      con <- eurocontrol::db_connection(schema = "PRU_READ")

      sql_template <- get(paste0(stk, "_traffic_update_query"))

      base_query <- as.character(
        glue::glue_sql(
          sql_template,
          .con = con
        )
      )

      DBI::dbDisconnect(con)

      ### execute query ----
      data_base_raw <- export_query(base_query)

      ### normalise dataset
      norm_base_colnames <- c("ID", "FLT_DATE", "FLT")
      colnames(data_base_raw) <- norm_base_colnames

      data_base <- data_base_raw %>%
        mutate(
          AGG_PERIOD = agg_period,
          FLT_DATE = as.Date(FLT_DATE)
        ) %>%
        group_by(ID, AGG_PERIOD) %>%
        summarise(
          FLT = sum(FLT, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          AVG_FLT = FLT / days_period,
          INIT_DATE_PERIOD = from_date,
          LAST_UPDATED = now(),
          NEW_ENTRY = "Y"
        ) %>%
        select(-AGG_PERIOD)

      ### import source table
      source_table <- paste0("RECORD_", toupper(stk), "_FLT")
      data_source <- export_query(glue("select * from {source_table}")) %>%
        mutate(INIT_DATE_PERIOD = as.Date(INIT_DATE_PERIOD))

      ### backup before manipulating
      data_source %>%
        write_csv(here(
          backup_folder,
          'LastVersion',
          paste0(tolower(source_table), ".csv")
        ))

      data_source %>%
        write_csv(here(
          backup_folder,
          paste0(
            format(current_date + days(-1), "%Y%m%d"),
            "_",
            tolower(source_table),
            ".csv"
          )
        ))

      ### normalise dataset
      norm_source_colnames <- c(
        "ID",
        "INIT_DATE_PERIOD",
        "AVG_FLT",
        "RANK",
        "PERIOD",
        "LAST_UPDATED"
      )
      colnames(data_source) <- norm_source_colnames

      ### process data ----
      ### set up rank_period list depending on agg period
      if (agg_period == "day") {
        seq_dates <- seq.Date(from_date, current_date - days(1))
        rank_period <- toupper(wday(
          seq_dates,
          label = TRUE,
          abbr = FALSE,
          week_start = 1
        )) %>%
          unique()

        rank_period <- append(rank_period, "DAY")
        # rank_period <- "DAY"
      } else if (agg_period == "week") {
        rank_period <- "WEEK"
      } else if (agg_period == "month") {
        rank_period <- paste0("MONTH_", sprintf("%02d", 1:12))
        rank_period <- append(rank_period, "MONTH")
      } else if (agg_period == "quarter") {
        rank_period <- paste0("QUARTER_", sprintf("%02d", 1:4))
        rank_period <- append(rank_period, "QUARTER")
      } else if (agg_period == "year") {
        rank_period <- "YEAR"
      }

      data_ranking_update <- function(rank_period) {
        # rank_period <- "MONTH_04"
        ### filter new and source data depending on rank_period
        if (rank_period %in% c("DAY", "WEEK", "MONTH", "QUARTER", "YEAR")) {
          data_base_period <- data_base
        } else if (agg_period == "day") {
          data_base_period <- data_base %>%
            filter(
              toupper(wday(
                INIT_DATE_PERIOD,
                label = TRUE,
                abbr = FALSE,
                week_start = 1
              )) ==
                rank_period
            )
        } else if (agg_period == "month") {
          data_base_period <- data_base %>%
            filter(
              sprintf("%02d", month(INIT_DATE_PERIOD)) ==
                str_sub(rank_period, -2, -1)
            )
        } else if (agg_period == "quarter") {
          data_base_period <- data_base %>%
            filter(
              sprintf("%02d", quarter(INIT_DATE_PERIOD)) ==
                str_sub(rank_period, -2, -1)
            )
        }

        data_source_period <- data_source %>% filter(PERIOD == rank_period)

        # test_wzair <- data_source_period %>% filter(ID %in% c(1724, 1754, 1752, 2261))

        ### create list of stakeholders to be updated
        stk_to_be_updated <- data_source_period %>%
          group_by(ID) %>%
          summarise(
            min_flt = min(AVG_FLT, na.rm = TRUE),
            max_flt = max(AVG_FLT, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          inner_join(data_base_period, by = "ID") %>%
          filter(AVG_FLT >= min_flt) %>%
          distinct(ID)

        ### create df with potential new entries
        new_entries <- data_base_period %>%
          filter(ID %in% stk_to_be_updated$ID) %>%
          select(
            ID,
            INIT_DATE_PERIOD,
            AVG_FLT,
            LAST_UPDATED,
            NEW_ENTRY
          ) %>%
          anti_join(data_source_period, by = c("ID", "INIT_DATE_PERIOD"))

        data_updated <- data_source_period %>%
          filter(ID %in% stk_to_be_updated$ID) %>%
          select(
            ID,
            INIT_DATE_PERIOD,
            AVG_FLT,
            LAST_UPDATED
          ) %>%
          mutate(NEW_ENTRY = "N") %>%
          rbind(new_entries) %>%
          group_by(ID) %>%
          mutate(RANK = dense_rank(-AVG_FLT)) %>%
          arrange(ID, RANK, desc(INIT_DATE_PERIOD)) %>%
          mutate(
            MIN_RANK_UPDATE = if (any(NEW_ENTRY == "Y" & !is.na(RANK))) {
              min(RANK[NEW_ENTRY == "Y"], na.rm = TRUE)
            } else {
              9999
            }
          ) %>%
          filter(RANK <= 10) %>%
          ungroup() %>%
          mutate(
            INIT_DATE_PERIOD = as.Date(INIT_DATE_PERIOD),
            PERIOD = rank_period,
            LAST_UPDATED = if_else(
              RANK >= MIN_RANK_UPDATE,
              now(),
              LAST_UPDATED
            ),
            DAYS_PERIOD = days_period
          ) %>%
          select(
            ID,
            INIT_DATE_PERIOD,
            AVG_FLT,
            RANK,
            PERIOD,
            LAST_UPDATED,
            NEW_ENTRY,
            DAYS_PERIOD
          )

        ## rename columns specific to stakeholder
        mycolnames <- get(paste0('colnames_', stk))
        mycolnames <- append(mycolnames, c("NEW_ENTRY", "DAYS_PERIOD"))
        colnames(data_updated) <- mycolnames

        if (
          nrow(filter(data_updated, NEW_ENTRY == 'Y')) > 0 & toggle_write_db
        ) {
          ### delete impacted entries from table ----
          list_id_to_be_updated <- stk_to_be_updated %>% pull(ID)

          con <- eurocontrol::db_connection(schema = "PRU_READ")

          source_table_sql <- source_table <- DBI::SQL(source_table)
          id_field <- DBI::SQL(get(paste0("colnames_", stk))[1])

          sql_delete <- glue_sql(
            "DELETE FROM {source_table_sql}
             WHERE {id_field} in ({list_id_to_be_updated*})
                  AND PERIOD = {rank_period}
          ",
            .con = con
          )

          DBI::dbExecute(con, sql_delete)
          DBI::dbCommit(con)

          DBI::dbDisconnect(con)

          ### write table ----
          data_updated_table <- data_updated %>%
            select(-NEW_ENTRY, -DAYS_PERIOD)
          write_table_oracle(data_updated_table, source_table, append = TRUE)
        }

        return(data_updated)
      }

      data_updated <- map_dfr(rank_period, data_ranking_update)
      return(data_updated)
    }

    # execute function for all periods ----
    data_updated <- map_dfr(agg_period, run_for_agg_period)

    dim_stk <- get(paste0("dim_", stk))

    id_col <- get(paste0("colnames_", stk))[1]
    metric_col <- get(paste0("colnames_", stk))[3]

    data_updated_new <- data_updated %>%
      mutate(
        DATE_END = INIT_DATE_PERIOD + days(DAYS_PERIOD)
      ) %>%
      left_join(
        dim_stk,
        by = join_by(
          !!sym(id_col) == STK_ID,
          DATE_END >= VALID_FROM,
          DATE_END <= VALID_TO
        )
      ) %>%
      filter(NEW_ENTRY == 'Y') %>%
      mutate(
        AGG_PERIOD = case_when(
          str_detect(PERIOD, "DAY") ~ "day",
          str_detect(PERIOD, "MONTH") ~ "month",
          str_detect(PERIOD, "QUARTER") ~ "quarter",
          str_detect(PERIOD, "WEEK") ~ "week",
          str_detect(PERIOD, "YEAR") ~ "year",
        )
      ) %>%
      select(
        STK_ID = !!sym(id_col),
        STK_NAME,
        STK_CODE,
        INIT_DATE_PERIOD,
        !!sym(metric_col),
        RANK,
        AGG_PERIOD,
        RANK_PERIOD = PERIOD
      ) %>%
      arrange(STK_NAME, AGG_PERIOD, RANK_PERIOD)

    # send email ----
    ## email parameters ----
    if (nrow(data_updated_new) > 0) {
      sbj = paste(stk, "record table updated for ", current_date)

      table_html <- knitr::kable(
        data_updated_new,
        format = "html",
        table.attr = "border='1' cellpadding='3' cellspacing='0'"
      )

      msg <- paste0(
        "<html><body>",
        "<p>Hello,</p>",
        "<p>The following rows were updated:</p>",
        table_html,
        "</body></html>"
      )
    } else {
      sbj = paste(
        "Script",
        stk,
        "record executed for ",
        current_date,
        " - no updates"
      )
      msg = ""
    }

    from <- "oscar.alfaro@eurocontrol.int"
    to <- c(
      "oscar.alfaro@eurocontrol.int"
      # "quinten.goens@eurocontrol.int",
      # "enrico.spinielli@eurocontrol.int",
      # "delia.budulan@eurocontrol.int",
      # , "nora.cashman@eurocontrol.int"
    )

    control <- list(smtpServer = "mailservices.eurocontrol.int")

    ## send ----
    sendmail(
      from = from,
      to = to,
      subject = sbj,
      msg = mime_part_html(msg),
      control = control
    )
  }

  walk(stk, run_for_stk)
}

walk(current_date, run_for_day)

# test commit
