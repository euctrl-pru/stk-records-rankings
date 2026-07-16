# LIBRARIES ----
source("R/libraries.R")
library(tidyr)

# FUNCTIONS ----
source("R/helpers.R")

# DIMENSIONS ----
dim_ms <- export_query(
  "
select
SK_FLT_TYPE_RULE_ID as ms_id,
rule_description as MARKET_SEGMENT
from  SWH_FCT.DIM_FLIGHT_TYPE_RULE
"
)

# DATA ----
data_raw <- export_query(
  "
  SELECT
    count(A.flt_uid) as flt,
    a.ao_icao_id as ao_code,
    A.SK_FLT_TYPE_RULE_ID AS ms_id
  FROM prudev.v_aiu_flt_mark_seg A
  WHERE A.flt_lobt >= to_date('01-01-2026', 'dd-mm-yyyy') - 2
    AND A.flt_lobt <  TRUNC(SYSDATE) + 2
    AND A.flt_a_asp_prof_time_entry >= to_date('01-01-2026', 'dd-mm-yyyy')
    AND A.flt_a_asp_prof_time_entry <  TRUNC(SYSDATE)
    AND A.flt_state IN ('TE','TA','AA')
  group by a.ao_icao_id, SK_FLT_TYPE_RULE_ID

  "
)

data_sorted <- data_raw %>%
  filter(AO_CODE != 'ZZZ') %>%
  left_join(dim_ms, by = "MS_ID") %>%
  select(
    AO_ICAO = AO_CODE,
    MARKET_SEGMENT,
    FLT
  ) %>%
  pivot_wider(
    id_cols = NULL,
    names_from = MARKET_SEGMENT,
    values_from = FLT
  ) %>%
  mutate(all_flights = rowSums(across(-AO_ICAO), na.rm = TRUE)) %>%
  mutate(across(everything(), ~ replace_na(., 0))) %>%
  mutate(across(
    -c(AO_ICAO, all_flights),
    ~ . / all_flights
  )) %>%
  arrange(desc(all_flights))

data_sorted %>%
  write_csv(
    'G:/HQ/dgof-pru/Data/DataProcessing/Covid19/Oscar/Develop/ao_market_segment_2026.csv'
  )
