# COLUMN NAMES ----
common_columns <- c(
  "PLACEHOLDER_ID",
  "INIT_DATE_PERIOD",
  "PLACEHOLDER_FLT",
  "RANK",
  "PERIOD",
  "DAYS_PERIOD",
  "LAST_UPDATED"
)

## Aircraft operator ----
colnames_ao <- common_columns %>%
  gsub("PLACEHOLDER_ID", "AO_ID", .) %>%
  gsub("PLACEHOLDER_FLT", "AVG_FLT", .)

colnames_ao_grp <- common_columns %>%
  gsub("PLACEHOLDER_ID", "AO_GRP_NAME", .) %>%
  gsub("PLACEHOLDER_FLT", "AVG_FLT", .)

## Airport ----
colnames_ap <- common_columns %>%
  gsub("PLACEHOLDER_ID", "BK_AP_ID", .) %>%
  gsub("PLACEHOLDER_FLT", "AVG_DEP_ARR", .)

## ANSP ----
colnames_sp <- common_columns %>%
  gsub("ANSP_ID", "BK_AP_ID", .) %>%
  gsub("PLACEHOLDER_FLT", "AVG_FLT", .)

## COUNTRY DAIO ----
colnames_st_aua_daio <- common_columns %>%
  gsub("ANSP_ID", "EC_ICAO_COUNTRY_CODE", .) %>%
  gsub("PLACEHOLDER_FLT", "AVG_FLT_DAIO", .)

## COUNTRY DAI ----
colnames_st_dai <- common_columns %>%
  gsub("ANSP_ID", "EC_ICAO_COUNTRY_CODE", .) %>%
  gsub("PLACEHOLDER_FLT", "AVG_FLT_DAI", .)
