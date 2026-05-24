## Airlines and groups ----
dim_ao <- export_query(
  "select
    AO_ID as STK_ID,
    AO_CODE as STK_CODE,
    AO_NAME as STK_NAME,
    min(wef) as VALID_FROM,
    max(TIL) as VALID_TO
  from LDW_ACC.AO_GROUPS_ASSOCIATION
  GROUP BY ao_id, ao_code, ao_name
"
)

dim_ao_grp <- export_query(
  "WITH ranked AS (
    SELECT
        ao_grp_name,
        ao_grp_id,
        ao_code,
        ao_grp_code,
        wef,
        til,
        ROW_NUMBER() OVER (
            PARTITION BY ao_grp_name, ao_grp_id
            ORDER BY
                til DESC,
                wef DESC
        ) AS rn
    FROM ldw_acc.ao_groups_association
)
SELECT
    ao_grp_name AS stk_id,
    CASE
        WHEN ao_grp_id = 99 THEN ao_code
        ELSE ao_grp_code
    END AS stk_code,
    ao_grp_name AS stk_name
FROM ranked
WHERE ao_grp_id <> 99
   OR rn = 1
GROUP BY ao_grp_name, CASE
        WHEN ao_grp_id = 99 THEN ao_code
        ELSE ao_grp_code
    END
ORDER BY stk_code
"
) %>%
  mutate(
    VALID_FROM = ymd("19000101"),
    VALID_TO = ymd("29991231"),
  )


list_ao_grp <- export_query(
  '
	SELECT 	ao_id,
	    ao_code,
			ao_name,
			wef,
			til,
			ao_grp_code,
			ao_grp_name
	FROM pruread.v_aiu_app_list_ao_grp
'
)

rel_ao_id_ao_grp <- list_ao_grp %>%
  distinct(AO_ID, AO_GRP_NAME) %>%
  arrange(AO_GRP_NAME)

list_id_ao <- list_ao_grp %>% distinct(AO_ID) %>% pull(AO_ID)
list_icao_ao <- list_ao_grp %>% distinct(AO_CODE) %>% pull(AO_CODE)

rel_list_ao_old_dim_ao <- read_excel(
  path = here("data", "rel_app_list_ao_dim_ao.xlsx"),
  sheet = "REL",
  range = cell_limits(c(1, 1), c(NA, 5))
) %>%
  select(BK_OP_ID, AO_ID)

list_old_id_ao <- rel_list_ao_old_dim_ao %>%
  distinct(BK_OP_ID) %>%
  pull(BK_OP_ID)

## Airport ----
dim_ap <- export_query(
  "
                            select
                            BK_AP_ID as STK_ID,
                            CFMU_AP_CODE as STK_CODE,
                            EC_AP_NAME as STK_NAME,
                            VALID_FROM,
                            VALID_TO
                            from pruread.v_aiu_dim_airport
                            "
)


list_ap <- export_query(
  'SELECT BK_AP_ID FROM pruread.v_aiu_app_list_airport'
)

list_id_ap <- list_ap |> pull(BK_AP_ID)
# list_id_ap <- 5410

## ANSP ----
dim_sp <- export_query(
  "
SELECT
       ANSP_ID as STK_ID,
       ANSP_ID as STK_CODE,
       ANSP_NAME STK_NAME
FROM PRUDEV.V_PRU_REL_CFMU_AUA_ANSP
--WHERE ANSP_ID  in (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,26,27,28,29,30,31,32,33,39,42,44,45,46,50,53,56,57)
 group by  ANSP_ID, ANSP_NAME
"
) %>%
  mutate(
    VALID_FROM = ymd("19000101"),
    VALID_TO = ymd("29991231"),
  )

list_sp <- dim_sp

## COUNTRY ICAO ----
dim_st_icao <- export_query(
  "
SELECT DISTINCT
 EC_ICAO_COUNTRY_CODE as stk_id,
 EC_ICAO_COUNTRY_CODE as stk_code,
 CASE WHEN EC_ICAO_COUNTRY_CODE = 'LU' THEN 'Moldova'
      WHEN EC_ICAO_COUNTRY_CODE = 'LE' THEN 'Spain Continental'
      WHEN EC_ICAO_COUNTRY_CODE = 'GC' THEN 'Spain Canaries'
 		ELSE EC_ICAO_COUNTRY_NAME
 END stk_name
FROM SWH_FCT.dim_icao_country
WHERE valid_to >= trunc(sysdate) -1

union all

select
  'LEGC' as stk_id,
    'LEGC' as stk_code,
    'Spain' as stk_name
from dual

"
) %>%
  mutate(
    STK_ID = STK_CODE,
    VALID_FROM = ymd("19000101"),
    VALID_TO = ymd("29991231"),
  )

list_st_icao_daio <- dim_st_icao %>%
  select(STK_CODE) %>%
  filter(
    substr(STK_CODE, 1, 1) %in%
      c('E', 'L') |
      substr(STK_CODE, 1, 2) %in%
        c('GC', 'GM', 'GE', 'UD', 'UG', 'UK', 'YY', 'BI')
  )

list_st_icao_code_daio <- list_st_icao_daio |> pull(STK_CODE)

list_st_icao_dai <- dim_st_icao %>%
  filter(
    substr(STK_CODE, 1, 1) %in%
      c('E', 'L') |
      substr(STK_CODE, 1, 2) %in%
        c('GC', 'GM', 'GE', 'UD', 'UG', 'UK', 'BI')
  ) %>%
  filter(!(STK_CODE %in% c('LV', 'LX', 'EU', 'LN'))) %>%
  filter(STK_CODE != 'LEGC')

list_st_icao_code_dai <- list_st_icao_dai |> pull(STK_CODE)

dim_st_dai <- dim_st_icao
