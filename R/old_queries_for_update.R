# QUERIES ----
## Network ----
nw_traffic_update_query <- "
  select     'NM Area' as STK_ID,
a_first_entry_time_date FLIGHT_DATE ,
SUM(nvl(a.all_traffic,0)) FLT
FROM  prudev.v_aiu_agg_global_daily_counts  a
WHERE
a.a_first_entry_time_date >= TO_DATE({from_date_str}, 'YYYY-MM-DD')
AND a.a_first_entry_time_date  < TO_DATE({to_date_str}, 'YYYY-MM-DD')
GROUP BY  a.a_first_entry_time_date
ORDER BY a_first_entry_time_date
"

nw_delay_update_query <-
  "SELECT
   'NM Area' as STK_ID,
   A_FIRST_ENTRY_TIME_DATE AS regulation_date,
         SUM (TOTAL_DELAY_IN_MINUTES)                                AS TDM
--       ,  SUM (TOTAL_DELAY_IN_MINUTES - AIRPORT_DELAY_IN_MINUTES)     AS TDM_ERT
    FROM ARU_SYN.AGG_GLOBAL_DAILY_COUNTS SYN
   WHERE A_FIRST_ENTRY_TIME_DATE >= TO_DATE({from_date_str}, 'YYYY-MM-DD')
      AND A_FIRST_ENTRY_TIME_DATE < TO_DATE({to_date_str}, 'YYYY-MM-DD')
   GROUP BY A_FIRST_ENTRY_TIME_DATE
"

## Airport ----
ap_traffic_update_query <- "
  WITH
  all_data AS (
      SELECT
          a.ADEP_DAY_FLT_DATE AS flt_date,
          b.bk_ap_id,
          SUM(a.ADEP_DAY_ALL_TRF) AS mvt,
          'dep' AS flow_type
      FROM prudev.v_aiu_agg_dep_day a
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
      FROM prudev.v_aiu_agg_arr_day a
      LEFT JOIN pruread.v_aiu_dim_airport b
        ON a.ADES_DAY_ADES_CTFM = b.CFMU_AP_CODE
       AND a.ADES_DAY_FLT_DATE >= b.valid_from
       AND a.ADES_DAY_FLT_DATE <= b.valid_to
      WHERE a.ADES_DAY_FLT_DATE >= TO_DATE({from_date_str}, 'YYYY-MM-DD')
      AND a.ADES_DAY_FLT_DATE < TO_DATE({to_date_str}, 'YYYY-MM-DD')
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
