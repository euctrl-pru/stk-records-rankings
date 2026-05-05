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

# dims in create table - check differences
## Airport ----
# dim_ap <- export_query(
#   "
#                             select
#                             BK_AP_ID,
#                             CFMU_AP_CODE,
#                             VALID_FROM,
#                             VALID_TO
#                             from pruread.v_aiu_dim_airport
#                             "
# )
#
# list_ap <- export_query(
#   'SELECT BK_AP_ID, EC_AP_NAME FROM pruread.v_aiu_app_list_airport'
# )
#
# list_id_ap <- list_ap |> pull(BK_AP_ID)
# # list_airport_ids <- 5410
#
# ## ANSP ----
# dim_sp <- "
# SELECT
# ANSP_ID, ANSP_NAME
# FROM PRUDEV.V_PRU_REL_CFMU_AUA_ANSP
# --WHERE ANSP_ID  in (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,26,27,28,29,30,31,32,33,39,42,44,45,46,50,53,56,57)
# group by  ANSP_ID, ANSP_NAME
# "
#
# list_sp <- dim_sp
