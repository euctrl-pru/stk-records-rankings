# BACKUP FOLDER ----
backup_folder <- 'G:/HQ/dgof-pru/Data/DataProcessing/Covid19/Archive'

mapping_kpa_stk_kpi <- list(
  tfc = c(
    nw = "flt_daio",
    ap = "flt_da",
    sp = "flt_daio",
    ao = "flt",
    ao_grp = "flt",
    st = "flt_dai"
  ),
  atfm_dly = c(
    nw = "total_atfm_dly"
  )
)

mapping_kpa_email <- c(
  tfc = "traffic",
  atfm_dly = "delay"
)
