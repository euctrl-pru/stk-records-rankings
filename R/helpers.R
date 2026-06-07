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
  append = TRUE,
  overwrite = FALSE
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
    overwrite = overwrite,
    row.names = FALSE
  )
}
