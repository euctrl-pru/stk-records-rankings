# LIBRARIES ----
source("R/libraries.R")

# FUNCTIONS ----
source("R/helpers.R")

# DIMENSIONS ----
source("R/dimensions.R")

# PARAMS ----
source("R/params.R")

# QUERIES ----
source("R/queries_for_update.R")

# CALCS ----
## set params & toggles ----
toggle_write_db <- FALSE
toggle_test <- FALSE
agg_period <- "day"
# agg_period <- "month"
# current date not included in the dataset. Max day + 1
current_date <- today()
# current_date <- ymd("20260530")
# current_date <- seq.Date(ymd("20260604"), ymd("20260616"))

stk <- c("nw", "ap", "sp", "ao", "ao_grp", "st_dai")
# stk <- c("ao_grp")
kpi <- c("flt", "dly")
# kpi <- c("dly")

mapping_kpi <- c(
  flt = "traffic",
  dly = "delay"
)

# Connect to the pocketlog (pl) instance (picks up credentials from environment variables)
if (!toggle_test) {
  conn <- pl_connect()
}

# tryCatch block to log both successes and errors
tryCatch(
  {
    run_for_day <- function(current_date) {
      if (day(current_date) == 1) {
        agg_period <- append(agg_period, "month")
      }
      if (wday(current_date, week_start = 1) == 1) {
        agg_period <- append(agg_period, "week")
      }
      if (day(current_date) == 1 & month(current_date) %in% c(1, 4, 7, 10)) {
        agg_period <- append(agg_period, "quarter")
      }
      if (yday(current_date) == 1) {
        agg_period <- append(agg_period, "year")
      }

      run_for_kpi <- function(kpi) {
        run_for_stk <- function(stk) {
          message(paste("running", stk))

          run_for_agg_period <- function(agg_period) {
            # agg_period <- "day"
            if (agg_period == "day") {
              # from_date <- ymd("20260522") - days(7)
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

            sql_template <- get(paste0(
              stk,
              "_",
              mapping_kpi[kpi],
              "_update_query"
            ))

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
                DAYS_PERIOD = days_period,
                LAST_UPDATED = now(),
                NEW_ENTRY = "Y"
              ) %>%
              select(-AGG_PERIOD)

            ### import source table
            source_table <- paste0("RECORD_", toupper(stk), "_", toupper(kpi))
            if (toggle_test) {
              source_table <- paste0("ZZ_BAD_", source_table)
            }

            data_source <- export_query(glue(
              "select * from {source_table}"
            )) %>%
              mutate(INIT_DATE_PERIOD = as.Date(INIT_DATE_PERIOD))

            ### backup before manipulating
            if (!toggle_test) {
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
            }

            ### normalise dataset
            norm_source_colnames <- c(
              "ID",
              "INIT_DATE_PERIOD",
              "AVG_FLT",
              "RANK",
              "PERIOD",
              "DAYS_PERIOD",
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
              # rank_period <- "DAY"
              ### filter new and source data depending on rank_period
              if (
                rank_period %in% c("DAY", "WEEK", "MONTH", "QUARTER", "YEAR")
              ) {
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

              data_source_period <- data_source %>%
                filter(PERIOD == rank_period)

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
                  DAYS_PERIOD,
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
                  DAYS_PERIOD,
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
                  )
                ) %>%
                select(
                  ID,
                  INIT_DATE_PERIOD,
                  AVG_FLT,
                  RANK,
                  PERIOD,
                  DAYS_PERIOD,
                  LAST_UPDATED,
                  NEW_ENTRY
                )

              ## rename columns specific to stakeholder
              mycolnames <- get(paste0('colnames_', stk))
              mycolnames <- append(mycolnames, c("NEW_ENTRY"))
              colnames(data_updated) <- mycolnames

              if (
                nrow(filter(data_updated, NEW_ENTRY == 'Y')) > 0 &
                  toggle_write_db
              ) {
                ### delete impacted entries from table ----
                list_id_to_be_updated <- stk_to_be_updated %>% pull(ID)

                con <- eurocontrol::db_connection(schema = "PRU_READ")

                source_table_sql <- DBI::SQL(source_table)
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
                  select(-NEW_ENTRY)
                write_table_oracle(
                  data_updated_table,
                  source_table,
                  append = TRUE
                )
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
              DATE_END = INIT_DATE_PERIOD + days(DAYS_PERIOD - 1)
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
              RANK_PERIOD = PERIOD,
              DAYS_PERIOD
            ) %>%
            arrange(STK_NAME, AGG_PERIOD, RANK_PERIOD)

          if (stk == 'ao_grp') {
            data_updated_new <- data_updated_new %>%
              mutate(
                FINAL_DATE_PERIOD = INIT_DATE_PERIOD + days(DAYS_PERIOD - 1)
              ) %>%
              left_join(
                list_ao_grp,
                by = join_by(
                  STK_ID == AO_GRP_NAME
                )
              ) %>%
              mutate(
                STK_CODE = AO_GRP_CODE,
                STK_NAME = STK_ID
              ) %>%
              select(
                STK_ID,
                STK_NAME,
                STK_CODE,
                INIT_DATE_PERIOD,
                AVG_FLT,
                RANK,
                AGG_PERIOD,
                RANK_PERIOD,
                DAYS_PERIOD
              )
          }

          if (stk == 'sp') {
            data_updated_new <- data_updated_new %>%
              # fmt: skip
              filter(STK_ID %in% c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,26,27,28,29,30,31,32,33,39,42,44,45,46,50,53,56,57))
          }

          return(data_updated_new)
        }

        stk_run <- if (kpi == "dly") {
          "nw"
        } else {
          stk
        }

        results <- set_names(stk_run) |>
          map(run_for_stk)

        # results <- data_updated_new
        # print(names(results))
        # print(nrow(results[["ap"]]))
        # print(results[["ap"]])

        # send emails ----
        ## daily ----
        ### email parameters ----
        msg <- ''
        msg <- paste0(
          "<html><body>",
          "<p>Hello,</p>"
        )
        sbj = paste(
          "Record",
          mapping_kpi[kpi],
          "table updates for",
          current_date - days(1)
        )

        for (s in stk_run) {
          data_updated_s <- results[[s]] %>%
            mutate(
              across(
                -STK_ID & where(is.numeric),
                ~ format(
                  round(.x, 0),
                  big.mark = ",",
                  scientific = FALSE,
                  trim = TRUE
                )
              )
            )

          if (nrow(data_updated_s) > 0) {
            table_html <- knitr::kable(
              data_updated_s,
              format = "html",
              table.attr = "border='1' cellpadding='3' cellspacing='0'",
              align = c("l", "l", "l", rep("r", ncol(data_updated_s) - 3))
            )

            msg <- paste0(
              msg,
              "<p>The following rows were updated:</p>",
              table_html
            )
          } else {
            msg = paste0(
              msg,
              "<p>Script ",
              s,
              " record executed for ",
              current_date - days(1),
              " - no updates</p>"
            )
          }
        }

        msg <- paste0(msg, "</body></html>")

        from <- "oscar.alfaro@eurocontrol.int"
        to <- c(
          "oscar.alfaro@eurocontrol.int"
          # "quinten.goens@eurocontrol.int",
          # "enrico.spinielli@eurocontrol.int",
          # "delia.budulan@eurocontrol.int",
          # , "nora.cashman@eurocontrol.int"
        )

        control <- list(smtpServer = "mailservices.eurocontrol.int")

        ### send ----
        message("Reached daily email send")
        print(to)
        print(sbj)

        sendmail(
          from = from,
          to = to,
          subject = sbj,
          msg = mime_part_html(msg),
          control = control
        )

        ## big records only ----
        ### email params ----
        msg <- ''
        msg <- paste0(
          "<html><body>",
          "<p>Hello,</p>",
          "<p>The stakeholders below beat previous records:</p>"
        )
        sbj = paste(
          "New stakeholder",
          mapping_kpi[kpi],
          "records for",
          current_date - days(1)
        )

        records_beat <- 0

        for (s in stk_run[!stk_run %in% c("ao")]) {
          data_updated_s <- results[[s]] %>%
            filter(
              RANK_PERIOD %in%
                c("DAY", "WEEK", "MONTH", "QUARTER", "YEAR") &
                RANK == 1
            )

          if (nrow(data_updated_s) > 0) {
            records_beat <- records_beat + 1

            ### load rec table to get #2 entry ----
            s_id_list <- data_updated_s %>% select(STK_ID) %>% pull()

            rec_table_name <- paste0("RECORD_", toupper(s), "_", toupper(kpi))
            if (toggle_test) {
              rec_table_name <- paste0("ZZ_BAD_", rec_table_name)
            }

            rec_table <- export_query(
              glue("SELECT * FROM {rec_table_name}")
            )

            col_name_flt_avg <- names(rec_table)[grepl(
              "AVG",
              names(rec_table)
            )][
              1
            ]

            rec_table_filtered <- rec_table %>%
              rename(
                STK_ID = 1,
                AVG_FLT_PREV = 3,
                INIT_DATE_PERIOD_PREV = INIT_DATE_PERIOD,
                DAYS_PERIOD_PREV = DAYS_PERIOD
              ) %>%
              filter(
                STK_ID %in% s_id_list,
                RANK == if_else(toggle_write_db, 2, 1)
              ) %>%
              group_by(STK_ID, PERIOD) %>%
              arrange(desc(INIT_DATE_PERIOD_PREV), .by_group = TRUE) %>%
              slice(1) %>%
              ungroup()

            data_updated_s_prev_num <- data_updated_s %>%
              left_join(
                rec_table_filtered,
                by = c("STK_ID", "RANK_PERIOD" = "PERIOD")
              ) %>%
              mutate(
                INIT_DATE_PERIOD_PREV = as.Date(INIT_DATE_PERIOD_PREV),
                TOTAL_FLT_PREV = round(AVG_FLT_PREV * DAYS_PERIOD_PREV, 0),
                TOTAL_FLT = round(.data[[col_name_flt_avg]] * DAYS_PERIOD, 0),
                CHECK_EQUAL_RECORD = !!sym(col_name_flt_avg) - AVG_FLT_PREV
              ) %>%
              select(
                STK_NAME,
                STK_CODE,
                RANK_PERIOD,
                INIT_DATE_PERIOD,
                !!sym(col_name_flt_avg),
                TOTAL_FLT,
                INIT_DATE_PERIOD_PREV,
                AVG_FLT_PREV,
                TOTAL_FLT_PREV,
                CHECK_EQUAL_RECORD
              ) %>%
              filter(CHECK_EQUAL_RECORD != 0)

            # equaled records don't count
            if (nrow(data_updated_s_prev_num) == 0) {
              records_beat <- records_beat - 1
              next
            }

            data_updated_s_prev <- data_updated_s_prev_num %>%
              select(-CHECK_EQUAL_RECORD) %>%
              mutate(
                across(
                  where(is.numeric),
                  ~ format(
                    round(.x, 0),
                    big.mark = ",",
                    scientific = FALSE,
                    trim = TRUE
                  )
                )
              )

            new_colnames <- c(
              "STK_NAME",
              "STK_CODE",
              "PERIOD",
              "INIT_DATE_PERIOD",
              col_name_flt_avg,
              sub("AVG", "TOTAL", col_name_flt_avg),
              "INIT_DATE_PERIOD_PREV",
              paste0(col_name_flt_avg, "_PREV"),
              paste0(sub("AVG", "TOTAL", col_name_flt_avg), "_PREV")
            )

            colnames(data_updated_s_prev) <- new_colnames

            table_html <- knitr::kable(
              data_updated_s_prev,
              format = "html",
              table.attr = "border='1' cellpadding='3' cellspacing='0'",
              align = c("l", "l", "l", rep("r", ncol(data_updated_s_prev) - 3))
            )

            msg <- paste0(
              msg,
              table_html,
              '</br>'
            )
          }
        }

        msg <- paste0(msg, "</body></html>")

        from <- "oscar.alfaro@eurocontrol.int"
        # fmt: skip
        to <- c(
        "oscar.alfaro@eurocontrol.int"
        , "denis.huet@eurocontrol.int"
        , "nora.cashman@eurocontrol.int"
        , "kateryna.alifirenko.ext@eurocontrol.int"
        , "daria.andrzejewska@eurocontrol.int"
      )

        control <- list(smtpServer = "mailservices.eurocontrol.int")

        ### send ----
        if (records_beat != 0) {
          sendmail(
            from = from,
            to = to,
            subject = sbj,
            msg = mime_part_html(msg),
            control = control
          )
        }
        # walk(stk, run_for_stk)
      }
      walk(kpi, run_for_kpi)
    }

    walk(current_date, run_for_day)

    # After successful execution, log the success with relevant metadata at the end
    if (!toggle_test) {
      pl_success(
        conn,
        flow = "stakeholder_traffic_rankings",
        log_type = "data_job",
        message = "Data updated successfully",
        metadata = NULL
      )
    }
  },
  error = function(e) {
    # In case of an error, log the error details with relevant metadata
    pl_error(
      conn,
      flow = "stakeholder_traffic_rankings",
      log_type = "data_job",
      message = sprintf("Process failed. %s", conditionMessage(e)),
      metadata = NULL
    )
  }
)

#add day period to tables
# mytable <- "RECORD_ST_DAI_FLT_OLD"
#
# df <- export_query(glue("select * from {mytable}"))
#
# df_mod <- df %>%
#   mutate(
#     init_date_period = as.Date(INIT_DATE_PERIOD),
#     DAYS_PERIOD = case_when(
#       PERIOD %in%
#         c(
#           "DAY",
#           "MONDAY",
#           "TUESDAY",
#           "WEDNESDAY",
#           "THURSDAY",
#           "FRIDAY",
#           "SATURDAY",
#           "SUNDAY"
#         ) ~ 1L,
#       PERIOD == "WEEK" ~ 7L,
#       PERIOD %in% c("MONTH", paste0("MONTH_", sprintf("%02d", 1:12))) ~
#         as.integer(days_in_month(init_date_period)),
#       PERIOD %in% c("QUARTER", paste0("QUARTER_", sprintf("%02d", 1:4))) ~
#         as.integer((init_date_period %m+% months(3)) - init_date_period),
#       PERIOD == "YEAR" ~
#         as.integer((init_date_period %m+% years(1)) - init_date_period),
#       TRUE ~ NA_integer_
#     )
#   ) %>%
#   select(-init_date_period) %>%
#   relocate(DAYS_PERIOD, .before = LAST_UPDATED)
#
#
# write_table_oracle(df_mod, "RECORD_ST_DAI_FLT", append = FALSE)
