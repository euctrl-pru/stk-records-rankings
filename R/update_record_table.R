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
toggle_write_db <- TRUE
toggle_test <- FALSE
agg_period <- "day"
# agg_period <- "month"
# current date not included in the dataset. Max day + 1
current_date <- today()
# current_date <- ymd("20260622")
# current_date <- seq.Date(ymd("20260622"), ymd("20260628"))

stk <- c("nw", "ap", "sp", "ao", "ao_grp", "st")
# stk <- c("nw")

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

      run_for_kpa <- function(kpa) {
        stk_kpi_run <- mapping_kpa_stk_kpi[[kpa]]
        stk_kpi_run <- stk_kpi_run[names(stk_kpi_run) %in% stk]

        # Keep a pre-update copy of each stakeholder record table.
        # The big-record email must compare against the table as it existed
        # before this run writes the new rankings to the database.
        previous_record_tables <- list()

        run_for_stk <- function(stk, kpi) {
          message(paste("running", stk))

          run_for_agg_period <- function(agg_period) {
            # agg_period <- "day"
            # kpa <- "tfc"
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
              kpa,
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

            if (!"FLT_UID" %in% names(data_base_raw)) {
              data_base_raw <- data_base_raw %>%
                mutate(FLT_UID = NA)
            }

            ### pivot and normalise dataset
            data_base_pivot <- data_base_raw %>%
              tidyr::pivot_longer(
                -c(STK_ID, STK_TYPE, FLIGHT_DATE, FLT_UID),
                names_to = "KPI",
                values_to = "KPI_VALUE"
              ) %>%
              mutate(
                KPI = tolower(KPI),
                KPA = kpa
              )

            data_base <- data_base_pivot %>%
              mutate(
                AGG_PERIOD = agg_period,
                FLIGHT_DATE = as.Date(FLIGHT_DATE)
              ) %>%
              group_by(STK_ID, STK_TYPE, KPA, KPI, AGG_PERIOD) %>%
              summarise(
                KPI_VALUE = sum(KPI_VALUE, na.rm = TRUE),
                .groups = "drop"
              ) %>%
              mutate(
                KPI_AVG_VALUE = KPI_VALUE / days_period,
                INIT_DATE_PERIOD = from_date,
                DAYS_PERIOD = days_period,
                LAST_UPDATED = now(),
                NEW_ENTRY = "Y"
              ) %>%
              select(-AGG_PERIOD)

            ### import source table
            source_table <- paste0("RECORD_", toupper(stk))
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
                filter(PERIOD == rank_period, KPI == kpi)

              # test_wzair <- data_source_period %>% filter(ID %in% c(1724, 1754, 1752, 2261))

              ### create list of stakeholders to be updated
              stk_to_be_updated <- data_source_period %>%
                group_by(STK_ID) %>%
                summarise(
                  min_kpi = min(KPI_AVG_VALUE, na.rm = TRUE),
                  max_kpi = max(KPI_AVG_VALUE, na.rm = TRUE),
                  .groups = "drop"
                ) %>%
                inner_join(data_base_period, by = "STK_ID") %>%
                filter(KPI_AVG_VALUE >= min_kpi) %>%
                distinct(STK_ID)

              ### create df with potential new entries
              new_entries <- data_base_period %>%
                filter(STK_ID %in% stk_to_be_updated$STK_ID) %>%
                select(
                  STK_ID,
                  STK_TYPE,
                  INIT_DATE_PERIOD,
                  KPA,
                  KPI,
                  KPI_AVG_VALUE,
                  DAYS_PERIOD,
                  LAST_UPDATED,
                  NEW_ENTRY
                ) %>%
                anti_join(
                  data_source_period,
                  by = c("STK_ID", "INIT_DATE_PERIOD", "KPI")
                )

              data_updated <- data_source_period %>%
                filter(STK_ID %in% stk_to_be_updated$STK_ID) %>%
                select(
                  STK_ID,
                  STK_TYPE,
                  INIT_DATE_PERIOD,
                  KPA,
                  KPI,
                  KPI_AVG_VALUE,
                  DAYS_PERIOD,
                  LAST_UPDATED
                ) %>%
                mutate(NEW_ENTRY = "N") %>%
                rbind(new_entries) %>%
                group_by(STK_ID, KPI) %>%
                mutate(RANK = dense_rank(-KPI_AVG_VALUE)) %>%
                arrange(STK_ID, KPI, RANK, desc(INIT_DATE_PERIOD)) %>%
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
                  STK_ID,
                  STK_TYPE,
                  INIT_DATE_PERIOD,
                  KPA,
                  KPI,
                  KPI_AVG_VALUE,
                  RANK,
                  PERIOD,
                  DAYS_PERIOD,
                  LAST_UPDATED,
                  NEW_ENTRY
                )

              if (
                toggle_write_db &&
                  nrow(filter(data_updated, NEW_ENTRY == 'Y')) > 0
              ) {
                data_updated_table <- data_updated %>%
                  select(-NEW_ENTRY)

                data_source_keep <- data_source %>%
                  filter(
                    !(STK_ID %in%
                      stk_to_be_updated$STK_ID &
                      PERIOD == rank_period &
                      KPI == kpi)
                  )

                data_source_full_updated <- bind_rows(
                  data_source_keep,
                  data_updated_table
                )

                con <- eurocontrol::db_connection(schema = "PRU_READ")

                source_table_sql <- DBI::SQL(source_table)

                DBI::dbExecute(
                  con,
                  glue_sql(
                    "DELETE FROM {source_table_sql}",
                    .con = con
                  )
                )

                DBI::dbCommit(con)
                DBI::dbDisconnect(con)

                write_table_oracle(
                  data_source_full_updated,
                  source_table,
                  append = TRUE
                )

                data_source <- data_source_full_updated
              }

              return(data_updated)
            }

            data_updated <- map_dfr(rank_period, data_ranking_update)
            return(data_updated)
          }

          # Keep the source table in memory before any period updates are written.
          source_table_snapshot <- paste0("RECORD_", toupper(stk))
          if (toggle_test) {
            source_table_snapshot <- paste0("ZZ_BAD_", source_table_snapshot)
          }

          previous_record_tables[[stk]] <<- export_query(
            glue("SELECT * FROM {source_table_snapshot}")
          )

          # execute function for all periods ----
          data_updated <- map_dfr(agg_period, run_for_agg_period)

          dim_stk <- get(paste0("dim_", stk))

          # id_col <- get(paste0("colnames_", stk))[1]
          # metric_col <- get(paste0("colnames_", stk))[3]

          data_updated_new <- data_updated %>%
            mutate(
              DATE_END = INIT_DATE_PERIOD + days(DAYS_PERIOD - 1)
            ) %>%
            left_join(
              dim_stk,
              by = join_by(
                STK_ID == STK_ID,
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
              STK_ID,
              STK_NAME,
              STK_CODE,
              INIT_DATE_PERIOD,
              KPI,
              KPI_AVG_VALUE,
              RANK,
              AGG_PERIOD,
              RANK_PERIOD = PERIOD,
              DAYS_PERIOD
            ) %>%
            arrange(STK_NAME, KPI, AGG_PERIOD, RANK_PERIOD)

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
                KPI,
                KPI_AVG_VALUE,
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

        results <- imap(stk_kpi_run, function(kpi, stk) {
          run_for_stk(stk = stk, kpi = kpi)
        })

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
          mapping_kpa_email[kpa],
          "table updates for",
          current_date - days(1)
        )

        for (s in names(results)) {
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
              align = c("l", "l", "l", "l", rep("r", ncol(data_updated_s) - 3))
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
          "oscar.alfaro@eurocontrol.int",
          "nora.cashman@eurocontrol.int"
          # "quinten.goens@eurocontrol.int",
          # "enrico.spinielli@eurocontrol.int",
          # "delia.budulan@eurocontrol.int",
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
          mapping_kpa_email[kpa],
          "records for",
          current_date - days(1)
        )

        records_beat <- 0

        for (s in names(results)[!names(results) %in% c("ao")]) {
          kpi_s <- unname(stk_kpi_run[s])

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

            # Use the pre-update copy captured before database writes.
            rec_table <- previous_record_tables[[s]]

            current_records <- data_updated_s %>%
              select(
                STK_ID,
                KPI,
                PERIOD = RANK_PERIOD,
                INIT_DATE_PERIOD
              )

            rec_table_filtered <- rec_table %>%
              rename(
                STK_ID = 1,
                KPI_AVG_VALUE_PREV = 6,
                INIT_DATE_PERIOD_PREV = INIT_DATE_PERIOD,
                DAYS_PERIOD_PREV = DAYS_PERIOD
              ) %>%
              filter(
                STK_ID %in% s_id_list,
                KPI == kpi_s
              ) %>%
              anti_join(
                current_records,
                by = c(
                  "STK_ID",
                  "KPI",
                  "PERIOD",
                  "INIT_DATE_PERIOD_PREV" = "INIT_DATE_PERIOD"
                )
              ) %>%
              group_by(STK_ID, KPI, PERIOD) %>%
              arrange(
                desc(KPI_AVG_VALUE_PREV),
                desc(INIT_DATE_PERIOD_PREV),
                .by_group = TRUE
              ) %>%
              slice(1) %>%
              ungroup()

            data_updated_s_prev_num <- data_updated_s %>%
              left_join(
                rec_table_filtered,
                by = c("STK_ID", "RANK_PERIOD" = "PERIOD", "KPI")
              ) %>%
              mutate(
                INIT_DATE_PERIOD_PREV = as.Date(INIT_DATE_PERIOD_PREV),
                KPI_VALUE_PREV = round(
                  KPI_AVG_VALUE_PREV * DAYS_PERIOD_PREV,
                  0
                ),
                KPI_VALUE = round(KPI_AVG_VALUE * DAYS_PERIOD, 0),
                CHECK_EQUAL_RECORD = KPI_AVG_VALUE - KPI_AVG_VALUE_PREV
              ) %>%
              select(
                STK_NAME,
                STK_CODE,
                RANK_PERIOD,
                INIT_DATE_PERIOD,
                KPI,
                KPI_AVG_VALUE,
                KPI_VALUE,
                INIT_DATE_PERIOD_PREV,
                KPI_AVG_VALUE_PREV,
                KPI_VALUE_PREV,
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

            table_html <- knitr::kable(
              data_updated_s_prev,
              format = "html",
              table.attr = "border='1' cellpadding='3' cellspacing='0'",
              align = c(
                "l",
                "l",
                "l",
                "l",
                rep("r", ncol(data_updated_s_prev) - 3)
              )
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
      walk(names(mapping_kpa_stk_kpi), run_for_kpa)
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
  # error = function(e) {
  #   # In case of an error, log the error details with relevant metadata
  #   if (!toggle_test) {
  #     pl_error(
  #       conn,
  #       flow = "stakeholder_traffic_rankings",
  #       log_type = "data_job",
  #       message = sprintf("Process failed. %s", conditionMessage(e)),
  #       metadata = NULL
  #     )
  #   } else {
  #     stop(e)
  #   }
  # }
  error = function(e) {
    error_msg <- conditionMessage(e)
    message("ERROR: ", error_msg)

    if (!toggle_test) {
      pl_error(
        conn,
        flow = "stakeholder_traffic_rankings",
        log_type = "data_job",
        message = sprintf("Process failed. %s", error_msg),
        metadata = NULL
      )
    }

    stop(e)
  }
)
