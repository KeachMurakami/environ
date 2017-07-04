read_env <- function(logger_type = "mch",
                     logger_id = "01",
                     start_time = lubridate::now() - lubridate::days(7),
                     end_time = lubridate::now(),
                     folder = "~/Dropbox/env_log/"){

  # suppress warning for tidyr::separate
  warn.value <- as.numeric(options("warn"))
  options(warn = -1)

  path_folders <-
    paste0(folder, logger_id, "/") %>%
    dir(full.name = T)

  init_date <-
    path_folders %>%
    basename %>%
    lubridate::ymd()

  terms_in_range <-
    (start_time < init_date & init_date < end_time) %>%
    which %>%
    range

  if(is.infinite(terms_in_range[1])){
    selected_index <-
      (init_date < start_time) %>%
      sum
  } else {
    selected_index <-
      (terms_in_range[1] - 1):terms_in_range[2]
  }

  selected_files <-
    path_folders[selected_index] %>%
    purrr::map_chr(~ dir(., full.names = T))

  # if there is no data in the target span
  if(length(selected_files) == 0){
    print("no data files detected")
    break
  }

  env_df <-
    selected_files %>%
    purrr::map_df(~ data.table::fread(.)) %>%
    dplyr::mutate(time = lubridate::ymd_hms(paste(Date , Time))) %>%
    dplyr::filter(time > start_time, time < end_time) %>%
    na.omit %>%
    column_set %>%
    calibrate %>%
    data.table::as.data.table()

  options(warn = warn.value) # reset warning

  return(env_df)
}

column_set <-
  function(df_wide){
    if(tolower(logger_type) == c("mch")){
      variable_mch <- c("rh" = 1, "temp" = 2, "co2" = 3)

      df_long <-
        df_wide %>%
        dplyr::transmute(time, rh = Ch1_Value, temp = Ch2_Value, co2 = Ch3_Value) %>%
        tidyr::gather(variable, value, -time) %>%
        dplyr::mutate(channel = variable_mch[variable])
    } else if(tolower(logger_type) %in% c("graphtec", "graphtech", "gl")){

    } else {
      stop("{environ} is not designed to handle the given data-format.\n")
    }
  }


calibrate <-
  function(df_long){
    calb <-
      calibration_table %>%
      dplyr::filter(logger == tolower(logger_type), id == logger_id)
    if(nrow(calb) != 0){
        dplyr::left_join(df_long, calb, by = c("variable", "channel")) %>%
        dplyr::transmute(time, logger, id, channel, variable, value = value * slope + intercept)
    } else {
      warning("Calibration records were not found.\nRaw values are shown.")
      df_long
    }
  }
