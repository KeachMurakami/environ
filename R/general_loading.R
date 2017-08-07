#' @param
#'
#' @return data.table object
#'
#' @export

read_env <- function(logger_type = "mch",
                     logger_id = "1",
                     start_time = lubridate::now() - lubridate::days(7),
                     end_time = lubridate::now(),
                     folder = "~/Dropbox/env_log/"){

  # suppress warning for tidyr::separate
  warn.value <- as.numeric(options("warn"))
  options(warn = -1)

  logger_type <-
    switch(logger_type,
           mch = "mch", mch383SD = "mch", MCH = "mch",
           gl = "graphtec", GL = "graphtec", graphtech = "graphtec", graptec = "graphtec",
           hioki = "hioki", lr5000 = "hioki", Hioki = "hioki")


  handler <-
    switch(logger_type,
           mch = mch_handler,
           graphtec = graphtech_handler,
           hioki = hioki_handler)

  if(is.null(handler)) stop("{environ} is not designed to handle the given data-format.\n")

  file_read <- handler[[1]]
  calibrate <- handler[[2]]

  calb <-
    calibration_table %>%
    dplyr::filter(type == tolower(logger_type), id %in% logger_id) %>%
    rename(logger_type = type, logger_id = id)


#### file load

  path_folders <-
    paste0(folder, logger_type, "_log/", logger_id, "/") %>%
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
    purrr::map(~ dir(., full.names = T)) %>%
    unlist

  # if there is no data in the target span
  if(length(selected_files) == 0){
    stop("No data files found")
    break
  }

  env_df <-
    selected_files %>%
    purrr::map_df(~ file_read(.)) %>%
    dplyr::filter(time > start_time, time < end_time) %>%
    na.omit %>%
    calibrate %>%
    data.table::as.data.table()

  options(warn = warn.value) # reset warning
  if(nrow(calb) == 0) warning("Calibration records were not found.\nRaw values are shown.")
  return(env_df)
}
