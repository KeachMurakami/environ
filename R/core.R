set_calibration_table <-
  function(path){
    calibration_table <<-
      data.table::fread(path)
  }

env_stat <-
  function(grouped_env_df){
    grouped_env_df %>%
      summarise(mean = mean(value), sd = sd(value), n = n())
  }

env_timecourse <-
  function(env_df, ...){
    if("mean" %in% names(env_df)){
      ggplot(env_df, aes_string("time", "mean", ...)) +
        geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd)) +
        geom_line() +
        geom_point() +
        facet_grid(variable ~ ., scale = "free")
    } else {
      ggplot(env_df, aes_string("time", "value", ...)) +
        geom_line() +
        geom_point() +
        facet_grid(variable ~ ., scale = "free")
    }
  }

env_hist <-
  function(env_df, ...){
    if("mean" %in% names(env_df)){
      ggplot(env_df, aes_string("mean", ...)) +
        geom_histogram() +
        facet_wrap(~ variable, ncol = 1, scale = "free")
    } else {
      ggplot(env_df, aes_string("value", ...)) +
        geom_histogram() +
        facet_wrap(~ variable, ncol = 1, scale = "free")
    }
  }


by_year <-
  function(env_df){
    env_df %>%
      dplyr::mutate(time = lubridate::year(time)) %>%
      dplyr::group_by_(logger, id, channel, variable, time)
    }

by_month <-
  function(env_df){
    env_df %>%
      dplyr::mutate(time = paste0(lubridate::year(time), "-",
                                  stringr::str_pad(lubridate::month(time), 2, pad = "0"))) %>%
      dplyr::group_by(logger, id, channel, variable, time)
  }

by_day <-
  function(env_df){
    env_df %>%
      dplyr::mutate(time = lubridate::date(time)) %>%
      dplyr::group_by(logger, id, channel, variable, time)
  }

by_hour <-
  function(env_df){
    env_df %>%
      dplyr::mutate(time = paste0(lubridate::date(time), " ",
                                  stringr::str_pad(lubridate::hour(time), 2, pad = "0")),
                    time = lubridate::ymd_h(time)) %>%
      dplyr::group_by(logger, id, channel, variable, time)
  }


by_minute <-
  function(env_df){
    env_df %>%
      dplyr::mutate(time = paste0(lubridate::date(time), " ",
                                  stringr::str_pad(lubridate::hour(time), 2, pad = "0"), ":",
                                  stringr::str_pad(lubridate::minute(time), 2, pad = "0")),
                    time = lubridate::ymd_hm(time)) %>%
      dplyr::group_by(logger, id, channel, variable, time)
  }

by_second <-
  function(env_df){
    env_df %>%
      dplyr::mutate(time = paste0(lubridate::date(time), " ",
                                  stringr::str_pad(lubridate::hour(time), 2, pad = "0"), ":",
                                  stringr::str_pad(lubridate::minute(time), 2, pad = "0"), ":",
                                  stringr::str_pad(lubridate::second(time), 2, pad = "0")),
                    time = lubridate::ymd_hms(time)) %>%
      dplyr::group_by(logger, id, channel, variable, time)
  }

by_condition_daily <-
  function(env_df, condition_name, start_time = 7, end_time = 23){
    env_df[,condition_name] <-
      (lubridate::hour(env_df$time) >= start_time) & (lubridate::hour(env_df$time) < end_time)
    env_df %>%
      dplyr::group_by_(condition_name, add = T)
  }
