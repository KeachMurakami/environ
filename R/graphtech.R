graphtech_handler <-
  list(
    file_read  =
      function(file_name){
        variable_mch <- c("rh" = 1, "temp" = 2, "co2" = 3)

        data.table::fread(file_name) %>%
          dplyr::transmute(time = lubridate::ymd_hms(paste(Date , Time)),
                           rh = Ch1_Value, temp = Ch2_Value, co2 = Ch3_Value) %>%
          tidyr::gather(variable, value, -time) %>%
          dplyr::mutate(channel = variable_mch[variable], logger_id)
      },

    calibrate =
      function(df_long){
        dplyr::left_join(df_long, calb, by = c("variable", "channel", "logger_id")) %>%
          dplyr::transmute(time, logger, id, channel, variable, value = value * slope + intercept)
      }
  )
