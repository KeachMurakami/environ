hioki_handler <-
  list(
    file_read =
      function(file_name){
        variable_hioki <- c("span_mean" = 1, "span_max" = 2, "span_min" = 3, "instantaneous" = 4)

        data.table::fread(file_name, skip = 10, header = F) %>%
          transmute(time = lubridate::ymd_hms(paste0(stringr::str_sub(V1, 1, 10), V2)),
                    span_mean = V3, span_max = V4, span_min = V5,
                    instantaneous = V6) %>%
          tidyr::gather(variable, value, -time) %>%
          dplyr::mutate(channel = variable_hioki[variable], logger_id)
      },

    calibrate =
      function(df_long){
        dplyr::left_join(df_long, calb, by = c("channel", "logger_id")) %>%
          dplyr::transmute(time, logger_type, logger_id, channel,
                           variable = paste0(variable.y, "_", variable.x), value = value * slope + intercept)
      }
  )
