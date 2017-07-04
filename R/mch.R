mch_handler <-
  list(
    function(df_wide){
      variable_mch <- c("rh" = 1, "temp" = 2, "co2" = 3)

      df_long <-
        df_wide %>%
        dplyr::transmute(time, rh = Ch1_Value, temp = Ch2_Value, co2 = Ch3_Value) %>%
        tidyr::gather(variable, value, -time) %>%
        dplyr::mutate(channel = variable_mch[variable])
    },
    
    function(df_long){
      dplyr::left_join(df_long, calb, by = c("variable", "channel")) %>%
        dplyr::transmute(time, logger, id, channel, variable, value = value * slope + intercept)
    }
  )
