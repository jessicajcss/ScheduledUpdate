#ADAPTED FOR GHA FROM: ------------------------------------------------------------------------------------------#
#   Author: Antonio Willian Flores de Melo                                                 #
#           Doctor in Tropical Forests Sciences                                            #
#   Institution: Federal University of Acre                                                #
#                Multidisciplinary Center - Campus Floresta                                #
#                Estrada do Canela Fina, km 12                                             #
#                Gleba Formoso, 66980-000                                                  #
#                Cruzeiro do Sul, Acre, Brasil                                             #
#   E-mails: willianflores@ufac.br                                                         #
#            willianflores@gmail.com                                                       #
#------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------#
#   Author: Antonio Willian Flores de Melo                                                 #
#           Doctor in Tropical Forests Sciences                                            #
#   Institution: Federal University of Acre                                                #
#                Multidisciplinary Center - Campus Floresta                                #
#                Estrada do Canela Fina, km 12                                             #
#                Gleba Formoso, 66980-000                                                  #
#                Cruzeiro do Sul, Acre, Brasil                                             #
#   E-mails: willianflores@ufac.br                                                         #
#            willianflores@gmail.com                                                       #
#------------------------------------------------------------------------------------------#

getPurpleairApiHistory <- function(
    sensorIndex=NULL,
    apiReadKey=NULL,
    startTimeStamp=NULL,
    endTimeStamp=NULL,
    average = NULL,
    fields = NULL
) {
  # Load packages
  if (!require('httr')) {
    install.packages('httr')
    library(httr)
  }
  if (!require('jsonlite')) {
    install.packages('jsonlite')
    library(jsonlite)
  }
  if (!require('tidyverse')) {
    install.packages('tidyverse')
    library(tidyverse)
  }
  if (!require('lubridate')) {
    install.packages('lubridate')
    library(lubridate)
  }
  if (!require('httpcode')) {
    install.packages('httpcode')
    library(httpcode)
  }

  # Validate parameters
  if ( is.null(sensorIndex) ) stop("sensorIndex not defined!")
  if ( is.null(apiReadKey) ) stop("apiReadKey not defined!")
  if ( is.null(startTimeStamp) ) stop("startTimeStamp not defined!")
  if ( is.null(endTimeStamp) ) stop("endTimeStamp not defined!")
  if ( is.null(average) ) stop("average not defined!")
  if ( is.null(fields) ) stop("fields not defined!")

  # Set Time Stamp
  t_dif <- as.POSIXct(endTimeStamp, tz="UTC") - as.POSIXct(startTimeStamp, tz="UTC")

  if (t_dif <= as.difftime(48, units = 'hours')) {
    start_timestamps <- as.POSIXct(startTimeStamp, tz="UTC")
    end_timestamps   <- as.POSIXct(endTimeStamp, tz="UTC")
  } else {
    start_timestamps <- seq(from=as.POSIXct(startTimeStamp, tz="UTC"), to=as.POSIXct(endTimeStamp, tz="UTC"), by="2 days")
    end_timestamps   <- seq(from=as.POSIXct(startTimeStamp, tz="UTC") + as.difftime(172799, units = 'secs'),
                            to=as.POSIXct(endTimeStamp, tz="UTC"), by="2 days")

    if (length(start_timestamps) != length(end_timestamps)) {
      end_timestamps <- c(end_timestamps, as.POSIXct(endTimeStamp, tz="UTC"))
    }
  }

  # Set variables for filling missing dates
  dif_map <- c("10" = 10, "30" = 30, "60" = 60, "360" = 360, "1440" = 1440)
  if (average %in% names(dif_map)) {
    dif <- as.difftime(dif_map[[average]], units = 'mins')
    other_df <- data.frame(time_stamp = seq(from = as.POSIXct(startTimeStamp, tz="UTC"),
                                            to = as.POSIXct(endTimeStamp, tz="UTC"), by = dif))
  }

  # Loop for multiple sensor requests
  if (length(sensorIndex) > 1) {
    r <- data.frame()
    r_for <- data.frame()
    n <- length(sensorIndex)

    pb <- txtProgressBar(min = 0, max = n, style = 3)  # Text-based progress bar

    for (i in seq_along(sensorIndex)) {
      URLbase <- paste0('https://api.purpleair.com/v1/sensors/', sensorIndex[i], '/history')

      for (j in seq_along(start_timestamps)) {
        queryList <- list(
          start_timestamp = as.character(as.integer(as.POSIXct(start_timestamps[j], tz="UTC"))),
          end_timestamp = as.character(as.integer(as.POSIXct(end_timestamps[j], tz="UTC"))),
          average = average,
          fields = fields
        )

        # GET PurpleAir sensor history data
        r_temp <- httr::GET(
          URLbase,
          query = queryList,
          config = add_headers("X-API-Key" = apiReadKey)
        )

        # Handle errors
        if (httr::http_error(r_temp)) {
          status_code <- httr::status_code(r_temp)
          err_msg <- sprintf("Web service error %s", status_code)
          stop(err_msg)
        }

        # Parse JSON response
        r_parsed <- fromJSON(content(r_temp, as="text"))
        r_dataframe <- as.data.frame(r_parsed$data)

        if (length(r_dataframe) == 0) {
          r_dataframe <- data.frame(matrix(ncol = length(r_parsed$fields), nrow = 1))
          names(r_dataframe) <- r_parsed$fields
          r_dataframe$time_stamp <- as.integer(as.POSIXct(start_timestamps[j], tz="UTC"))
        } else {
          names(r_dataframe) <- r_parsed$fields
        }

        # Convert datetime format
        r_dataframe$time_stamp <- as.POSIXct(as.integer(r_dataframe$time_stamp), origin="1970-01-01", tz="UTC")

        # Fill missing dates
        if (average != "0") {
          other_df$time_stamp <- as.POSIXct(other_df$time_stamp)
          r_dataframe <- dplyr::full_join(other_df, r_dataframe)
        }

        # Order by date
        r_dataframe <- r_dataframe[order(r_dataframe$time_stamp),]

        r_for <- rbind(r_for, r_dataframe)
      }

      # Add sensor ID
      if (nrow(r_for) != 0) r_for$sensor_id <- sensorIndex[i]

      r <- rbind(r, r_for)
      r_for <- data.frame()

      # Update progress
      setTxtProgressBar(pb, i)
    }

    close(pb)  # Close progress bar

  } else {
    URLbase <- paste0('https://api.purpleair.com/v1/sensors/', sensorIndex, '/history')
    r <- data.frame()
    r_for <- data.frame()
    n <- length(start_timestamps)

    pb <- txtProgressBar(min = 0, max = n, style = 3)  # Text-based progress bar

    for (j in seq_along(start_timestamps)) {
      queryList <- list(
        start_timestamp = as.character(as.integer(as.POSIXct(start_timestamps[j], tz="UTC"))),
        end_timestamp = as.character(as.integer(as.POSIXct(end_timestamps[j], tz="UTC"))),
        average = average,
        fields = fields
      )

      r_temp <- httr::GET(
        URLbase,
        query = queryList,
        config = add_headers("X-API-Key" = apiReadKey)
      )

      if (httr::http_error(r_temp)) {
        status_code <- httr::status_code(r_temp)
        stop(sprintf("Web service error %s", status_code))
      }

      r_parsed <- fromJSON(content(r_temp, as="text"))
      r_dataframe <- as.data.frame(r_parsed$data)

      if (length(r_dataframe) == 0) {
        r_dataframe <- data.frame(matrix(ncol = length(r_parsed$fields), nrow = 1))
        names(r_dataframe) <- r_parsed$fields
        r_dataframe$time_stamp <- as.integer(as.POSIXct(start_timestamps[j], tz="UTC"))
      } else {
        names(r_dataframe) <- r_parsed$fields
      }

      r_dataframe$time_stamp <- as.POSIXct(as.integer(r_dataframe$time_stamp), origin="1970-01-01", tz="UTC")

      if (average != "0") {
        other_df$time_stamp <- as.POSIXct(other_df$time_stamp)
        r_dataframe <- dplyr::full_join(other_df, r_dataframe)
      }

      r_dataframe <- r_dataframe[order(r_dataframe$time_stamp),]

      r_for <- rbind(r_for, r_dataframe)

      setTxtProgressBar(pb, j)
    }

    close(pb)

    if (nrow(r_for) != 0) r_for$sensor_id <- sensorIndex

    r <- r_for
  }

  return(r)
}
