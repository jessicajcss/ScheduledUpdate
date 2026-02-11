### DOWNLOAD DATA INMET
# Elaborado por Santos-Silva, J. C.
## Last update: 2026-02-11

# Refs:
# https://portal.inmet.gov.br/dadoshistoricos/2020.zip
# https://github.com/wallissoncarvalho/hydrobr/blob/master/hydrobr/get_data.py
# https://wallissoncarvalho.medium.com/utilizando-a-biblioteca-hydrobr-parte-2-42d54778bf08
# https://github.com/FilgueirasR/BrazilMet/tree/master/R
# https://portal.inmet.gov.br/noticias/saiba-como-acessar-os-dados-meteorol%C3%B3gicos-dispon%C3%ADveis-no-site-do-inmet
# https://github.com/jdtatsch/inmetr/blob/master/R/bdmep.R

## Code based on:
# https://github.com/JuliaClimate/INMET.jl/blob/master/README.md
# https://discourse.julialang.org/t/ann-inmet-jl/65990/3
# https://tempo.inmet.gov.br/TabelaEstacoes/B806

############################

# Function to get INMET token from environment variables
get_inmet_token <- function() {
  token <- Sys.getenv("INMET_TOKEN")
  if (token == "") {
    stop("The INMET API requires a token. Please set the INMET_TOKEN environment variable.")
  }
  return(token)
}

# Function to download and parse data from the INMET API
download_data <- function(start_date, end_date, station_code, freq = "") {
  token <- get_inmet_token()

  # Ensure dates are properly formatted as strings (YYYY-MM-DD)
  start_str <- if (inherits(start_date, "Date")) {
    format(start_date, "%Y-%m-%d")
  } else {
    as.character(start_date)
  }

  end_str <- if (inherits(end_date, "Date")) {
    format(end_date, "%Y-%m-%d")
  } else {
    as.character(end_date)
  }

  # Match the Julia INMET.jl URL structure exactly
  # For hourly data, freq is empty string, for daily use "diaria"
  if (freq == "") {
    # Hourly data: no "diaria" in URL
    url <- sprintf("https://apitempo.inmet.gov.br/token/estacao/%s/%s/%s/%s",
                   start_str, end_str, station_code, token)
  } else {
    # Daily data: includes "diaria"
    url <- sprintf("https://apitempo.inmet.gov.br/token/estacao/%s/%s/%s/%s/%s",
                   freq, start_str, end_str, station_code, token)
  }

  cat("Requesting data from:", start_str, "to", end_str, "\n")

  # Add browser-like headers to avoid 403 errors
  response <- httr::GET(
    url,
    httr::add_headers(
      `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36",
      `Accept` = "application/json, text/plain, */*",
      `Accept-Language` = "en-US,en;q=0.9,pt-BR;q=0.8,pt;q=0.7",
      `Accept-Encoding` = "gzip, deflate, br"
    ),
    httr::timeout(60)
  )

  # Check if the request was successful
  if (httr::status_code(response) != 200) {
    stop(paste("Error:", httr::status_code(response), "-", httr::http_status(response)$message))
  }

  content <- httr::content(response, "text", encoding = "UTF-8")

  # Check if response is empty
  if (nzchar(content) == FALSE) {
    stop("Error: API response is empty.")
  }

  # Try to parse JSON safely
  parsed_json <- tryCatch(
    jsonlite::fromJSON(content),
    error = function(e) {
      stop("Error: Failed to parse JSON response. Check API request.")
    }
  )

  return(parsed_json)
}

# Function to convert JSON data to a data frame
convert_to_df <- function(data, freq = "") {
  if (length(data) == 0) {
    stop("Error: No data available for the given parameters.")
  }

  # Replace NULL values with NA
  as_missing <- function(v) ifelse(is.null(v), NA, v)

  df <- as.data.frame(lapply(data, function(col) sapply(col, as_missing)))

  # Different variable names for daily (diaria) vs hourly data
  if (freq == "diaria") {
    # Daily data variable names
    num_cols <- c("VL_LONGITUDE", "VL_LATITUDE", "VL_ALTITUDE",
                  "TEMP_MIN", "TEMP_MED", "TEMP_MAX",
                  "UMID_MIN", "UMID_MED", "UMID_MAX",
                  "PRESSAO_MED", "PRESSAO_MIN", "PRESSAO_MAX",
                  "VEL_VENTO_MED", "RAJADA_MAX", "DIR_VENTO_MED",
                  "PTO_ORVALHO_MED", "PTO_ORVALHO_MIN", "PTO_ORVALHO_MAX",
                  "RADIACAO_GLOBAL", "CHUVA")
  } else {
    # Hourly data variable names (instantaneous)
    num_cols <- c("VL_LONGITUDE", "VL_LATITUDE", "VL_ALTITUDE",
                  "TEM_INS", "TEM_MIN", "TEM_MAX",
                  "UMD_INS", "UMD_MIN", "UMD_MAX",
                  "PRE_INS", "PRE_MIN", "PRE_MAX",
                  "VEN_VEL", "VEN_RAJ", "VEN_DIR",
                  "PTO_INS", "PTO_MIN", "PTO_MAX",
                  "RAD_GLO", "CHUVA")
  }

  # Convert numeric columns to numeric types (only those present in df)
  for (col in num_cols) {
    if (col %in% names(df)) {
      df[[col]] <- as.numeric(df[[col]])
    }
  }

  return(df)
}


############################
# MAIN WORKFLOW
############################

# Load previous data
load(file = "./data/meteo/meteo_colombo.Rda")
last_meteo_colombo <- meteo_colombo
rm(meteo_colombo)

# Get the last date from the existing data
ultima_data <- last_meteo_colombo |>
  dplyr::mutate(date = as.Date(date)) |>
  dplyr::arrange(date) |>
  tail(1) |>
  dplyr::select(date)

# Set parameters for download
start_date <- as.Date(ultima_data$date)  # Start from last available date
end_date <- Sys.Date() + 1                # Up to tomorrow
station_code <- "B806"                    # Colombo station
freq <- ""                                # HOURLY DATA (use "diaria" for daily)

# Download data
data <- download_data(start_date, end_date, station_code, freq = freq)
df <- convert_to_df(data, freq = freq)

# Print the first few rows and available columns
cat("\nAvailable columns:\n")
print(names(df))
cat("\nFirst few rows:\n")
print(head(df))


#########################################################
### Formatting dataset ----
#########################################################

# Map hourly API variables to your standard format
meteo_colombo <- df |>
  dplyr::mutate(
    data = as.Date(DT_MEDICAO),
    time = sub("00$", "", HR_MEDICAO),    # Remove trailing "00" from hour
    date = lubridate::ymd_hms(paste0(data, " ", time, ":00:00")),
    temp = TEM_INS,                       # Instantaneous temperature
    ws = VEN_VEL,                         # Wind speed
    wd = VEN_DIR,                         # Wind direction
    prec = CHUVA,                         # Precipitation
    umid = UMD_INS,                       # Instantaneous humidity
    rad = ifelse("RAD_GLO" %in% names(df), RAD_GLO, NA),  # Solar radiation
    press = ifelse("PRE_INS" %in% names(df), PRE_INS, NA),  # Instantaneous pressure
    uv = NA,                              # UV not available
    Cidade = "Colombo"
  ) |>
  dplyr::select(Cidade, date, temp, ws, wd, prec, umid, rad, press, uv)

# Ensure proper timezone handling
meteo_colombo <- meteo_colombo |>
  dplyr::mutate(date = lubridate::with_tz(date, tz = "America/Chicago")) |>
  dplyr::mutate(date = lubridate::force_tz(date, tz = "America/Sao_Paulo")) |>
  dplyr::mutate(
    Cidade = "Colombo",
    across(c(temp, ws, wd, prec, umid, rad, press, uv), as.numeric)
  )

# Combine with historical data and clean
meteo_colombo <- rbind(meteo_colombo, last_meteo_colombo) |>
  dplyr::arrange(date) |>
  unique() |>
  subset(!is.na(temp))

# Save updated data
save(meteo_colombo, file = "./data/meteo/meteo_colombo.Rda")

cat("\n=== Data successfully downloaded and saved! ===\n")
cat("Total records:", nrow(meteo_colombo), "\n")
cat("Date range:", as.character(min(meteo_colombo$date)), "to", as.character(max(meteo_colombo$date)), "\n")
cat("Variables:", paste(names(meteo_colombo), collapse=", "), "\n")

