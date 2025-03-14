### DOWNLOAD DATA INMET
# Elaborado por Santos-Silva, J. C.
## Last update: 2025-03-13



# Refs:

# https://portal.inmet.gov.br/dadoshistoricos/2020.zip
# https://github.com/wallissoncarvalho/hydrobr/blob/master/hydrobr/get_data.py
# https://wallissoncarvalho.medium.com/utilizando-a-biblioteca-hydrobr-parte-2-42d54778bf08
# https://github.com/FilgueirasR/BrazilMet/tree/master/R
# https://portal.inmet.gov.br/noticias/saiba-como-acessar-os-dados-meteorol%C3%B3gicos-dispon%C3%ADveis-no-site-do-inmet#:~:text=Os%20dados%20coletados%20pelo%20Inmet,inmet.gov.br).
# https://github.com/jdtatsch/inmetr/blob/master/R/bdmep.R
############################

## Code based on:
#>>> https://github.com/JuliaClimate/INMET.jl/blob/master/README.md
#>> https://discourse.julialang.org/t/ann-inmet-jl/65990/3
#> https://tempo.inmet.gov.br/TabelaEstacoes/B806


# Function to get INMET token from environment variables
get_inmet_token <- function() {
  token <- seu_token
  if (token == "") {
    stop("The INMET API requires a token. Please set the INMET_TOKEN environment variable.")
  }
  return(token)
}

# Function to download and parse data from the INMET API
download_data <- function(start_date, end_date, station_code) {
  token <- get_inmet_token()
  url <- sprintf("https://apitempo.inmet.gov.br/token/estacao/%s/%s/%s/%s",
                 start_date, end_date, station_code, token)

  response <- httr::GET(url)

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
convert_to_df <- function(data) {
  if (length(data) == 0) {
    stop("Error: No data available for the given parameters.")
  }

  # Replace NULL values with NA
  as_missing <- function(v) ifelse(is.null(v), NA, v)

  df <- as.data.frame(lapply(data, function(col) sapply(col, as_missing)))

  # Convert numeric columns to numeric types
  num_cols <- c("VL_LONGITUDE", "VL_LATITUDE", "VL_ALTITUDE",
                "TEM_INS", "TEM_MIN", "TEM_MAX",
                "TEMP_MIN", "TEMP_MED", "TEMP_MAX",
                "UMD_INS", "UMD_MIN", "UMD_MAX",
                "UMID_MIN", "UMID_MED", "UMID_MAX",
                "PRE_INS", "PRE_MIN", "PRE_MAX",
                "VEN_VEL", "VEN_RAJ", "VEN_DIR",
                "PTO_INS", "PTO_MIN", "PTO_MAX",
                "RAD_GLO", "CHUVA")

  for (col in num_cols) {
    if (col %in% names(df)) {
      df[[col]] <- as.numeric(df[[col]])
    }
  }

  return(df)
}





load(file = "./data/meteo_colombo.Rda")
last_meteo_colombo <- meteo_colombo
rm(meteo_colombo)

ultima_data <- last_meteo_colombo |>
  dplyr::mutate(date = as.Date(date)) |>
  dplyr::arrange(date) |>
  tail(1) |>
  dplyr::select(date)


# Exampledate# Example usage ----
start_date <- ultima_data$date #"2023-06-01" # máximo de um ano!!
end_date <- Sys.Date()+1
station_code <- "B806"
seu_token <- Sys.getenv("INMET_TOKEN")

data <- download_data(start_date, end_date, station_code)
df <- convert_to_df(data)


# Print the first few rows
print(head(df))





#########################################################
#########################################################
### Formating dataset ----


meteo_colombo <- df |>
  dplyr::mutate(data = as.Date(DT_MEDICAO),
         time = sub("00", "", HR_MEDICAO),# sub("(\\d+)(\\d{2})", "\\1:\\2", Hora.Medicao))
         date = lubridate::ymd_hms(paste0(data," ", time, ":00:00")),
         uv = NA) |>
  dplyr::select(DC_NOME, date, TEM_INS, VEN_VEL, VEN_DIR, CHUVA, UMD_INS, RAD_GLO, PRE_INS, uv)

colnames(meteo_colombo) <- c('Cidade', 'date', 'temp', 'ws', 'wd', 'prec', 'umid', 'rad', 'press', 'uv')

meteo_colombo <- meteo_colombo |>
  dplyr::mutate(date = lubridate::with_tz(date, tz = "America/Chicago")) |>
  dplyr::mutate(date = lubridate::force_tz(date, tz = "America/Sao_Paulo")) |>
  dplyr::mutate(Cidade = "Colombo",
         across(c(temp, ws, wd, prec, umid, rad, press, uv), as.numeric))

meteo_colombo <- rbind(meteo_colombo, last_meteo_colombo) |>
  dplyr::arrange(date) |>
  unique() |>
  subset(!is.na(temp))

save(meteo_colombo, file = "./data/meteo/meteo_colombo.Rda")


