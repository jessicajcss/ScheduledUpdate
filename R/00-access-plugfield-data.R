###### Dados meteorológicos - Plugfield
## via API key
## Last update: 03/12/2025


# https://wdg.plugfield.com.br/doc-api/index.html#/Login/post_login
# POSTMAN: https://cloudy-meadow-20129.postman.co/workspace/My-Workspace~aedb22c3-f19c-4c9a-80b5-3e273693bb2b/request/31843085-b52a9c2f-8959-4ab5-ac6c-b9bd02c72ee5?tab=headers&ctx=code
# https://cloudy-meadow-20129.postman.co/workspace/My-Workspace~aedb22c3-f19c-4c9a-80b5-3e273693bb2b/request/31843085-b52a9c2f-8959-4ab5-ac6c-b9bd02c72ee5?tab=headers&ctx=code

# Function to get JWT token (only when needed)
get_jwt_token <- function() {
  if (exists("global_jwt_token") && !is.null(global_jwt_token)) {
    return(global_jwt_token)
  }

  plugfield_auth <- Sys.getenv("PLUG_AUTH")  # Simula a requisição do token, new Auth using POSTMAN
  global_jwt_token <<- plugfield_auth  # Save token globally
  return(global_jwt_token)
}



jwt_token <- NULL  # Variável global

get_jwt_token <- function() {
  if (!is.null(jwt_token)) {
    return(jwt_token)
  }

  plugfield_auth <- Sys.getenv("PLUG_AUTH")  # Simula requisição

  assign("jwt_token", plugfield_auth, envir = .GlobalEnv)  # Salva na memória
  return(plugfield_auth)
}


# Function to fetch data (optimizing request frequency)
fetch_large_data <- function(device_id, start_date, end_date) {
  max_days <- 30  # API limit
  all_data <- list()  # Store all chunks
  current_start <- start_date

  plugfield_api <- Sys.getenv("PLUG_API")
  jwt_token <- get_jwt_token()  # Fetch token once

  while (current_start < end_date) {
    current_end <- min(current_start + lubridate::days(max_days), end_date)  # 30-day chunks

    # Format API request
    begin_date <- format(current_start, "%d/%m/%Y+%H")
    end_date_formatted <- format(current_end, "%d/%m/%Y+%H")
    url <- paste0("https://prod-api.plugfield.com.br/data/hourly?device=", device_id,
                  "&begin=", begin_date,
                  "&end=", end_date_formatted)

    # Debugging: Print the URL to check for correctness
    print(paste("Requesting URL:", url))

    headers <- httr::add_headers(
      'accept' = 'application/json',
      'x-api-key' = plugfield_api,
      'Authorization' = jwt_token
    )

    # Request data
    res <- httr::GET(url, headers)

    if (httr::status_code(res) == 403) {
      message("Token expired, requesting new token...")
      jwt_token <<- get_jwt_token()  # Refresh token
      headers <- httr::add_headers(
        'accept' = 'application/json',
        'x-api-key' = plugfield_api,
        'Authorization' = jwt_token
      )
      res <- httr::GET(url, headers)  # Retry request
    }

    # Process response
    if (httr::status_code(res) == 200) {
      json_data <- jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"), flatten = TRUE)
      df <- as.data.frame(json_data)
      all_data <- append(all_data, list(df))
    } else {
      message("Error:", httr::status_code(res))
    }

    # Move to next period
    current_start <- current_end + lubridate::seconds(1)
    Sys.sleep(0.2)  # Avoid excessive requests
  }

  return(dplyr::bind_rows(all_data))
}

#########################################################################

load(file = "./data/meteo/meteo_rbs.Rda")
last_meteo_rbs <- meteo_rbs |>
  dplyr::mutate(date = lubridate::force_tz(date, tz = "America/Sao_Paulo"))
rm(meteo_rbs)

ultima_data <- last_meteo_rbs |>
  dplyr::mutate(date = lubridate::as_datetime(date, tz = "America/Sao_Paulo")) |>
  dplyr::arrange(date) |>
  tail(1) |>
  dplyr::select(date)



#########################################################################
start_date <- ultima_data$date #"2023-06-01" # máximo de um ano!!
#start_date <- ymd_hms("2025-01-01 00:00:00")
end_date <- Sys.time() #"26/02/2025 00:00:00"  # End period (more than 30 days)


# DADOS ESCOLA
device_id <- 3184
df_escola <- fetch_large_data(device_id, start_date, end_date)

# DADOS DEFESA CIVIL
device_id <- 3118
df_defesacivil <- fetch_large_data(device_id, start_date, end_date)



# joining all data from rio branco do sul
df <- rbind(df_escola, df_defesacivil)

# to remove columns where all values are either NA or 0 from your dataframe df:
# Print first rows
head(df)

# selecionar e pardonizar variaveis, renomear estacao
# criar loop update horário
# criar repositório com senha e token


df <- df |>
  dplyr::mutate(across(where(is.list), ~ purrr::map_chr(., ~ if (length(.) == 0) NA else paste(., collapse = ",")))) |>  # Flatten lists
  dplyr::select(where(~ !all(is.na(.))) &  where(~ !all(. == "NULL", na.rm = TRUE)))  # Remove all-NA or all-NULL columns [where(~ !all(. == 0, na.rm = TRUE)) &]

head(df)  # Check the cleaned dataframe




# fix timestamp format
df <- df |>
  dplyr::mutate(timestamp = lubridate::ymd_hms(localDateTime))

head(df$timestamp)
summary(df)


# Preparando periodo sem dados na estação defesa civil [NA] para substituir por dados escola
meteo_rbs_dc <- df |>
  subset(deviceId == 3118)

meteo_rbs_esc <- df |>
  subset(deviceId == 3184)

res <- meteo_rbs_dc[is.na(meteo_rbs_dc$tempMax), ] # dados inválidos DC

if (nrow(res) > 0) {
  res[,c(1:37)] <- NA

res1 <- meteo_rbs_dc[!is.na(meteo_rbs_dc$tempMax), ] # dados válidos DC

res2 <- meteo_rbs_esc[!is.na(meteo_rbs_esc$tempMax), ] |>
  unique() # dados válidos ESC

meteo_rbs_dc <- rbind(res1, res) |>
  dplyr::arrange(timestamp) |>
  unique()
}

## padronizando sequencia datas para preenchimento

temp <- data.frame(timestamp = seq(
  from = lubridate::as_datetime(start_date),
  to = lubridate::as_datetime(end_date),
  by = "hour"))

meteo_rbs_dc <- dplyr::right_join(meteo_rbs_dc, temp, by = "timestamp")
res2 <- dplyr::right_join(res2, temp, by = "timestamp")

# substituindo periodo dados NA estação defesa civil por dados escola
meteo_rbs <- dplyr::rows_patch(meteo_rbs_dc, res2, by = "timestamp")
summary(meteo_rbs) # valores dentro do "normal"

#selecionando dados existentes
meteo_rbs <- meteo_rbs[!is.na(meteo_rbs$deltat), ]

# selecionando variáveis de interesse
meteo_rbs <- meteo_rbs |>
  dplyr::mutate(Cidade = "Rio Branco do Sul") |>
  dplyr::select(Cidade, timestamp, temp, wind, direction, rain, humidity, radiation, pressure, uv)

colnames(meteo_rbs) <- c('Cidade', 'date',
                         'temp', 'ws', 'wd', 'prec', 'umid', 'rad', 'press', 'uv')


meteo_rbs <- rbind(meteo_rbs, last_meteo_rbs) |>
  dplyr::mutate(date = lubridate::force_tz(date, tz = "America/Sao_Paulo")) |>
  unique()

tz(meteo_rbs$date)

# gerar arquivo
save(meteo_rbs, file = "./data/meteo/meteo_rbs.Rda")


