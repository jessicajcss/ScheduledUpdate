
# https://wdg.plugfield.com.br/doc-api/index.html#/Login/post_login
# POSTMAN: https://cloudy-meadow-20129.postman.co/workspace/My-Workspace~aedb22c3-f19c-4c9a-80b5-3e273693bb2b/request/31843085-b52a9c2f-8959-4ab5-ac6c-b9bd02c72ee5?tab=headers&ctx=code
# https://cloudy-meadow-20129.postman.co/workspace/My-Workspace~aedb22c3-f19c-4c9a-80b5-3e273693bb2b/request/31843085-b52a9c2f-8959-4ab5-ac6c-b9bd02c72ee5?tab=headers&ctx=code

library(httr)
library(jsonlite)
library(lubridate)
library(dplyr)

# Function to fetch data in 30-day chunks
fetch_large_data <- function(device_id, start_date, end_date) {
  start_date <- as.POSIXct(start_date, format = "%d/%m/%Y %H:%M:%S", tz = "UTC")
  end_date <- as.POSIXct(end_date, format = "%d/%m/%Y %H:%M:%S", tz = "UTC")

  max_days <- 30  # API limit
  all_data <- list()  # Store all chunks

  current_start <- start_date
  while (current_start < end_date) {
    current_end <- min(current_start + days(max_days), end_date)  # 30 days or less

    # Format dates for API
    begin_encoded <- URLencode(format(current_start, "%d/%m/%Y %H"))
    end_encoded <- URLencode(format(current_end, "%d/%m/%Y %H"))

    # Build URL
    url <- paste0("https://prod-api.plugfield.com.br/data/hourly?device=", device_id,
                  "&begin=", begin_encoded, "&end=", end_encoded)

    plugfield_api <- Sys.getenv("PLUG_API")
    plugfield_auth <- Sys.getenv("PLUG_AUTH")

    # Headers
    headers <- add_headers(
      'accept' = 'application/json',
      'x-api-key' = plugfield_api,
      'Authorization' = plugfield_auth
    )

    # Request
    res <- GET(url, headers)

    # Process Response
    if (status_code(res) == 200) {
      content_text <- content(res, "text", encoding = "UTF-8")
      json_data <- fromJSON(content_text, flatten = TRUE)
      df <- as.data.frame(json_data)

      # Format timestamp
      if ("timestamp" %in% colnames(df)) {
        df <- df %>%
          mutate(timestamp = as.POSIXct(timestamp, format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC")) %>%
          mutate(timestamp = format(timestamp, "%d/%m/%Y %H:%M:%S"))
      }

      all_data <- append(all_data, list(df))  # Store in list
    } else {
      print(paste("Error:", status_code(res), "for period", format(current_start, "%d/%m/%Y"), "-", format(current_end, "%d/%m/%Y")))
    }

    # Move to next period
    current_start <- current_end + seconds(1)  # Avoid overlaps
  }

  # Combine all data chunks into one dataframe
  final_df <- bind_rows(all_data)
  return(final_df)
}

# DADOS ESCOLA
device_id <- 3184
start_date <- "01/06/2023 00:00:00"  # Start period
end_date <- Sys.time() #"26/02/2025 00:00:00"  # End period (more than 30 days)

df_escola <- fetch_large_data(device_id, start_date, end_date)

# DADOS DEFESA CIVIL
device_id <- 3118
start_date <- "01/06/2023 00:00:00"  # Start period
end_date <- Sys.time() #"26/02/2025 00:00:00"  # End period (more than 30 days)

df_defesacivil <- fetch_large_data(device_id, start_date, end_date)



# joining all data from rio branco do sul
df <- rbind(df_escola, df_defesacivil)

# to remove columns where all values are either NA or 0 from your dataframe df:
# Print first rows
head(df)

# selecionar e pardonizar variaveis, renomear estacao
# criar loop update horário
# criar repositório com senha e token
library(lubridate)
library(dplyr)
library(purrr)

df <- df %>%
  mutate(across(where(is.list), ~ map_chr(., ~ if (length(.) == 0) NA else paste(., collapse = ",")))) %>%  # Flatten lists
  select(where(~ !all(is.na(.))) & where(~ !all(. == 0, na.rm = TRUE)) & where(~ !all(. == "NULL", na.rm = TRUE)))  # Remove all-NA or all-0 columns

head(df)  # Check the cleaned dataframe




# fix timestamp format
df <- df %>%
  mutate(timestamp = ymd_hms(localDateTime))

head(df$timestamp)
summary(df)


# Preparando periodo sem dados na estação defesa civil [NA] para substituir por dados escola
meteo_rbs_dc <- df %>%
  subset(deviceId == 3118)

meteo_rbs_esc <- df %>%
  subset(deviceId == 3184)

res <- meteo_rbs_dc[is.na(meteo_rbs_dc$tempMax), ] # dados inválidos DC
res[,c(1:37)] <- NA

res1 <- meteo_rbs_dc[!is.na(meteo_rbs_dc$tempMax), ] # dados válidos DC

res2 <- meteo_rbs_esc[!is.na(meteo_rbs_esc$tempMax), ] %>%
  unique() # dados válidos ESC

meteo_rbs_dc <- rbind(res1, res) %>%
  arrange(timestamp) %>%
  unique()


## padronizando sequencia datas para preenchimento

temp <- data.frame(timestamp = seq(
  from = dmy_hms(start_date),
  to = as_datetime(end_date),
  by = "hour"))

meteo_rbs_dc <- right_join(meteo_rbs_dc, temp, by = "timestamp")
res2 <- right_join(res2, temp, by = "timestamp")

# substituindo periodo dados NA estação defesa civil por dados escola
meteo_rbs <- rows_patch(meteo_rbs_dc, res2, by = "timestamp")
summary(meteo_rbs) # valores dentro do "normal"

#selecionando dados existentes
meteo_rbs <- meteo_rbs[!is.na(meteo_rbs$deltat), ]

# selecionando variáveis de interesse
meteo_rbs <- meteo_rbs %>%
  mutate(Cidade = "Rio Branco do Sul") %>%
  select(Cidade, timestamp, temp, wind, direction, rain, humidity, radiation, pressure, uv)

colnames(meteo_rbs) <- c('Cidade', 'date',
                         'temp', 'ws', 'wd', 'prec', 'umid', 'rad', 'press', 'uv')


# gerar arquivo
save(meteo_rbs, file = "./data/meteo/meteo_rbs.Rda")





#########################################################
#########################################################

meteo_colombo <- read.csv("./data/meteo/dados_B806_H_2023-06-01_2025-02-26.csv",
                          skip = 10,
                          sep = ";")


meteo_colombo <- meteo_colombo %>%
  mutate(data = dmy(Data.Medicao),
         time = sub("00", "", Hora.Medicao),# sub("(\\d+)(\\d{2})", "\\1:\\2", Hora.Medicao))
         date = ymd_hms(paste0(data," ", time, ":00:00"))) %>%
  select(-Data.Medicao, -Hora.Medicao, -data, -time)

colnames(meteo_colombo) <- c('prec', 'press', 'press2', 'press3', 'press4',
                             'rad', 'temp1', 'temp', 'temp2', 'temp3', 'temp4', 'temp5', 'temp6',
                             'tensao', 'umid1', 'umid2', 'umid',
                             'wd', 'raj', 'ws', 'date')

meteo_colombo <- meteo_colombo %>%
  mutate(Cidade = "Colombo",
         uv = NA,) %>%
  select(Cidade, date, temp, ws, wd, prec, umid, rad, press, uv) %>%
  mutate(across(c(temp, ws, wd, prec, umid, rad, press, uv), as.numeric))


summary(meteo_colombo) # dentro da normalidade


# gerar arquivo
save(meteo_colombo, file = "./data/meteo/meteo_colombo.Rda")



#########################################################
#########################################################
meteo_hour <- rbind(meteo_rbs, meteo_colombo)
save(meteo_hour, file = "./data/meteo_hour.Rda")
