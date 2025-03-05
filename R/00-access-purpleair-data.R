#### Acessing Purpleair data and scheduling its update
### Last update: 2025-03-04
## By: Santos-Silva, J. C.


source("R/00-getPurpleairApiHistory.R")
purpleair_api <- Sys.getenv("PURPLEAIR_API")



#esse id pode ser obtido no mapa da PurpleAir, selecione apenas os de interesse
sensor_id <-  c('175095', '175099', '175101', '175103',
                '175109', '175115', '175121', '175123',
                '175235', '175393', '175395', '175387',
                '175403', '175407', '175451',
                '91267', '99667')

#Mais informações em https://api.purpleair.com/
variaveis <- c("latitude, longitude, humidity, temperature,
               pm1.0_atm, pm1.0_atm_a, pm1.0_atm_b,
               pm2.5_atm, pm2.5_atm_a, pm2.5_atm_b,
               pm2.5_cf_1, pm2.5_cf_1_a, pm2.5_cf_1_b") # para corrigir PM2.5


# Baixando os dados últimas 24h

todos_purpleair <- getPurpleairApiHistory(
  sensorIndex    = sensor_id,
  apiReadKey     = purpleair_api, #https://develop.purpleair.com/keys ### AJUSTA AQUI
  startTimeStamp = Sys.time() - 86400, ### AJUSTA AQUI
  endTimeStamp   = Sys.time(), ### AJUSTA AQUI
  average        = "0", ### em tempo real
  fields         = variaveis)


todos_purpleair$date <- with_tz(todos_purpleair$time_stamp, tz = "America/Sao_Paulo")



#### corrigindo concentracao conforme razão entre a/b e melhor coerência nas concentracoes
todos_purpleair[c(4:12)][todos_purpleair[c(4:12)] > 100] <- NA
summary(todos_purpleair)

purpleair <- todos_purpleair %>%
  mutate(a_b = pm2.5_cf_1_a/pm2.5_cf_1_b,
         pm2.5_cf = ifelse((is.na(a_b) & is.na(pm2.5_cf_1_b)), pm2.5_cf_1_a,
                    ifelse((is.na(a_b) & is.na(pm2.5_cf_1_a)), pm2.5_cf_1_b,
                    ifelse((a_b > 1.1 | a_b < 0.9) & (pm2.5_cf_1_a > pm2.5_cf_1_b), pm2.5_cf_1_b,
                    ifelse((a_b > 1.1 | a_b < 0.9) & (pm2.5_cf_1_b > pm2.5_cf_1_a), pm2.5_cf_1_a,
                           pm2.5_cf_1)))),
         PM2.5 = 0.524*pm2.5_cf - 0.0862*humidity + 5.75,
         temperature = (temperature - 32) * 5/9)

summary(purpleair)




### Filtering according to the installation date



purpleair <- purpleair %>%
  mutate(sensor_id = case_when(sensor_id == '91267' & date <= "2023-10-19 00:00:00" ~ NA, TRUE ~ sensor_id),
         sensor_id = case_when(sensor_id == '99667' & date <= "2023-10-19 00:00:00" ~ NA, TRUE ~ sensor_id),
         sensor_id = case_when(sensor_id == '175095' & date <= "2023-07-24 00:00:00" ~ NA, TRUE ~ sensor_id),
         sensor_id = case_when(sensor_id == '175099' & date <= "2023-10-19 00:00:00" ~ NA, TRUE ~ sensor_id),
         sensor_id = case_when(sensor_id == '175101' & date <= "2023-07-20 00:00:00" ~ NA, TRUE ~ sensor_id),
         sensor_id = case_when(sensor_id == '175103' & date <= "2023-09-15 00:00:00" ~ NA, TRUE ~ sensor_id),
         sensor_id = case_when(sensor_id == '175109' & date <= "2023-07-20 00:00:00" ~ NA, TRUE ~ sensor_id),
         sensor_id = case_when(sensor_id == '175115' & date <= "2023-08-15 00:00:00" ~ NA, TRUE ~ sensor_id),
         sensor_id = case_when(sensor_id == '175121' & date <= "2023-07-20 00:00:00" ~ NA, TRUE ~ sensor_id),
         sensor_id = case_when(sensor_id == '175123' & date <= "2023-05-31 00:00:00" ~ NA, TRUE ~ sensor_id),
         sensor_id = case_when(sensor_id == '175235' & date <= "2023-10-23 00:00:00" ~ NA, TRUE ~ sensor_id),
         sensor_id = case_when(sensor_id == '175387' & date <= "2023-08-22 00:00:00" ~ NA, TRUE ~ sensor_id),
         sensor_id = case_when(sensor_id == '175393' & date <= "2023-09-14 00:00:00" ~ NA, TRUE ~ sensor_id),
         sensor_id = case_when(sensor_id == '175395' & date <= "2023-05-31 00:00:00" ~ NA, TRUE ~ sensor_id),
         sensor_id = case_when(sensor_id == '175403' & date <= "2023-09-25 00:00:00" ~ NA, TRUE ~ sensor_id),
         sensor_id = case_when(sensor_id == '175407' & date <= "2023-06-06 00:00:00" ~ NA, TRUE ~ sensor_id),
         sensor_id = case_when(sensor_id == '175411' & date <= "2023-10-19 00:00:00" ~ NA, TRUE ~ sensor_id),
         sensor_id = case_when(sensor_id == '175451' & date <= "2023-09-14 00:00:00" ~ NA, TRUE ~ sensor_id))

purpleair <- purpleair %>%
  filter(!is.na(sensor_id))



## identificando os locais de sensores
data_purpleair <- purpleair %>%
  filter(sensor_id != "175411") %>%
  mutate(sensor_id = as.character(sensor_id),
         date = force_tz(date, tz = "America/Sao_Paulo"),
         Cidade = case_when(sensor_id == '91267' ~ "Rio Branco do Sul",
                            sensor_id == '99667' ~ "Campo Largo",
                            sensor_id == "175095" ~ "Almirante Tamandaré",
                            sensor_id == '175099' ~ "Rio Branco do Sul",
                            sensor_id == '175101' ~ "Rio Branco do Sul",
                            sensor_id == '175103' ~ "Campo Largo",
                            sensor_id == '175109' ~ "Rio Branco do Sul",
                            sensor_id == '175115' ~ "Almirante Tamandaré",
                            sensor_id == '175121' ~ "Itaperuçú",
                            sensor_id == '175123' ~ "Rio Branco do Sul",
                            sensor_id == '175395' ~ "Rio Branco do Sul",
                            sensor_id == '175235' ~ "Campo Largo",
                            sensor_id == '175387' ~ "Rio Branco do Sul",
                            sensor_id == '175393' ~ "Itaperuçú",
                            sensor_id == '175385' ~ "Rio Branco do Sul",
                            sensor_id == "175403" ~ "Campo Largo",
                            sensor_id == "175407" ~ "Colombo",
                            sensor_id == "175451" ~ "Itaperuçú",
                            TRUE ~ sensor_id),
         Tipo = case_when(sensor_id == '91267' ~ "outdoor",
                               sensor_id == '99667' ~ "outdoor",
                               sensor_id == "175095" ~ "outdoor",
                               sensor_id == '175099' ~ "outdoor",
                               sensor_id == '175101' ~ "outdoor",
                               sensor_id == '175103' ~ "outdoor",
                               sensor_id == '175109' ~ "outdoor",
                               sensor_id == '175115' ~ "outdoor",
                               sensor_id == '175121' ~ "outdoor",
                               sensor_id == '175123' ~ "outdoor",
                               sensor_id == '175395' ~ "outdoor",
                               sensor_id == '175235' ~ "outdoor",
                               sensor_id == '175387' ~ "outdoor",
                               sensor_id == '175393' ~ "outdoor",
                               sensor_id == '175385' ~ "outdoor",
                               sensor_id == "175403" ~ "outdoor",
                               sensor_id == "175407" ~ "outdoor",
                               sensor_id == "175451" ~ "outdoor",
                               TRUE ~ NA),
         sensor_id = case_when(sensor_id == '91267' ~ "Dona Suzana (outdoor)",
                               sensor_id == '99667' ~ "Escola Hans (outdoor)",
                               sensor_id == "175095" ~ "Prefeitura (outdoor)",
                               sensor_id == '175099' ~ "Dona Suzana (indoor)",
                               sensor_id == '175101' ~ "Dona Eliete (outdoor)",
                               sensor_id == '175103' ~ "Escola Madalena (outdoor)",
                               sensor_id == '175109' ~ "Dona Eliete (indoor)",
                               sensor_id == '175115' ~ "Escola João Candido (outdoor)",
                               sensor_id == '175121' ~ "Prefeitura (outdoor)",
                               sensor_id == '175123' ~ "Defesa Civil (indoor)",
                               sensor_id == '175395' ~ "Defesa Civil (outdoor)",
                               sensor_id == '175235' ~ "SME (outdoor)",
                               sensor_id == '175387' ~ "Escola Hilda (outdoor)",
                               sensor_id == '175393' ~ "Escola Cândidos (outdoor)",
                               sensor_id == '175385' ~ "Defesa Civil (outdoor)",
                               sensor_id == "175403" ~ "SMMA (outdoor)",
                               sensor_id == "175407" ~ "Embrapa (outdoor)",
                               sensor_id == "175451" ~ "Pombas (outdoor)",
                               TRUE ~ sensor_id))




#  --------------------------------------------------------------------------------------------------------
#                                              AQI FUNCTIONS
#  --------------------------------------------------------------------------------------------------------


calcAQI <- function(Cp, Ih, Il, BPh, BPl) {
  a <- (Ih - Il)
  b <- (BPh - BPl)
  c <- (Cp - BPl)
  return(round((a/b) * c + Il))
}





#################### AQI_INDEX BY POLLUTANT
AQI_Qualidade <- function(aqi) {

  #                                     AQI
  # Good                               0 - 50   |
  # Moderate                          51 - 100  |
  # Unhealthy for Sensitive Groups   101 – 150  |
  # Unhealthy                        151 – 200  |
  # Very Unhealthy                   201 – 300  |
  # Hazardous                        301 – 500  |

  if (aqi > 200) {
    return("Péssima")  # Hazardous
  } else if (aqi > 200) {
    return("Muito Ruim")  # Very Unhealthy
  } else if (aqi > 150) {
    return("Ruim")   # Unhealthy
  } else if (aqi > 100) {
    return("Ruim para grupos sensíveis")    # Unhealthy for Sensitive Groups
  } else if (aqi > 50) {
    return("Moderada")     # Moderate
  } else if (aqi >= 0) {
    return("Boa")            # Good
  } else {
    return(NA)
  }
}

#################### BY POLLUTANT
aqiFromPM25 <- function(pm) {

  #                                     AQI         RAW PM2.5  (ug/m³, 24-hour)
  # Good                               0 - 50   |  0.0 – 9.0
  # Moderate                          51 - 100  |  9.1 – 35.4
  # Unhealthy for Sensitive Groups   101 – 150  |  35.5 – 55.4
  # Unhealthy                        151 – 200  |  55.5 – 125.4
  # Very Unhealthy                   201 – 300  |  125.5 – 225.4
  # Hazardous                        301 – 500  |  225.5 – 500.4

  if (pm > 225.5) {
    return(calcAQI(pm, 500, 301, 500.4, 225.5))  # Hazardous
  } else if (pm > 125.5) {
    return(calcAQI(pm, 300, 201, 225.4, 125.5))  # Very Unhealthy
  } else if (pm > 55.5) {
    return(calcAQI(pm, 200, 151, 125.4, 55.5))   # Unhealthy
  } else if (pm > 35.5) {
    return(calcAQI(pm, 150, 101, 55.4, 35.5))    # Unhealthy for Sensitive Groups
  } else if (pm > 9.1) {
    return(calcAQI(pm, 100, 51, 35.4, 9.1))     # Moderate
  } else if (pm >= 0) {
    return(calcAQI(pm, 50, 0, 9, 0))            # Good
  } else {
    return(NA)
  }
}

###########################################



# Matching thermo data X legislation

data_purpleair <- data_purpleair %>%
  select(Cidade, Tipo, sensor_id, date, PM2.5) %>%
  dplyr::mutate(sample_day = as.Date(date, format = "%Y-%m-%d", , tz = "America/Sao_Paulo")) %>%
  select(-date) %>%
  drop_na() %>%
  dplyr::group_by(Cidade, sample_day) %>%
  dplyr::mutate(PM2.5 = mean(PM2.5, na.rm = T)) %>%
  unique() %>%
  replace(is.na(.), -999) %>% #WORKING AROUND NA VALUES
  dplyr::rowwise() %>%
  dplyr::mutate(AQI_Qualidade = aqiFromPM25(PM2.5))  %>%
  mutate_all(~ ifelse(. < 0, NA, .))


save(data_purpleair, file = "./data/data_purpleair.Rda")
