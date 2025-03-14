### Unifying meteo data
# Author: Santos-Silva, J. C.
## Last update: 03/12/2025


#########################################################
#########################################################
load(file = "./data/meteo/meteo_colombo.Rda")
summary(meteo_colombo) # dentro da normalidade

load(file = "./data/meteo/meteo_rbs.Rda")
summary(meteo_rbs) # dentro da normalidade

meteo_hour <- rbind(meteo_rbs, meteo_colombo) |>
  dplyr::mutate(date = lubridate::as_datetime(date, tz = "America/Sao_Paulo"))


# saving
save(meteo_hour, file = "./data/meteo_hour.Rda")
