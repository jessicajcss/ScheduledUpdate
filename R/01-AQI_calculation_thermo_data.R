#Adaptation of US EPA according to
## https://community.purpleair.com/t/how-to-calculate-the-us-epa-pm2-5-aqi/877/12
## https://document.airnow.gov/technical-assistance-document-for-the-reporting-of-daily-air-quailty.pdf
## Last update: 2024-08-23

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


aqiFromPM10 <- function(pm) {

  #                                     AQI         RAW PM10 (ug/m³, 24-hour)
  # Good                               0 - 50   |  0.0 – 54
  # Moderate                          51 - 100  |  55 – 154
  # Unhealthy for Sensitive Groups   101 – 150  |  155 – 254
  # Unhealthy                        151 – 200  |  255 – 354
  # Very Unhealthy                   201 – 300  |  355 – 424
  # Hazardous                        301 – 500  |  425+

  if (pm > 425) {
    return(calcAQI(pm, 500, 301, 850, 425))  # Hazardous
  } else if (pm > 355) {
    return(calcAQI(pm, 300, 201, 424, 355))  # Very Unhealthy
  } else if (pm > 255) {
    return(calcAQI(pm, 200, 151, 354, 255))   # Unhealthy
  } else if (pm > 155) {
    return(calcAQI(pm, 150, 101, 254, 155))    # Unhealthy for Sensitive Groups
  } else if (pm > 55) {
    return(calcAQI(pm, 100, 51, 154, 55))     # Moderate
  } else if (pm >= 0) {
    return(calcAQI(pm, 50, 0, 54, 0))            # Good
  } else {
    return(NA)
  }
}

###########################################

aqiFromSO2 <- function(gas) {

  #                                     AQI         RAW SO2 (ppb, 1-hour max 98th)
  # Good                               0 - 50   |  0.0 – 35
  # Moderate                          51 - 100  |  36 – 75
  # Unhealthy for Sensitive Groups   101 – 150  |  76 – 185
  # Unhealthy                        151 – 200  |  186 – 304
  # Very Unhealthy                   201 – 300  |  305 – 604
  # Hazardous                        301 – 500  |  605+

  if (gas > 605) {
    return(calcAQI(gas, 500, 301, 1210, 605))  # Hazardous
  } else if (gas > 305) {
    return(calcAQI(gas, 300, 201, 604, 305))  # Very Unhealthy
  } else if (gas > 186) {
    return(calcAQI(gas, 200, 151, 304, 186))   # Unhealthy
  } else if (gas > 76) {
    return(calcAQI(gas, 150, 101, 185, 76))    # Unhealthy for Sensitive Groups
  } else if (gas > 36) {
    return(calcAQI(gas, 100, 51, 75, 36))     # Moderate
  } else if (gas >= 0) {
    return(calcAQI(gas, 50, 0, 35, 0))            # Good
  } else {
    return(NA)
  }
}


###########################################

aqiFromNO2 <- function(gas) {

  #                                     AQI         RAW NO2  (ppb, 1-hour max 98th)
  # Good                               0 - 50   |  0.0 – 53
  # Moderate                          51 - 100  |  54 – 100
  # Unhealthy for Sensitive Groups   101 – 150  |  101 – 360
  # Unhealthy                        151 – 200  |  361 – 649
  # Very Unhealthy                   201 – 300  |  650 – 1249
  # Hazardous                        301 – 500  |  1250+

  if (gas > 1250) {
    return(calcAQI(gas, 500, 301, 2498, 1250))  # Hazardous
  } else if (gas > 650) {
    return(calcAQI(gas, 300, 201, 1249, 650))  # Very Unhealthy
  } else if (gas > 361) {
    return(calcAQI(gas, 200, 151, 649, 361))   # Unhealthy
  } else if (gas > 101) {
    return(calcAQI(gas, 150, 101, 360, 101))    # Unhealthy for Sensitive Groups
  } else if (gas > 54) {
    return(calcAQI(gas, 100, 51, 100, 54))     # Moderate
  } else if (gas >= 0) {
    return(calcAQI(gas, 50, 0, 53, 0))            # Good
  } else {
    return(NA)
  }
}



###########################################

aqiFromCO <- function(gas) {

  #                                     AQI         RAW CO   (ppm, 8-hour max 98th)
  # Good                               0 - 50   |  0.0 – 4.4
  # Moderate                          51 - 100  |  4.5 – 9.4
  # Unhealthy for Sensitive Groups   101 – 150  |  9.5 – 12.4
  # Unhealthy                        151 – 200  |  12.5 – 15.4
  # Very Unhealthy                   201 – 300  |  15.5 – 30.4
  # Hazardous                        301 – 500  |  30.5+

  if (gas > 30.5) {
    return(calcAQI(gas, 500, 301, 61, 30.5))  # Hazardous
  } else if (gas > 15.5) {
    return(calcAQI(gas, 300, 201, 30.4, 15.5))  # Very Unhealthy
  } else if (gas > 12.5) {
    return(calcAQI(gas, 200, 151, 15.4, 12.5))   # Unhealthy
  } else if (gas > 9.4) {
    return(calcAQI(gas, 150, 101, 12.4, 9.4))    # Unhealthy for Sensitive Groups
  } else if (gas > 4.4) {
    return(calcAQI(gas, 100, 51, 9.4, 4.4))     # Moderate
  } else if (gas >= 0) {
    return(calcAQI(gas, 50, 0, 4.4, 0))            # Good
  } else {
    return(NA)
  }
}


###########################################

aqiFromO3 <- function(gas) {

  #                                     AQI         RAW O3  (ppb, 8-hour max 98th)
  # Good                               0 - 50   |  0.000 - 54
  # Moderate                          51 - 100  |  55 - 70
  # Unhealthy for Sensitive Groups   101 – 150  |  71 – 85
  # Unhealthy                        151 – 200  |  86 – 105
  # Very Unhealthy                   201 – 300  |  106 – 200
  # Hazardous                        301 – 500  |  201+

  if (gas > 201) {
    return(calcAQI(gas, 500, 301, 402, 201))  # Hazardous
  } else if (gas > 106) {
    return(calcAQI(gas, 300, 201, 200, 106))  # Very Unhealthy
  } else if (gas > 86) {
    return(calcAQI(gas, 200, 151, 105, 86))   # Unhealthy
  } else if (gas > 71) {
    return(calcAQI(gas, 150, 101, 85, 71))    # Unhealthy for Sensitive Groups
  } else if (gas > 55) {
    return(calcAQI(gas, 100, 51, 70, 55))     # Moderate
  } else if (gas >= 0) {
    return(calcAQI(gas, 50, 0, 54, 0))            # Good
  } else {
    return(NA)
  }
}



#  --------------------------------------------------------------------------------------------------------
#                                              READING THE FILES
#  --------------------------------------------------------------------------------------------------------



load("./data/dataaggfinal.Rda")
load("./data/air_quality_data_ugm3.Rda")

#  --------------------------------------------------------------------------------------------------------
#                                              AQI Determination
#  --------------------------------------------------------------------------------------------------------


# Matching thermo data X legislation

library(dplyr)
library(tidyr)
library(lubridate)
library(zoo)
w <- 8

#### reviewed by chatGPT ####

library(dplyr)
library(tidyr)
library(lubridate)
library(zoo)

w <- 8
air_quality_data2 <- dataaggfinal %>%
  select(Cidade, date, SO2, NO2, O3, CO, PM2.5, PM10) %>%
  pivot_longer(cols = SO2:PM10, names_to = "variable", values_to = "value") %>%
  mutate(variable = factor(variable, levels = c("SO2", "NO2", "O3", "CO", "PM10", "PM2.5"))) %>%
  group_by(Cidade, variable) %>%
  mutate(
    Npoints = 1:n() - findInterval(date - hours(w), date),
    Mean8 = zoo::rollapply(value, Npoints, mean, partial = TRUE, fill = NA)
  ) %>%
  ungroup() %>%
  mutate(
    sample_day = as.Date(date, tz = "America/Sao_Paulo"),
    Mean8 = if_else(Npoints < 8 | !(variable %in% c("O3", "CO")), NA_real_, Mean8),
    value = if_else(variable %in% c("O3", "CO"), Mean8, value)
  ) %>%
  select(-date, -Npoints, -Mean8) %>%
  drop_na() %>%
  group_by(Cidade, sample_day, variable) %>%
  mutate(
    value = case_when(
      variable %in% c("O3", "CO", "SO2", "NO2") ~ max(value),
      TRUE ~ mean(value, na.rm = TRUE)
    )
  ) %>%
  distinct() %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  replace(is.na(.), -999) %>%
  rowwise() %>%
  mutate(
    AQI_SO2 = aqiFromSO2(SO2),
    AQI_NO2 = aqiFromNO2(NO2),
    AQI_O3 = aqiFromO3(O3),
    AQI_CO = aqiFromCO(CO),
    AQI_PM25 = aqiFromPM25(`PM2.5`),
    AQI_PM10 = aqiFromPM10(PM10)
  ) %>%
  ungroup() %>%
  mutate(across(where(is.numeric), ~ ifelse(. < 0, NA, .))) %>%
  mutate(
    AQI = pmax(AQI_SO2, AQI_NO2, AQI_O3, AQI_CO, AQI_PM25, AQI_PM10, na.rm = TRUE),
    AQI_Qualidade = sapply(AQI, AQI_Qualidade)  # Adjusted to use sapply for vectorized application
  )

summary(air_quality_data2)

save(air_quality_data, file="./data/air_quality_data.Rda")
