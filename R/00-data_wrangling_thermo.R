#* DATA WRANGLING *#
#* Local: R_Analysis/Post_Doc >> /script  ||  /data/data_input
#* Date: 14/04/2024, Last update: 2025-03-05
#* By: Jéssica C. dos Santos-Silva



#####################
## Dealing with

#> Data Time Zone, everything in Local Time (tz = "America/Sao_Paulo")
#> Missing data: excluded if <50% at raw interval, and if <10h a day (if >10h, but <24h, data was filled with the hour average value).
#> Outliers: meeting the rule [x > Percentile3 + (IPR * 1.5) | x < Percentile1 - (IPR * 1.5)] Thermo data, excluded percentil <5% and >95% / PurpleAir data, excluded <25% and >75%
#> Correções das concentrações para unidades padrões da legislação
#> Outputs in Line [246]
#> Data thermo updated with GitHub files


library(tidyverse)


##########################################
### SOURCE OF DATA ----


####* Dados qualidade do ar - THERMO


### Dados qualidade do ar - Thermo baixados in situ

# Run Script "insitu_thermo_data.R" [OK]
#> dataframe: *data_thermo >> output: insitu_thermo.csv* [OK, LocalTime]

##########################################
#  https://www.r-bloggers.com/2022/11/using-functional-analysis-to-model-air-pollution-data-in-r/#amp_tf=De%20%251%24s&aoh=17118517416160&referrer=https%3A%2F%2Fwww.google.com&ampshare=https%3A%2F%2Fwww.r-bloggers.com%2F2022%2F11%2Fusing-functional-analysis-to-model-air-pollution-data-in-r%2F

### Checking for Data integrity ----

library(tidyverse)
library(dplyr)

#source("./scripts/00-preprocessing_thermo_GitHub_data.R")
load(url("https://github.com/jessicajcss/ScheduledUpdate/raw/refs/heads/main/data/data_thermo_update.Rda"))
#load("./data/data_thermo_update.Rda")

### Dealing with outliers
#### https://www.geeksforgeeks.org/how-to-remove-outliers-from-multiple-columns-in-r-dataframe/


# create detect outlier function
detect_outlier <- function (x) {

  # calculate first percentile
  Percentile1 <- quantile(x, probs=.05)

  # calculate third percentile
  Percentile3 <- quantile(x, probs=.95)

  # calculate interpercentile range
  IPR = Percentile3 - Percentile1

  # return true or false
  x > Percentile3 + (IPR * 1.5) | x < Percentile1 - (IPR * 1.5)
}

# create remove outlier function
remove_outlier <- function(dataframe, columns = names(dataframe)) {

  # for loop to traverse in columns vector
  for (col in columns) {

    # remove observation if it satisfies outlier function
    dataframe <- dataframe[!detect_outlier(dataframe[[col]]), ]
  }

  # return dataframe
  print("Remove outliers")
  print(dataframe)
}






#  --------------------------------------------------------------------------------------------------------
#                                             THERMO DATA
#  --------------------------------------------------------------------------------------------------------

#### UPLOAD DATA THERMO ----

data_thermo <- data_thermo %>%
  mutate(date = force_tz(date, tz = "America/Sao_Paulo"))




### WORKING THERMO DATA ----

# A remoção de outliers consistiu em remoção das leituras abaixo do percentil de 5% e acima do percentil de 95%.


data_thermo <- data_thermo %>%
  dplyr::group_by(Cidade) %>%
  remove_outlier(., c('SO2', 'NO2', 'O3', 'CO', 'PM2.5','PM10'))


data_thermo_instantaneo <- data_thermo %>%
  # UNIT CONVERSION: https://www.breeze-technologies.de/blog/air-pollution-how-to-convert-between-mgm3-%C2%B5gm3-ppm-ppb/
  mutate(CO = CO*1.15, #from ppm to mg/m³
         O3 = O3*1.96, #from ppb to ug/m³
         NO2 = NO2*1.88, #from ppb to ug/m³
         SO2 = SO2*2.62, #from ppb to ug/m³
         PM2.5 = PM2.5, # ug/m³
         PM10 = PM10) #ug/m³

save(data_thermo_instantaneo, file="./data/data_thermo_instantaneo_ugm3.Rda")



### Dealing with missing data - if a value is missing from 04:00 on a specific day, we’ll use the mean of the non-missing values taken at 04:00 on every other day.

# hourly data


x <- 12 # data available x times per hour
h <- 1 # aggregate to every h hours
# aggregation puts NA if data has not x valid values per hour
#data_thermo$date <- as.POSIXct(strptime(data_thermo$date, '%Y-%m-%d %H:%M:%S'))

missing_dataagg <- data_thermo %>%
  mutate(Cidade = as.factor(Cidade)) %>%
  group_by(date = floor_date(date, '1 hour')) %>%
  filter(!is.na(PM2.5))  %>%
  count(Cidade, date) %>%
  arrange(desc(n))



# which ones are missing >= 10 hours of data
too_many_missing <- missing_dataagg %>%
  filter(n < x/2) %>%
  mutate(LocalTime = paste(Cidade, date, sep = " "))

# remove missing data
dataagg <- data_thermo %>%
  mutate(Cidade = as.factor(Cidade)) %>%
  group_by(date = floor_date(date, '1 hour')) %>%
  mutate(LocalTime = paste(Cidade, date, sep = " ")) %>%
  filter(!(LocalTime %in% too_many_missing$LocalTime)) %>%
  dplyr::select(-LocalTime) %>%
  group_by(Cidade, date) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
  unique()


# Now fill up missing datetimes with NA
a <- seq(min(dataagg$date, na.rm = T),
         max(dataagg$date, na.rm = T), by=paste(h,"hours"))

date <- a[seq(1, length(a), by=1)]

tdf <- as.data.frame(date)
tdf$Cidade <- "Rio Branco do Sul"
tdf2 <- tdf %>%
  mutate(Cidade = "Almirante Tamandaré")

tdf3 <- rbind(tdf, tdf2) %>%
  mutate(date = force_tz(date, tz = "America/Sao_Paulo"),
         date = as_datetime(date),
         Cidade = as.factor(Cidade)) %>%
  as_tibble()


dataaggfinal <- merge(dataagg, tdf3, by = c("Cidade", "date"), all = T)


rm(tdf, tdf2, tdf3)

# days with missing values
missing <- dataaggfinal %>%
  mutate(datepaste = as.Date(date, tz = "America/Sao_Paulo")) %>%
  select(Cidade, datepaste, PM2.5) %>%
  dplyr::group_by(Cidade, datepaste) %>%
  filter(is.na(PM2.5))  %>%
  count(Cidade, datepaste) %>%
  arrange(desc(n))


# which ones are missing >= 10 hours of data
too_many_missing <- missing %>%
  filter(n >= 10) %>%
  mutate(LocalTime = paste(Cidade, datepaste, sep = " "))



# remove missing data
dataaggfinal <- dataaggfinal %>%
  mutate(LocalTime = paste(Cidade, as.Date(date), sep = " ")) %>%
  filter(!(LocalTime %in% too_many_missing$LocalTime)) %>%
  dplyr::select(-LocalTime)

View(dataaggfinal)

# mean imputation for the others

avg_hour <- dataaggfinal %>%
  mutate(hour = hour(date)) %>%
  dplyr::group_by(Cidade, hour) %>%
  dplyr::summarize(RHavg = mean(rh_sensor, na.rm = TRUE),
            COavg = mean(CO, na.rm = TRUE),
            O3avg = mean(O3, na.rm = TRUE),
            NO2avg = mean(NO2, na.rm = TRUE),
            SO2avg = mean(SO2, na.rm = TRUE),
            PM2.5avg = mean(PM2.5, na.rm = TRUE),
            PM10avg = mean(PM10, na.rm = TRUE))

# anulado, por ora!!!! 2025-03-05
dataaggfinal2 <- dataaggfinal %>%
  mutate(hour = hour(date)) %>%
  left_join(avg_hour, by = c("Cidade", "hour")) %>%
  mutate(rh_sensor = case_when(is.na(rh_sensor) ~ RHavg, TRUE ~ rh_sensor),
         CO = case_when(is.na(CO) ~ COavg, TRUE ~ CO),
         O3 = case_when(is.na(O3) ~ O3avg, TRUE ~ O3),
         NO2 = case_when(is.na(NO2) ~ NO2avg, TRUE ~ NO2),
         SO2 = case_when(is.na(SO2) ~ SO2avg, TRUE ~ SO2),
         PM2.5 = case_when(is.na(PM2.5) ~ PM2.5avg, TRUE ~ PM2.5),
         PM10 = case_when(is.na(PM10) ~ PM10avg, TRUE ~ PM10)) %>%
  dplyr::select(Cidade, date, SO2, NO2, O3, CO, PM2.5, PM10, rh_sensor)


dataaggfinal[, c(3:9)] <- data.frame(sapply(dataaggfinal[, c(3:9)],
                                  function(x) ifelse(is.nan(x), NA, x)))



save(dataaggfinal, file="./data/dataaggfinal.Rda")
library(tidyverse)

## testar aqui pq aparece AMT 03/03 após calculos com data_thermo

#################### TO COMPARE WITH WHO, 2021 AQG

# https://www.breeze-technologies.de/blog/air-pollution-how-to-convert-between-mgm3-%C2%B5gm3-ppm-ppb/


data_thermo_agg <- dataaggfinal %>%
  # UNIT CONVERSION: https://www.breeze-technologies.de/blog/air-pollution-how-to-convert-between-mgm3-%C2%B5gm3-ppm-ppb/
  mutate(CO = CO*1.15, #from ppm to mg/m³
         O3 = O3*1.96, #from ppb to ug/m³
         NO2 = NO2*1.88, #from ppb to ug/m³
         SO2 = SO2*2.62, #from ppb to ug/m³
         PM2.5 = PM2.5, # ug/m³
         PM10 = PM10) #ug/m³

air_quality_data_ugm3 <- data_thermo_agg
save(air_quality_data_ugm3, file="./data/air_quality_data_ugm3.Rda")

