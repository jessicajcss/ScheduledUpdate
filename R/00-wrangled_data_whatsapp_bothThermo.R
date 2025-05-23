# Wrangled instantaneous data
#Last update: 15/03/2025
# By Santos-Silva, J. C.

#!!!!#!!!!#!!!!#!!!!#!!!!#!!!!#!!!!#!!!!#!!!!#!!!!#!!!!#!!!!#!!!!#!!!!#!!!!#!!!!#!!!!#!!!!#!!!!
#!!!! CHECAR LINHAS 51, 112 & LOOP message, e 00-send_whatsapp para AMT, se necessário ----
#!!!!#!!!!#!!!!#!!!!#!!!!#!!!!#!!!!#!!!!#!!!!#!!!!#!!!!#!!!!#!!!!#!!!!#!!!!#!!!!#!!!!#!!!!#!!!!


library(httr)
library(jsonlite)

#Sys.setlocale("LC_ALL", 'pt_BR')

sessionInfo()


#################################################################

# Selecionando dados do dia, última hora
hoje_hora <- Sys.time() |> lubridate::force_tz("America/Sao_Paulo")


if (lubridate::hour(hoje_hora) != 0) {
  # Selecionando dados do dia
  hoje <- Sys.Date() |> lubridate::force_tz("America/Sao_Paulo")
} else {
  hoje <- (Sys.Date() -1) |> lubridate::force_tz("America/Sao_Paulo")

}

# to test:
## hoje_hora <- lubridate::ymd_hms("2025-03-17 11:00:00")  |> lubridate::force_tz("America/Sao_Paulo")
## hoje <- as.Date("2025-03-17") |> lubridate::force_tz("America/Sao_Paulo")


# THERMO data in real time, in ug/m³ and mg/m³ (CO)
load(file = "./data/data_thermo_instantaneo_ugm3.Rda")

lubridate::tz(data_thermo_instantaneo$date)

# PURPLEAIR data in real time, in ug/m³
load(file = "./data/data_purpleair_instantaneo.Rda")

# para valores de concentracao, preferência é dada ao registrado pelos GM

if (hoje %in% data_thermo_instantaneo$date[data_thermo_instantaneo$Cidade == "Rio Branco do Sul"]) {

  data_purpleair_instantaneo <- data_purpleair_instantaneo |>
    dplyr::select(Cidade, date, PM2.5) |>
    subset(Cidade != "Rio Branco do Sul") #& Cidade != "Almirante Tamandaré")

} else {

  data_purpleair_instantaneo <- data_purpleair_instantaneo |>
    dplyr::select(Cidade, date, PM2.5)

}


if (hoje %in% data_thermo_instantaneo$date[data_thermo_instantaneo$Cidade == "Almirante Tamandaré"]) {

  data_purpleair_instantaneo <- data_purpleair_instantaneo |>
    dplyr::select(Cidade, date, PM2.5) |>
    subset(Cidade != "Almirante Tamandaré")

}

data_purpleair_instantaneo$PM2.5[data_purpleair_instantaneo$PM2.5 < 0] <- 0

# unificando dados sensores
real_time <- dplyr::bind_rows(data_thermo_instantaneo, data_purpleair_instantaneo)

# selecionando ultimo registro do dia
alerta <- real_time |>
  dplyr::select(-rh_sensor) |>
  tidyr::pivot_longer(-c('Cidade','date'),
                      values_to = "concentration",
                      names_to = "pollutant") |>
  dplyr::mutate(
    limite = ifelse(pollutant == 'SO2' & concentration >= 40, 1,
                    ifelse(pollutant == 'NO2' & concentration >= 25, 1,
                           ifelse(pollutant == 'PM10' & concentration >= 45, 1,
                                  ifelse(pollutant == 'PM2.5' & concentration >= 15, 1,
                                         ifelse(pollutant == 'CO' & concentration >= 4, 1, 0)))))) |>
  dplyr::mutate(day = as.Date(date, tz = "America/Sao_Paulo")) |>
  subset(day == hoje & !is.na(concentration)) |>
  dplyr::group_by(Cidade, day, pollutant) |>
  dplyr::slice_max(order_by = date, n = 1) |>
  unique()


################# NOTICE: IQA last 24 h #################
# THERMO IQA last 24h
load(file = "./data/air_quality_data.Rda")

thermo_iqa <- air_quality_data |>
  dplyr::mutate(sensor = "Thermo GM-5000")

# PURPLEAIR IQA last 24h
load(file = "./data/data_purpleair.Rda")



# para valores de concentracao, preferência é dada ao registrado pelos GM

if (hoje %in% data_thermo_instantaneo$date[data_thermo_instantaneo$Cidade == "Rio Branco do Sul"]) {

  purpleair_iqa <- data_purpleair |>
    dplyr::mutate(sensor = "PurpleAir",
                  AQI = AQI_PM2.5)  |>
    subset(Cidade != "Rio Branco do Sul") #& Cidade != "Almirante Tamandaré")

} else {

  purpleair_iqa <- data_purpleair |>
    dplyr::mutate(sensor = "PurpleAir",
                  AQI = AQI_PM2.5)
}


if (hoje %in% data_thermo_instantaneo$date[data_thermo_instantaneo$Cidade == "Almirante Tamandaré"]) {

  purpleair_iqa <- data_purpleair |>
    dplyr::mutate(sensor = "PurpleAir",
                  AQI = AQI_PM2.5)  |>
    subset(Cidade != "Almirante Tamandaré")

}



# unificando dados sensores
IQA_last24H <- dplyr::bind_rows(thermo_iqa, purpleair_iqa)


IQA_last24H <- IQA_last24H |>
  dplyr::mutate(date = lubridate::force_tz(sample_day, tz = "America/Sao_Paulo")) |>
  subset(date == hoje) |>
  dplyr::select(Cidade, AQI, AQI_Qualidade) |>
  dplyr::group_by(Cidade) |>
  dplyr::slice_max(order_by = AQI, n = 1) |> #selecionar valor máximo das últimas 24h
  unique()

# Simulated value (replace with real data)
threshold <- 1

#if (length(unique(alerta$pollutant)) > 1) {

# out <- vector("list", length(unique(alerta$pollutant))) # vetor com o correspondente numero de variaveis meteorologicas

#} else {

# out <- vector("list",  1) # vetor com o correspondente numero de variaveis meteorologicas

#}

IQA_last24H <- dplyr::filter(IQA_last24H, !is.na(Cidade))

Cidades <- IQA_last24H$Cidade |> # realocado aqui
  unique() |>
  sort()


print(dim(IQA_last24H))  # <-- Isso imprimirá (número de linhas, número de colunas)

out <- vector("list", nrow(IQA_last24H))  # Initialize the list

for (i in 1:nrow(IQA_last24H)) {

  for (j in 1:length(unique(alerta$pollutant))) {

    City <- Cidades[i]
    print(paste("City:", City))  # Debugging
    print(paste("Iteração", i, "- Cidade:", City))

    if ((City != "Rio Branco do Sul" | !(as.Date(lubridate::force_tz(hoje, tz = "America/Sao_Paulo")) %in% as.Date(lubridate::force_tz(data_thermo_instantaneo$date, tz = "America/Sao_Paulo"))[data_thermo_instantaneo$Cidade == "Rio Branco do Sul"])) & # quando RBS, FALSE
        (City != "Almirante Tamandaré" | !(as.Date(lubridate::force_tz(hoje, tz = "America/Sao_Paulo")) %in% as.Date(lubridate::force_tz(data_thermo_instantaneo$date, tz = "America/Sao_Paulo"))[data_thermo_instantaneo$Cidade == "Almirante Tamandaré"]))) {

      subset_alerta <- alerta |> subset(Cidade == City & pollutant == "PM2.5") |>
        dplyr::mutate(concentration = round(concentration, 1))

      if (nrow(subset_alerta) == 0) next  # Skip if empty

      if (is.null(out[[i]])) out[[i]] <- list()  # Ensure the list exists

      excede <- ifelse(rlang::is_empty(subset_alerta$limite), "não medido", subset_alerta$limite)
      particulas <- 'MP2.5'
      valor <- subset_alerta$concentration
      registro <- unique(lubridate::ymd_hms(subset_alerta$date)) |>
        lubridate::force_tz("America/Sao_Paulo") |>
        format(format = "%d/%m/%Y %H:%Mh")
      unidade <- " µg/m³"
      iqa <- IQA_last24H$AQI[IQA_last24H$Cidade == subset_alerta$Cidade]
      qualidade <- IQA_last24H$AQI_Qualidade[IQA_last24H$Cidade == subset_alerta$Cidade] |>
        stringr::str_to_upper()


      out[[i]]$Cidade <- subset_alerta$Cidade
      current_value <- ifelse(excede >= 1, 1, 0)

      message <- if (current_value >= threshold) {
        paste0(
          "\n🌬 Poluente: \n   ", particulas, ": ", valor, unidade,
          "\n🗓 Registro: ", registro,
          "\n🚨 ALERTA: Valor excede o limite recomendado!"
        )
      } else {
        paste0(
          "\n🌬 Poluente: \n   ", particulas, ": ", valor, unidade,
          "\n🗓 Registro: ", registro,
          "\n✅ Valor dentro do limite recomendado.\n"
        )
      }

      out[[i]]$message <- message
      out[[i]]$qualidade <- paste0("\n *Qualidade do Ar* (IQA): \n *", qualidade, "* nas últimas 24h ⚠️")

    } else {

      subset_alerta <- alerta |> subset(Cidade == City) |>
        dplyr::mutate(concentration = round(concentration, 1)) |>
        dplyr::mutate(pollutant = replace(pollutant, pollutant == 'PM2.5', "MP2.5"),
                      pollutant = replace(pollutant, pollutant == 'PM10', "MP10"))

      if (nrow(subset_alerta) == 0) next  # Skip if empty

      if (is.null(out[[i]])) out[[i]] <- list()  # Ensure the list exists

      excede <- sum(subset_alerta$limite, na.rm = T)

      if (j > nrow(subset_alerta)) next  # Avoid out-of-bounds error

      valor <- subset_alerta$concentration[j]
      registro <- lubridate::ymd_hms(subset_alerta$date)[j] |>
        lubridate::force_tz("America/Sao_Paulo") |>
        format(format = "%d/%m/%Y %H:%Mh")

      unidade <- ifelse(subset_alerta$pollutant[j] == "CO", " mg/m³", " µg/m³")
      Cidade <- subset_alerta$Cidade |> unique()
      iqa <- IQA_last24H$AQI[IQA_last24H$Cidade == Cidade]
      qualidade <- IQA_last24H$AQI_Qualidade[IQA_last24H$Cidade == Cidade] |>
        stringr::str_to_upper()

      out[[i]]$Cidade <- Cidade
      current_value <- ifelse(excede >= 1, 1, 0)

      message <- if (current_value >= threshold) {
        paste0(
          "\n Poluente: \n 🌬  ", subset_alerta$pollutant[4], ":  ", subset_alerta$concentration[4], " ", unidade,
          "\n 🌬  ", subset_alerta$pollutant[5], ":  ", subset_alerta$concentration[5], " ", unidade,
          "\n 🌬  ", subset_alerta$pollutant[1], ":  ", subset_alerta$concentration[1], " ", unidade,
          "\n 🌬  ", subset_alerta$pollutant[3], ":  ", subset_alerta$concentration[3], " ", unidade,
          "\n 🌬  ", subset_alerta$pollutant[6], ":  ", subset_alerta$concentration[6], " ", unidade,
          "\n 🌬  ", subset_alerta$pollutant[2], ":  ", subset_alerta$concentration[2], " ", unidade,
          "\n🗓 Registro: ", registro,
          "\n🚨 ALERTA: Valor(es) excede(m) o limite recomendado!\n"
        )
      } else {
        paste0(
          "\n Poluente: \n 🌬  ", subset_alerta$pollutant[4], ":  ", subset_alerta$concentration[4], " ", unidade,
          "\n 🌬  ", subset_alerta$pollutant[5], ":  ", subset_alerta$concentration[5], " ", unidade,
          "\n 🌬  ", subset_alerta$pollutant[1], ":  ", subset_alerta$concentration[1], " ", unidade,
          "\n 🌬  ", subset_alerta$pollutant[3], ":  ", subset_alerta$concentration[3], " ", unidade,
          "\n 🌬  ", subset_alerta$pollutant[6], ":  ", subset_alerta$concentration[6], " ", unidade,
          "\n 🌬  ", subset_alerta$pollutant[2], ":  ", subset_alerta$concentration[2], " ", unidade,
          "\n🗓 Registro: ", registro,
          "\n✅ Valor dentro do limite recomendado.\n"
        )
      }

      out[[i]]$message <- message
      out[[i]]$qualidade <- paste0("\n *Qualidade do Ar* (IQA): \n *", qualidade, "* nas últimas 24h ⚠️")

    }
  }

#  out[[i]]$qualidade <- paste0("\n *Qualidade do Ar* (IQA): \n *", qualidade, "* nas últimas 24h ⚠️")
}

# Unificando o dataset

output <- dplyr::bind_rows(out)

