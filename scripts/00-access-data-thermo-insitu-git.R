# GHA YAML: https://docs.github.com/en/actions/writing-workflows/workflow-syntax-for-github-actions
# https://beamilz.com/posts/series-gha/2022-series-gha-2-creating-your-first-action/en/



print("Hi! Welcome to a GH Actions with R - to create a full datafile each time a new insitu dataset is uploaded in the folder :)")

##################################################################
######################## GM INSITU DATA ##########################
##################################################################

# baixar banco de dados unificado
load("./data_raw/sensores_thermo/20250222_sensores_thermo_unificado.Rda")
data_thermo_insitu <- data_thermo

# adicionar novas planilhas de campo

#Dados em tempo local, mas lidos em UTC
#salvar novos arquivos insitu nessa pasta, para então puxar aqui
temp <- list.files(path = "./data_raw/sensores_thermo",
                   pattern = "*.csv") # listar arquivos .csv do diretório



# aplicar leitura das planilhas contidas na listagem temp
dir <- "./data_raw/sensores_thermo"
temp.qualified <- paste(dir, temp, sep = "/")
myfiles <- lapply(temp.qualified,
                  readr::read_delim,
                  col_select = c(1:21))
colnames(myfiles[[1]]) == colnames(myfiles[[4]])
colnames(myfiles[[4]]) <- colnames(myfiles[[1]])

colnames(myfiles[[1]]) == colnames(myfiles[[5]])
colnames(myfiles[[5]]) <- colnames(myfiles[[1]])

# unificar planilhas de dados
library(tidyverse)
data_thermo_new <- do.call("bind_rows", myfiles)
View(data_thermo_new)
colnames(data_thermo_insitu) <- colnames(data_thermo_new)
data_thermo <- bind_rows(data_thermo_insitu, data_thermo_new[, c(1:21)]) |>
  unique()

data_thermo_insitu <- data_thermo |>
  dplyr::mutate(date = as.Date(localDate, tz = "America/Sao_Paulo"), # Local time
         Cidade = dplyr::recode((instrumentName),
                        "thermo-grid-883f4a344cbe" = "Rio Branco do Sul", #02/08/23 15:50
                        "thermo-grid-f4e11e8e1321" = "Almirante Tamandaré"))  |> #15/08/2023 11:45
  subset(Cidade == "Rio Branco do Sul" | Cidade == "Almirante Tamandaré")  |>
  subset(date >= as.Date("2023-08-02", tz = "America/Sao_Paulo")) |>
  unique() |>
  dplyr::mutate(date = lubridate::ymd_hms(paste0(date,
                                                          " ", localTime),
                                                   tz = "America/Sao_Paulo")) |>
  dplyr::select(Cidade, date, so2, no2, o3, co, pm2p5, pm10,  rh)

colnames(data_thermo_insitu) <- c('Cidade','date','SO2', 'NO2', 'O3', 'CO', 'PM2.5','PM10', 'rh_sensor')



# Saving
save(data_thermo_insitu, file = "./data_raw/wrangled_data_insitu_thermo.Rda")


## see line 190

#################### TO COMPARE WITH WHO, 2021 AQG

# https://www.breeze-technologies.de/blog/air-pollution-how-to-convert-between-mgm3-%C2%B5gm3-ppm-ppb/

library(tidyverse)

data_thermo_converted <- data_thermo_insitu %>%
  mutate(CO = CO*1.15, #from ppm to mg/m³
         O3 = O3*1.96, #from ppb to ug/m³
         NO2 = NO2*1.88, #from ppb to ug/m³
         SO2 = SO2*2.62, #from ppb to ug/m³
         PM2.5 = PM2.5, # ug/m³
         PM10 = PM10) #from ppb to ug/m³




##################################################################
################## GM GITHUB REPOSITORY DATA #####################
##################################################################

# get information about the repositories on the Quarto organizations.

thermo_git <- c("GM-RioBranco")

thermo_repos_raw <-
  purrr::map(thermo_git, ~ gh::gh(
    "GET /repos/jessicajcss/Dados_GM_UFPR/git/trees/main?recursive=1/",
    .token = Sys.getenv("GITHUB_PAT"),
    .accept = "application/vnd.github.v3.raw")
  )


# transform into a tibble with few cols
thermo_repos <- thermo_repos_raw[[1]]$tree |>
  #purrr::flatten() |>
  purrr::map(unlist, recursive = TRUE)  |>
  purrr::map_dfr(tibble::enframe, .id = "id_repo") |>
  tidyr::pivot_wider() |>
  dplyr::filter(stringr::str_detect(path,'.lsi')) |>
  tidyr::separate(path, c('folder','filename'),'/')


# save unique url from files already downloaded to exclude them from the next loop
url <- thermo_repos$url
file_path <- url |>
  as.data.frame()

#Looping and save file to local
##We`ll loop through all the paths and request the data by generating url for each file, converting datatype of all columns to character and appending all files to a consolidated one.

i <- 1
out <- vector("list", nrow(thermo_repos)) # vetor com o correspondente numero de variaveis meteorologicas

for (i in seq(i, nrow(thermo_repos))){
  thermo_repos$filename[i] <- sub(" ", "%20", thermo_repos$filename[i])
  path <- paste0("https://raw.githubusercontent.com/jessicajcss/Dados_GM_UFPR/refs/heads/main/", thermo_repos$folder[i], "/", thermo_repos$filename[i])
  response <- httr::GET(path,
                        httr::authenticate(Sys.getenv("GITHUB_PAT"), ""),
                        Accept = "application/vnd.github.v3.raw")
  file_content <- readLines(textConnection(httr::content(response, as = "text")))
  out[[i]]$Cidade <- thermo_repos$folder[i]
  out[[i]]$value <- file_content[1]
  print(path)
  Sys.sleep(0.000001)
}


# Unificando o dataset

output <- dplyr::bind_rows(out) |>
  dplyr::distinct() # combine the output into a single data frame



# Preparando o banco de dados
data_git_base <- output |>
  tidyr::separate(col = value,
           into = c("id", "date", "no2", "x4",
                    "o3", "x6", "so2", "x8",
                    "rh_sensor", "x10", "co", "x12",
                    "pm2p5", "x14", "pm10", "x16", "x17"),
           sep = ",") |>
  dplyr::mutate(date = as.POSIXct(date,
                                  format = "%m/%d/%Y %I:%M:%S %p",
                                  tz = 'America/Sao_Paulo')) |> #https://www.kaggle.com/discussions/questions-and-answers/382740
  tidyr::drop_na() |>
  dplyr::mutate_if(is.character, as.numeric)

data_git_base$Cidade <- "Rio Branco do Sul"
data_git_base <- data_git_base |>
  dplyr::select(Cidade, date, so2, no2, o3, co, pm2p5, pm10, rh_sensor)

colnames(data_git_base) <- c('Cidade','date','SO2', 'NO2', 'O3', 'CO', 'PM2.5','PM10', 'rh_sensor')


### !!! DO NOT RUN !!! to adapt yet - more cities
data_git_base <- output |>
  tidyr::separate(col = value,
                  into = c("Cidade", "id", "date", "no2", "x4",
                           "o3", "x6", "so2", "x8",
                           "rh", "x10", "co", "x12",
                           "pm2p5", "x14", "pm10", "x16", "x17"),
                  sep = ",") |>
  dplyr::select(Cidade, date, no2, o3, so2, rh, co, pm2p5, pm10) |>
  dplyr::mutate(date = as.POSIXct(date,
                                  format = "%m/%d/%Y %I:%M:%S %p",
                                  tz = 'America/Sao_Paulo')) |> #https://www.kaggle.com/discussions/questions-and-answers/382740
  tidyr::drop_na() |>
  dplyr::mutate_if(is.character, as.numeric) #|>
  dplyr::mutate(Cidade = dplyr::recode((Cidade),
                                     "GM-RioBranco" = "Rio Branco do Sul", #02/08/23 15:50
                                     "GM-AlmiranteTamandare" = "Almirante Tamandaré"))


# saving output dados até [22-02-2025 22h]
save(data_git_base, file="./data_raw/data_git_thermo.Rda")
save(file_path, file="./data_raw/file_path.Rda")



#################################################################
########## UNIFICANDO BANCOS DE DADOS 'BASE' IN SITU & GIT
## use this when repository data is updated
#load(file="./data/data_thermo_update.Rda")
library(tidyverse)
data_thermo <- bind_rows(data_thermo_insitu, data_thermo) |> unique()
View(data_thermo)
save(data_thermo, file="./data_raw/data_thermo.Rda")

save(data_thermo, file="./data/data_thermo.Rda")
