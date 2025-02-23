# GHA YAML: https://docs.github.com/en/actions/writing-workflows/workflow-syntax-for-github-actions
# https://beamilz.com/posts/series-gha/2022-series-gha-2-creating-your-first-action/en/

#usethis::use_github_action("check-release")
#install.packages("pak", repos = "https://r-lib.github.io/p/pak/dev/")

print("Hi! Welcome to a GH Actions with R - to create a full datafile each time a new insitu dataset is uploaded in the folder :)")

##################################################################
################## GM GITHUB REPOSITORY DATA #####################
##################################################################

# get information about the repositories on the Quarto organizations.


#Extracting the path variable from the output of above request
thermo_repos_raw <- httr::GET("https://api.github.com/repos/jessicajcss/Dados_GM_UFPR/git/trees/main?recursive=1/",
                              httr::authenticate(Sys.getenv("GITHUB_PAT"), ""),
                              Accept = "application/vnd.github.v3.raw")


thermo_repos0 <- httr::content(thermo_repos_raw)$tree


thermo_repos <- thermo_repos0 |>
    purrr::map(unlist, recursive = TRUE) |>
    purrr::map_dfr(tibble::enframe, .id = "id_repo") |> # Create the data frame
    as.data.frame()
  # Check the structure of thermo_repos
print(class(thermo_repos))
str(thermo_repos)
head(thermo_repos)
print(colnames(thermo_repos))

#columns_to_select <- paste0("c(", colnames(thermo_repos[1]), ", ",
 #                           colnames(thermo_repos[2]), ", ",
  #                          colnames(thermo_repos[3]), ")")

# Now apply select to the data frame
thermo_repos <- thermo_repos[, c(1:3)] |>
    #dplyr::select(columns_to_select) |>
    tidyr::pivot_wider(
      names_from = paste(colnames(thermo_repos[2])),
      values_from = paste(colnames(thermo_repos[3]))
    ) |>
    subset(stringr::str_detect(path, '.lsi')) |>
    tidyr::separate(path, c('folder', 'filename'), '/')

  # Check the resulting data frame

# Check column names and data after map_dfr
colnames(thermo_repos)
head(thermo_repos)



load("./data_raw/file_path.Rda") # data previously downloaded

url <- thermo_repos$url
file_path_new <- url |>
  as.data.frame()
# Updating only data since the last download ---

result <- thermo_repos |>
  dplyr::anti_join(file_path, by = "url")


# paths to save unique url from files already downloaded to exclude them from the next loop
file_path <- file_path_new

#Looping and save file to local
##We`ll loop through all the paths and request the data by generating url for each file, converting datatype of all columns to character and appending all files to a consolidated one.

i <- 1
out <- vector("list", nrow(result)) # vetor com o correspondente numero de variaveis meteorologicas

for (i in seq(i, nrow(result))){
  result$filename[i] <- sub(" ", "%20", result$filename[i])
  path <- paste0("https://raw.githubusercontent.com/jessicajcss/Dados_GM_UFPR/refs/heads/main/", result$folder[i], "/", result$filename[i])
  response <- httr::GET(path,
                        httr::authenticate(Sys.getenv("GITHUB_PAT"), ""),
                        Accept = "application/vnd.github.v3.raw")
  file_content <- readLines(textConnection(httr::content(response, as = "text")))
  out[[i]]$Cidade <- result$folder[i]
  out[[i]]$value <- file_content[1]
  print(path)
  Sys.sleep(0.000001)
}


# Unificando o dataset

output <- dplyr::bind_rows(out) |>
  dplyr::distinct() # combine the output into a single data frame


# Preparando o banco de dados
data_git <- output |>
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
  dplyr::mutate(across(c(so2, no2, o3, co, pm2p5, pm10, rh_sensor), as.numeric)) |>
  dplyr::select(Cidade, date, so2, no2, o3, co, pm2p5, pm10, rh_sensor) |>
  dplyr::mutate(Cidade = dplyr::recode((Cidade),
                                       "GM-RioBranco" = "Rio Branco do Sul", #02/08/23 15:50
                                       "GM-AlmiranteTamandare" = "Almirante Tamandaré"))

colnames(data_git) <- c('Cidade','date','SO2', 'NO2', 'O3', 'CO', 'PM2.5','PM10', 'rh_sensor')



# Loading previou full dataset

load("./data/data_thermo_update.Rda")

data_thermo <- rbind(data_thermo, data_git) |>
  unique() |>
  dplyr::arrange(date)



#################################################################
########## UNIFICANDO UPDATED BANCO DE DADOS 'BASE' IN SITU & GIT


# saving output dados até [22-02-2025 24h]
save(data_thermo, file="./data/data_thermo_update.Rda")

save(file_path, file="./data_raw/file_path.Rda")


