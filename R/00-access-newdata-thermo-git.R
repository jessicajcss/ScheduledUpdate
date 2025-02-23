# GHA YAML: https://docs.github.com/en/actions/writing-workflows/workflow-syntax-for-github-actions
# https://beamilz.com/posts/series-gha/2022-series-gha-2-creating-your-first-action/en/

#usethis::use_github_action("check-release")
#install.packages("pak", repos = "https://r-lib.github.io/p/pak/dev/")

print("Hi! Welcome to a GH Actions with R - to create a full datafile each time a new insitu dataset is uploaded in the folder :)")

##################################################################
################## GM GITHUB REPOSITORY DATA #####################
##################################################################

# get information about the repositories on the Quarto organizations.

#thermo_git <- c("GM-RioBranco")

#thermo_repos_raw <-
  #purrr::map(thermo_git, ~ gh::gh(
    #"GET /repos/jessicajcss/Dados_GM_UFPR/git/trees/main?recursive=1/",
    #.token = Sys.getenv("GITHUB_PAT"),
   # .accept = "application/vnd.github.v3.raw")
 # )

#thermo_repos_raw <-
 # purrr::map(thermo_git, ~ gh::gh("GET /repos/{owner}/{repo}/git/trees/{branch}?recursive=1",
  #     owner = "jessicajcss",
   #    repo = "Dados_GM_UFPR",
    #   branch = "main",
     #  .token = Sys.getenv("GITHUB_PAT")
#))




#Extracting the path variable from the output of above request
thermo_repos_raw <- httr::GET("https://api.github.com/repos/jessicajcss/Dados_GM_UFPR/git/trees/main?recursive=1/",
                              httr::authenticate(Sys.getenv("GITHUB_PAT"), ""),
                              Accept = "application/vnd.github.v3.raw")

# transform into a tibble with few cols
#thermo_repos <- thermo_repos_raw[[1]]$tree |>
  ##purrr::flatten() |>
 #purrr::map(unlist, recursive = TRUE)  |>
 #purrr::map_dfr(tibble::enframe, .id = "id_repo") |>
 #tidyr::pivot_wider() |>
 #dplyr::filter(stringr::str_detect(path,'.lsi')) |>
 #tidyr::separate(path, c('folder','filename'),'/')

thermo_repos <- httr::content(thermo_repos_raw)$tree |>
  #purrr::flatten() |>
  purrr::map(unlist, recursive = TRUE)  |>
  purrr::map_dfr(tibble::enframe, .id = "id_repo") |>
  purrr::set_names(
    c("id_repo", "section", "value")
  ) |>
  as.data.frame() |>
  tidyr::pivot_wider(names_from = section, values_from = value) |>
  #reshape2::dcast(... ~section) |>
  dplyr::filter(stringr::str_detect(path,'.lsi')) |>
  tidyr::separate(path, c('folder','filename'),'/')



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


