# Define repositories to fetch (replace with your list of repositories)
thermo_orgs <- c("Dados_GM_UFPR")  # Replace with actual repo names
#gh::gh_whoami()

github_pat <- Sys.getenv("GITHUB_PAT")

# Function to fetch the trees for each repository
get_repo_trees <- function(repo_name) {
  # GitHub API endpoint to get the repository's tree
  cat("Fetching tree for repo:", repo_name, "\n")
  response <- gh::gh(
    "GET /repos/{owner}/{repo}/git/trees/{tree_sha}",
    owner = "jessicajcss",  # Replace with the owner of the repository
    repo = repo_name,
    tree_sha = "main",  # Replace with the tree SHA you want to retrieve, e.g., 'main' or a specific SHA
    .token = github_pat,  # Use the GitHub PAT from the environment
    .accept = "application/vnd.github.v3.raw",
    recursive = F
    )
  cat("Response received for repo:", repo_name, "\n")
  return(response)
}

# Iterate over all repositories and fetch the trees
thermo_repos_raw <- purrr::map(thermo_orgs, function(repo) {
  tryCatch(
    {
      response <- get_repo_trees(repo)
      cat("Successfully fetched tree for repo:", repo, "\n")
      return(response)
    },
    error = function(e) {
      cat("Error fetching tree for repo:", repo, "\n", "Error message:", e$message, "\n")
      return(NULL)
    }
  )
})

# Print the result
print(thermo_repos_raw)


thermo_repos <- thermo_repos_raw[[1]]$tree |>
  #purrr::flatten() |>
  purrr::map(unlist, recursive = TRUE)  |>
  purrr::map_dfr(tibble::enframe, .id = "id_repo") |>
  tidyr::pivot_wider() |>
  dplyr::filter(stringr::str_detect(path,'.lsi')) |>
  tidyr::separate(path, c('folder','filename'),'/')

# Check the resulting data frame

load("./data_raw/file_path.Rda") # data previously downloaded

url <- thermo_repos$url

file_path_new <- url |>
  as.data.frame()


# Updating only data since the last download ---

result <- thermo_repos |>
  dplyr::anti_join(file_path, by = "url")



if (nrow(result) > 0) {
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



# Loading previous full dataset

load(file = "./data/data_thermo_update.Rda")


data_thermo <- rbind(data_thermo, data_git) |>
  unique() |>
  dplyr::arrange(date)

} else {
  load(file = "./data/data_thermo_update.Rda")|>
    unique() |>
    dplyr::arrange(date)

  data_thermo <- data_thermo


  file_path <- file_path
}

#################################################################
########## UNIFICANDO UPDATED BANCO DE DADOS 'BASE' IN SITU & GIT


# saving output dados até [22-02-2025 24h]
save(data_thermo, file="./data/data_thermo_update.Rda")

save(file_path, file="./data_raw/file_path.Rda")


