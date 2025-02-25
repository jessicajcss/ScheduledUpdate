# Define repositories to fetch (replace with your list of repositories)
thermo_git <- c("Dados_GM_UFPR")  # Replace with actual repo names

# Function to fetch the trees for each repository
#thermo_git <- c("GM-RioBranco")

github_pat <- Sys.getenv("GITHUB_PAT")


thermo_repos_raw <- purrr::map(thermo_git, ~ gh::gh(
    endpoint = "GET /repos/{owner}/{repo}/git/trees/{tree_sha}?recursive=1/",
    owner = "jessicajcss",  # Replace with the owner of the repository
    repo = "Dados_GM_UFPR",
    tree_sha = "main",  # Replace with the tree SHA you want to retrieve, e.g., 'main' or a specific SHA
    .token = GITHUB_PAT,  # Use the GitHub PAT from the environment
    .accept = "application/vnd.github.v3.raw"
  ))


print(thermo_repos_raw)

thermo_repos0 <- thermo_repos_raw[[1]]$tree



# Print the result
print(thermo_repos0)

thermo_repos1 <- thermo_repos0 |>
  purrr::map(unlist, recursive = TRUE) |>
  purrr::map_dfr(function(x) {
    tibble::enframe(x, name = "name", value = "value")
  }, .id = "id_repo") |>
  as.data.frame()

print(thermo_repos1)

thermo_repos <- data.frame('id_repo' = thermo_repos1$id_repo,
                           'name' = thermo_repos1$name,
                           'value' = thermo_repos1$value)

print(thermo_repos)
class(thermo_repos)

readr::write_csv(thermo_repos, "./data_raw/thermo_repos.csv")
