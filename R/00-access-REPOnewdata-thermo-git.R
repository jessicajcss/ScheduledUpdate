# Define repositories to fetch (replace with your list of repositories)
quarto_orgs <- c("Dados_GM_UFPR")  # Replace with actual repo names
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
thermo_repos_raw <- purrr::map(quarto_orgs, function(repo) {
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

thermo_repos0 <- purrr::map(thermo_repos_raw, ~ .x$tree)

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


