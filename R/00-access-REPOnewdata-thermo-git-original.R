# Define repositories to fetch (replace with your list of repositories)
quarto_orgs <- c("Dados_GM_UFPR")  # Replace with actual repo names

# Function to fetch the trees for each repository
get_repo_trees <- function(repo_name, owner) {
  # GitHub API endpoint to get the repository's tree
  cat("Fetching tree for repo:", repo_name, "\n")
  response <- httr::GET(
    paste0("https://api.github.com/repos/", owner, "/", repo_name, "/git/trees/main?recursive=1"),
    httr::authenticate(Sys.getenv("GITHUB_PAT"), ""),
    httr::add_headers(Accept = "application/vnd.github.v3.raw")
  )
  cat("Response received for repo:", repo_name, "\n")
  return(response)
}

# Iterate over all repositories and fetch the trees
thermo_repos_raw <- purrr::map(quarto_orgs, function(repo) {
  tryCatch(
    {
      response <- get_repo_trees(repo, "jessicajcss")  # Replace "jessicajcss" with the correct owner if different
      cat("Successfully fetched tree for repo:", repo, "\n")
      return(response)
    },
    error = function(e) {
      cat("Error fetching tree for repo:", repo, "\n", "Error message:", e$message, "\n")
      return(NULL)
    }
  )
})

# Check if any responses were NULL
cat("Checking for NULL responses...\n")
if (any(sapply(thermo_repos_raw, is.null))) {
  cat("Some responses were NULL\n")
} else {
  cat("No NULL responses\n")
}

# Extract the tree elements
thermo_repos0 <- purrr::map(thermo_repos_raw, function(response) {
  if (!is.null(response)) {
    httr::content(response)$tree
  } else {
    NULL
  }
})

# Check for NULL trees
cat("Checking for NULL trees...\n")
if (any(sapply(thermo_repos0, is.null))) {
  cat("Some trees were NULL\n")
} else {
  cat("No NULL trees\n")
}

# Print the extracted trees
print(thermo_repos0)

# Further processing
thermo_repos1 <- thermo_repos0 |>
  purrr::map(unlist, recursive = TRUE) |>
  purrr::map_dfr(function(x) {
    tibble::enframe(x, name = "name", value = "value")
  }, .id = "id_repo") |>
  as.data.frame()

# Print the processed data
print(thermo_repos1)

thermo_repos <- data.frame('id_repo' = thermo_repos1$id_repo,
                           'name' = thermo_repos1$name,
                           'value' = thermo_repos1$value)

# Print the final data
print(thermo_repos)
cat("Class of thermo_repos:", class(thermo_repos), "\n")

# Write to CSV
readr::write_csv(thermo_repos, "./data_raw/thermo_repos.csv")
cat("CSV file written to ./data_raw/thermo_repos.csv\n")
