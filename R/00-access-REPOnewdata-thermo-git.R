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
# Set your GitHub Personal Access Token (GitPAT) as an environment variable (replace with your token)
# Define repositories to fetch (replace with your list of repositories)
quarto_orgs <- c("Dados_GM_UFPR")  # Replace with actual repo names

# Function to fetch the trees for each repository
get_repo_trees <- function(repo_name) {
  # GitHub API endpoint to get the repository's tree
  gh::gh(
    endpoint = "GET /repos/{owner}/{repo}/git/trees/{tree_sha}",
    owner = "jessicajcss",  # Replace with the owner of the repository
    repo = repo_name,
    tree_sha = "main",  # Replace with the tree SHA you want to retrieve, e.g., 'main' or a specific SHA
    .token = Sys.getenv("GITHUB_PAT"),  # Use the GitHub PAT from the environment
    headers = list('X-GitHub-Api-Version' = '2022-11-28')
  )
}

# Iterate over all repositories and fetch the trees
thermo_repos_raw <- purrr::map(quarto_orgs, get_repo_trees)

# Print the result
print(thermo_repos_raw)


thermo_repos0 <- httr::content(thermo_repos_raw)$tree


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
