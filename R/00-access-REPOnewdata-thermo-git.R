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

print(thermo_repos_raw)

thermo_repos0 <- httr::content(thermo_repos_raw)$tree

print(thermo_repos0)

thermo_repos <- thermo_repos0 |>
  purrr::map(unlist, recursive = TRUE) |>
  purrr::map_dfr(function(x) {
    tibble::enframe(x, name = "name", value = "value")
  }, .id = "id_repo") |>
  as.data.frame()

print(thermo_repos)

readr::write_csv(thermo_repos, "./data_raw/thermo_repos.csv")
#thermo_repos <- readr::read_csv("./data_raw/thermo_repos.csv")
