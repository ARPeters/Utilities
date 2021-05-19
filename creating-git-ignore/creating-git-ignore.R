# Following instructions for creating a gitignore file here:
# https://docs.ropensci.org/gitignore/

# library(devtools)
# devtools::install_github("ropensci/gitignore")

library(gitignore)


head(gi_available_templates(), 25)
length(gi_available_templates())

gi_fetch_templates("R")

git_ignore_text <- gi_fetch_templates("R")
gi_write_gitignore(git_ignore_text, gitignore_file = here::here(".gitignore"))


# Adding this line of text to gitigore file to identify data-unshared folder (without the quotes)
# "# ---- Protected Information -----------------------------------------------"
# "/data-unshared/*"


# ---- testing
ds_test <- tibble::as_tibble(head(mtcars))


path_out_csv <- "./data-unshared/test_1.csv"
path_out_rds <- "./data-unshared/test_1.rds"

readr::write_csv(ds_test, path = path_out_csv)
readr::write_rds(ds_test, path = path_out_rds)

