source(here::here("config", "setup.R"))
trees_path <- file.path(dir_raw, "trees.csv")
traits_path <- file.path(dir_raw, "traits.xlsx")

trees <- read.csv(trees_path)
names(trees)
#unique(trees$sp_name)












