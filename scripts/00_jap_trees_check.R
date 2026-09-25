source(here::here("config", "setup.R"))
jap_path <- file.path(dir_raw, "jap_trees.csv")
jap <- read.csv(jap_path)
jap$経度 # longitudine
a <- (jap$経度)

# Quanti hanno georeferenziazione utilizzabile?
jap <- jap %>%
  mutate(
    coord_status = case_when(
      is.na(`経度`) | is.na(`緯度`) ~ "assente_NA",
      `経度` == 0 & `緯度` == 0 ~ "assente_zero",
      `経度` == round(`経度`, 0) & `緯度` == round(`緯度`, 0) ~ "grossolana",
      TRUE ~ "precisa"))
table(jap$coord_status)
n_utilizzabili <- sum(jap$coord_status == "precisa"); n_utilizzabili


