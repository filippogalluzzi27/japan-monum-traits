source(here::here("config", "setup.R"))
jap_path <- file.path(dir_raw, "jap_trees.csv")
jap <- read.csv(jap_path)
jap$経度 # longitudine

# georeferenziazione 
jap <- jap %>%
  mutate(
    coord_status = case_when(
      is.na(`経度`) | is.na(`緯度`) ~ "assente_NA",
      `経度` == 0 & `緯度` == 0 ~ "assente_zero",
      `経度` == round(`経度`, 2) & `緯度` == round(`緯度`, 2) ~ "grossolana",
      TRUE ~ "precisa"))
table(jap$coord_status)
n_utilizzabili <- sum(jap$coord_status == "precisa"); n_utilizzabili


# Creazione dataset lat-lon per GEE
# non sono tutti gli alberi di Nakadai!
jap <- jap %>% mutate(id = row_number())   # chiave per il join successivo
gee <- jap %>%
  filter(coord_status == "precisa") %>%
  transmute(id,
            longitude = `経度`,
            latitude  = `緯度`)

summary(gee$longitude); summary(gee$latitude)   # controllo degli intervalli
write.csv(gee, file.path(dir_raw, "jap_trees_gee.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
