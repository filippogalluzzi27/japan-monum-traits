source(here::here("config", "setup.R"))
trees_path <- file.path(dir_raw, "trees.csv")
traits_path <- file.path(dir_raw, "species_mean_traits.xlsx")
# file di output del dataset completo
trees_traits_path <- file.path(dir_raw, "trees_traits.csv")

# Dataset trees
trees <- read.csv(trees_path)
names(trees)

# Dataset tratti
excel_sheets(traits_path)
traits <- read_excel(traits_path, sheet = 1, col_names = TRUE)

# Controlli traits
dim(traits)
head(traits, 10); tail(traits, 10)  
names(traits)
righe_vuote <- rowSums(!is.na(traits)) == 0; sum(righe_vuote)
colonne_vuote <- colSums(!is.na(traits)) == 0; sum(colonne_vuote)

# Controllo nomi specie
head(unique(trees$sp_name), 10)
head(unique(traits$`Species name standardized against TPL`), 10)



# ============================================================================
# Modifica dei nomi per corrispondenza tra i datasets
# ============================================================================
# Aggiungere "_" tra genere e specie in "traits"
traits_sp <- traits %>%
  mutate(sp_name = gsub(" ", "_", trimws(`Species name standardized against TPL`)))
head(traits_sp$sp_name, 10)

# Controllo corrispondenze
trees <- trees %>%
  mutate(sp_name = trimws(sp_name))
match_vec <- unique(trees$sp_name) %in% traits_sp$sp_name
table(match_vec)
sum(match_vec)
# specie senza corrispondenza
unique(trees$sp_name)[!match_vec]

#-----------------------------------------------------------------------------
# Platanus_X_acerifolia => ibridi?
# "Cerasus itosakura" invece di "Prunus itosakura" => sinonomi
# Tsuga_densifolia invece di Tsuga diversifolia => errore di battitura ?
# specie endemiche
#-----------------------------------------------------------------------------
# Correzione di sinonimi
# sinonimo in trees -> accettato in traits
sinonimi <- c(
  "Cerasus_itosakura"  = "Prunus_itosakura",
  "Cerasus_yedoensis"  = "Prunus_yedoensis",
  "Cerasus_jamasakura" = "Prunus_jamasakura",
  "Cerasus_sargentii"  = "Prunus_sargentii",
  "Cerasus_speciosa"   = "Prunus_speciosa",
  "Morella_rubra"      = "Myrica_rubra",
  "Thuja_orientalis"   = "Platycladus_orientalis",
  "Aria_alnifolia"     = "Sorbus_alnifolia",
  "Eucalyptus_globula" = "Eucalyptus_globulus",
  "Tsuga_densifolia"   = "Tsuga_diversifolia")
sinonimi[!(sinonimi %in% traits_sp$sp_name)]

# Aggiunta dei sinonimi validi al dataset
trees <- trees %>%
  mutate(
    sp_name = ifelse(sp_name %in% names(sinonimi),
                          sinonimi[sp_name],
                          sp_name))
match_vec2 <- unique(trees$sp_name) %in% traits_sp$sp_name
table(match_vec2)
sum(match_vec2)
# rimanente senza corrispondenza
unique(trees$sp_name)[!match_vec2]


# ============================================================================
# Unione delle variabili tratti per sp_name
# ============================================================================
sum(duplicated(traits_sp$sp_name))
df <- trees %>%
  left_join(traits_sp, by = c("sp_name" = "sp_name"))

dim(df)      # il numero di righe deve restare identico a trees_sp
names(df)    # verifica che le colonne di traits siano state aggiunte in coda
write.csv(df, trees_traits_path, row.names=FALSE)



