source(here::here("config", "setup.R"))

# Definisco i percorsi dei file e dell'output
trees_path <- file.path(dir_raw, "trees.csv")  # dataset Nakadai
traits_path <- file.path(dir_raw, "species_mean_traits.xlsx") # dataset TRY per tratti
trees_traits_path <- file.path(dir_raw, "trees_traits.csv") # file di output del dataset completo

# apro il dataset di Nakadai
trees <- read.csv(trees_path)
names(trees)
# apro il dataset dei tratti
excel_sheets(traits_path)
traits <- read_excel(traits_path, sheet = 1, col_names = TRUE)

# Controlli vari su traits
dim(traits)
head(traits, 10); tail(traits, 10)  
names(traits)
righe_vuote <- rowSums(!is.na(traits)) == 0; sum(righe_vuote)
colonne_vuote <- colSums(!is.na(traits)) == 0; sum(colonne_vuote)

# Controllo nomi specie dei due dataset
head(unique(trees$sp_name), 10)
head(unique(traits$`Species name standardized against TPL`), 10)



# ============================================================================
# Modifica dei nomi per corrispondenza tra i datasets
# ============================================================================
# Aggiungo "_" tra genere e specie nel daatset "traits" per corrispondenza con Nakadai
traits_sp <- traits %>%
  mutate(sp_name = gsub(" ", "_", trimws(`Species name standardized against TPL`)))
head(traits_sp$sp_name, 10)

# Controllo corrispondenze
trees <- trees %>%
  mutate(sp_name = trimws(sp_name))
match_vec <- unique(trees$sp_name) %in% traits_sp$sp_name
table(match_vec)  
# Ci sono 53 specie che non corrispondono

# elenco delle specie senza corrispondenza
unique(trees$sp_name)[!match_vec]


#-----------------------------------------------------------------------------
# Mancate corrispondenze dovute a (?):
  # Platanus_X_acerifolia => ibridi
  # "Cerasus itosakura" invece di "Prunus itosakura" => sinonimi
  # specie endemiche non presenti in TRY
#-----------------------------------------------------------------------------


# Correzione dei sinonimi
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
  "Tsuga_densifolia"   = "Tsuga_diversifolia",
  "Quercus_crispula"   = "Quercus_mongolica",  
  "Gamblea_innovans"   = "Evodiopanax_innovans") 
sinonimi[!(sinonimi %in% traits_sp$sp_name)]
# alcune specie non sono presenti nel dataset TRY

# Aggiunta dei sinonimi validi al dataset
trees <- trees %>%
  mutate(
    sp_name = ifelse(sp_name %in% names(sinonimi),
                          sinonimi[sp_name],
                          sp_name))
match_vec2 <- unique(trees$sp_name) %in% traits_sp$sp_name
table(match_vec2)
unique(trees$sp_name)[!match_vec2]
# rimangono 45 specie senza corrispondenza

# elenco delle specie rimanenti non ibride
non_hyb <- unique(trees$sp_name)[!match_vec2]
non_hyb[!grepl("_X_", non_hyb)]
# 8 sono ibridi, quindi non presenti in TRY

# rimuovo le 45 specie senza corrispondenze
esclusi <- trees %>% filter(!sp_name %in% traits_sp$sp_name)

# specie, alberi e individui sacri esclusi
bind_rows(
  totale  = trees   %>% summarise(specie = n_distinct(sp_name), alberi = n(), sacri = sum(religion)),
  esclusi = esclusi %>% summarise(specie = n_distinct(sp_name), alberi = n(), sacri = sum(religion)),
  .id = "gruppo")
# tabella riassuntiva
esclusi %>%
  count(sp_name, name = "n_alberi", sort = TRUE)
trees <- trees %>% filter(sp_name %in% traits_sp$sp_name)


# ============================================================================
# Unione delle variabili tratti per sp_name
# ============================================================================
sum(duplicated(traits_sp$sp_name))
df <- trees %>%
  left_join(traits_sp, by = c("sp_name" = "sp_name"))

# Seleziono solo variabili che mi interessano
df <- select(df,
             sp_name,
             latitude,
             elevation,
             annual_mean_temp,
             annual_prec,
             Trunk_circumference,
             age_rank,
             unique_name,
             religion,
             `Leaf area (mm2)`,
             `Nmass (mg/g)`,
             `LMA (g/m2)`,
             `Plant height (m)`,
             `Diaspore mass (mg)`,
             `SSD observed (mg/mm3)`,
             `SSD imputed (mg/mm3)`
)
df <- rename(df, 
       leaf_area=`Leaf area (mm2)`,
       n_mass=`Nmass (mg/g)`,
       lma=`LMA (g/m2)`,
       plant_height=`Plant height (m)`,
       diaspore_mass=`Diaspore mass (mg)`,
       ssd_observed=`SSD observed (mg/mm3)`,
       ssd_imputed=`SSD imputed (mg/mm3)`)

# sostituisco i valori NA di ssd_observed con ssd_imputed
df$ssd_observed <- ifelse(is.na(df$ssd_observed), df$ssd_imputed, df$ssd_observed)
df$ssd_imputed <- NULL

       
names(df)
dim(df)

# grafico rango-abbondanza
n_sp <- sort(table(df$sp_name), decreasing = TRUE)
plot(as.vector(n_sp), log = "y", pch = 16, cex = 0.6,
     xlab = "Specie",
     ylab = "# alberi (log)")
text(1:3, as.vector(n_sp)[1:7], names(n_sp)[1:7], pos = 4, cex = 0.6)




# ============================================================================
# Controlli sulla copertura dei tratti per Nakadai
# ============================================================================
tratti <- c("plant_height", "ssd_observed", "leaf_area", "lma", "n_mass", "diaspore_mass")
sp <- df %>%
  distinct(sp_name, across(all_of(tratti)))
stopifnot(!any(duplicated(sp$sp_name)))   # una riga per specie

sp <- sp %>%
  mutate(n_tratti = rowSums(!is.na(across(all_of(tratti)))),
         completa = n_tratti == 6)

# quante specie hanno X tratti
table(sp$n_tratti)       
# quante specie hanno ciascun tratto
sp %>% summarise(across(all_of(tratti), ~ sum(!is.na(.x))))
# quante specie resterebbero con una PCA ridotta a 4 tratti (senza LA e SM)
sp %>% summarise(ridotta_4 = sum(complete.cases(plant_height, ssd_observed, lma, n_mass)))


# Quanti alberi sacri coprono le specie con 6 e 4 tratti
per_sp <- df %>%
  group_by(sp_name) %>%
  summarise(n_alberi = n(),
            n_sacri  = sum(religion),
            n_nome   = sum(unique_name),
            across(all_of(tratti), first),       # i tratti sono costanti nella specie
            .groups = "drop") %>%
  mutate(p_sacro   = n_sacri / n_alberi,
         p_nome    = n_nome / n_alberi,
         n_tratti  = rowSums(!is.na(across(all_of(tratti)))),
         ridotta_4 = complete.cases(plant_height, ssd_observed, lma, n_mass),
         ha_lma    = !is.na(lma))

per_sp %>%
  summarise(sacri_tot = sum(n_sacri),
            quota_6   = sum(n_sacri[n_tratti == 6]) / sacri_tot,
            quota_4   = sum(n_sacri[ridotta_4])      / sacri_tot)

# specie con molti alberi sacri ma senza i 4 tratti
nomi_diaz <- gsub(" ", "_", trimws(traits$`Species name standardized against TPL`))
tab <- per_sp %>%
  filter(!ridotta_4) %>%
  arrange(desc(n_sacri)) %>%
  mutate(in_diaz = ifelse(sp_name %in% nomi_diaz, "sì", "no"),
         mancano = apply(is.na(across(all_of(tratti))), 1,
                         function(r) paste(tratti[r], collapse = ", "))) %>%
  select(sp_name, n_alberi, n_sacri, in_diaz, mancano)
writexl::write_xlsx(tab, file.path(dir_outputs, "specie_tratti_mancanti.xlsx"))


# ============================================================================
# Salvataggio
# ============================================================================
# Il dataset comprende tutti gli individui di Nakadai;
# per ogni individuo, in base alla specie a cui appartiene, ho aggiunto i tratti
# presi dal dataset TRY
# 45 specie di Nakadai non sono presenti in TRY, quindi tutti i valori dei tratti saranno NA
# Inoltre, non tutte le 191 specie di Nakadai presenti in TRY hanno tutti e 6 i tratti completi
write.csv(df, trees_traits_path, row.names=FALSE)





