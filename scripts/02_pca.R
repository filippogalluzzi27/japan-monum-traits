source(here::here("config", "setup.R"))
df     <- read.csv(file.path(dir_raw, "trees_traits.csv"))
traits <- read_excel(file.path(dir_raw, "species_mean_traits.xlsx"), sheet = 1)
trees_traits_path <- file.path(dir_raw, "trees_traits_pca.csv") # file di output del dataset completo

# ============================================================================
# PCA globale su 4 e 6 tratti
# ============================================================================
# 6 (e 4) tratti essenziali
sel <- c(LA    = "Leaf area (mm2)",
         Nmass = "Nmass (mg/g)",
         LMA   = "LMA (g/m2)",
         H     = "Plant height (m)",
         SM    = "Diaspore mass (mg)",
         SSD   = "SSD combined (mg/mm3)")   # osservata + stimata da LDMC, come Díaz
tr6 <- names(sel)
tr4 <- c("H", "SSD", "LMA", "Nmass")

# global = dataset globale con soli tratti di interesse
global <- as.data.frame(traits[, c("Species name standardized against TPL", "Woodiness", sel)])
names(global) <- c("specie", "legnosa", tr6)
global$sp_name <- gsub(" ", "_", trimws(global$specie))

# log10 dei tratti; valori nulli o negativi -> NA
global[tr6] <- lapply(global[tr6], function(x) ifelse(x > 0, log10(x), NA))

# orientare le PCs: H positiva su PC1, Nmass positiva su PC2
orienta <- function(p) {
  if (p$rotation["H", 1] < 0)     { p$rotation[, 1] <- -p$rotation[, 1]; p$x[, 1] <- -p$x[, 1] }
  if (p$rotation["Nmass", 2] < 0) { p$rotation[, 2] <- -p$rotation[, 2]; p$x[, 2] <- -p$x[, 2] }
  p
}

# replica PCA di Diaz per controllo
g6 <- global[complete.cases(global[tr6]), ]; nrow(g6)
pca6 <- orienta(prcomp(g6[tr6], center = TRUE, scale. = TRUE))
round(100 * pca6$sdev^2 / sum(pca6$sdev^2), 1)   # Diaz => 49 e 25
round(pca6$rotation[, 1:2], 2)

# PCA solo sui 4 tratti
pca4 <- orienta(prcomp(g6[tr4], center = TRUE, scale. = TRUE))
round(100 * pca4$sdev^2 / sum(pca4$sdev^2), 1)
round(pca4$rotation[, 1:2], 2)

# confronto delle due PCs per le due PCA
cor(pca6$x[, 1:2], pca4$x[, 1:2])
# solo sulle legnose
leg <- g6$legnosa == "woody"; sum(leg)
cor(pca6$x[leg, 1:2], pca4$x[leg, 1:2]) 

# proietta le specie con i 4 tratti completi sugli assi di PCA4
g4  <- global[complete.cases(global[tr4]), ]
sc4 <- predict(pca4, newdata = g4[tr4])
# tabella con le coordinate su PCA6 e PCA4 per ogni specie di Diaz
coord <- full_join(
  data.frame(sp_name = g6$sp_name, PC1_6 = pca6$x[, 1], PC2_6 = pca6$x[, 2]),
  data.frame(sp_name = g4$sp_name, PC1_4 = sc4[, 1],   PC2_4 = sc4[, 2]),
  by = "sp_name")


# aggiungo al dataset di Nakadai le cooridnate delle due PCAs
# Una riga per specie di Nakadai, con le coordinate PCA
# NA dove le specie non fanno parte del dataset dei tratti
per_sp <- df %>%
  group_by(sp_name) %>%
  summarise(n_alberi = n(),
            n_sacri  = sum(religion),
            .groups  = "drop") %>%
  mutate(p_sacro = n_sacri / n_alberi) %>%
  left_join(coord, by = "sp_name")

# tabella riassuntiva
per_sp %>%
  summarise(specie   = n(),
            specie_6 = sum(!is.na(PC1_6)), sacri_6 = sum(n_sacri[!is.na(PC1_6)]) / sum(n_sacri),
            specie_4 = sum(!is.na(PC1_4)), sacri_4 = sum(n_sacri[!is.na(PC1_4)]) / sum(n_sacri))

# aggiungo a ogni albero di Nakadai le coordinate della sua specie
n_prima <- nrow(df)
df <- df %>% left_join(coord, by = "sp_name")
stopifnot(nrow(df) == n_prima)   # controlla che l'unione non abbia duplicato righe
write.csv(df, trees_traits_path, row.names=FALSE)




