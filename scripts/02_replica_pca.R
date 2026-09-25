source(here::here("config", "setup.R"))
df_path <- file.path(dir_raw, "trees_traits.csv")
df <- read.csv(df_path)
traits_path <- file.path(dir_raw, "species_mean_traits.xlsx")
traits <- read_excel(traits_path, sheet = 1, col_names = TRUE)

# Tratti di interesse
# global = dataset solo per tratti di interesse
sel_traits <- c(LA    = "Leaf area (mm2)",
            Nmass = "Nmass (mg/g)",
            LMA   = "LMA (g/m2)",
            H     = "Plant height (m)",
            SM    = "Diaspore mass (mg)",
            SSD   = "SSD combined (mg/mm3)")   # osservata + imputata da LDMC
global <- as.data.frame(traits[, c("TRY 30 AccSpecies ID",
                                 "Species name standardized against TPL",
                                 "Family", "Phylogenetic Group General",
                                 "Woodiness", sel_traits)])
names(global) <- c("try_id", "specie", "famiglia", "gruppo", "legnosa",
                 names(sel_traits))

# vedere se tutte le specie hanno tutti i tratti
#e controllo valori negativi
colSums(!is.na(global[, names(sel_traits)]))
sapply(global[, names(sel_traits)], function(x) sum(x <= 0, na.rm = TRUE))

# selezione delle sole specie con tutti e sei i tratti (positivi)
X <- global[, names(sel_traits)]
completo <- complete.cases(X) & rowSums(X <= 0, na.rm = TRUE) == 0
global_6 <- global[completo, ]
cat("Totale:", nrow(global_6))


# ========================================================================
# PCA
L <- log10(global_6[, names(sel_traits)])
pca <- prcomp(L, center = TRUE, scale. = TRUE)
if (pca$rotation["H", 1] < 0) {
  pca$rotation[, 1] <- -pca$rotation[, 1]; pca$x[, 1] <- -pca$x[, 1]
}
if (pca$rotation["Nmass", 2] < 0) {
  pca$rotation[, 2] <- -pca$rotation[, 2]; pca$x[, 2] <- -pca$x[, 2]
}

# Varianza spiegata (%) - Díaz: PC1 = 49, PC2 = 25
round(100 * pca$sdev^2 / sum(pca$sdev^2), 1)

# Autovalori - Díaz: PC1 = 2.93, PC2 = 1.50
round(pca$sdev[1:2]^2, 2)


# ========================================================================
# una riga per specie per trees Nakadai
n_alberi <- tapply(df$religion,    df$sp_name, length)
n_culto  <- tapply(df$religion,    df$sp_name, sum)
n_nome   <- tapply(df$unique_name, df$sp_name, sum)
id_try   <- tapply(df$TRY.30.AccSpecies.ID, df$sp_name, function(x) x[1])
sp <- data.frame(sp_name  = names(n_alberi),
                 try_id   = as.vector(id_try[names(n_alberi)]),
                 n_alberi = as.vector(n_alberi),
                 n_culto  = as.vector(n_culto[names(n_alberi)]),
                 n_nome   = as.vector(n_nome[names(n_alberi)]))
sp$prop_culto <- sp$n_culto / sp$n_alberi
sp$prop_nome  <- sp$n_nome  / sp$n_alberi

global_6$PC1 <- pca$x[, 1]
global_6$PC2 <- pca$x[, 2]

# ========================================================================
# unione per ID TRY
sp <- merge(sp, global_6[!is.na(global_6$try_id),
                         c("try_id", "specie", "gruppo", "legnosa", "PC1", "PC2")],
            by = "try_id", all.x = TRUE)
nrow(sp)                      # deve essere di nuovo il numero di specie di Nakadai
any(duplicated(sp$sp_name))   # deve essere FALSE
ok <- !is.na(sp$PC1)

global_6$gigante <- global_6$try_id %in% sp$try_id[!is.na(sp$try_id)]
table(global_6$legnosa)
# confronto delle specie Nakadai con specie del mondo
aggregate(cbind(PC1, PC2) ~ gigante, data = global_6, FUN = mean)
# confronto delle specie Nakadai con solo legnose
aggregate(cbind(PC1, PC2) ~ gigante, data = global_6[global_6$legnosa == "woody", ],
          FUN = mean)



# ========================================================================






