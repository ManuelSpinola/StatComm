# =============================================================================
# StatCoom — Prepare Example Datasets
# =============================================================================
# Saves each dataset as a named list with components:
#   $Y      : species/response matrix  (sites × species)
#   $X      : environmental predictors (sites × predictors), NULL if none
#   $traits : species traits           (species × traits),   NULL if none
#   $meta   : list with dataset info for UI display
#
# Output: inst/extdata/*.rds  (one file per dataset)
# Run once from project root: source("data-raw/prepare_example_data.R")
# =============================================================================

library(mvabund)   # spider
library(gllvm)     # fungi, microbiome (antTraits for traits example)
library(vegan)     # mites
library(ade4)      # doubs

dir.create("inst/data", recursive = TRUE, showWarnings = FALSE)

# Helper: enforce matching rownames between Y and X, and species names in traits
validate_and_save <- function(Y, X = NULL, traits = NULL, meta, filename) {

  Y <- as.data.frame(Y)

  # --- X validation ---
  if (!is.null(X)) {
    X <- as.data.frame(X)
    stopifnot(
      "Row names of Y and X must match" =
        identical(rownames(Y), rownames(X))
    )
  }

  # --- traits validation ---
  if (!is.null(traits)) {
    traits <- as.data.frame(traits)
    stopifnot(
      "Row names of traits must match column names of Y (species)" =
        identical(rownames(traits), colnames(Y))
    )
  }

  obj <- list(Y = Y, X = X, traits = traits, meta = meta)
  saveRDS(obj, file = file.path("inst/data", filename))
  message("Saved: ", filename,
          " | sites=", nrow(Y),
          " | species=", ncol(Y),
          " | predictors=", if (is.null(X)) 0 else ncol(X),
          " | traits=", if (is.null(traits)) 0 else ncol(traits))
}


# =============================================================================
# 1. SPIDER  (mvabund)
#    12 hunting spider species, 28 sites, 6 environmental variables
#    Response type: counts (abundances)
# =============================================================================
data(spider, package = "mvabund")

spider_Y <- as.data.frame(spider$abund)
spider_X <- as.data.frame(spider$x)

# Ensure consistent site labels
rownames(spider_Y) <- rownames(spider_X) <- paste0("site", seq_len(nrow(spider_Y)))

validate_and_save(
  Y      = spider_Y,
  X      = spider_X,
  traits = NULL,
  meta   = list(
    name        = "Arañas cazadoras",
    description = "Hunting spider abundances at 28 sites with 6 habitat variables.",
    source      = "mvabund::spider",
    response    = "counts",
    family_suggestion = "negative.binomial",
    n_sites     = nrow(spider_Y),
    n_species   = ncol(spider_Y),
    n_predictors = ncol(spider_X),
    has_traits  = FALSE,
    reference   = "van der Aart & Smeenk-Enserink (1975)"
  ),
  filename = "spider.rds"
)


# =============================================================================
# 2. FUNGI  (gllvm)
#    Presence/absence of fungi species across sites
#    Response type: binary (presence/absence)
# =============================================================================
data(fungi, package = "gllvm")

# fungi is a list with $Y and $X
fungi_Y <- as.data.frame(fungi$Y)
fungi_X <- as.data.frame(fungi$X)

rownames(fungi_Y) <- rownames(fungi_X) <- paste0("site", seq_len(nrow(fungi_Y)))

validate_and_save(
  Y      = fungi_Y,
  X      = fungi_X,
  traits = NULL,
  meta   = list(
    name        = "Comunidad de hongos",
    description = "Presence/absence of fungi species with environmental predictors.",
    source      = "gllvm::fungi",
    response    = "binary",
    family_suggestion = "binomial",
    n_sites     = nrow(fungi_Y),
    n_species   = ncol(fungi_Y),
    n_predictors = ncol(fungi_X),
    has_traits  = FALSE,
    reference   = "Abrego et al."
  ),
  filename = "fungi.rds"
)


# =============================================================================
# 3. MICROBIALDATA  (gllvm)
#    High-dimensional microbial count data
#    Response type: counts (often overdispersed)
# =============================================================================
data(microbialdata, package = "gllvm")

# microbialdata: $Y = OTU counts, $Xenv = environmental variables
microbiome_Y <- as.data.frame(microbialdata$Y)
microbiome_X <- as.data.frame(microbialdata$Xenv)

# Preserve factor structure (Region, Site, Soiltype already factors)
microbiome_X$Region   <- factor(microbiome_X$Region)
microbiome_X$Site     <- factor(microbiome_X$Site)
microbiome_X$Soiltype <- factor(microbiome_X$Soiltype)

rownames(microbiome_Y) <- rownames(microbiome_X) <- rownames(microbialdata$Xenv)

validate_and_save(
  Y      = microbiome_Y,
  X      = microbiome_X,
  traits = NULL,
  meta   = list(
    name        = "Comunidad microbiana",
    description = "High-dimensional microbial OTU counts with environmental predictors.",
    source      = "gllvm::microbialdata",
    response    = "counts",
    family_suggestion = "negative.binomial",
    n_sites     = nrow(microbiome_Y),
    n_species   = ncol(microbiome_Y),
    n_predictors = ncol(microbiome_X),
    has_traits  = FALSE,
    predictor_types = list(
      continuous = c("SOM", "pH", "Phosp"),
      factor     = c("Region", "Site", "Soiltype")
    ),
    reference   = "Tedersoo et al."
  ),
  filename = "microbiome.rds"
)



# =============================================================================
# 4. MITES  (vegan)
#    70 sites × 35 oribatid mite species
#    Predictors: SubsDens (num), WatrCont (num), Substrate (factor),
#                Shrub (factor), Topo (factor)
#    Response type: counts
# =============================================================================
data(mite,        package = "vegan")
data(mite.env,    package = "vegan")

mite_Y <- as.data.frame(mite)
mite_X <- as.data.frame(mite.env)

# Preserve factor structure — critical for gllvm with factor predictors
# Substrate: 7 levels, Shrub: 3 levels, Topo: 2 levels
mite_X$Substrate <- factor(mite_X$Substrate)
mite_X$Shrub     <- factor(mite_X$Shrub)
mite_X$Topo      <- factor(mite_X$Topo)

rownames(mite_Y) <- rownames(mite_X) <- paste0("site", seq_len(nrow(mite_Y)))

validate_and_save(
  Y      = mite_Y,
  X      = mite_X,
  traits = NULL,
  meta   = list(
    name        = "Ácaros oribátidos",
    description = "Oribatid mite abundances at 70 sites with continuous and factor predictors.",
    source      = "vegan::mite + vegan::mite.env",
    response    = "counts",
    family_suggestion = "negative.binomial",
    n_sites     = nrow(mite_Y),
    n_species   = ncol(mite_Y),
    n_predictors = ncol(mite_X),
    has_traits  = FALSE,
    predictor_types = list(
      continuous = c("SubsDens", "WatrCont"),
      factor     = c("Substrate", "Shrub", "Topo")
    ),
    reference   = "Borcard & Legendre (1994)"
  ),
  filename = "mites.rds"
)


# =============================================================================
# 5. DOUBS — Fish community  (ade4)
#    30 sites along the Doubs river × 27 fish species
#    11 physicochemical variables as predictors
#    Response type: counts (abundances)
# =============================================================================
data(doubs, package = "ade4")

doubs_Y <- as.data.frame(doubs$fish)
doubs_X <- as.data.frame(doubs$env)

# Site 8 is empty in the original — remove to avoid issues with zero-only rows
empty_sites <- which(rowSums(doubs_Y) == 0)
if (length(empty_sites) > 0) {
  doubs_Y <- doubs_Y[-empty_sites, ]
  doubs_X <- doubs_X[-empty_sites, ]
  message("Doubs: removed ", length(empty_sites), " empty site(s): ",
          paste(empty_sites, collapse = ", "))
}

rownames(doubs_Y) <- rownames(doubs_X) <- paste0("site", seq_len(nrow(doubs_Y)))

validate_and_save(
  Y      = doubs_Y,
  X      = doubs_X,
  traits = NULL,
  meta   = list(
    name        = "Peces río Doubs",
    description = "Fish community along the Doubs river (France/Switzerland) with physicochemical variables.",
    source      = "ade4::doubs",
    response    = "counts",
    family_suggestion = "negative.binomial",
    n_sites     = nrow(doubs_Y),
    n_species   = ncol(doubs_Y),
    n_predictors = ncol(doubs_X),
    has_traits  = FALSE,
    reference   = "Verneaux (1973)"
  ),
  filename = "doubs.rds"
)




# =============================================================================
# 6. BEETLE  (gllvm)
#    87 sites x 68 ground beetle species
#    22 environmental predictors (continuous + factors)
#    22 species traits (morphological + functional) — fourth-corner ready
#    Response type: counts
# =============================================================================
data(beetle, package = "gllvm")

beetle_Y <- as.data.frame(beetle$Y)
beetle_X <- as.data.frame(beetle$X)
beetle_TR <- as.data.frame(beetle$TR)

# beetle$X has numeric rownames — align with Y site codes
rownames(beetle_X) <- rownames(beetle_Y)

# Preserve factor structure in X
beetle_X$SiteCode <- factor(beetle_X$SiteCode)
beetle_X$Landuse  <- factor(beetle_X$Landuse)
beetle_X$Grid     <- factor(beetle_X$Grid)
beetle_X$Area     <- factor(beetle_X$Area)

# Traits: set rownames = species names (colnames of Y)
rownames(beetle_TR) <- colnames(beetle_Y)

# Remove SPECIES and CODE columns from traits (metadata, not traits per se)
beetle_TR <- beetle_TR[, !names(beetle_TR) %in% c("SPECIES", "CODE")]

validate_and_save(
  Y      = beetle_Y,
  X      = beetle_X,
  traits = beetle_TR,
  meta   = list(
    name        = "Escarabajos (ground beetles)",
    description = "Ground beetle assemblages at 87 grassland sites with environmental and species trait data.",
    source      = "gllvm::beetle",
    response    = "counts",
    family_suggestion = "negative.binomial",
    n_sites      = nrow(beetle_Y),
    n_species    = ncol(beetle_Y),
    n_predictors = ncol(beetle_X),
    has_traits   = TRUE,
    n_traits     = ncol(beetle_TR),
    predictor_types = list(
      continuous = c("Texture", "Org", "pH", "AvailP", "AvailK", "Moist",
                     "Bare", "Litter", "Bryophyte", "Plants.m2", "Canopyheight",
                     "Stemdensity", "Biom_l5", "Biom_m5", "Reprobiom",
                     "Elevation", "Management", "Samplingyear"),
      factor     = c("SiteCode", "Landuse", "Grid", "Area")
    ),
    trait_types = list(
      continuous = c("LYW", "LAL", "LPW", "LPH", "LEW", "LFL",
                     "LTR", "LRL", "LFW", "LTL"),
      factor     = c("CLG", "CLB", "WIN", "PRS", "OVE", "FOA",
                     "DAY", "BRE", "EME", "ACT")
    ),
    reference   = "Ribera et al. (2001)"
  ),
  filename = "beetle.rds"
)
# =============================================================================
# Summary
# =============================================================================
message("\n--- All datasets saved to inst/data/ ---")
files <- list.files("inst/data", pattern = "\\.rds$", full.names = TRUE)
for (f in files) {
  obj <- readRDS(f)
  message(basename(f), ": Y=", nrow(obj$Y), "×", ncol(obj$Y),
          " | X=", if (is.null(obj$X)) "none" else paste0(ncol(obj$X), " vars"),
          " | traits=", if (is.null(obj$traits)) "none" else paste0(ncol(obj$traits), " traits"))
}
