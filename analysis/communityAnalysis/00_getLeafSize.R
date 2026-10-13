library(tidyverse)

# had to write this out because my codes weren't specific enough
# the toughest, worst ones seem to be in the fen data and are commented out
# thank goodness
spp <- c(
  # "Amelanchier arborea", "Amelanchier canadensis", "Amelanchier laevis",
  # "Crataegus phaenopyrum", "Crataegus crus-galli", "Crataegus macrosperma", # this may not work because NO ONE knows hawthorns
  # "Fraxinus americana", "Fraxinus pennsylvanica",
         "Ligustrum obtusifolium",
         "Lonicera morrowii", "Lonicera tatarica", "Lonicera x bella",
         "Malus sieboldii", "Malus floribunda", "Malus prunifolia",
         "Malus pumila", "Malus domestica",
  # "Prunus serotina",
         "Rosa multiflora",
         "Rubus allegheniensis", "Rubus occidentalis",
  # "Viburnum acerifolium", 
         "Viburnum dentatum", "Viburnum dilatatum",
         "Viburnum lentago", "Viburnum plicatum", "Viburnum sieboldii")

# Species with compound leaves: only whole-leaf records allowed
compound <- c("Fraxinus americana", "Fraxinus pennsylvanica",
              "Rosa multiflora", "Rubus allegheniensis", "Rubus occidentalis")

# try BIEN
library(BIEN)

bien_raw <- BIEN_trait_traitbyspecies(species = spp, trait = "leaf area")

# bien has 8, though some of them (rubus) look bad for compound. 
# I will revisit BIEN if I end up truly needing it


# try GIFT

library(GIFT)

meta <- GIFT_traits_meta()
lw <- meta %>% filter(grepl("leaf.?(length|width)", Trait2, ignore.case = TRUE))
print(lw[, c("Lvl3", "Trait2", "Units")]) 
length_id <- "4.6.3"
width_id <- "4.7.3"
gift_raw <- GIFT_traits(trait_IDs = c(length_id, width_id),
                        bias_ref = FALSE, bias_deriv = FALSE)

# Area from length x width x shape factor. 0.67 = ellipse-ish leaf.
# This is an approximation. Only use it where no measured value exists.
gift <- gift_raw %>%
  filter(work_species %in% spp) %>%
  select(species = work_species, starts_with("trait_value")) %>%
  rename(len = 2, wid = 3) %>%
  mutate(gift_area_est = 0.67 * as.numeric(len) * as.numeric(wid))
# Check GIFT's units (cm vs mm) in `lw` before trusting the area.

# ---- Part C: TRY (when the release arrives) -------------------------------
# install.packages("data.table")
library(data.table)

try_raw <- fread("YOUR_TRY_RELEASE.txt", encoding = "Latin-1",
                 select = c("AccSpeciesName", "TraitID", "StdValue",
                            "UnitName", "ObservationID", "OrigObsDataID",
                            "ErrorRisk"))

try_la <- try_raw %>%
  as_tibble() %>%
  filter(TraitID %in% 3108:3114,
         is.na(OrigObsDataID),          # drop duplicate records
         !is.na(StdValue),
         is.na(ErrorRisk) | ErrorRisk < 4) %>%   # drop likely outliers
  mutate(species = AccSpeciesName,
         species = recode(species, "Malus domestica" = "Malus pumila")) %>%
  filter(species %in% spp) %>%
  # Compound species: whole-leaf traits only (3108, 3110, 3112)
  filter(!(species %in% compound & !TraitID %in% c(3108, 3110, 3112)))

# StdValue for leaf area is in mm2
try_sum <- try_la %>%
  group_by(species) %>%
  summarise(try_mean_mm2 = mean(StdValue), try_sd = sd(StdValue),
            try_n = n(), traits_used = paste(sort(unique(TraitID)),
                                             collapse = ","),
            .groups = "drop")

# ---- Combine -------------------------------------------------------------
out <- tibble(species = spp) %>%
  left_join(code_map, by = "species") %>%
  left_join(try_sum, by = "species") %>%
  left_join(bien, by = "species") %>%
  left_join(gift %>% select(species, gift_area_est), by = "species")

print(out, n = Inf)

# Code-level means (what joins to your caterpillar data).
# Uses TRY where present; fill gaps by hand after checking units above.
code_means <- out %>%
  group_by(code) %>%
  summarise(leaf_area_mm2 = mean(try_mean_mm2, na.rm = TRUE),
            n_species_with_data = sum(!is.na(try_mean_mm2)),
            .groups = "drop")
print(code_means)

write_csv(out, "leaf_area_by_species.csv")
write_csv(code_means, "leaf_area_by_code.csv")