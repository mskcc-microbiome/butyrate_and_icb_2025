# ==================================================================================
# WILLETT FFQ TO DIETARYINDEX PREPROCESSING SCRIPT
# ==================================================================================
#
# This script converts Willett FFQ data to the format required by the dietaryindex
# R package (Zhan et al. AJCN 2024) for calculating AHEI-2010 and other indices.
#
# See AHEI_calculation.R for the frequency conversion functions and
# food group serving calculations used here.
# ==================================================================================

library(tidyverse)
library(readxl)

# Source the corrected conversion functions
source("AHEI_calculation.R")

# ==================================================================================
# PREPROCESSING FUNCTION FOR DIETARYINDEX PACKAGE
# ==================================================================================
# This function takes Willett FFQ data and creates variables in the exact units
# expected by the dietaryindex AHEI() function. Unit specifications are from:
#   https://jamesjiadazhan.github.io/dietaryindex_manual/reference/AHEI.html
# ==================================================================================

prepare_willett_for_dietaryindex <- function(willett_data) {

  cat("Preparing Willett FFQ data for dietaryindex package...\n\n")

  # Step 1: Calculate food group servings using conversion functions
  willett_data <- calculate_food_servings(willett_data)

  # Step 2: Format data for dietaryindex package
  dietaryindex_data <- willett_data %>%
    mutate(

      # ===== REQUIRED: RESPONDENT ID =====
      RESPONDENTID = record_id,


      # ===== SEX =====
      # dietaryindex expects: 1 = male, 2 = female
      GENDER = as.numeric(sex),


      # ===== TOTAL CALORIES =====
      # Unit: kcal/day
      TOTALKCAL_AHEI = calor,


      # =====================================================================
      # AHEI COMPONENTS — with units required by dietaryindex
      # =====================================================================

      # 1. VEGETABLES (excluding potatoes and legumes)
      # Unit: servings/day (1 serving = 0.5 cup vegetables OR 1 cup green leafy)
      VEG_SERV_AHEI = veg_servings,


      # 2. WHOLE FRUITS (no fruit juice)
      # Unit: servings/day (1 serving = 0.5 cup berries OR 1 medium fruit)
      FRT_SERV_AHEI = fruit_servings,


      # 3. WHOLE GRAINS
      # Unit: grams/day (the package docs say "unit=grams/day")
      WGRAIN_SERV_AHEI = whgrn,


      # 4. NUTS, LEGUMES, AND VEGETABLE PROTEIN
      # Unit: servings/day (1 serving = 1 oz nuts/legumes OR 1 TBSP peanut butter)
      NUTSLEG_SERV_AHEI = nut_servings + legume_servings,


      # 5. LONG-CHAIN OMEGA-3 FATS (EPA + DHA)
      # Unit: mg/day
      # f205 (EPA) and f226 (DHA) are in grams → multiply by 1000
      N3FAT_SERV_AHEI = (f205 + f226) * 1000,


      # 6. PUFA (Polyunsaturated fatty acids)
      # Unit: % of energy
      PUFA_SERV_AHEI = (poly * 9 / calor) * 100,


      # 7. SUGAR-SWEETENED BEVERAGES + FRUIT JUICE
      # Unit: servings/day (1 serving = 8 oz)
      SSB_FRTJ_SERV_AHEI = ssb_servings + fruit_juice_servings,


      # 8. RED AND PROCESSED MEAT
      # Unit: servings/day (1 serving = 4 oz unprocessed meat; 1.5 oz processed)
      REDPROC_MEAT_SERV_AHEI = red_meat_servings,


      # 9. TRANS FAT
      # Unit: % of energy
      TRANS_SERV_AHEI = (trn11 * 9 / calor) * 100,


      # 10. SODIUM
      # Unit: mg/day per 2000 kcal (energy-adjusted)
      SODIUM_SERV_AHEI = sodium * (2000 / calor),


      # 11. ALCOHOL
      # Unit: drinks/day (1 drink = 14g alcohol = 12 oz beer = 5 oz wine = 1.5 oz spirits)
      ALCOHOL_SERV_AHEI = alco / 14,


      # =====================================================================
      # HEI-2015 ADDITIONAL COMPONENTS (if needed)
      # =====================================================================

      # Greens and beans (dark green vegetables + legumes)
      # Unit: cup equivalents per day
      GREENSANDBEAN_SERV_HEI = (convert_freq_9level(broc) * 0.3 +
                                convert_freq_9level(spin.ckd) * 0.3 +
                                convert_freq_9level(spin.raw) * 0.3 +
                                convert_freq_9level(kale) * 0.3 +
                                convert_freq_9level(rom.let) * 0.2 +
                                legume_servings) / 2,


      # Total protein foods
      # Unit: oz equivalents per day
      TOTALPROT_SERV_HEI = red_meat_servings + poultry_servings +
                           fish_servings + egg_servings +
                           nut_servings + legume_servings,


      # Seafood and plant proteins
      # Unit: oz equivalents per day
      SEAPLANTPROT_SERV_HEI = fish_servings + nut_servings + legume_servings,


      # Dairy
      # Unit: cup equivalents per day
      DAIRY_SERV_HEI = dairy_servings,


      # Refined grains
      # Unit: oz equivalents per day
      REFINEDGRAIN_SERV_HEI = refined_grain_servings,


      # Saturated fat
      # Unit: % of energy (corrected: grams × 9 / kcal × 100)
      SATFAT_SERV_HEI = (satfat * 9 / calor) * 100,


      # Added sugars
      # Unit: % of energy (grams × 4 kcal/g / kcal × 100)
      ADDEDSUGARS_SERV_HEI = (addsug * 4 / calor) * 100,


      # Fatty acid ratio (for HEI)
      # Unit: (PUFA + MUFA) / SFA
      FATTYACIDRATIO_HEI = (poly + monfat) / satfat,


      # =====================================================================
      # MEDITERRANEAN DIET COMPONENTS (if needed)
      # =====================================================================

      MUFA_RATIO_MED = monfat / satfat,
      FISH_SERV_MED = fish_servings,
      LEGUME_SERV_MED = legume_servings,
      CEREAL_SERV_MED = wholegrain_servings + refined_grain_servings,
      MEAT_SERV_MED = red_meat_servings + poultry_servings,


      # =====================================================================
      # DII COMPONENTS (if needed)
      # =====================================================================

      DII_ENERGY = calor,
      DII_PROTEIN = prot,
      DII_CARB = carbo,
      DII_FAT = tfat,
      DII_FIBER = aofib,
      DII_VITC = vitc,
      DII_VITD = vitd,
      DII_B12 = b12,
      DII_IRON = iron,
      DII_MAGN = magn,
      DII_ZINC = zn,
      DII_SELENIUM = se,
      DII_VITB6 = b6,
      DII_NIACIN = niacin,
      DII_FOLATE = fol98,
      DII_VITA = rae,
      DII_VITE = e02mg,
      DII_SATFAT = satfat,
      DII_MUFA = monfat,
      DII_PUFA = poly,
      DII_OMEGA3 = omega,
      DII_ALCOHOL = alco,
      DII_CAFFEINE = caff
    )

  # Select only the columns needed for dietaryindex
  dietaryindex_data <- dietaryindex_data %>%
    select(RESPONDENTID, GENDER, starts_with("TOTALKCAL"),
           ends_with("_AHEI"), ends_with("_HEI"), ends_with("_MED"),
           starts_with("DII_"))

  cat("Data successfully formatted for dietaryindex!\n\n")
  cat("Variables created:\n")
  cat("  - ", ncol(dietaryindex_data), " variables total\n")
  cat("  - AHEI components: 11 + total kcal\n")
  cat("  - HEI additional: 7\n")
  cat("  - MED additional: 5\n")
  cat("  - DII components: 22\n\n")

  return(dietaryindex_data)
}


# ==================================================================================
# WRAPPER FUNCTION: CALCULATE AHEI USING DIETARYINDEX PACKAGE
# ==================================================================================

calculate_AHEI_with_dietaryindex <- function(willett_data) {

  # Install dietaryindex if not already installed
  if (!require("dietaryindex", quietly = TRUE)) {
    cat("Installing dietaryindex package...\n")
    devtools::install_github("jamesjiadazhan/dietaryindex")
    library(dietaryindex)
  }

  # Prepare data
  prepared_data <- prepare_willett_for_dietaryindex(willett_data)

  # Calculate AHEI using dietaryindex
  cat("Calculating AHEI-2010 using dietaryindex package...\n")

  ahei_results <- AHEI(
    SERV_DATA = prepared_data,
    RESPONDENTID = prepared_data$RESPONDENTID,
    GENDER = prepared_data$GENDER,
    TOTALKCAL_AHEI = prepared_data$TOTALKCAL_AHEI,
    VEG_SERV_AHEI = prepared_data$VEG_SERV_AHEI,
    FRT_SERV_AHEI = prepared_data$FRT_SERV_AHEI,
    WGRAIN_SERV_AHEI = prepared_data$WGRAIN_SERV_AHEI,
    NUTSLEG_SERV_AHEI = prepared_data$NUTSLEG_SERV_AHEI,
    N3FAT_SERV_AHEI = prepared_data$N3FAT_SERV_AHEI,
    PUFA_SERV_AHEI = prepared_data$PUFA_SERV_AHEI,
    SSB_FRTJ_SERV_AHEI = prepared_data$SSB_FRTJ_SERV_AHEI,
    REDPROC_MEAT_SERV_AHEI = prepared_data$REDPROC_MEAT_SERV_AHEI,
    TRANS_SERV_AHEI = prepared_data$TRANS_SERV_AHEI,
    SODIUM_SERV_AHEI = prepared_data$SODIUM_SERV_AHEI,
    ALCOHOL_SERV_AHEI = prepared_data$ALCOHOL_SERV_AHEI
  )

  # Merge back with original data
  results <- willett_data %>%
    left_join(ahei_results, by = c("id" = "RESPONDENTID"))

  return(results)
}