# ==================================================================================
# FFQ CONVERSION AND AHEI CALCULATION
# ==================================================================================

library(tidyverse)
library(readxl)
library(survival)

# Standard 9-level scale (default for most food items)
# Used for items with max observed value ≤ 9
convert_freq_9level <- function(freq_code) {
  case_when(
    freq_code == 1 ~ 0,          # Never / <1 per month
    freq_code == 2 ~ 2/30,       # 1-3 per month (midpoint 2/month)
    freq_code == 3 ~ 1/7,        # 1 per week
    freq_code == 4 ~ 3/7,        # 2-4 per week (midpoint 3/week)
    freq_code == 5 ~ 5.5/7,      # 5-6 per week (midpoint 5.5/week)
    freq_code == 6 ~ 1,          # 1 per day
    freq_code == 7 ~ 2.5,        # 2-3 per day (midpoint 2.5/day)
    freq_code == 8 ~ 4.5,        # 4-5 per day (midpoint 4.5/day)
    freq_code == 9 ~ 6,          # 6+ per day
    TRUE ~ NA_real_
  )
}

# 10-level scale (milk, some fruits, SSBs, peanut butter, some sweets)
convert_freq_10level <- function(freq_code) {
  case_when(
    freq_code == 1 ~ 0,
    freq_code == 2 ~ 2/30,
    freq_code == 3 ~ 1/7,
    freq_code == 4 ~ 3/7,
    freq_code == 5 ~ 5.5/7,
    freq_code == 6 ~ 1,
    freq_code == 7 ~ 2.5,
    freq_code == 8 ~ 4.5,
    freq_code == 9 ~ 6,
    freq_code == 10 ~ 6,         # Same as 6+/day (top category)
    TRUE ~ NA_real_
  )
}

# 11-level scale (breads, coffee, tea, water, alcohol)
convert_freq_11level <- function(freq_code) {
  case_when(
    freq_code == 1 ~ 0,
    freq_code == 2 ~ 2/30,
    freq_code == 3 ~ 1/7,
    freq_code == 4 ~ 3/7,
    freq_code == 5 ~ 5.5/7,
    freq_code == 6 ~ 1,
    freq_code == 7 ~ 2.5,
    freq_code == 8 ~ 4.5,
    freq_code == 9 ~ 6,
    freq_code == 10 ~ 6,
    freq_code == 11 ~ 6,         # Same as 6+/day (top category)
    TRUE ~ NA_real_
  )
}

# ==================================================================================
# CALCULATE FOOD GROUP SERVINGS
# ==================================================================================

calculate_food_servings <- function(AllMetdataFiber) {

  AllMetdataFiber <- AllMetdataFiber %>%
    mutate(

      # =====================================================================
      # VEGETABLES (excluding potatoes and legumes per AHEI definition)
      # =====================================================================
      veg_servings =
        convert_freq_9level(tom) +           # Tomatoes (max 8)
        convert_freq_9level(tom.j) +         # Tomato juice (max 9) 
        convert_freq_9level(tom.s) +         # Tomato sauce (max 7) 
        convert_freq_9level(broc) +          # Broccoli (max 8)
        convert_freq_9level(cabb.cole) +     # Cabbage/coleslaw (max 8)
        convert_freq_9level(caul) +          # Cauliflower (max 8)
        convert_freq_9level(brusl) +         # Brussels sprouts (max 8)
        convert_freq_9level(carrot.r) +      # Raw carrots (max 9) 
        convert_freq_9level(carrot.c) +      # Cooked carrots (max 9) 
        convert_freq_9level(corn) +          # Corn (max 8)
        convert_freq_9level(peas) +          # Peas/lima beans (max 8)
        convert_freq_9level(mix.veg) +       # Mixed vegetables (max 8)
        convert_freq_9level(st.beans) +      # String beans (max 7)
        convert_freq_9level(yel.sqs) +       # Dark orange squash (max 8)
        convert_freq_9level(zuke) +          # Zucchini/eggplant (max 8)
        convert_freq_9level(swt.pot) +       # Sweet potatoes (max 8)
        convert_freq_9level(spin.ckd) +      # Spinach cooked (max 7)
        convert_freq_9level(spin.raw) +      # Spinach raw (max 8)
        convert_freq_9level(kale) +          # Kale/mustard/chard (max 8)
        convert_freq_9level(ice.let) +       # Iceberg lettuce (max 9) 
        convert_freq_9level(rom.let) +       # Romaine lettuce (max 9) 
        convert_freq_9level(celery) +        # Celery (max 9) 
        convert_freq_9level(peppers) +       # Green peppers (max 8)
        convert_freq_9level(onions) +        # Onions as garnish (max 8)
        convert_freq_9level(onions1),        # Onions as vegetable (max 8) 


      # =====================================================================
      # FRUITS (whole fruits only — no fruit juice per AHEI definition)
      # =====================================================================
      fruit_servings =
        convert_freq_9level(raisgrp) +       # Raisins/grapes (max 9)
        convert_freq_9level(prun) +          # Prunes (max 8)
        convert_freq_9level(ban) +           # Bananas (max 9) 
        convert_freq_10level(cant) +         # Cantaloupe (max 10) 
        convert_freq_9level(avocado) +       # Avocado (max 9) 
        convert_freq_9level(a.sce) +         # Applesauce (max 8)
        convert_freq_10level(apple) +        # Apple/pears (max 10)
        convert_freq_10level(orang) +        # Oranges (max 10) 
        convert_freq_10level(grfrt) +        # Grapefruit (max 10)
        convert_freq_9level(straw) +         # Strawberries (max 8)
        convert_freq_9level(blue) +          # Blueberries (max 7)
        convert_freq_9level(peaches) +       # Peaches/plums (max 8)
        convert_freq_9level(apricot),        # Apricots (max 7)


      # =====================================================================
      # FRUIT JUICE (counted AGAINST diet quality in AHEI — combined with SSB)
      # =====================================================================
      fruit_juice_servings =
        convert_freq_9level(prun.j) +        # Prune juice (max 9)
        convert_freq_9level(a.j) +           # Apple juice (max 9)
        convert_freq_9level(o.j) +           # Orange juice (max 9)
        convert_freq_9level(o.j.calc) +      # OJ calcium fortified (max 9)
        convert_freq_9level(grfrt.j) +       # Grapefruit juice (max 9)
        convert_freq_9level(oth.f.j),        # Other fruit juice (max 9)


      # =====================================================================
      # SUGAR-SWEETENED BEVERAGES
      # =====================================================================
      # These are 10-level items (max observed = 10), NOT 11-level
      # =====================================================================
      ssb_servings =
        convert_freq_10level(coke) +         # Cola with sugar (max 10)
        convert_freq_10level(oth.carb) +     # Other carb beverage w sugar (max 10)
        convert_freq_10level(punch),         # Punch/lemonade (max 10)


      # =====================================================================
      # WHOLE GRAINS
      # =====================================================================
      # whgrn is already in grams/day from the nutrient data file
      # For the custom AHEI: convert to servings (1 serving ≈ 16g)
      # For dietaryindex: pass grams directly (the package expects grams)
      # =====================================================================
      wholegrain_servings = whgrn / 16,


      # =====================================================================
      # REFINED GRAINS
      # =====================================================================
      refined_grain_servings =
        convert_freq_11level(wh.br) +        # White bread (max 11)
        convert_freq_9level(wh.rice) +       # White rice (max 8)
        convert_freq_9level(pasta),          # Pasta (max 7)


      # =====================================================================
      # LEGUMES (for nuts+legumes component)
      # =====================================================================
      # Tofu is included here per AHEI definition of "vegetable protein"
      # =====================================================================
      legume_servings =
        convert_freq_9level(tofu) +          # Tofu/soybeans (max 9)
        convert_freq_9level(beans),          # Beans/lentils (max 8)


      # =====================================================================
      # NUTS (for nuts+legumes component)
      # =====================================================================
      # dietaryindex: 1 serving = 1 oz nuts OR 1 TBSP peanut butter
      # Willett FFQ asks about frequency with a specified portion:
      #   "Peanuts (small packet or 1 oz)" → 1 occasion = 1 serving
      #   "Peanut butter (1 Tbsp)" → 1 occasion = 1 serving
      # So NO halving of peanut butter is needed
      # =====================================================================
      nut_servings =
        convert_freq_9level(nuts) +          # Peanuts (max 9)
        convert_freq_9level(walnuts) +       # Walnuts (max 9)
        convert_freq_9level(oth.nuts) +      # Other nuts (max 9)
        convert_freq_10level(p.bu),          # Peanut butter (max 10)


      # =====================================================================
      # RED AND PROCESSED MEAT
      # =====================================================================
      red_meat_servings =
        convert_freq_9level(bacon) +         # Bacon (max 8)
        convert_freq_9level(hotdog) +        # Beef/pork hot dogs (max 9)
        convert_freq_9level(bologna) +       # Bologna/processed meat sandwiches (max 7)
        convert_freq_9level(proc.mts) +      # Other processed meats (max 9)
        convert_freq_9level(xtrlean.hamburg) + # Lean hamburger (max 8)
        convert_freq_9level(hamb) +          # Regular hamburger (max 8)
        convert_freq_9level(sand.bf.ham) +   # Beef/pork sandwich/mixed dish (max 8)
        convert_freq_9level(pork) +          # Pork main dish (max 8)
        convert_freq_9level(beef02) +        # Beef/lamb main dish (max 8)
        convert_freq_9level(liver),          # Beef/calf/pork liver (max 7)


      # =====================================================================
      # POULTRY
      # =====================================================================
      poultry_servings =
        convert_freq_9level(chix.no.sand) +  # Chicken sandwich/frozen dinner (max 7)
        convert_freq_9level(chix.sk) +       # Chicken with skin (max 9) 
        convert_freq_9level(chix.no) +       # Chicken without skin (max 9)
        convert_freq_9level(chix.dog) +      # Chicken hot dogs (max 9)
        convert_freq_9level(chix.liver),     # Chicken liver (max 7)


      # =====================================================================
      # FISH / SEAFOOD
      # =====================================================================
      fish_servings =
        convert_freq_9level(tuna) +          # Canned tuna (max 9)
        convert_freq_9level(fr.fish.kids) +  # Breaded fish/fish sticks (max 8)
        convert_freq_9level(shrimp.ckd) +    # Shrimp/lobster/scallops (max 8)
        convert_freq_9level(dk.fish) +       # Dark meat fish (max 8)
        convert_freq_9level(oth.fish),       # Other fish (max 8)


      # =====================================================================
      # EGGS
      # =====================================================================
      egg_servings =
        convert_freq_9level(egg.beat) +      # Egg Beaters/whites (max 9)
        convert_freq_9level(eggs.omega) +    # Omega-fortified eggs (max 9)
        convert_freq_9level(eggs),           # Whole eggs (max 9)


      # =====================================================================
      # DAIRY
      # =====================================================================
      dairy_servings =
        convert_freq_10level(skim.kids) +    # Skim milk (max 10)
        convert_freq_10level(milk2) +        # 1-2% milk (max 10)
        convert_freq_10level(milk) +         # Whole milk (max 10)
        convert_freq_10level(soymilk.fort) + # Soy milk (max 10)
        convert_freq_9level(yog) +           # Flavored yogurt (max 9)
        convert_freq_9level(yog.plain) +     # Plain yogurt (max 9)
        convert_freq_9level(cot.ch) +        # Cottage cheese (max 9)
        convert_freq_9level(oth.ch),         # Other cheese (max 9)


      # =====================================================================
      # SWEETS / DESSERTS (not directly used in AHEI, but available)
      # =====================================================================
      sweets_servings =
        convert_freq_10level(choc) +         # Chocolate (max 10)
        convert_freq_10level(choc.dark) +    # Dark chocolate (max 10)
        convert_freq_10level(candy_and_nuts) + # Candy with nuts (max 10)
        convert_freq_10level(candy) +        # Candy without (max 10)
        convert_freq_10level(choc.chip.cookie) + # Cookies ready-made (max 10)
        convert_freq_10level(coox.home.cc) + # Cookies home-baked (max 10)
        convert_freq_9level(brownie) +       # Brownies (max 9)
        convert_freq_10level(donut) +        # Donuts (max 10)
        convert_freq_9level(cake.home) +     # Cake homemade (max 8)
        convert_freq_9level(cake.comm) +     # Cake ready-made (max 8)
        convert_freq_9level(pie.home) +      # Pie homemade (max 8)
        convert_freq_9level(pie.comm)        # Pie ready-made (max 8)
    )

  return(AllMetdataFiber)
}


# ==================================================================================
# CALCULATE AHEI-2010 (CUSTOM — independent of dietaryindex package)
# ==================================================================================
# Scoring criteria from Chiuve SE et al. J Nutr. 2012;142:1009-1018
# Each component scores 0-10 points; total range 0-110
# ==================================================================================

calculate_AHEI <- function(AllMetdataFiber) {

  # First calculate food servings
  AllMetdataFiber <- calculate_food_servings(AllMetdataFiber)

  AllMetdataFiber <- AllMetdataFiber %>%
    mutate(

      # ===== COMPONENT 1: VEGETABLES =====
      # Best (10 pts): ≥5 servings/day; Worst (0 pts): 0 servings/day
      ahei_vegetables = pmin(10, (veg_servings / 5) * 10),


      # ===== COMPONENT 2: WHOLE FRUIT =====
      # Best (10 pts): ≥4 servings/day; Worst (0 pts): 0 servings/day
      ahei_fruit = pmin(10, (fruit_servings / 4) * 10),


      # ===== COMPONENT 3: WHOLE GRAINS =====
      # Sex-specific thresholds (Chiuve et al. 2012):
      #   Women: Best (10 pts) at ≥75 g/day
      #   Men:   Best (10 pts) at ≥90 g/day
      # Worst (0 pts): 0 g/day
      ahei_whole_grains = case_when(
        sex == 2 ~ pmin(10, (whgrn / 75) * 10),   # Women
        sex == 1 ~ pmin(10, (whgrn / 90) * 10),   # Men
        TRUE ~ pmin(10, (whgrn / 75) * 10)         # Default to female if unknown
      ),


      # ===== COMPONENT 4: SSB + FRUIT JUICE =====
      # Best (10 pts): 0 servings/day; Worst (0 pts): ≥1 serving/day
      total_ssb_juice = ssb_servings + fruit_juice_servings,
      ahei_ssb = case_when(
        total_ssb_juice == 0 ~ 10,
        total_ssb_juice >= 1 ~ 0,
        TRUE ~ 10 - (total_ssb_juice / 1) * 10
      ),


      # ===== COMPONENT 5: NUTS AND LEGUMES =====
      # Best (10 pts): ≥1 serving/day; Worst (0 pts): 0 servings/day
      total_nuts_legumes = nut_servings + legume_servings,
      ahei_nuts_legumes = pmin(10, (total_nuts_legumes / 1) * 10),


      # ===== COMPONENT 6: RED/PROCESSED MEAT =====
      # Best (10 pts): 0 servings/day; Worst (0 pts): ≥1.5 servings/day
      ahei_red_meat = case_when(
        red_meat_servings == 0 ~ 10,
        red_meat_servings >= 1.5 ~ 0,
        TRUE ~ 10 - (red_meat_servings / 1.5) * 10
      ),


      # ===== COMPONENT 7: TRANS FAT =====
      # Best (10 pts): ≤0.5% energy; Worst (0 pts): ≥4% energy
      trans_pct_energy = (trn11 * 9 / calor) * 100,
      ahei_trans_fat = case_when(
        trans_pct_energy <= 0.5 ~ 10,
        trans_pct_energy >= 4 ~ 0,
        TRUE ~ 10 - ((trans_pct_energy - 0.5) / (4 - 0.5)) * 10
      ),


      # ===== COMPONENT 8: OMEGA-3 FATS (EPA + DHA) =====
      # Best (10 pts): ≥250 mg/day; Worst (0 pts): 0 mg/day
      # f205 (EPA) and f226 (DHA) are in grams → multiply by 1000 for mg
      epa_dha_mg = (f205 + f226) * 1000,
      ahei_omega3 = pmin(10, (epa_dha_mg / 250) * 10),


      # ===== COMPONENT 9: PUFA =====
      # Best (10 pts): ≥10% energy; Worst (0 pts): ≤2% energy
      pufa_pct_energy = (poly * 9 / calor) * 100,
      ahei_pufa = case_when(
        pufa_pct_energy <= 2 ~ 0,
        pufa_pct_energy >= 10 ~ 10,
        TRUE ~ ((pufa_pct_energy - 2) / (10 - 2)) * 10
      ),


      # ===== COMPONENT 10: SODIUM =====
      # Scored on energy-adjusted sodium (mg per 2000 kcal)
      # Best (10 pts): bottom decile; Worst (0 pts): top decile
      # Using absolute thresholds from Chiuve et al.:
      #   Best: ≤1112 mg/2000 kcal = 10; Worst: ≥3337 mg/2000 kcal = 0
      sodium_per_2000kcal = sodium * (2000 / calor),
      ahei_sodium = case_when(
        sodium_per_2000kcal <= 1112 ~ 10,
        sodium_per_2000kcal >= 3337 ~ 0,
        TRUE ~ 10 - ((sodium_per_2000kcal - 1112) / (3337 - 1112)) * 10
      ),


      # ===== COMPONENT 11: ALCOHOL =====
      # Moderate consumption scores best (U-shaped)
      # Women: Best (10 pts) at 5-15 g/day
      # Men: Best (10 pts) at 5-30 g/day (Chiuve uses same 5-15 for both sexes)
      # Using Chiuve et al. 2012: 0.5-1.5 drinks/day (7-21 g) → 10 pts
      # Simplified: moderate = 5-15 g/day (as per original Fung et al.)
      ahei_alcohol = case_when(
        alco >= 5 & alco <= 15 ~ 10,
        alco < 5 ~ (alco / 5) * 10,
        alco > 15 & alco <= 30 ~ 10 - ((alco - 15) / 15) * 10,
        TRUE ~ 0
      ),


      # ===== TOTAL AHEI SCORE =====
      AHEI_total = ahei_vegetables + ahei_fruit + ahei_whole_grains +
                   ahei_ssb + ahei_nuts_legumes + ahei_red_meat +
                   ahei_trans_fat + ahei_omega3 + ahei_pufa +
                   ahei_sodium + ahei_alcohol,

      # Create categorical variables
      AHEI_tertile = cut(AHEI_total,
                         breaks = quantile(AHEI_total, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
                         labels = c("Low", "Medium", "High"),
                         include.lowest = TRUE),

      AHEI_quartile = cut(AHEI_total,
                          breaks = quantile(AHEI_total, probs = 0:4/4, na.rm = TRUE),
                          labels = c("Q1", "Q2", "Q3", "Q4"),
                          include.lowest = TRUE)
    )

  return(AllMetdataFiber)
}