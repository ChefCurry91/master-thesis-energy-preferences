getwd()
setwd("/Users/fabio/Desktop/UNINE/Master_Thesis/Code")
library(dplyr)
library(ggplot2)
library(tibble)
library(MASS)  # for polr() function
library(car)
library(pscl) # Pseudo R-squared measures for logit ordered model
library(DescTools) # # For binary model
library(reshape2)
library(margins)
library(boot)
library(purrr)
library(tibble)

#install.packages("marginaleffects")
#library(marginaleffects)


# SHEDS Data

SHEDS_2025_Table <- readxl::read_excel("/Users/fabio/Desktop/UNINE/Master_Thesis/SHEDS_Data_2025/SHEDS_2025.xlsx", sheet='data_labels')

SHEDS_2025_Table <- SHEDS_2025_Table %>%
  tibble::as_tibble()


#View(SHEDS_2025_Table)

#glimpse(SHEDS_2025_Table)

SHEDS_2025_Table_relevant_variables <- SHEDS_2025_Table %>% dplyr::select(
  id,
  homestatus,
  age_corr,
  agegr_corr,
  sex,
  zip,
  region,
  flat_rented,
  house_rented,
  flat_owned,
  house_owned,
  residtype,
  md_bildung,
  md_ek,
  md_wohntyp3_alt,
  md_hhgr,
  md_hh_agep1,
  md_hh_agep2,
  md_hh_agep3,
  md_hh_agep5,
  md_701,
  md_702,
  md_703,
  md_704,
  md_705,
  md_706,
  md_707,
  md_708,
  md_220,
  accom1,
  accom5,
  accom8_1,
  accom8_2,
  accom8_3,
  accom4a3,
  heat1_1,
  heat1_2,
  heat5a1_1,
  heat5a1_2,
  heat5a1_3,
  heat6_1,
  heat6_2,
  heat7,
  elec14a,
  elec7_1,
  elec8_1,
  mob2_3,
  mob2_4,
  mob2_5,
  mob3_change,
  mob3_3,
  mob2_e,
  mob25_1,
  mob25_2,
  mob25_3,
  mob25_4,
  mob25_5,
  mob27,
  mob28,
  mob17_1,
  mob17_2,
  mob17_4,
  mob17_9,
  mob17_11,
  mob17_16,
  mob17_16_other,
  mob18_1,
  mob18_2,
  mob18_4,
  mob18_9,
  mob18_14_other,
  mob19_1,
  mob19_2,
  mob19_4,
  mob19_9,
  mob19_11,
  mob19_17_other,
  psy4_1,
  psy4_2,
  psy4_4,
  psy4_5,
  psy4_6,
  psy4_7,
  psy4_8,
  psy4_9,
  psy4_10,
  psy4_11,
  psy4_12,
  psy4_13,
  psy4_14,
  psy4_15,
  psy4_16,
  psy6_4,
  psy6_7,
  psy7,
  psy8_1,
  psy8_2,
  psy8_3,
  psy8_4,
  psy8_5,
  envpsy2,
  envpsy3,
  soc6_1,
  soc6_2,
  soc6_3,
  soc6_4,
  soc6_6,
  soc6_14,
  soc7_3,
  soc7_6,
  soc7_14,
  enlit5_score,
  enlit18_1,
  enlit18_2,
  enlit18_3,
  enlit18_4,
  enlit18_5,
  enlit19_1,
  enlit19_2,
  enlit19_3,
  enlit19_4,
  enlit19_5)


### agegr_corr : age respondents

# Create dummies with 35-54 as reference

SHEDS_2025_Table_relevant_variables <- SHEDS_2025_Table_relevant_variables %>%
  mutate(
    age_18_34 = ifelse(agegr_corr == "18-34", 1, 0),   # Young vs Middle
    age_55_plus = ifelse(agegr_corr == "55+", 1, 0)    # Old vs Middle
    # 35-54 is reference (both dummies = 0)
  ) %>%
  relocate(age_18_34, age_55_plus, .after = agegr_corr)


### Gender

# Create dummy variables for gender

SHEDS_2025_Table_relevant_variables <-
  SHEDS_2025_Table_relevant_variables %>%
  dplyr::mutate(
    gender_dummy = case_when(
      sex %in% c("f") ~ 1,
      sex %in% c("m")  ~ 0,
    )
  ) %>%
  relocate(gender_dummy, .after = sex)


### Education

# Create dummy variables for education

SHEDS_2025_Table_relevant_variables <- SHEDS_2025_Table_relevant_variables %>%
  mutate(
    # Create 4 educational path categories
    education_path = case_when(
      md_bildung == "University, ETH, university of applied sciences" ~ "University",
      md_bildung == "Apprenticeship" ~ "Apprenticeship",
      md_bildung == "High school" ~ "High_school",
      TRUE ~ "Other"  # Combines vocational + compulsory (7.6%)
    ),

    # Create dummies with University as reference
    educ_apprenticeship = ifelse(education_path == "Apprenticeship", 1, 0),
    educ_high_school = ifelse(education_path == "High_school", 1, 0),
    educ_other = ifelse(education_path == "Other", 1, 0)
    # University is reference
  ) %>%
  relocate(education_path, starts_with("educ_"), .after = md_bildung)


### Households incomes

# Create dummy variables for household income
# Reference category: medium income (6,001-9,000)

SHEDS_2025_Table_relevant_variables <- SHEDS_2025_Table_relevant_variables %>%
  dplyr::mutate(
    income_low = case_when(
      md_ek %in% c("Less than 3,000",
                   "3,000-4,500",
                   "4,501-6,000") ~ 1,
      md_ek %in% c("6,001-9,000",
                   "9,001-12,000",
                   "More than 12,000") ~ 0,
      TRUE ~ NA_real_
    ),
    income_high = case_when(
      md_ek %in% c("9,001-12,000",
                   "More than 12,000") ~ 1,
      md_ek %in% c("Less than 3,000",
                   "3,000-4,500",
                   "4,501-6,000",
                   "6,001-9,000") ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  relocate(income_low, income_high, .after = md_ek)


# Tenant

# Create dummy variables for home status
# Reference category: Tenant

SHEDS_2025_Table_relevant_variables <- SHEDS_2025_Table_relevant_variables %>%
  dplyr::mutate(
    homestatus_dummy = case_when(
      accom1 == "Owner" ~ 1,
      accom1 == "Tenant" ~ 0,
      TRUE ~ NA_real_
    )
  )


# Living area

# Create dummy variables for living area
# Reference category: Countryside (campagne)

SHEDS_2025_Table_relevant_variables <-
  SHEDS_2025_Table_relevant_variables %>%
  dplyr::mutate(
    city_dummy = case_when(
      md_wohntyp3_alt == "City" ~ 1,
      md_wohntyp3_alt %in% c("Agglomeration", "Countryside") ~ 0,
      TRUE ~ NA_real_
    ),
    agglomeration_dummy = case_when(
      md_wohntyp3_alt == "Agglomeration" ~ 1,
      md_wohntyp3_alt %in% c("City", "Countryside") ~ 0,
      TRUE ~ NA_real_
    )
    # Countryside is reference: both dummies = 0
  ) %>%
  relocate(city_dummy, agglomeration_dummy, .after = md_wohntyp3_alt)


# Linguistic region

# Create dummy variables for linguistic region
# Reference category: Swissgerman

SHEDS_2025_Table_relevant_variables <- SHEDS_2025_Table_relevant_variables %>%
  dplyr::mutate(
    romandie_dummy = case_when(
      region == "Suisse romande" ~ 1,
      region %in% c("Alpen und Voralpen",
                    "Westmittelland",
                    "Ostmittelland") ~ 0,
      region == "Ticino" ~ 0,
      TRUE ~ NA_real_
    ),
    ticino_dummy = case_when(
      region == "Ticino" ~ 1,
      region %in% c("Alpen und Voralpen",
                    "Westmittelland",
                    "Ostmittelland") ~ 0,
      region == "Suisse romande" ~ 0,
      TRUE ~ NA_real_
    )
    # Swiss German is reference: both dummies = 0
  ) %>%
  relocate(romandie_dummy, ticino_dummy, .after = region)



########################################################################


# Create column EQUALITY: equal opportunity for all

SHEDS_2025_Table_relevant_variables <-
  SHEDS_2025_Table_relevant_variables %>%
  dplyr::mutate(
    EQUALITY_Values = case_when(
      psy4_1 == "Extremely important" ~ 5,
      psy4_1 == "4" ~ 4,
      psy4_1 == "3" ~ 3,
      psy4_1 == "2" ~ 2,
      psy4_1 == "Not important" ~ 1,
      psy4_1 == "dna" ~ NA_real_,
      TRUE ~ NA_real_
    )
  ) %>%
  relocate(EQUALITY_Values, .after = psy4_1)


# Create column WEALTH_MATERIAL_POSSESSIONS_MONEY_Values: material possessions, money

SHEDS_2025_Table_relevant_variables <-
  SHEDS_2025_Table_relevant_variables %>%
  dplyr::mutate(
    WEALTH_MATERIAL_POSSESSIONS_MONEY_Values = case_when(
      psy4_7 == "Extremely important" ~ 5,
      psy4_7 == "4" ~ 4,
      psy4_7 == "3" ~ 3,
      psy4_7 == "2" ~ 2,
      psy4_7 == "Not important" ~ 1,
      psy4_7 == "dna" ~ NA_real_,
      TRUE ~ NA_real_
    )
  ) %>%
  relocate(WEALTH_MATERIAL_POSSESSIONS_MONEY_Values, .after = psy4_7)


# Create column AUTHORITY_Values: the right to lead or command

SHEDS_2025_Table_relevant_variables <-
  SHEDS_2025_Table_relevant_variables %>%
  dplyr::mutate(
    AUTHORITY_Values = case_when(
      psy4_8 == "Extremely important" ~ 5,
      psy4_8 == "4" ~ 4,
      psy4_8 == "3" ~ 3,
      psy4_8 == "2" ~ 2,
      psy4_8 == "Not important" ~ 1,
      psy4_8 == "dna" ~ NA_real_,
      TRUE ~ NA_real_
    )
  ) %>%
  relocate(AUTHORITY_Values, .after = psy4_8)


# Create column RESPECTING_EARTH_Values: harmony with other species

SHEDS_2025_Table_relevant_variables <-
  SHEDS_2025_Table_relevant_variables %>%
  dplyr::mutate(
    RESPECTING_EARTH_Values = case_when(
      psy4_2 == "Extremely important" ~ 5,
      psy4_2 == "4" ~ 4,
      psy4_2 == "3" ~ 3,
      psy4_2 == "2" ~ 2,
      psy4_2 == "Not important" ~ 1,
      psy4_2 == "dna" ~ NA_real_,
      TRUE ~ NA_real_
    )
  ) %>%
  relocate(AUTHORITY_Values, .after = psy4_2)


# Create column UNITY_NATURE_Values:  fitting into nature

SHEDS_2025_Table_relevant_variables <-
  SHEDS_2025_Table_relevant_variables %>%
  dplyr::mutate(
    UNITY_NATURE_Values = case_when(
      psy4_5 == "Extremely important" ~ 5,
      psy4_5 == "4" ~ 4,
      psy4_5 == "3" ~ 3,
      psy4_5 == "2" ~ 2,
      psy4_5 == "Not important" ~ 1,
      psy4_5 == "dna" ~ NA_real_,
      TRUE ~ NA_real_
    )
  ) %>%
  relocate(UNITY_NATURE_Values, .after = psy4_5)


# Create column PROTECTING_ENVIRONMENT_Values:  preserving nature

SHEDS_2025_Table_relevant_variables <-
  SHEDS_2025_Table_relevant_variables %>%
  dplyr::mutate(
    PROTECTING_ENVIRONMENT_Values = case_when(
      psy4_11 == "Extremely important" ~ 5,
      psy4_11 == "4" ~ 4,
      psy4_11 == "3" ~ 3,
      psy4_11 == "2" ~ 2,
      psy4_11 == "Not important" ~ 1,
      psy4_11 == "dna" ~ NA_real_,
      TRUE ~ NA_real_
    )
  ) %>%
  relocate(UNITY_NATURE_Values, .after = psy4_11)


# Create column Social_Justice_Values:correcting injustice, care for the weak

SHEDS_2025_Table_relevant_variables <-
  SHEDS_2025_Table_relevant_variables %>%
  dplyr::mutate(
    Social_Justice_Values = case_when(
      psy4_9 == "Extremely important" ~ 5,
      psy4_9 == "4" ~ 4,
      psy4_9 == "3" ~ 3,
      psy4_9 == "2" ~ 2,
      psy4_9 == "Not important" ~ 1,
      psy4_9 == "dna" ~ NA_real_,
      TRUE ~ NA_real_
    )
  ) %>%
  relocate(Social_Justice_Values, .after = psy4_9)


# Transform variable into integer

SHEDS_2025_Table_relevant_variables <-
  SHEDS_2025_Table_relevant_variables %>%
  dplyr::mutate(
    WEALTH_MATERIAL_POSSESSIONS_MONEY_Values = as.numeric(WEALTH_MATERIAL_POSSESSIONS_MONEY_Values),
    EQUALITY_Values = as.numeric(EQUALITY_Values),
    AUTHORITY_Values = as.numeric(AUTHORITY_Values),
    RESPECTING_EARTH_Values = as.numeric(RESPECTING_EARTH_Values),
    UNITY_NATURE_Values = as.numeric(UNITY_NATURE_Values),
    Social_Justice_Values = as.numeric(Social_Justice_Values),
    PROTECTING_ENVIRONMENT_Values = as.numeric(PROTECTING_ENVIRONMENT_Values)
  )


########################################################################


# Create ordinal dependent variable for wind acceptance

SHEDS_2025_Table_relevant_variables_filtered_enlit18 <-
  SHEDS_2025_Table_relevant_variables %>%
  dplyr::mutate(
    wind_preference_score = case_when(
      enlit18_3 == "Totally agree" ~ 5,
      enlit18_3 == 4 ~ 4,
      enlit18_3 == 3 ~ 3,
      enlit18_3 == 2 ~ 2,
      enlit18_3 == "Totally disagree" ~ 1,
      TRUE ~ NA_real_),
    wind_acceptance_ordered = factor(
      wind_preference_score,
      levels = c(1, 2, 3, 4, 5),
      ordered = TRUE
    )
  )


# Bloc 1 - controls only wind acceptance

ordered_probit_bloc1_wind_acceptance <- polr(
  wind_acceptance_ordered ~
    age_18_34 +
    age_55_plus +
    gender_dummy +
    educ_apprenticeship +
    educ_high_school +
    educ_other +
    city_dummy +
    agglomeration_dummy +
    romandie_dummy +
    ticino_dummy,
  data = SHEDS_2025_Table_relevant_variables_filtered_enlit18,
  method = "probit",
  Hess = TRUE
)

summary(ordered_probit_bloc1_wind_acceptance)
nobs(ordered_probit_bloc1_wind_acceptance)


# Bloc 2 - adding value orientations wind acceptance

ordered_probit_bloc2_wind_acceptance <- polr(
  wind_acceptance_ordered ~
    age_18_34 +
    age_55_plus +
    gender_dummy +
    educ_apprenticeship +
    educ_high_school +
    educ_other +
    city_dummy +
    agglomeration_dummy +
    romandie_dummy +
    ticino_dummy +
    RESPECTING_EARTH_Values +
    UNITY_NATURE_Values +
    PROTECTING_ENVIRONMENT_Values +
    EQUALITY_Values +
    AUTHORITY_Values +
    WEALTH_MATERIAL_POSSESSIONS_MONEY_Values,
  data = SHEDS_2025_Table_relevant_variables_filtered_enlit18,
  method = "probit",
  Hess = TRUE
)

summary(ordered_probit_bloc2_wind_acceptance)
nobs(ordered_probit_bloc2_wind_acceptance)
vif(ordered_probit_bloc2_wind_acceptance)


# --- Align Block 1 to the same sample as Block 2 ---

vars_bloc2 <- c(
  "wind_acceptance_ordered",
  "age_18_34", "age_55_plus", "gender_dummy",
  "educ_apprenticeship", "educ_high_school", "educ_other",
  "city_dummy", "agglomeration_dummy", "romandie_dummy", "ticino_dummy",
  "RESPECTING_EARTH_Values", "UNITY_NATURE_Values",
  "PROTECTING_ENVIRONMENT_Values", "EQUALITY_Values",
  "AUTHORITY_Values", "WEALTH_MATERIAL_POSSESSIONS_MONEY_Values"
)

data_common <- SHEDS_2025_Table_relevant_variables_filtered_enlit18 %>%
  tidyr::drop_na(all_of(vars_bloc2))


nrow(data_common)  # must match the nobs of block2

ordered_probit_bloc1_common <- polr(
  wind_acceptance_ordered ~
    age_18_34 +
    age_55_plus +
    gender_dummy +
    educ_apprenticeship +
    educ_high_school +
    educ_other +
    city_dummy +
    agglomeration_dummy +
    romandie_dummy +
    ticino_dummy,
  data = data_common,
  method = "probit",
  Hess = TRUE
)

summary(ordered_probit_bloc1_common)
nobs(ordered_probit_bloc1_common)  # doit égaler nobs(bloc2)


# Calculate marginal effects AME for al categories - Bloc 2 wind acceptance

for(cat in c("1", "2", "3", "4", "5")) {
  cat_result <- margins(
    ordered_probit_bloc2_wind_acceptance,
    category = cat
  )
  cat("\nCategory:", cat, "\n")
  print(summary(cat_result))
}


########################################################################


# Create preference score from agreement levels for relative preference model

SHEDS_2025_Table_relevant_variables_filtered_enlit18 <-
  SHEDS_2025_Table_relevant_variables_filtered_enlit18 %>%
  dplyr::mutate(
    nuclear_preference_score = case_when(
      enlit18_2 == "Totally agree" ~ 5,
      enlit18_2 == 4 ~ 4,
      enlit18_2 == 3 ~ 3,
      enlit18_2 == 2 ~ 2,
      enlit18_2 == "Totally disagree" ~ 1,
      TRUE ~ NA_real_),
    wind_preference_score = case_when(
      enlit18_3 == "Totally agree" ~ 5,
      enlit18_3 == 4 ~ 4,
      enlit18_3 == 3 ~ 3,
      enlit18_3 == 2 ~ 2,
      enlit18_3 == "Totally disagree" ~ 1,
      TRUE ~ NA_real_)
  ) %>%
  relocate(nuclear_preference_score, wind_preference_score, .after = enlit18_3)


############### 9 categories ordered probit model #########################

# Create difference score
# After creating preference_difference, convert to ordered factor

SHEDS_2025_Table_relevant_variables_filtered_enlit18 <-
  SHEDS_2025_Table_relevant_variables_filtered_enlit18 %>%
  dplyr::mutate(
    preference_difference = wind_preference_score - nuclear_preference_score,

    # Convert to ordered factor (THIS IS NECESSARY)
    preference_ordered_9_categories_model = factor(
      preference_difference,
      levels = c(-4, -3, -2, -1, 0, 1, 2, 3, 4),
      ordered = TRUE
    )
  ) %>%
  relocate(preference_difference, preference_ordered_9_categories_model, .after = wind_preference_score)


# Run ordered probit model for 9 categories bloc 1


ordered_probit_bloc1_9_categories_model <- polr(
  preference_ordered_9_categories_model ~
    age_18_34 +
    age_55_plus +
    gender_dummy +
    educ_apprenticeship +
    educ_high_school +
    educ_other +
    #income_low +
    #income_high +
    #homestatus_dummy +
    city_dummy +
    agglomeration_dummy +
    romandie_dummy +
    ticino_dummy,
  data = SHEDS_2025_Table_relevant_variables_filtered_enlit18,
  method = "probit",
  Hess = TRUE
)

# View results
summary(ordered_probit_bloc1_9_categories_model)
nobs(ordered_probit_bloc1_9_categories_model)



# Run ordered probit model for 9 categories bloc 2

ordered_probit_bloc2_9_categories_model <- polr(
  preference_ordered_9_categories_model ~
    age_18_34 +
    age_55_plus +
    gender_dummy +
    educ_apprenticeship +
    educ_high_school +
    educ_other +
    #income_low +
    #income_high +
    #homestatus_dummy +
    city_dummy +
    agglomeration_dummy +
    romandie_dummy +
    ticino_dummy+
    RESPECTING_EARTH_Values +
    UNITY_NATURE_Values +
    PROTECTING_ENVIRONMENT_Values +
    EQUALITY_Values +
    AUTHORITY_Values +
    WEALTH_MATERIAL_POSSESSIONS_MONEY_Values,
  data = SHEDS_2025_Table_relevant_variables_filtered_enlit18,
  method = "probit",
  Hess = TRUE
)

# View results
summary(ordered_probit_bloc2_9_categories_model)
nobs(ordered_probit_bloc2_9_categories_model)


vif_ordered_probit_bloc2_9_categories_model <- vif(ordered_probit_bloc2_9_categories_model)
print("VIF for Ordered Probit Model:")
print(vif_ordered_probit_bloc2_9_categories_model)


# --- Align Block 1 to the same sample as Block 2 ---

vars_bloc2_relative <- c(
  "preference_ordered_9_categories_model",
  "age_18_34", "age_55_plus", "gender_dummy",
  "educ_apprenticeship", "educ_high_school", "educ_other",
  "city_dummy", "agglomeration_dummy", "romandie_dummy", "ticino_dummy",
  "RESPECTING_EARTH_Values", "UNITY_NATURE_Values",
  "PROTECTING_ENVIRONMENT_Values", "EQUALITY_Values",
  "AUTHORITY_Values", "WEALTH_MATERIAL_POSSESSIONS_MONEY_Values"
)



data_common_relative <- SHEDS_2025_Table_relevant_variables_filtered_enlit18 %>%
  tidyr::drop_na(all_of(vars_bloc2_relative))

nrow(data_common_relative)  # # must match the nobs of block2


ordered_probit_bloc1_9_categories_common <- polr(
  preference_ordered_9_categories_model ~
    age_18_34 + age_55_plus + gender_dummy +
    educ_apprenticeship + educ_high_school + educ_other +
    city_dummy + agglomeration_dummy + romandie_dummy + ticino_dummy,
  data = data_common_relative,
  method = "probit",
  Hess = TRUE
)

summary(ordered_probit_bloc1_9_categories_common)
nobs(ordered_probit_bloc1_9_categories_common)  # must match 4298



##### SIMULATION TABLE

# Récupérer les observations utilisées dans Bloc 2
# extrait exactement les observations qui ont été utilisées pour estimer le modèle Bloc 2
# — c'est-à-dire les ~4,298 observations avec toutes les variables disponibles (SVS items + contrôles)

SHEDS_bloc2 <- model.frame(ordered_probit_bloc2_9_categories_model)

# Ajouter les probabilités prédites
# predict() avec type = "probs" calcule pour chaque observation la probabilité prédite d'être
# dans chacune des 9 catégories (−4 à +4) selon le modèle

predicted_probs <- predict(ordered_probit_bloc2_9_categories_model,
                           type = "probs")

# bind_cols() ajoute les probabilités prédites comme nouvelles colonnes au dataset
# — une colonne par catégorie (−4, −3, ..., +4)

SHEDS_bloc2 <- bind_cols(SHEDS_bloc2, as.data.frame(predicted_probs))

# 4. Calculate SD

SHEDS_bloc2 %>%
  summarise(
    sd_indifferent = sd(`0`),
    sd_pro_wind = sd(`1` + `2` + `3` + `4`),
    sd_pro_nuclear = sd(`-4` + `-3` + `-2` + `-1`)
  )


# national simulation

national_simulation <- SHEDS_bloc2 %>%
  summarise(
    pro_wind = mean(`1` + `2` + `3` + `4`),
    indifferent = mean(`0`),
    pro_nuclear = mean(`-4` + `-3` + `-2` + `-1`)
  )

print("National simulation :")
print(national_simulation)


# Pour Romandie

simulation_romandie <- SHEDS_bloc2 %>%
  group_by(romandie_dummy) %>%
  summarise(
    pro_wind = mean(`1` + `2` + `3` + `4`),
    indifferent = mean(`0`),
    pro_nuclear = mean(`-4` + `-3` + `-2` + `-1`)
  )

print("Simulation for romandie:")
print(simulation_romandie)


# Bootstrap pour intervalles de confiance - National

boot_simulation <- function(data, indices) {
  d <- data[indices, ]
  c(
    pro_wind = mean(d$`1` + d$`2` + d$`3` + d$`4`),
    indifferent = mean(d$`0`),
    pro_nuclear = mean(d$`-4` + d$`-3` + d$`-2` + d$`-1`)
  )
}

set.seed(123)
boot_national <- boot(SHEDS_bloc2, boot_simulation, R = 1000)

print("Confidence intervals - National:")
boot.ci(boot_national, type = "perc", index = 1)  # pro_wind
boot.ci(boot_national, type = "perc", index = 2)  # indifferent
boot.ci(boot_national, type = "perc", index = 3)  # pro_nuclear


# Bootstrap pour Romandie

boot_romandie <- function(data, indices) {
  d <- data[indices, ]
  d_romandie <- d %>% filter(romandie_dummy == 1)
  d_other <- d %>% filter(romandie_dummy == 0)

  c(
    pro_wind_romandie = mean(d_romandie$`1` + d_romandie$`2` + d_romandie$`3` + d_romandie$`4`),
    indifferent_romandie = mean(d_romandie$`0`),
    pro_nuclear_romandie = mean(d_romandie$`-4` + d_romandie$`-3` + d_romandie$`-2` + d_romandie$`-1`),
    pro_wind_other = mean(d_other$`1` + d_other$`2` + d_other$`3` + d_other$`4`),
    indifferent_other = mean(d_other$`0`),
    pro_nuclear_other = mean(d_other$`-4` + d_other$`-3` + d_other$`-2` + d_other$`-1`)
  )
}

set.seed(123)
boot_region <- boot(SHEDS_bloc2, boot_romandie, R = 1000)

print("Confidence intervals - Romandie:")
boot.ci(boot_region, type = "perc", index = 1)  # pro_wind_romandie
boot.ci(boot_region, type = "perc", index = 2)  # indifferent_romandie
boot.ci(boot_region, type = "perc", index = 3)  # pro_nuclear_romandie

print("Confidence intervals - Other regions:")
boot.ci(boot_region, type = "perc", index = 4)  # pro_wind_other
boot.ci(boot_region, type = "perc", index = 5)  # indifferent_other
boot.ci(boot_region, type = "perc", index = 6)  # pro_nuclear_other



# Calculate marginal effects AME for al categories - Bloc 2

# Pour chaque catégorie

for(cat in c("-4", "-3", "-2", "-1", "0", "1", "2", "3", "4")) {
  cat_result <- margins(
    ordered_probit_bloc2_9_categories_model,
    category = cat
  )
  cat("\nCategory:", cat, "\n")
  print(summary(cat_result))
}





#########################################################


# Calculate mean, SD, min, max for each variable
# Dependants variables (preference_difference et wind_preference_score) and 6 independants variables
# Table 6: Descriptive Statistics — Dependent Variable

SHEDS_2025_Table_relevant_variables_filtered_enlit18 %>%
  dplyr::summarise(
    across(
      c(wind_preference_score,
        preference_difference,
        EQUALITY_Values,
        RESPECTING_EARTH_Values,
        UNITY_NATURE_Values,
        PROTECTING_ENVIRONMENT_Values,
        AUTHORITY_Values,
        WEALTH_MATERIAL_POSSESSIONS_MONEY_Values),
      list(
        mean = \(x) round(mean(x, na.rm=TRUE), 2),
        sd   = \(x) round(sd(x, na.rm=TRUE), 2),
        min  = \(x) min(x, na.rm=TRUE),
        max  = \(x) max(x, na.rm=TRUE)
      )
    )
  ) %>%
  tidyr::pivot_longer(everything()) %>%
  print(n=100)


# vérifier combien d'observations valides tu as par variabl

SHEDS_2025_Table_relevant_variables_filtered_enlit18 %>%
  dplyr::summarise(
    across(
      c(wind_preference_score,
        preference_difference,
        EQUALITY_Values,
        RESPECTING_EARTH_Values,
        UNITY_NATURE_Values,
        PROTECTING_ENVIRONMENT_Values,
        AUTHORITY_Values,
        WEALTH_MATERIAL_POSSESSIONS_MONEY_Values),
      \(x) sum(!is.na(x))
    )
  ) %>%
  tidyr::pivot_longer(everything()) %>%
  print(n=100)


#########################################################



### Stats descriptives

# Échantillon analytique complet

# Filter on:

analytical_sample <-
  SHEDS_2025_Table_relevant_variables_filtered_enlit18 %>%
  filter(complete.cases(dplyr::select(.,
    EQUALITY_Values, AUTHORITY_Values,
    WEALTH_MATERIAL_POSSESSIONS_MONEY_Values,
    preference_difference,
    gender_dummy,
    age_18_34,
    educ_apprenticeship,
    city_dummy,
    romandie_dummy)))

nrow(analytical_sample)





# Calcul proportion

analytical_sample %>%
  dplyr::summarise(
    across(
      c(gender_dummy,
        age_18_34, age_55_plus,
        educ_apprenticeship,
        educ_high_school,
        educ_other,
        city_dummy,
        agglomeration_dummy,
        romandie_dummy,
        ticino_dummy),
      \(x) round(mean(x, na.rm=TRUE)*100, 1)
    )
  ) %>%
  tidyr::pivot_longer(everything()) %>%
  print(n=20)

# Distribution 9 catégories
analytical_sample %>%
  count(preference_ordered_9_categories_model) %>%
  mutate(pct = round(n/sum(n)*100, 1)) %>%
  print()



### T-Test


# Analytical sample: complete cases on all model predictors (6 SVS items + controls)
# Used for descriptive stats and Romandie t-test, aligned with regression samples

analytical_sample_6items <- SHEDS_2025_Table_relevant_variables_filtered_enlit18 %>%
  filter(complete.cases(dplyr::select(.,
                                      age_18_34, age_55_plus, gender_dummy,
                                      educ_apprenticeship, educ_high_school, educ_other,
                                      city_dummy, agglomeration_dummy, romandie_dummy, ticino_dummy,
                                      RESPECTING_EARTH_Values, UNITY_NATURE_Values,
                                      PROTECTING_ENVIRONMENT_Values, EQUALITY_Values,
                                      AUTHORITY_Values, WEALTH_MATERIAL_POSSESSIONS_MONEY_Values)))

nrow(analytical_sample_6items)

# List of 6 SVS value-based items
value_items <- c("EQUALITY_Values", "RESPECTING_EARTH_Values", "UNITY_NATURE_Values",
                 "PROTECTING_ENVIRONMENT_Values", "AUTHORITY_Values",
                 "WEALTH_MATERIAL_POSSESSIONS_MONEY_Values")

# T-test: compare mean value scores between Romandie and rest of Switzerland
ttest_romandie_analytical <- purrr::map_dfr(value_items, function(var) {
  d <- analytical_sample_6items

  group_romandie <- d[[var]][d$romandie_dummy == 1]
  group_rest     <- d[[var]][d$romandie_dummy == 0]

  test <- t.test(group_romandie, group_rest)

  tibble(
    Variable = var,
    N_Romandie = sum(!is.na(group_romandie)),
    N_Rest = sum(!is.na(group_rest)),
    Mean_Romandie = round(mean(group_romandie, na.rm = TRUE), 2),
    SD_Romandie = round(sd(group_romandie, na.rm = TRUE), 2),
    Mean_Rest = round(mean(group_rest, na.rm = TRUE), 2),
    SD_Rest = round(sd(group_rest, na.rm = TRUE), 2),
    Diff = round(mean(group_romandie, na.rm = TRUE) - mean(group_rest, na.rm = TRUE), 2),
    t_value = round(test$statistic, 2),
    p_value = round(test$p.value, 4)
  )
})

print(ttest_romandie_analytical, n = Inf)



