getwd()
setwd('/Users/fabio/Desktop')
library(dplyr)
library(ggplot2)
library(tibble)
library(MASS)  # for polr() function
library(car)
library(pscl) # Pseudo R-squared measures for logit ordered model
library(DescTools) # # For binary model
library(reshape2)
library(dplyr)
library(gridExtra)
library(cowplot)



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
  psy4_5,
  psy4_6,
  psy4_7,
  psy4_8,
  psy4_9,
  psy4_11,
  psy4_13,
  psy4_14,
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




### Create regional subset

SHEDS_swiss_german <- SHEDS_2025_Table_relevant_variables %>%
  filter(region %in% c("Alpen und Voralpen", "Westmittelland", "Ostmittelland"))

SHEDS_romandie <- SHEDS_2025_Table_relevant_variables %>%
  filter(region == "Suisse romande")

SHEDS_ticino <- SHEDS_2025_Table_relevant_variables %>%
  filter(region == "Ticino")






### Descriptive part: combination relative preference


## filter na value for question enlit_18_2 & enlit_18_3

# enlit_18_2: Nuclear power should be a part of the future electricity supply in Switzerland.
# enlit_18_3: Wind energy should be an essential part of the future electricity supply in Switzerland


### Switzerland

SHEDS_2025_Table_relevant_variables_preference_difference_score <- SHEDS_2025_Table_relevant_variables %>%
  filter(
    !is.na(enlit18_2) &
      !is.na(enlit18_3)
  )


# Create preference score from agreement levels for national level

SHEDS_2025_Table_relevant_variables_preference_difference_score <-
  SHEDS_2025_Table_relevant_variables_preference_difference_score %>%
  dplyr::mutate(
    nuclear_preference_score_national_level = case_when(
      enlit18_2 == "Totally agree" ~ 5,
      enlit18_2 == 4 ~ 4,
      enlit18_2 == 3 ~ 3,
      enlit18_2 == 2 ~ 2,
      enlit18_2 == "Totally disagree" ~ 1),
    wind_preference_score_national_level = case_when(
      enlit18_3 == "Totally agree" ~ 5,
      enlit18_3 == 4 ~ 4,
      enlit18_3 == 3 ~ 3,
      enlit18_3 == 2 ~ 2,
      enlit18_3 == "Totally disagree" ~ 1)
  ) %>%
  relocate(nuclear_preference_score_national_level, wind_preference_score_national_level, .after = enlit18_3)



# Create difference score
# After creating preference_difference, convert to ordered factor

SHEDS_2025_Table_relevant_variables_preference_difference_score <-
  SHEDS_2025_Table_relevant_variables_preference_difference_score %>%
  dplyr::mutate(
    preference_difference = wind_preference_score_national_level - nuclear_preference_score_national_level,

    # Convert to ordered factor
    preference_ordered_9_categories_model = factor(
      preference_difference,
      levels = c(-4, -3, -2, -1, 0, 1, 2, 3, 4),
      ordered = TRUE
    )
  ) %>%
  relocate(preference_difference, preference_ordered_9_categories_model, .after = wind_preference_score_national_level)


# See how many observations in each difference score

table(SHEDS_2025_Table_relevant_variables_preference_difference_score$preference_difference)


# Create cross-tabulation

combination_table <- table(
  Nuclear = SHEDS_2025_Table_relevant_variables_preference_difference_score$nuclear_preference_score_national_level,
  Wind = SHEDS_2025_Table_relevant_variables_preference_difference_score$wind_preference_score_national_level
)


# As proportion (optional)

prop_table_combination <- prop.table(combination_table) * 100  # Percentages

# REVERSE the rows
prop_table_combination <- prop_table_combination[5:1, ]
print(prop_table_combination)


# Round for readability

prop_table_combination <- round(prop_table_combination, 2)


# Convert to long format

combination_long <- melt(prop_table_combination)
names(combination_long) <- c("Nuclear", "Wind", "Percentage")

# Définir les cellules modérées  ← AJOUTE ICI
moderate_cells <- data.frame(
  Nuclear = c(2, 3, 3, 3, 4, 4, 4, 5, 5),
  Wind    = c(3, 2, 3, 4, 3, 4, 5, 4, 5)
)


# Create general heatmap for Switzerland

ggplot(combination_long, aes(x = Wind, y = Nuclear, fill = Percentage)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Percentage, 1)), size = 3) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(
    title = "Distribution of Nuclear vs Wind Support Combinations",
    subtitle = "Scale: 1 = Totally Disagree, 5 = Totally Agree | N = 5,512",
    x = "Wind Energy Support Intensity",
    y = "Nuclear Energy Support Intensity"
  ) +
  theme_minimal() +
  theme(
    plot.subtitle = element_text(size = 9, face = "bold", , color = "gray30")
  )
ggplot(combination_long, aes(x = Wind, y = Nuclear, fill = Percentage)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Percentage, 1)), size = 3) +
  # Encadré modérés
  geom_tile(data = moderate_cells, aes(x = Wind, y = Nuclear),
            fill = NA, color = "#2C5F2D", linewidth = 1.2, inherit.aes = FALSE) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(
    title = "Distribution of Nuclear vs Wind Support Combinations",
    subtitle = "Scale: 1 = Totally Disagree, 5 = Totally Agree | N = 5,512",
    x = "Wind Energy Support Intensity",
    y = "Nuclear Energy Support Intensity"
  ) +
  theme_minimal() +
  theme(
    plot.subtitle = element_text(size = 9, face = "bold", color = "gray30")
  )



### Descriptive part: combination intensity preference

## SHEDS_swiss_german

## filter na value for question enlit_18_2 & enlit_18_3

# enlit_18_2: Nuclear power should be a part of the future electricity supply in Switzerland.
# enlit_18_3: Wind energy should be an essential part of the future electricity supply in Switzerland

SHEDS_2025_swiss_german_preference_difference_score <- SHEDS_swiss_german %>%
  filter(
    !is.na(enlit18_2) &
      !is.na(enlit18_3)
  )


# Create preference score from agreement levels for national level

SHEDS_2025_swiss_german_preference_difference_score <-
  SHEDS_2025_swiss_german_preference_difference_score %>%
  dplyr::mutate(
    nuclear_preference_score_national_level = case_when(
      enlit18_2 == "Totally agree" ~ 5,
      enlit18_2 == 4 ~ 4,
      enlit18_2 == 3 ~ 3,
      enlit18_2 == 2 ~ 2,
      enlit18_2 == "Totally disagree" ~ 1),
    wind_preference_score_national_level = case_when(
      enlit18_3 == "Totally agree" ~ 5,
      enlit18_3 == 4 ~ 4,
      enlit18_3 == 3 ~ 3,
      enlit18_3 == 2 ~ 2,
      enlit18_3 == "Totally disagree" ~ 1)
  ) %>%
  relocate(nuclear_preference_score_national_level, wind_preference_score_national_level, .after = enlit18_3)



# Create difference score
# After creating preference_difference, convert to ordered factor

SHEDS_2025_swiss_german_preference_difference_score <-
  SHEDS_2025_swiss_german_preference_difference_score %>%
  dplyr::mutate(
    preference_difference = wind_preference_score_national_level - nuclear_preference_score_national_level,

    # Convert to ordered factor
    preference_ordered_9_categories_model = factor(
      preference_difference,
      levels = c(-4, -3, -2, -1, 0, 1, 2, 3, 4),
      ordered = TRUE
    )
  ) %>%
  relocate(preference_difference, preference_ordered_9_categories_model, .after = wind_preference_score_national_level)


# See how many observations in each difference score

table(SHEDS_2025_swiss_german_preference_difference_score$preference_difference)


# Create cross-tabulation

combination_table <- table(
  Nuclear = SHEDS_2025_swiss_german_preference_difference_score$nuclear_preference_score_national_level,
  Wind = SHEDS_2025_swiss_german_preference_difference_score$wind_preference_score_national_level
)


# As proportion (optional)

prop_table_combination <- prop.table(combination_table) * 100  # Percentages
prop_table_combination <- prop_table_combination[5:1, ]

print(prop_table_combination)


# Round for readability

prop_table_combination <- round(prop_table_combination, 2)


# Convert to long format

combination_long <- melt(prop_table_combination)
names(combination_long) <- c("Nuclear", "Wind", "Percentage")


# Create general heatmap for Swiss German Part

ggplot(combination_long, aes(x = Wind, y = Nuclear, fill = Percentage)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Percentage, 1)), size = 3) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(
    title = "Distribution of Nuclear vs Wind Support Combinations Swiss German Part",
    subtitle = "Scale: 1 = Totally Disagree, 5 = Totally Agree | N = 3,754",
    x = "Wind Energy Support Intensity",
    y = "Nuclear Energy Support Intensity"
  ) +
  theme_minimal() +
  theme(
    plot.subtitle = element_text(size = 9, face = "bold", , color = "gray30")
  )
ggplot(combination_long, aes(x = Wind, y = Nuclear, fill = Percentage)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Percentage, 1)), size = 3) +
  # Encadré modérés
  geom_tile(data = moderate_cells, aes(x = Wind, y = Nuclear),
            fill = NA, color = "#2C5F2D", linewidth = 1.2, inherit.aes = FALSE) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(
    title = "Distribution of Nuclear vs Wind Support Combinations Swiss German Part",
    subtitle = "Scale: 1 = Totally Disagree, 5 = Totally Agree | N = 3,754",
    x = "Wind Energy Support Intensity",
    y = "Nuclear Energy Support Intensity"
  ) +
  theme_minimal() +
  theme(
    plot.subtitle = element_text(size = 9, face = "bold", color = "gray30")
  )





### Descriptive part: combination intensity preference

## SHEDS_romandie


## filter na value for question enlit_18_2 & enlit_18_3

# enlit_18_2: Nuclear power should be a part of the future electricity supply in Switzerland.
# enlit_18_3: Wind energy should be an essential part of the future electricity supply in Switzerland

SHEDS_2025_SHEDS_romandie_preference_difference_score <- SHEDS_romandie %>%
  filter(
    !is.na(enlit18_2) &
      !is.na(enlit18_3)
  )


# Create preference score from agreement levels for national level

SHEDS_2025_SHEDS_romandie_preference_difference_score <-
  SHEDS_2025_SHEDS_romandie_preference_difference_score %>%
  dplyr::mutate(
    nuclear_preference_score_national_level = case_when(
      enlit18_2 == "Totally agree" ~ 5,
      enlit18_2 == 4 ~ 4,
      enlit18_2 == 3 ~ 3,
      enlit18_2 == 2 ~ 2,
      enlit18_2 == "Totally disagree" ~ 1),
    wind_preference_score_national_level = case_when(
      enlit18_3 == "Totally agree" ~ 5,
      enlit18_3 == 4 ~ 4,
      enlit18_3 == 3 ~ 3,
      enlit18_3 == 2 ~ 2,
      enlit18_3 == "Totally disagree" ~ 1)
  ) %>%
  relocate(nuclear_preference_score_national_level, wind_preference_score_national_level, .after = enlit18_3)



# Create difference score
# After creating preference_difference, convert to ordered factor

SHEDS_2025_SHEDS_romandie_preference_difference_score <-
  SHEDS_2025_SHEDS_romandie_preference_difference_score %>%
  dplyr::mutate(
    preference_difference = wind_preference_score_national_level - nuclear_preference_score_national_level,

    # Convert to ordered factor
    preference_ordered_9_categories_model = factor(
      preference_difference,
      levels = c(-4, -3, -2, -1, 0, 1, 2, 3, 4),
      ordered = TRUE
    )
  ) %>%
  relocate(preference_difference, preference_ordered_9_categories_model, .after = wind_preference_score_national_level)


# See how many observations in each difference score

table(SHEDS_2025_SHEDS_romandie_preference_difference_score$preference_difference)


# Create cross-tabulation

combination_table <- table(
  Nuclear = SHEDS_2025_SHEDS_romandie_preference_difference_score$nuclear_preference_score_national_level,
  Wind = SHEDS_2025_SHEDS_romandie_preference_difference_score$wind_preference_score_national_level
)


# As proportion (optional)

prop_table_combination <- prop.table(combination_table) * 100  # Percentages
prop_table_combination <- prop_table_combination[5:1, ]
print(prop_table_combination)


# Round for readability

prop_table_combination <- round(prop_table_combination, 2)


# Convert to long format

combination_long <- melt(prop_table_combination)
names(combination_long) <- c("Nuclear", "Wind", "Percentage")


# Create general heatmap for Romandie

ggplot(combination_long, aes(x = Wind, y = Nuclear, fill = Percentage)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Percentage, 1)), size = 3) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(
    title = "Distribution of Nuclear vs Wind Support Combinations Romandie",
    subtitle = "Scale: 1 = Totally Disagree, 5 = Totally Agree | N = 1,248",
    x = "Wind Energy Support Intensity",
    y = "Nuclear Energy Support Intensity"
  ) +
  theme_minimal() +
  theme(
    plot.subtitle = element_text(size = 9, face = "bold", , color = "gray30")
  )
ggplot(combination_long, aes(x = Wind, y = Nuclear, fill = Percentage)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Percentage, 1)), size = 3) +
  # Encadré modérés
  geom_tile(data = moderate_cells, aes(x = Wind, y = Nuclear),
            fill = NA, color = "#2C5F2D", linewidth = 1.2, inherit.aes = FALSE) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(
    title = "Distribution of Nuclear vs Wind Support Combinations Romandie",
    subtitle = "Scale: 1 = Totally Disagree, 5 = Totally Agree | N = 1,248",
    x = "Wind Energy Support Intensity",
    y = "Nuclear Energy Support Intensity"
  ) +
  theme_minimal() +
  theme(
    plot.subtitle = element_text(size = 9, face = "bold", color = "gray30")
  )




### Descriptive part: combination intensity preference

## SHEDS_ticino


## filter na value for question enlit_18_2 & enlit_18_3

# enlit_18_2: Nuclear power should be a part of the future electricity supply in Switzerland.
# enlit_18_3: Wind energy should be an essential part of the future electricity supply in Switzerland

SHEDS_2025_SHEDS_ticino_preference_difference_score <- SHEDS_ticino %>%
  filter(
    !is.na(enlit18_2) &
      !is.na(enlit18_3)
  )


# Create preference score from agreement levels for national level

SHEDS_2025_SHEDS_ticino_preference_difference_score <-
  SHEDS_2025_SHEDS_ticino_preference_difference_score %>%
  dplyr::mutate(
    nuclear_preference_score_national_level = case_when(
      enlit18_2 == "Totally agree" ~ 5,
      enlit18_2 == 4 ~ 4,
      enlit18_2 == 3 ~ 3,
      enlit18_2 == 2 ~ 2,
      enlit18_2 == "Totally disagree" ~ 1),
    wind_preference_score_national_level = case_when(
      enlit18_3 == "Totally agree" ~ 5,
      enlit18_3 == 4 ~ 4,
      enlit18_3 == 3 ~ 3,
      enlit18_3 == 2 ~ 2,
      enlit18_3 == "Totally disagree" ~ 1)
  ) %>%
  relocate(nuclear_preference_score_national_level, wind_preference_score_national_level, .after = enlit18_3)



# Create difference score
# After creating preference_difference, convert to ordered factor

SHEDS_2025_SHEDS_ticino_preference_difference_score <-
  SHEDS_2025_SHEDS_ticino_preference_difference_score %>%
  dplyr::mutate(
    preference_difference = wind_preference_score_national_level - nuclear_preference_score_national_level,

    # Convert to ordered factor
    preference_ordered_9_categories_model = factor(
      preference_difference,
      levels = c(-4, -3, -2, -1, 0, 1, 2, 3, 4),
      ordered = TRUE
    )
  ) %>%
  relocate(preference_difference, preference_ordered_9_categories_model, .after = wind_preference_score_national_level)


# See how many observations in each difference score

table(SHEDS_2025_SHEDS_ticino_preference_difference_score$preference_difference)


# Create cross-tabulation

combination_table <- table(
  Nuclear = SHEDS_2025_SHEDS_ticino_preference_difference_score$nuclear_preference_score_national_level,
  Wind = SHEDS_2025_SHEDS_ticino_preference_difference_score$wind_preference_score_national_level
)


# As proportion (optional)

prop_table_combination <- prop.table(combination_table) * 100  # Percentages
prop_table_combination <- prop_table_combination[5:1, ]

print(prop_table_combination)


# Round for readability

prop_table_combination <- round(prop_table_combination, 2)


# Convert to long format

combination_long <- melt(prop_table_combination)
names(combination_long) <- c("Nuclear", "Wind", "Percentage")


# Create general heatmap for Ticino

ggplot(combination_long, aes(x = Wind, y = Nuclear, fill = Percentage)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Percentage, 1)), size = 3) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(
    title = "Distribution of Nuclear vs Wind Support Combinations Ticino",
    subtitle = "Scale: 1 = Totally Disagree, 5 = Totally Agree | N = 509",
    x = "Wind Energy Support Intensity",
    y = "Nuclear Energy Support Intensity"
  ) +
  theme_minimal() +
  theme(
    plot.subtitle = element_text(size = 9, face = "bold", , color = "gray30")
  )
ggplot(combination_long, aes(x = Wind, y = Nuclear, fill = Percentage)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Percentage, 1)), size = 3) +
  # Encadré modérés
  geom_tile(data = moderate_cells, aes(x = Wind, y = Nuclear),
            fill = NA, color = "#2C5F2D", linewidth = 1.2, inherit.aes = FALSE) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(
    title = "Distribution of Nuclear vs Wind Support Combinations Ticino",
    subtitle = "Scale: 1 = Totally Disagree, 5 = Totally Agree | N = 509",
    x = "Wind Energy Support Intensity",
    y = "Nuclear Energy Support Intensity"
  ) +
  theme_minimal() +
  theme(
    plot.subtitle = element_text(size = 9, face = "bold", color = "gray30")
  )



### Create living area subset

SHEDS_City <- SHEDS_2025_Table_relevant_variables %>%
  filter(md_wohntyp3_alt  == "City")

SHEDS_Agglomeration <- SHEDS_2025_Table_relevant_variables %>%
  filter(md_wohntyp3_alt  == "Agglomeration")

SHEDS_Countryside <- SHEDS_2025_Table_relevant_variables %>%
  filter(md_wohntyp3_alt  == "Countryside")




### Descriptive part: combination intensity preference

## SHEDS_City

## filter na value for question enlit_18_2 & enlit_18_3

# enlit_18_2: Nuclear power should be a part of the future electricity supply in Switzerland.
# enlit_18_3: Wind energy should be an essential part of the future electricity supply in Switzerland

SHEDS_2025_SHEDS_City_preference_difference_score <- SHEDS_City %>%
  filter(
    !is.na(enlit18_2) &
      !is.na(enlit18_3)
  )


# Create preference score from agreement levels for national level

SHEDS_2025_SHEDS_City_preference_difference_score <-
  SHEDS_2025_SHEDS_City_preference_difference_score %>%
  dplyr::mutate(
    nuclear_preference_score_national_level = case_when(
      enlit18_2 == "Totally agree" ~ 5,
      enlit18_2 == 4 ~ 4,
      enlit18_2 == 3 ~ 3,
      enlit18_2 == 2 ~ 2,
      enlit18_2 == "Totally disagree" ~ 1),
    wind_preference_score_national_level = case_when(
      enlit18_3 == "Totally agree" ~ 5,
      enlit18_3 == 4 ~ 4,
      enlit18_3 == 3 ~ 3,
      enlit18_3 == 2 ~ 2,
      enlit18_3 == "Totally disagree" ~ 1)
  ) %>%
  relocate(nuclear_preference_score_national_level, wind_preference_score_national_level, .after = enlit18_3)



# Create difference score
# After creating preference_difference, convert to ordered factor

SHEDS_2025_SHEDS_City_preference_difference_score <-
  SHEDS_2025_SHEDS_City_preference_difference_score %>%
  dplyr::mutate(
    preference_difference = wind_preference_score_national_level - nuclear_preference_score_national_level,

    # Convert to ordered factor
    preference_ordered_9_categories_model = factor(
      preference_difference,
      levels = c(-4, -3, -2, -1, 0, 1, 2, 3, 4),
      ordered = TRUE
    )
  ) %>%
  relocate(preference_difference, preference_ordered_9_categories_model, .after = wind_preference_score_national_level)


# See how many observations in each difference score

table(SHEDS_2025_SHEDS_City_preference_difference_score$preference_difference)


# Create cross-tabulation

combination_table <- table(
  Nuclear = SHEDS_2025_SHEDS_City_preference_difference_score$nuclear_preference_score_national_level,
  Wind = SHEDS_2025_SHEDS_City_preference_difference_score$wind_preference_score_national_level
)


# As proportion (optional)

prop_table_combination <- prop.table(combination_table) * 100  # Percentages
prop_table_combination <- prop_table_combination[5:1, ]

print(prop_table_combination)


# Round for readability

prop_table_combination <- round(prop_table_combination, 2)


# Convert to long format

combination_long <- melt(prop_table_combination)
names(combination_long) <- c("Nuclear", "Wind", "Percentage")


# Create general heatmap for City

ggplot(combination_long, aes(x = Wind, y = Nuclear, fill = Percentage)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Percentage, 1)), size = 3) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(
    title = "Distribution of Nuclear vs Wind Support Combinations City",
    subtitle = "Scale: 1 = Totally Disagree, 5 = Totally Agree | N = 2,533",
    x = "Wind Energy Support Intensity",
    y = "Nuclear Energy Support Intensity"
  ) +
  theme_minimal() +
  theme(
    plot.subtitle = element_text(size = 9, face = "bold", , color = "gray30")
  )
ggplot(combination_long, aes(x = Wind, y = Nuclear, fill = Percentage)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Percentage, 1)), size = 3) +
  # Encadré modérés
  geom_tile(data = moderate_cells, aes(x = Wind, y = Nuclear),
            fill = NA, color = "#2C5F2D", linewidth = 1.2, inherit.aes = FALSE) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(
    title = "Distribution of Nuclear vs Wind Support Combinations City",
    subtitle = "Scale: 1 = Totally Disagree, 5 = Totally Agree | N = 2,533",
    x = "Wind Energy Support Intensity",
    y = "Nuclear Energy Support Intensity"
  ) +
  theme_minimal() +
  theme(
    plot.subtitle = element_text(size = 9, face = "bold", color = "gray30")
  )




### Descriptive part: combination intensity preference

## SHEDS_Agglomeration

## filter na value for question enlit_18_2 & enlit_18_3

# enlit_18_2: Nuclear power should be a part of the future electricity supply in Switzerland.
# enlit_18_3: Wind energy should be an essential part of the future electricity supply in Switzerland

SHEDS_2025_SHEDS_Agglomeration_preference_difference_score <- SHEDS_Agglomeration %>%
  filter(
    !is.na(enlit18_2) &
      !is.na(enlit18_3)
  )


# Create preference score from agreement levels for national level

SHEDS_2025_SHEDS_Agglomeration_preference_difference_score <-
  SHEDS_2025_SHEDS_Agglomeration_preference_difference_score %>%
  dplyr::mutate(
    nuclear_preference_score_national_level = case_when(
      enlit18_2 == "Totally agree" ~ 5,
      enlit18_2 == 4 ~ 4,
      enlit18_2 == 3 ~ 3,
      enlit18_2 == 2 ~ 2,
      enlit18_2 == "Totally disagree" ~ 1),
    wind_preference_score_national_level = case_when(
      enlit18_3 == "Totally agree" ~ 5,
      enlit18_3 == 4 ~ 4,
      enlit18_3 == 3 ~ 3,
      enlit18_3 == 2 ~ 2,
      enlit18_3 == "Totally disagree" ~ 1)
  ) %>%
  relocate(nuclear_preference_score_national_level, wind_preference_score_national_level, .after = enlit18_3)



# Create difference score
# After creating preference_difference, convert to ordered factor

SHEDS_2025_SHEDS_Agglomeration_preference_difference_score <-
  SHEDS_2025_SHEDS_Agglomeration_preference_difference_score %>%
  dplyr::mutate(
    preference_difference = wind_preference_score_national_level - nuclear_preference_score_national_level,

    # Convert to ordered factor
    preference_ordered_9_categories_model = factor(
      preference_difference,
      levels = c(-4, -3, -2, -1, 0, 1, 2, 3, 4),
      ordered = TRUE
    )
  ) %>%
  relocate(preference_difference, preference_ordered_9_categories_model, .after = wind_preference_score_national_level)


# See how many observations in each difference score

table(SHEDS_2025_SHEDS_Agglomeration_preference_difference_score$preference_difference)


# Create cross-tabulation

combination_table <- table(
  Nuclear = SHEDS_2025_SHEDS_Agglomeration_preference_difference_score$nuclear_preference_score_national_level,
  Wind = SHEDS_2025_SHEDS_Agglomeration_preference_difference_score$wind_preference_score_national_level
)


# As proportion (optional)

prop_table_combination <- prop.table(combination_table) * 100  # Percentages
prop_table_combination <- prop_table_combination[5:1, ]

print(prop_table_combination)


# Round for readability

prop_table_combination <- round(prop_table_combination, 2)


# Convert to long format

combination_long <- melt(prop_table_combination)
names(combination_long) <- c("Nuclear", "Wind", "Percentage")


# Create general heatmap for Agglomeration

ggplot(combination_long, aes(x = Wind, y = Nuclear, fill = Percentage)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Percentage, 1)), size = 3) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(
    title = "Distribution of Nuclear vs Wind Support Combinations Agglomeration",
    subtitle = "Scale: 1 = Totally Disagree, 5 = Totally Agree | N = 1,704",
    x = "Wind Energy Support Intensity",
    y = "Nuclear Energy Support Intensity"
  ) +
  theme_minimal() +
  theme(
    plot.subtitle = element_text(size = 9, face = "bold", , color = "gray30")
  )
ggplot(combination_long, aes(x = Wind, y = Nuclear, fill = Percentage)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Percentage, 1)), size = 3) +
  # Encadré modérés
  geom_tile(data = moderate_cells, aes(x = Wind, y = Nuclear),
            fill = NA, color = "#2C5F2D", linewidth = 1.2, inherit.aes = FALSE) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(
    title = "Distribution of Nuclear vs Wind Support Combinations Agglomeration",
    subtitle = "Scale: 1 = Totally Disagree, 5 = Totally Agree | N = 1,704",
    x = "Wind Energy Support Intensity",
    y = "Nuclear Energy Support Intensity"
  ) +
  theme_minimal() +
  theme(
    plot.subtitle = element_text(size = 9, face = "bold", color = "gray30")
  )



### Descriptive part: combination intensity preference

## SHEDS_Countryside

## filter na value for question enlit_18_2 & enlit_18_3

# enlit_18_2: Nuclear power should be a part of the future electricity supply in Switzerland.
# enlit_18_3: Wind energy should be an essential part of the future electricity supply in Switzerland

SHEDS_2025_SHEDS_Countryside_preference_difference_score <- SHEDS_Countryside %>%
  filter(
    !is.na(enlit18_2) &
      !is.na(enlit18_3)
  )


# Create preference score from agreement levels for national level

SHEDS_2025_SHEDS_Countryside_preference_difference_score <-
  SHEDS_2025_SHEDS_Countryside_preference_difference_score %>%
  dplyr::mutate(
    nuclear_preference_score_national_level = case_when(
      enlit18_2 == "Totally agree" ~ 5,
      enlit18_2 == 4 ~ 4,
      enlit18_2 == 3 ~ 3,
      enlit18_2 == 2 ~ 2,
      enlit18_2 == "Totally disagree" ~ 1),
    wind_preference_score_national_level = case_when(
      enlit18_3 == "Totally agree" ~ 5,
      enlit18_3 == 4 ~ 4,
      enlit18_3 == 3 ~ 3,
      enlit18_3 == 2 ~ 2,
      enlit18_3 == "Totally disagree" ~ 1)
  ) %>%
  relocate(nuclear_preference_score_national_level, wind_preference_score_national_level, .after = enlit18_3)



# Create difference score
# After creating preference_difference, convert to ordered factor

SHEDS_2025_SHEDS_Countryside_preference_difference_score <-
  SHEDS_2025_SHEDS_Countryside_preference_difference_score %>%
  dplyr::mutate(
    preference_difference = wind_preference_score_national_level - nuclear_preference_score_national_level,

    # Convert to ordered factor
    preference_ordered_9_categories_model = factor(
      preference_difference,
      levels = c(-4, -3, -2, -1, 0, 1, 2, 3, 4),
      ordered = TRUE
    )
  ) %>%
  relocate(preference_difference, preference_ordered_9_categories_model, .after = wind_preference_score_national_level)


# See how many observations in each difference score

table(SHEDS_2025_SHEDS_Countryside_preference_difference_score$preference_difference)


# Create cross-tabulation

combination_table <- table(
  Nuclear = SHEDS_2025_SHEDS_Countryside_preference_difference_score$nuclear_preference_score_national_level,
  Wind = SHEDS_2025_SHEDS_Countryside_preference_difference_score$wind_preference_score_national_level
)


# As proportion (optional)

prop_table_combination <- prop.table(combination_table) * 100  # Percentages
prop_table_combination <- prop_table_combination[5:1, ]

print(prop_table_combination)


# Round for readability

prop_table_combination <- round(prop_table_combination, 2)


# Convert to long format

combination_long <- melt(prop_table_combination)
names(combination_long) <- c("Nuclear", "Wind", "Percentage")


# Create general heatmap for Countryside

ggplot(combination_long, aes(x = Wind, y = Nuclear, fill = Percentage)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Percentage, 1)), size = 3) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(
    title = "Distribution of Nuclear vs Wind Support Combinations Countryside",
    subtitle = "Scale: 1 = Totally Disagree, 5 = Totally Agree | N = 1,272",
    x = "Wind Energy Support Intensity",
    y = "Nuclear Energy Support Intensity"
  ) +
  theme_minimal() +
  theme(
    plot.subtitle = element_text(size = 9, face = "bold", , color = "gray30")
  )
ggplot(combination_long, aes(x = Wind, y = Nuclear, fill = Percentage)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Percentage, 1)), size = 3) +
  # Encadré modérés
  geom_tile(data = moderate_cells, aes(x = Wind, y = Nuclear),
            fill = NA, color = "#2C5F2D", linewidth = 1.2, inherit.aes = FALSE) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(
    title = "Distribution of Nuclear vs Wind Support Combinations Countryside",
    subtitle = "Scale: 1 = Totally Disagree, 5 = Totally Agree | N = 1,272",
    x = "Wind Energy Support Intensity",
    y = "Nuclear Energy Support Intensity"
  ) +
  theme_minimal() +
  theme(
    plot.subtitle = element_text(size = 9, face = "bold", color = "gray30")
  )



##### Creation Bar Chart pour Wind acceptance Linguistic,Living Area #####

# Verify N for each segment
list(
  National = SHEDS_2025_Table_relevant_variables,
  Swiss_German = SHEDS_swiss_german,
  Romandie = SHEDS_romandie,
  Ticino = SHEDS_ticino,
  City = SHEDS_City,
  Agglomeration = SHEDS_Agglomeration,
  Countryside = SHEDS_Countryside,
  # Nouveaux segments
  male = SHEDS_2025_Table_relevant_variables %>% filter(sex == 'm'),
  female = SHEDS_2025_Table_relevant_variables %>% filter(sex == 'f'),
  university = SHEDS_2025_Table_relevant_variables %>% filter(md_bildung == "University, ETH, university of applied sciences"),
  appprenticeship = SHEDS_2025_Table_relevant_variables %>% filter(md_bildung == "Apprenticeship")
) %>%
  purrr::map_df(~sum(!is.na(.x$enlit18_3)), .id = "group")



# Distribution wind acceptance - National => Bar Charts
# Fonction réutilisable

plot_wind_acceptance <- function(data, title, n) {
  data %>%
    filter(!is.na(enlit18_3)) %>%
    mutate(wind_score = case_when(
      enlit18_3 == "Totally agree" ~ 5,
      enlit18_3 == 4 ~ 4,
      enlit18_3 == 3 ~ 3,
      enlit18_3 == 2 ~ 2,
      enlit18_3 == "Totally disagree" ~ 1
    )) %>%
    count(wind_score) %>%
    mutate(pct = round(n/sum(n)*100, 1)) %>%
    ggplot(aes(x = factor(wind_score), y = pct)) +
    geom_bar(stat = "identity", fill = "#D9D9D9") +
    geom_text(aes(label = paste0(pct, "%")), vjust = -0.5, size = 3) +
    labs(title = title, subtitle = paste0("N = ", n),
         x = "Agreement (1 = Totally Disagree, 5 = Totally Agree)",
         y = "Percentage (%)") +
    theme_minimal()
}

plot_wind_acceptance(SHEDS_2025_Table_relevant_variables,
                     "Distribution of Wind Energy Acceptance — National Level", "5,515")
plot_wind_acceptance(SHEDS_swiss_german,
                     "Distribution of Wind Energy Acceptance - German-speaking Switzerland", "3,755")

plot_wind_acceptance(SHEDS_romandie,
                     "Distribution of Wind Energy Acceptance - Romandie", "1,249")

plot_wind_acceptance(SHEDS_ticino,
                     "Distribution of Wind Energy Acceptance - Ticino", "510")

plot_wind_acceptance(SHEDS_City,
                     "Distribution of Wind Energy Acceptance - City", "2,534")

plot_wind_acceptance(SHEDS_Agglomeration,
                     "Distribution of Wind Energy Acceptance - Agglomeration", "1,706")

plot_wind_acceptance(SHEDS_Countryside,
                     "Distribution of Wind Energy Acceptance - Countryside", "1,272")

# Nouveaux segments

plot_wind_acceptance(SHEDS_2025_Table_relevant_variables %>% filter(sex == 'm'),
                     "Distribution of Wind Energy Acceptance — Male", "2,710")

plot_wind_acceptance(SHEDS_2025_Table_relevant_variables %>% filter(sex == 'f'),
                     "Distribution of Wind Energy Acceptance — Female", "2,777")

plot_wind_acceptance(SHEDS_2025_Table_relevant_variables %>% filter(md_bildung == "University, ETH, university of applied sciences"),
                     "Distribution of Wind Energy Acceptance — University", "2,914")

plot_wind_acceptance(SHEDS_2025_Table_relevant_variables %>% filter(md_bildung == "Apprenticeship"),
                     "Distribution of Wind Energy Acceptance — Apprenticeship", "1,450")



# Distribution wind acceptance => only number

list(
  National = SHEDS_2025_Table_relevant_variables,
  Swiss_German = SHEDS_swiss_german,
  Romandie = SHEDS_romandie,
  Ticino = SHEDS_ticino,
  City = SHEDS_City,
  Agglomeration = SHEDS_Agglomeration,
  Countryside = SHEDS_Countryside,
  # Nouveaux segments
  Male = SHEDS_2025_Table_relevant_variables %>% filter(sex == 'm'),
  Female = SHEDS_2025_Table_relevant_variables %>% filter(sex == 'f'),
  University = SHEDS_2025_Table_relevant_variables %>% filter(md_bildung == "University, ETH, university of applied sciences"),
  Apprenticeship = SHEDS_2025_Table_relevant_variables %>% filter(md_bildung == "Apprenticeship")

) %>%
  purrr::map_df(~{
    .x %>%
      filter(!is.na(enlit18_3)) %>%
      mutate(wind_score = case_when(
        enlit18_3 == "Totally agree" ~ 5,
        enlit18_3 == 4 ~ 4,
        enlit18_3 == 3 ~ 3,
        enlit18_3 == 2 ~ 2,
        enlit18_3 == "Totally disagree" ~ 1
      )) %>%
      count(wind_score) %>%
      mutate(pct = round(n/sum(n)*100, 1))
  }, .id = "group") %>% print(n = 200)





##### Creation renewables #####


#SHEDS_2025_Table_relevant_variables %>%
#  dplyr::summarise(
#    wind_r1 = sum(enlit19_4 == 1, na.rm = TRUE),
#    hydro_r1 = sum(enlit19_3 == 1, na.rm = TRUE),
#    solar_r1 = sum(enlit19_5 == 1, na.rm = TRUE),
#    wind_r2 = sum(enlit19_4 == 2, na.rm = TRUE),
#    hydro_r2 = sum(enlit19_3 == 2, na.rm = TRUE),
#    solar_r2 = sum(enlit19_5 == 2, na.rm = TRUE),
#    wind_r3 = sum(enlit19_4 == 3, na.rm = TRUE),
#    hydro_r3 = sum(enlit19_3 == 3, na.rm = TRUE),
#    solar_r3 = sum(enlit19_5 == 3, na.rm = TRUE)
#  ) %>%
#  dplyr::mutate(
#   total_r1 = wind_r1 + hydro_r1 + solar_r1,
#   total_r2 = wind_r2 + hydro_r2 + solar_r2,
#    total_r3 = wind_r3 + hydro_r3 + solar_r3,
#    pct_wind_r1 = round(wind_r1/total_r1*100, 1),
#    pct_hydro_r1 = round(hydro_r1/total_r1*100, 1),
#    pct_solar_r1 = round(solar_r1/total_r1*100, 1),
#    pct_wind_r2 = round(wind_r2/total_r2*100, 1),
#   pct_hydro_r2 = round(hydro_r2/total_r2*100, 1),
#    pct_solar_r2 = round(solar_r2/total_r2*100, 1),
#    pct_wind_r3 = round(wind_r3/total_r3*100, 1),
#    pct_hydro_r3 = round(hydro_r3/total_r3*100, 1),
#    pct_solar_r3 = round(solar_r3/total_r3*100, 1)
#  ) %>%
#  dplyr::select(starts_with("pct")) %>%
#  tidyr::pivot_longer(everything()) %>%
#  print(n = 20)




SHEDS_2025_Table_relevant_variables %>%
  dplyr::summarise(
    wind_r1 = sum(enlit19_4 == 1, na.rm = TRUE),
    hydro_r1 = sum(enlit19_3 == 1, na.rm = TRUE),
    solar_r1 = sum(enlit19_5 == 1, na.rm = TRUE),
    nuclear_r1 = sum(enlit19_1 == 1, na.rm = TRUE),
    wind_r2 = sum(enlit19_4 == 2, na.rm = TRUE),
    hydro_r2 = sum(enlit19_3 == 2, na.rm = TRUE),
    solar_r2 = sum(enlit19_5 == 2, na.rm = TRUE),
    nuclear_r2 = sum(enlit19_1 == 2, na.rm = TRUE),
    wind_r3 = sum(enlit19_4 == 3, na.rm = TRUE),
    hydro_r3 = sum(enlit19_3 == 3, na.rm = TRUE),
    solar_r3 = sum(enlit19_5 == 3, na.rm = TRUE),
    nuclear_r3 = sum(enlit19_1 == 3, na.rm = TRUE),
    wind_r4 = sum(enlit19_4 == 4, na.rm = TRUE),
    hydro_r4 = sum(enlit19_3 == 4, na.rm = TRUE),
    solar_r4 = sum(enlit19_5 == 4, na.rm = TRUE),
    nuclear_r4 = sum(enlit19_1 == 4, na.rm = TRUE)
  ) %>%
  dplyr::mutate(
    total_r1 = wind_r1 + hydro_r1 + solar_r1 + nuclear_r1,
    total_r2 = wind_r2 + hydro_r2 + solar_r2 + nuclear_r2,
    total_r3 = wind_r3 + hydro_r3 + solar_r3 + nuclear_r3,
    total_r4 = wind_r4 + hydro_r4 + solar_r4 + nuclear_r4,
    pct_wind_r1 = round(wind_r1/total_r1*100, 1),
    pct_hydro_r1 = round(hydro_r1/total_r1*100, 1),
    pct_solar_r1 = round(solar_r1/total_r1*100, 1),
    pct_nuclear_r1 = round(nuclear_r1/total_r1*100, 1),
    pct_wind_r2 = round(wind_r2/total_r2*100, 1),
    pct_hydro_r2 = round(hydro_r2/total_r2*100, 1),
    pct_solar_r2 = round(solar_r2/total_r2*100, 1),
    pct_nuclear_r2 = round(nuclear_r2/total_r2*100, 1),
    pct_wind_r3 = round(wind_r3/total_r3*100, 1),
    pct_hydro_r3 = round(hydro_r3/total_r3*100, 1),
    pct_solar_r3 = round(solar_r3/total_r3*100, 1),
    pct_nuclear_r3 = round(nuclear_r3/total_r3*100, 1),
    pct_wind_r4 = round(wind_r4/total_r4*100, 1),
    pct_hydro_r4 = round(hydro_r4/total_r4*100, 1),
    pct_solar_r4 = round(solar_r4/total_r4*100, 1),
    pct_nuclear_r4 = round(nuclear_r4/total_r4*100, 1)
  ) %>%
  dplyr::select(starts_with("pct")) %>%
  tidyr::pivot_longer(everything()) %>%
  print(n = 40)


ranks_data <- data.frame(
  rank = rep(c("Rank 1", "Rank 2", "Rank 3", "Rank 4"), each = 4),
  source = rep(c("Wind", "Hydro", "Solar", "Nuclear"), 4),
  pct = c(7.1, 42.3, 34.4, 16.2,
          19.3, 34.9, 36.7, 9.0,
          45.0, 20.5, 23.8, 10.8,
          36.5, 8.2, 13.2, 42.2)
)

colors <- c("Wind" = "#D9D9D9", "Hydro" = "#4472C4", "Solar" = "#FFC000", "Nuclear" = "#FF0000")

make_pie <- function(rank_label) {
  ranks_data %>%
    filter(rank == rank_label) %>%
    ggplot(aes(x = "", y = pct, fill = source)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y") +
    geom_text(aes(label = paste0(pct, "%")),
              position = position_stack(vjust = 0.5),
              size = 3.5, fontface = "bold", colour = "black") +
    scale_fill_manual(values = colors) +
    labs(title = rank_label, fill = "") +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
}

p1 <- make_pie("Rank 1")
p2 <- make_pie("Rank 2")
p3 <- make_pie("Rank 3")
p4 <- make_pie("Rank 4")







# Create Rank Nuclear Vs Wind agaist energy crises in Switzerland?


SHEDS_2025_Table_relevant_variables %>%
  dplyr::summarise(
    nuclear_rank1 = sum(enlit19_1 == 1, na.rm = TRUE),
    wind_rank1 = sum(enlit19_4 == 1, na.rm = TRUE),
    nuclear_rank2 = sum(enlit19_1 == 2, na.rm = TRUE),
    wind_rank2 = sum(enlit19_4 == 2, na.rm = TRUE),
    nuclear_rank3 = sum(enlit19_1 == 3, na.rm = TRUE),
    wind_rank3 = sum(enlit19_4 == 3, na.rm = TRUE)
  ) %>%
  dplyr::mutate(
    total_r1 = nuclear_rank1 + wind_rank1,
    total_r2 = nuclear_rank2 + wind_rank2,
    total_r3 = nuclear_rank3 + wind_rank3,
    pct_nuclear_r1 = round(nuclear_rank1/total_r1*100, 1),
    pct_wind_r1 = round(wind_rank1/total_r1*100, 1),
    pct_nuclear_r2 = round(nuclear_rank2/total_r2*100, 1),
    pct_wind_r2 = round(wind_rank2/total_r2*100, 1),
    pct_nuclear_r3 = round(nuclear_rank3/total_r3*100, 1),
    pct_wind_r3 = round(wind_rank3/total_r3*100, 1)
  ) %>%
  dplyr::select(starts_with("pct")) %>%
  tidyr::pivot_longer(everything()) %>%
  print(n = 20)


make_pie_nw <- function(rank_label) {
  ranks_nuclear_wind %>%
    filter(rank == rank_label) %>%
    ggplot(aes(x = "", y = pct, fill = source)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y") +
    geom_text(aes(label = paste0(pct, "%")),
              position = position_stack(vjust = 0.5),
              size = 3.5, fontface = "bold", colour = "black") +
    scale_fill_manual(values = colors) +
    labs(title = rank_label, fill = "") +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
}

p1nw <- make_pie_nw("Rank 1")
p2nw <- make_pie_nw("Rank 2")
p3nw <- make_pie_nw("Rank 3")

ggsave("figures/nuclear_wind_rank1.png", p1nw, width = 4, height = 4, dpi = 300)
ggsave("figures/nuclear_wind_rank2.png", p2nw, width = 4, height = 4, dpi = 300)
ggsave("figures/nuclear_wind_rank3.png", p3nw, width = 4, height = 4, dpi = 300)








