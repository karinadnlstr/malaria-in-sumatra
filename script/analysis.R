
# Packages and setup -----

# packages
library(tidyverse)
library(here)
library(openxlsx)
library(cowplot)
library(lubridate)
library(RColorBrewer)
library(broom)
library(car)
library(sf)
library(spdep)


# Data load -----

# malaria data
sismal <- readRDS(here("data", "esismal_2019-2021_sumatera_v5.rds")) |> 
  mutate(status = if_else(status == "Achieved", "Certified", status),
         age_group = if_else(age_group == "0-4"|age_group == "5-14", "<15", age_group),
         age_group = factor(age_group, levels = c("<15",
                                                  "15-24",
                                                  "25-64",
                                                  "65+")),
         occupation_mmp = if_else(age_group=="<15"&occupation=="Unemployed", NA_character_, occupation_mmp),
         occupation_mmp = factor(occupation_mmp, levels = c("MMP", "NonMMP")))

# land use data
land_use <- read.csv(here("data", "land_use.csv"))

# covariates from bps (human development index, poverty)
covariates_bps <- read.xlsx(here("data", "covariates_bps.xlsx")) |> 
  filter(Year=="2021")

# agriculture employment data
agriculture_employment <- read.csv(here("data", "agriculture_employment.csv"))

# urbanisation data
urbanisation <- read.xlsx(here("data", "urbanisation.xlsx"))

# shapefile
sumatra_map <- st_read(here("data", "sumatera.gpkg"), layer="sumatera") |> 
  filter(IDADMIN2!="1888" & IDADMIN2!="1688" & IDADMIN2!="1388" & IDADMIN2!="1288") |> 
  arrange(IDADMIN2)

# ensure the spatial data is in the correct projection (e.g., WGS 84)
sumatra_map <- st_transform(sumatra_map, 4326)

# clean geometries
sumatra_map <- st_make_valid(sumatra_map)

# aggregated malaria and endemicity status data
district_summary <- read.csv(here("data", "district_summary.csv")) |> 
  filter(year=="2021")

district_summary_status <- district_summary |> 
  select(IDADMIN, province, district, phase, status)

# api data
api_2010 <- read.csv(here("data", "malaria_2010_2019_yearly.csv")) |> 
  filter(region=="SUMATRA" & year=="2010")

# Malaria epidemiological characteristic ----

## Malaria three years overview ----

# an overview of malaria in sumatra region
malaria_overview <- sismal |>  
  select(year, gender, pregnancy_status, age_group, occupation_mmp, 
         case_detect, lab, parasite, treatment, hf_type,
         severity, hospitalisation, death) |> 
  group_by(year) |> 
  nest() |> 
  mutate(total_case = map(.x = data,
                          .f = function(input = .x){
                            as.data.frame(data) |>
                              pivot_longer(everything(), names_to = "key", values_to = "cat") |>
                              group_by(key, cat) |> summarise(n=n())
                          }
  )) |> 
  unnest(total_case) |> select(-data) |> 
  group_by(year, key) |>  
  mutate(prop = round(n / sum(n) * 100,1)) |> 
  pivot_wider(names_from = year, values_from = c(n, prop)) |> 
  rowwise() |> 
  mutate_if(is.numeric , replace_na, replace = 0) |>
  mutate(total = sum(c_across(starts_with('n')), na.rm = TRUE)) |> 
  group_by(key) |>  
  mutate(prop = round(total / sum(total) * 100,1),
         n_prop_2019 = paste0(n_2019, " (", prop_2019, ")"),
         n_prop_2020 = paste0(n_2020, " (", prop_2020, ")"),
         n_prop_2021 = paste0(n_2021, " (", prop_2021, ")"),
         n_prop_total = paste0(total, " (", prop, ")"),
         cat = if_else(is.na(cat), "Unknown", cat))

write.xlsx(malaria_overview, here("output"), rowNames=FALSE)


## Summary table of epidemiological investigation ----

pe_overview <- sismal |> 
  select(year, status, idadmin2, pe, parasite, relapsing, pq14, classification) |> 
  group_by(year) |> 
  nest() |> 
  mutate(no_of_district = map(.x = data,
                              .f = function(input = .x){
                                as.data.frame(data) |> 
                                  group_by(status) |> 
                                  distinct(idadmin2) |> 
                                  summarise(count = n()) |> 
                                  mutate(prop = round(count / 154 * 100,1)) |>
                                  mutate(category = "no_district",
                                         sort = 1)|> 
                                  select(category, everything())
                              }),
         no_of_cases = map(.x = data,
                           .f = function(input = .x){
                             as.data.frame(data) |> 
                               group_by(status) |> 
                               summarise(count = n()) |> 
                               mutate(prop = round(count / sum(count) * 100,1)) |> 
                               mutate(category = "no_cases",
                                      sort = 2) |> 
                               select(category, everything())
                           }),
         no_of_cases_inv = map(.x = data,
                               .f = function(input = .x){
                                 as.data.frame(data) |> 
                                   group_by(status, pe) |> 
                                   summarise(count = n()) |> 
                                   mutate(count = ifelse(is.na(count), 0, count),
                                          prop = round(count / sum(count) * 100,1),
                                          sort = 3) |>
                                   rename(category = pe)
                               }),
         no_of_cases_class = map(.x = data,
                                 .f = function(input = .x){
                                   as.data.frame(data) |> 
                                     filter(pe=="Investigated") |> 
                                     group_by(status, classification) |> 
                                     summarise(count = n()) |> 
                                     mutate(count = ifelse(is.na(count), 0, count),
                                            prop = round(count / sum(count) * 100,1),
                                            sort = 4) |> 
                                     rename(category = classification)
                                 }),
         no_of_cases_parasite = map(.x = data,
                                    .f = function(input = .x){
                                      as.data.frame(data) |> 
                                        filter(pe=="Investigated") |> 
                                        group_by(status, classification, parasite) |> 
                                        summarise(count = n()) |> 
                                        mutate(count = ifelse(is.na(count), 0, count),
                                               prop = round(count / sum(count) * 100,1),
                                               category = paste(classification, parasite, sep="_"),
                                               sort = 5) |>
                                        select(category, everything(), -c(classification, parasite))
                                    }),
         no_of_cases_new_relaps = map(.x = data,
                                      .f = function(input = .x){
                                        as.data.frame(data) |> 
                                          filter(pe=="Investigated" & parasite=="Pv") |> 
                                          group_by(status, relapsing) |> 
                                          summarise(count = n()) |> 
                                          mutate(count = ifelse(is.na(count), 0, count),
                                                 prop = round(count / sum(count) * 100,1),
                                                 sort = 6) |> 
                                          rename(category = relapsing)
                                      }),
         no_of_cases_pq14 = map(.x = data,
                                .f = function(input = .x){
                                  as.data.frame(data) |> 
                                    filter(pe=="Investigated" & parasite=="Pv" | parasite=="Mix") |> 
                                    group_by(status, pq14) |> 
                                    summarise(count = n()) |> 
                                    mutate(count = ifelse(is.na(count), 0, count),
                                           prop = round(count / sum(count) * 100,1),
                                           sort = 7) |> 
                                    rename(category = pq14)
                                })
  ) |> 
  select(-data) |> pivot_longer(-year, names_to = "var") |>
  unnest(where(is.list), keep_empty = TRUE) |>
  pivot_wider(names_from = c(year, status), values_from = c(count, prop)) |> 
  rowwise() |> 
  mutate_if(is.numeric , replace_na, replace = 0) |> 
  mutate(total_2019 = sum(c_across(starts_with('count_2019')), na.rm = TRUE),
         total_2020 = sum(c_across(starts_with('count_2020')), na.rm = TRUE),
         total_2021 = sum(c_across(starts_with('count_2021')), na.rm = TRUE)) |> 
  mutate(endemic_2019 = paste0(count_2019_Endemic, " (", prop_2019_Endemic, ")"),
         free_2019 = paste0(count_2019_Free, " (", prop_2019_Free, ")"),
         outbreak_2019 = paste0(count_2019_Outbreak, " (", prop_2019_Outbreak, ")"),
         certified_2020 = paste0(count_2020_Certified, " (", prop_2020_Certified, ")"),
         endemic_2020 = paste0(count_2020_Endemic, " (", prop_2020_Endemic, ")"),
         free_2020 = paste0(count_2020_Free, " (", prop_2020_Free, ")"),
         outbreak_2020 = paste0(count_2020_Outbreak, " (", prop_2020_Outbreak, ")"),
         certified_2021 = paste0(count_2021_Certified, " (", prop_2021_Certified, ")"),
         endemic_2021 = paste0(count_2021_Endemic, " (", prop_2021_Endemic, ")"),
         free_2021 = paste0(count_2021_Free, " (", prop_2021_Free, ")"),
         outbreak_2021 = paste0(count_2021_Outbreak, " (", prop_2021_Outbreak, ")"),
         category = if_else(is.na(category), "Unknown", category)) |> 
  ungroup() |> 
  group_by(var) |> 
  mutate(prop_2019 = round(total_2019 / sum(total_2019) * 100,1),
         prop_2020 = round(total_2020 / sum(total_2020) * 100,1),
         prop_2021 = round(total_2021 / sum(total_2021) * 100,1),
         tp_2019 = paste0(total_2019, " (", prop_2019, ")"),
         tp_2020 = paste0(total_2020, " (", prop_2020, ")"),
         tp_2021 = paste0(total_2021, " (", prop_2021, ")")) |> 
  arrange(sort, classification)

write.xlsx(pe_overview, here("output"), rowNames=FALSE)


## Epidemiology characteristic -----

characteristic_overview <- sismal |> 
  select(year, status, classification,
         gender, pregnancy_status, age_group, occupation_mmp, 
         case_detect, lab, parasite, relapsing, treatment, hf_type,
         severity, hospitalisation, death) |> 
  group_by(year, classification) |> 
  nest() |> 
  mutate(total_case = map(.x = data,
                          .f = function(input = .x){
                            as.data.frame(data) |>
                              pivot_longer(everything(), names_to = "key", values_to = "cat") |>
                              group_by(key, cat) |> summarise(n=n(), .groups = "drop") |> ungroup()
                          }
  )) |> 
  unnest(total_case) |> select(-data) |> 
  group_by(year, classification, key) |>  
  mutate(prop = round(n / sum(n) * 100,1),
         cat = if_else(is.na(cat), "Unknown", cat)) |> 
  pivot_wider(names_from = c(year, classification), values_from = c(n, prop)) |> 
  mutate_if(is.numeric , replace_na, replace = 0) |> 
  mutate(ind_2019 = paste0(n_2019_Indigenous, " (", prop_2019_Indigenous, ")"),
         imp_2019 = paste0(n_2019_Imported, " (", prop_2019_Imported, ")"),
         na_2019 = paste0(n_2019_NA, " (", prop_2019_NA, ")"),
         ind_2020 = paste0(n_2020_Indigenous, " (", prop_2020_Indigenous, ")"),
         imp_2020 = paste0(n_2020_Imported, " (", prop_2020_Imported, ")"),
         na_2020 = paste0(n_2020_NA, " (", prop_2020_NA, ")"),
         ind_2021 = paste0(n_2021_Indigenous, " (", prop_2021_Indigenous, ")"),
         imp_2021 = paste0(n_2021_Imported, " (", prop_2021_Imported, ")"),
         na_2021 = paste0(n_2021_NA, " (", prop_2021_NA, ")")) |> 
  rowwise() |> 
  mutate(n_1_ind = sum(c_across(starts_with('n') & ends_with('Indigenous')), na.rm = TRUE),
         n_1_imp = sum(c_across(starts_with('n') & ends_with('Imported')), na.rm = TRUE),
         n_1_na = sum(c_across(starts_with('n') & ends_with('NA')), na.rm = TRUE)) |>
  ungroup() |> 
  group_by(key) |> 
  mutate(prop_ind = round(n_1_ind / sum(n_1_ind) * 100,1),
         prop_imp = round(n_1_imp / sum(n_1_imp) * 100,1),
         prop_na = round(n_1_na / sum(n_1_na) * 100,1)) |> 
  ungroup() |> 
  mutate(ind = paste0(n_1_ind, " (", prop_ind, ")"),
         imp = paste0(n_1_imp, " (", prop_imp, ")"),
         na = paste0(n_1_na, " (", prop_na, ")")) |> 
  arrange(key)

write.xlsx(characteristic_overview, here("output"), rowNames=FALSE)

relapsing_overview <- sismal |> 
  filter(parasite=="Pv" | parasite=="Po") |> 
  group_by(year, classification, relapsing) |> 
  summarise(n = n()) |> 
  pivot_wider(names_from = c(classification, year), values_from = n) |> 
  rowwise() |> 
  mutate(ind_total = sum(c_across(starts_with('Indigenous')), na.rm = TRUE),
         imp_total = sum(c_across(starts_with('Imported')), na.rm = TRUE),
         na_total = sum(c_across(starts_with('NA')), na.rm = TRUE)) |>
  ungroup() |> 
  mutate(ind_prop = round(ind_total / sum(ind_total) * 100,1),
         imp_prop = round(imp_total / sum(imp_total) * 100,1),
         na_prop = round(na_total / sum(na_total) * 100,1))

pregnant_overview <- sismal |> 
  filter(gender=="Female") |> 
  group_by(year, classification, pregnancy_status) |> 
  summarise(n = n()) |> 
  pivot_wider(names_from = c(classification, year), values_from = n) |> 
  rowwise() |> 
  mutate(ind_total = sum(c_across(starts_with('Indigenous')), na.rm = TRUE),
         imp_total = sum(c_across(starts_with('Imported')), na.rm = TRUE),
         na_total = sum(c_across(starts_with('NA')), na.rm = TRUE)) |>
  ungroup() |> 
  mutate(ind_prop = round(ind_total / sum(ind_total) * 100,1),
         imp_prop = round(imp_total / sum(imp_total) * 100,1),
         na_prop = round(na_total / sum(na_total) * 100,1))

pq14_overview <- sismal |> 
  filter(pe=="Investigated" & parasite=="Pv" | parasite=="Mix") |> 
  group_by(year, classification, pq14) |> 
  summarise(n = n()) |> 
  pivot_wider(names_from = c(classification, year), values_from = n) |> 
  rowwise() |> 
  mutate(ind_total = sum(c_across(starts_with('Indigenous')), na.rm = TRUE),
         imp_total = sum(c_across(starts_with('Imported')), na.rm = TRUE),
         na_total = sum(c_across(starts_with('NA')), na.rm = TRUE)) |>
  ungroup() |> 
  mutate(ind_prop = round(ind_total / sum(ind_total) * 100,1),
         imp_prop = round(imp_total / sum(imp_total) * 100,1),
         na_prop = round(na_total / sum(na_total) * 100,1))


# Logistic regression for indigenous(ref) and imported cases characteristic comparison -----

# data modification, re-level malaria data for the regression
sismal <- sismal |> 
  mutate(
    age_group = relevel(age_group, ref = "25-64"),
    gender = relevel(gender, ref = "Male"),
    pregnancy_status = relevel(pregnancy_status, ref = "Not pregnant"),
    occupation_mmp = relevel(occupation_mmp, ref = "NonMMP"),
    hf_type = relevel(hf_type, ref = "PHC"),
    case_detect = relevel(case_detect, ref = "PCD"),
    lab = relevel(lab, ref = "Microscopy"),
    parasite = relevel(parasite, ref = "Pv"),
    hospitalisation = relevel(hospitalisation, ref = "Outpatient"),
    severity = relevel(severity, ref = "Uncomplicated malaria"),
    death = relevel(death, ref = "Recovered"),
    relapsing = relevel(relapsing, ref = "New case"),
    pq14 = relevel(pq14, ref = "Ya"))


## Bivariable logistic regression -----

characteristic_variables <- c("age_group", "gender", "pregnancy_status", "occupation_mmp", 
                              "hf_type", "case_detect", "lab", "parasite",
                              "hospitalisation", "severity", "death", "relapsing", "pq14")

char_bilogreg_results <- map_df(characteristic_variables, function(var) {
  formula_str <- paste("classification ~", var)
  model <- glm(as.formula(formula_str), data = sismal, family = binomial)
  tidy(model, exponentiate = TRUE, conf.int = TRUE) |> 
    mutate(variable = var)
})

char_bilogreg_results <- char_bilogreg_results |> 
  select(variable, term, estimate, conf.low, conf.high, p.value) |> 
  filter(term != "(Intercept)")

write.xlsx(char_bilogreg_results, here("output","char_bilogreg.xlsx"), rowNames=FALSE)

## Multivariable logistic regression -----

char_multilogreg <- glm(classification ~ age_group + gender + occupation_mmp  +
                          hf_type + case_detect + lab + parasite + hospitalisation +
                          severity, data = sismal, family = binomial)
summary(char_multilogreg)
char_multilogreg_result <- tidy(char_multilogreg, exponentiate = TRUE, conf.int = TRUE)

write.xlsx(char_multilogreg_result, here("output","char_multilogreg.xlsx"), rowNames=FALSE)


# Calculating adjacency matrix and nearest neighbours -----

# Create a neighbor list based on shared boundaries
nb <- poly2nb(sumatra_map, queen = TRUE)
nbw <- nb2listw(nb, style = "W", zero.policy = TRUE)

# Convert neighbor list to a binary adjacency matrix
adj_matrix <- nb2mat(nb, style = "B", zero.policy = TRUE)

# Convert to a data frame for easier manipulation with tidyverse
adj_df <- as.data.frame(adj_matrix)

# Optional: Add row and column names for better identification
row.names(adj_df) <- sumatra_map$IDADMIN2
colnames(adj_df) <- sumatra_map$IDADMIN2

adj_long <- adj_df |> 
  rownames_to_column(var = "district") |> 
  pivot_longer(cols = -district, names_to = "neighbor_district", values_to = "is_neighbor") |> 
  mutate(district = as.integer(district),
         neighbor_district = as.integer(neighbor_district)) |> 
  left_join(district_summary_status, by = c("neighbor_district" = "IDADMIN")) |> 
  mutate(is_neighbor_endemic = if_else(is_neighbor == 1 & status == "Endemic", 1, 0)) |> 
  rename(phase_neighbor = phase,
         status_neighbor = status,
         district = district.x) |> 
  left_join(district_summary_status, by = c("district" = "IDADMIN")) |> 
  select(district, phase, status, neighbor_district, is_neighbor, phase_neighbor, status_neighbor, is_neighbor_endemic)

adj_neighbor <- adj_long |> 
  group_by(district, status) |> 
  summarise(neighbor_endemic = sum(is_neighbor_endemic, na.rm = TRUE)) 
# mutate(neighbor_endemic = if_else(neighbor_endemic > 1, 1, neighbor_endemic))


# Environmental and socioeconomic risk factors association -----

# create dataset for regression merging certified and free into one category
logreg_data <- adj_neighbor |> 
  filter(status!="Outbreak") |> 
  mutate(status = if_else(status=="Endemic", "Endemic", "Certified and Free")) |> 
  left_join(land_use, by = c("district" = "IDADMIN2")) |>
  left_join(covariates_bps, by = c("district" = "ID.District")) |> 
  left_join(api_2010, by = c("district" = "idadmin2")) |> 
  left_join(agriculture_employment, by = c("district" = "idadmin2")) |> 
  left_join(urbanisation, by = c("district" = "idadmin2")) |>
  mutate(status = factor(status, levels = c("Endemic", "Certified and Free")),
         percentage_trees = round(prop_2 * 100, 2),
         percentage_vegetation = round(prop_4_5 * 100, 2),
         percentage_settlement = round(prop_7 * 100, 2),
         percentage_village = round(percent_village * 100, 2)) |> 
  select(district, status, neighbor_endemic, percentage_trees, percentage_vegetation, percentage_settlement, IPM_BPS, API, percentage_agriculture, percentage_village)


## Bivariable logistic regression -----

# List of variables
logreg_variables <- c("percentage_trees", "percentage_vegetation", "percentage_settlement", "percentage_agriculture",
                      "percentage_village", "IPM_BPS", "API", "neighbor_endemic")

# Run the logistic regressions and extract the summary statistics
results <- map_df(logreg_variables, function(var) {
  formula_str <- paste("status ~", var)
  model <- glm(as.formula(formula_str), data = logreg_data, family = binomial)
  tidy(model, exponentiate = TRUE, conf.int = TRUE) |> 
    mutate(variable = var)
})

# Clean up the results
results_tidy_bilogreg <- results |> 
  select(variable, term, estimate, conf.low, conf.high, p.value) |> 
  filter(term != "(Intercept)")

write.xlsx(results_tidy_bilogreg, here("output"), rowNames=FALSE)

## Multivariable logistic regression -----

# baseline model, all variables included
logreg_model_0 <- glm(status ~ neighbor_endemic + API + percentage_trees + percentage_vegetation + percentage_settlement + IPM_BPS + percentage_agriculture + percentage_village, data = logreg_data, family = binomial)
summary(logreg_model_0)
logreg_results_0 <- tidy(logreg_model_0, exponentiate = TRUE, conf.int = TRUE) |> 
  filter(term != "(Intercept)") |> 
  mutate(conf.low = round(conf.low, 3),
         conf.high = round(conf.high, 3))
vif(logreg_model_0)

# model1, exclude percentage_trees because the vif is 22.919327
logreg_model_1 <- glm(status ~ neighbor_endemic + API + percentage_vegetation + percentage_settlement + IPM_BPS + percentage_agriculture + percentage_village, data = logreg_data, family = binomial)
summary(logreg_model_1)
logreg_results_1 <- tidy(logreg_model_1, exponentiate = TRUE, conf.int = TRUE) |> 
  filter(term != "(Intercept)") |> 
  mutate(conf.low = round(conf.low, 3),
         conf.high = round(conf.high, 3))
vif(logreg_model_1)

# model2, exclude percentage_village because the vif is 6.268831
logreg_model_2 <- glm(status ~ neighbor_endemic + API + percentage_vegetation + percentage_settlement + IPM_BPS + percentage_agriculture, data = logreg_data, family = binomial)
summary(logreg_model_2)
logreg_results_2 <- tidy(logreg_model_2, exponentiate = TRUE, conf.int = TRUE) |> 
  filter(term != "(Intercept)") |> 
  mutate(conf.low = round(conf.low, 3),
         conf.high = round(conf.high, 3))
vif(logreg_model_2)

# model3, exclude percentage_agriculture because the vif is 2.360058
logreg_model_3 <- glm(status ~ neighbor_endemic + API + percentage_vegetation + percentage_settlement + IPM_BPS, data = logreg_data, family = binomial)
summary(logreg_model_3)
logreg_results_3 <- tidy(logreg_model_3, exponentiate = TRUE, conf.int = TRUE) |> 
  filter(term != "(Intercept)") |> 
  mutate(conf.low = round(conf.low, 3),
         conf.high = round(conf.high, 3))
vif(logreg_model_3)

# model4, exclude percentage_settlement because the pvalue is 0.792981
logreg_model_4 <- glm(status ~ neighbor_endemic + API + percentage_vegetation + IPM_BPS, data = logreg_data, family = binomial)
summary(logreg_model_4)
logreg_results_4 <- tidy(logreg_model_4, exponentiate = TRUE, conf.int = TRUE) |> 
  filter(term != "(Intercept)") |> 
  mutate(conf.low = round(conf.low, 3),
         conf.high = round(conf.high, 3))
vif(logreg_model_4)

# model5, exclude percentage_vegetation because the pvalue is 0.515861
logreg_model_5 <- glm(status ~ neighbor_endemic + API + IPM_BPS, data = logreg_data, family = binomial)
summary(logreg_model_5)
logreg_results_5 <- tidy(logreg_model_5, exponentiate = TRUE, conf.int = TRUE) |> 
  filter(term != "(Intercept)") |> 
  mutate(conf.low = round(conf.low, 3),
         conf.high = round(conf.high, 3))
vif(logreg_model_5)
# model5 was choosen because it has no multicollinearity independent variables and lowest aic value

write.xlsx(logreg_results_5, here("output"), rowNames=FALSE)


# Global Moran's I test -----

moran.test(district_summary$p_total, nbw, alternative = "greater")

moran.plot(district_summary$p_total, nbw)

gmoranMC <- moran.mc(district_summary$p_total, nbw, nsim = 999)
gmoranMC

hist(gmoranMC$res)
abline(v = gmoranMC$statistic, col = "red")

