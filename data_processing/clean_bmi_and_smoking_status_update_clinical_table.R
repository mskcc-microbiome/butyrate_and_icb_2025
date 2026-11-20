library(tidyverse)
library(phyloseq)
library(ggplot2)

acoa_data <- readRDS("data/cleaned_data/acoa_data.rds") %>% as.data.frame()


all_bmi_data <- readRDS("P:\\Josh\\ICI_butyrate\\data_from_tsoni\\bmi_solid_tumor_cohort_2026-03-20.rds") %>% 
  rename(mrn = MRN) %>% 
  mutate(service_date = as.Date(service_date), mrn = as.integer(mrn)) %>% 
  filter(!is.na(item_value)) %>% #sanity checks
  filter(item_value > 10) %>%
  select(c(service_date, mrn, item_value)) %>%
  group_by(mrn, service_date) %>%
  summarize(item_value = min(item_value)) %>%
  ungroup()
pt_clinical_data <- read.csv("P:\\Josh\\ICI_butyrate\\data_from_tsoni\\pt_list_2023_05_23.csv")

get_mode <- function(x) {
  ux <- unique(na.omit(x))
  ux[which.max(tabulate(match(x, ux)))]
}


cleaned_clinical_data <- pt_clinical_data %>%
  mutate(
    c1d1 = as.Date(c1d1),
    smoking_status = na_if(smoking_status, ""),
    smoking_mode = get_mode(smoking_status),
    imputed_smoking_status = if_else(is.na(smoking_status), get_mode(smoking_status), smoking_status),
    imputed_smoking_status = factor(imputed_smoking_status, levels = c("Never", "Current or Former"))) %>% #
  left_join(
    all_bmi_data,
    by = join_by(
      mrn,
      closest( c1d1 >= service_date)
    )
  ) %>%
  rename(bmi = item_value,
         bmi_date = service_date
         ) %>%
  filter(blinded_subject_id %in% acoa_data$blinded_subject_id)

#cleaned_clinical_data %>%
#  select(c(mrn, c1d1, blinded_subject_id)) %>%
#  write.csv("data/mrn_and_c1d1_solid_tumor_cohort.csv", row.names = F)
  
cleaned_clinical_data %>%
  mutate(time_bmi_from_service = c1d1 - bmi_date) %>%
  select(time_bmi_from_service) %>% 
  ggplot(aes(x = time_bmi_from_service)) +
  geom_histogram() +
  theme_minimal(base_size = 14) +
  labs(
    x = "Days BMI was measured before C1D1",
    y = "Num Patients"
  )
  

# quick sanity check to make sure patients are the same for shared rows:

not_matching <- cleaned_clinical_data %>%
  select(intersect(names(cleaned_clinical_data), names(acoa_data))) %>%
  select(-best_overall_response) %>%
  mutate(id_int = as.character(id_int),
         ecog = as.character(ecog),
         impact_tmb_score = as.character(impact_tmb_score),
         pfs_event = as.character(pfs_event)) %>%
  anti_join(acoa_data %>% select(intersect(names(cleaned_clinical_data), names(acoa_data))) %>% mutate(across(everything(), as.character)) %>% select(-best_overall_response))

acoa_data %>%
  filter(blinded_subject_id %in% not_matching$blinded_subject_id) %>%
  select(intersect(names(cleaned_clinical_data), names(acoa_data))) %>%
  arrange(blinded_subject_id)

cleaned_clinical_data %>%
  filter(blinded_subject_id %in% not_matching$blinded_subject_id) %>%
  select(intersect(names(cleaned_clinical_data), names(acoa_data))) %>%
  arrange(blinded_subject_id)

write_rds(cleaned_clinical_data, "data/updated_clinical_data.rds")
