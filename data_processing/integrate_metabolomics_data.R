library(tidyverse)

ici_folder = "P:\\Josh\\ICI_butyrate\\"

acoa_df_deid <- readRDS("data/cleaned_data/acoa_data.rds") %>%
  select(c(WMS_SGPID, pyruvate))

sample_name_connection <- read_delim(file.path(paste0(ici_folder, "data/raw/msk_solid_tumor_16Aug2022_data_transfer_samples.txt"))) %>%
  select(c(METABOLON_SCFA_SAMPID, WMS_SGPID))

scfa <- read_delim(file.path(paste0(ici_folder, "data/raw/MSK_Solid_Tumor_SCFA_data.txt"))) %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column("metabolomics_sample_id") %>%
  janitor::row_to_names(row_number =1) %>%
  dplyr::rename(METABOLON_SCFA_SAMPID = Analyte) %>%
  mutate_at(c(2:9), as.numeric) %>%
  mutate(sum = rowSums(.[2:9])) %>%
  left_join(sample_name_connection) %>%
  left_join(acoa_df_deid)


saveRDS(scfa, "data/cleaned_data/metabolomics_data.rds")
