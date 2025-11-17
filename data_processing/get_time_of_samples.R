library(tidyverse)
devtools::load_all('c:/Users/baichom1/Documents/Github/vdbR')
connect_database()

get_table_from_database("samples_mmf")
get_table_from_database("isabl_api_analysis_targets")
get_table_from_database("isabl_api_sample")
get_table_from_database("isabl_api_experiment")

seres_ps <- readRDS("data/data_from_josh/gu_project/serse_phylo.rds")
  
sample_data(seres_ps) <- sample_data(seres_ps) %>%
  as.tibble() %>%
  as.data.frame() %>%
  left_join(isabl_api_analysis_targets %>% select(analysis_id, experiment_id), by = c("analysis_id")) %>%
  left_join(isabl_api_experiment %>% select(id, sample_id), by = c("experiment_id" = "id")) %>%
  left_join(isabl_api_sample %>% rename(s_identifier = identifier) %>% select(id, s_identifier), by = c("sample_id" = "id")) %>% 
  left_join(samples_mmf %>% select(sampleid, datecollection), by = c("s_identifier" = "sampleid")) %>%
  sample_data() %>% View()

View(sample_data(seres_ps))
