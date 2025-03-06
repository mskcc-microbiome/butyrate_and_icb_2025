library(tidyverse)
library(phyloseq)

serse_phylo <- readRDS("data/data_from_josh/gu_project/serse_phylo.rds")
acoa_df <- readRDS("data/cleaned_data/acoa_data.rds")
ps <- ps_filter(serse_phylo, experiments %in% acoa_df$WMS_SGPID)

result <- ps %>%
  get.otu.melt() %>% 
  select(sample,cohort, ecog, tt_pfs_d, tt_os_d, pfs_event, event_os, Class, Order, Family, Species, numseqs) %>%
  group_by(sample) %>%
  summarise(
    rum = sum(numseqs[Family == "Ruminococcaceae"], na.rm=T),
    rum_lach = sum(numseqs[Family %in% c("Ruminococcaceae", "Lachnospiraceae")], na.rm=T),
    clostridiales = sum(numseqs[Order == "Clostridiales"], na.rm=T),
    clostridia = sum(numseqs[Class == "Clostridia"], na.rm=T),
    faecalibacterium_prausnitzii = sum(numseqs[Species == "Faecalibacterium_prausnitzii"], na.rm=T)
  ) %>%
  mutate(
    rum = replace_na(rum, 0),
    rum_lach = replace_na(rum_lach, 0),
    clostridiales = replace_na(clostridiales, 0),
    clostridia = replace_na(clostridia, 0),
    faecalibacterium_prausnitzii = replace_na(faecalibacterium_prausnitzii, 0)
  ) %>%
  left_join(acoa_df, by = c("sample" = "WMS_SGPID"))

saveRDS(result, "data/cleaned_data/interal_msk_data_for_univariate_model.RDS")
