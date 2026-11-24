
library(tidyverse)
library(speedyseq)

# scp iris:/data1/collab004/isabl/data_lake/analyses/38/55/113855/merged/ micom-0.0.3-merged

exchanges_raw <- read.csv("../data/micom-0.0.3-merged/merged.exchanges.csv") %>%
  separate_wider_delim(sample_id, delim = "__", names=c("sample", "medium"))

met_key <- exchanges_raw %>% select(metabolite_raw = metabolite) %>% distinct() %>%
  mutate(metabolite = janitor::make_clean_names(metabolite_raw, allow_dupes = T))

growth_raw <- read.csv("../data/micom-0.0.3-merged/merged.growth_rates.csv") %>%
  separate_wider_delim(sample_id, delim = "__", names=c("sample", "medium"), cols_remove = FALSE)

growth <- growth_raw %>% select(growth_rate, taxon, sample, medium) %>%
  filter(taxon != "rare") %>%
  filter(medium!="agora_high_fiber") %>%
  pivot_wider(values_from = "growth_rate", names_from = "medium", values_fill = 0)  %>%
  mutate(diff = agora_western_fiber_pulse - agora_western) %>%
  group_by(taxon) %>%
  summarize(
    median_diff = median(diff, na.rm = T),
    mean_diff = mean(diff, na.rm = T),
    n_pos_in_western = sum(agora_western > 0, na.rm = T),
    n_pos_in_pulse = sum(agora_western_fiber_pulse > 0, na.rm = T),
    n_samples = n_distinct(growth_raw$sample),
    prev_pct = round(n_pos_in_western / n_samples * 100, 1)
  ) %>%
  ungroup() %>%
  filter(n_pos_in_western > 10) %>%
  arrange(desc(n_pos_in_western), desc(median_diff))





prod_raw <- read.csv("../data/micom-0.0.3-merged/merged.production_rates.csv") %>%
  separate_wider_delim(sample_id, delim = "__", names=c("sample", "medium"), cols_remove = FALSE) %>%
  dplyr::rename(metabolite_raw = metabolite)  %>%  left_join(met_key, by="metabolite_raw")

prod <- prod_raw %>%
  select(sample, medium, metabolite, flux) %>%
  pivot_wider(values_from = "flux", names_from = "metabolite", values_fill = 0) %>%
  pivot_longer(cols=-c( sample, medium), names_to = "metabolite", values_to = "flux")

scfas <- read.csv("data/scfas.csv") %>% janitor::clean_names() %>%
  dplyr::rename(metabolite_raw = agora_code)  %>%  left_join(met_key, by="metabolite_raw")


micomphy <- phyloseq(
  prod_raw %>% select(sample_id, sample, medium) %>% distinct() %>% sample_data(),
  prod_raw %>% select(metabolite, name:lipidmaps) %>% distinct() %>%
    column_to_rownames("metabolite") %>% as.matrix() %>%  tax_table(),
  prod_raw %>% select(metabolite, sample_id, flux) %>%
    pivot_wider(names_from=metabolite, values_from = "flux", values_fill = 0)  %>%
    column_to_rownames("sample_id") %>% otu_table(taxa_are_rows = FALSE)

)

save(list = c("micomphy", "prod_raw", "prod", "scfas", "exchanges_raw","growth_raw", "growth" ), file = "../data/micom_results.Rdata")
