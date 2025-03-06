library(tidyverse)
prefix = "data/data_from_josh/gu_project/"


serse_phylo <- readRDS(paste0(prefix, "serse_phylo.rds"))
buty <- read.csv(paste0(prefix, "18_buty_bladder.csv"))
pt <- readxl::read_xlsx(paste0(prefix, "seres_data_2023-02-07.xlsx"))
ids <- read.delim(paste0(prefix, "sampleids.txt"), sep="\t")
sdiv <- estimate_richness(serse_phylo, measures="Shannon") %>%
  rownames_to_column("identifier.y")
named <- readRDS("data/data_from_josh/ici_butyrate/genedata2-May3.RDS")

#create survival variables and smaller dataset
data <- pt %>%
  mutate(tt_os_d = as.numeric(as.Date(date_of_last_follow_up) - as.Date(c1d1)),
         event_os = case_when(
           vital_status == "Alive" ~ 0,
           vital_status == "Deceased" ~ 1,
           TRUE ~ NA_integer_
         ),
         tt_pfs_d = as.numeric(as.Date(date_for_pfs) - as.Date(c1d1)),
         age = as.numeric(as.Date(c1d1) - as.Date(date_of_birth))/365.25,
  ) %>%
  select(blinded_subject_id, id_int, cohort, age, impact_tmb_score, cpb_drug, ecog, best_overall_response, tt_pfs_d, pfs_event, tt_os_d, event_os) %>%
  left_join(ids %>%
              select(SUBJID, TMPT, WMS_SGPID, SAMPID),
            by=c("blinded_subject_id" = "SUBJID")) %>%
  left_join(buty %>%
              mutate(sampleid = gsub("../data/buty//|_butyrate.txt", "", sampleid)),
            by=c("WMS_SGPID"="sampleid")) %>%
  mutate(samp_id = factor(row_number()),
         identifier.y = substr(SAMPID, 1, nchar(SAMPID) - 2)) %>%
  left_join(sdiv, by="identifier.y")%>%
  mutate(tx = case_when(
    grepl("ipilimumab", tolower(cpb_drug)) ~ "dual cpb",
    TRUE ~ "PD1 axis only"
  ),
  age10 = age/10)


#group by median at both timepoints
tpt1 <- data %>%
  group_by(blinded_subject_id) %>%
  arrange(TMPT) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(med_buty = cut(total, c(-Inf, median(total, na.rm=T), Inf)))



genename <- named %>%
  mutate(gene_name = if_else(grepl("^[^(\\[]", gene_info), # If string doesn't start with ( or [
                             sub("\\s*\\[.*", "", gene_info), # Remove starting from [
                             gene_info)) %>%
  mutate(gene_name = if_else(grepl("^\\(", gene_name), # If cleaned string starts with (
                             sub("^\\(.*?\\)\\s*", "", gene_name), # Remove the first ()
                             gene_name)) %>%
  mutate(gene_name = sub("\\s*\\(.*", "", gene_name)) %>% # Finally, remove starting from
  mutate(gene_name = sub("\\s*[\\[\\(].*", "", gene_name)) %>%
  mutate(WMS_SGPID = gsub("_butyrate", "", sample_id)) %>%
  group_by(WMS_SGPID, gene_path_true) %>%
  dplyr::filter(gene_path_true != "other" & !is.na(gene_path_true)) %>%
  dplyr::summarise(total_count = sum(Count)) %>%
  ungroup() %>%
  pivot_wider(id_cols= WMS_SGPID, names_from=gene_path_true, values_from=total_count)

tpt1_a <- tpt1 %>%
  left_join(genename, by=c("WMS_SGPID")) %>%
  filter(!is.na(pyruvate))

write_rds(tpt1_a, "data/cleaned_data/acoa_data.rds")

