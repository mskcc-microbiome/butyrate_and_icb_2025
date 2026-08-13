library(tidyverse)
library(phyloseq)
library(yingtools2)
prefix = "data/data_from_josh/gu_project/"

get_mode <- function(x) {
  ux <- unique(na.omit(x))
  ux[which.max(tabulate(match(x, ux)))]
}


serse_phylo <- readRDS(paste0(prefix, "serse_phylo.rds"))
buty <- read.csv(paste0(prefix, "18_buty_bladder.csv"))
pt <- readxl::read_xlsx(paste0(prefix, "seres_data_2023-02-07.xlsx"))
ids <- read.delim(paste0(prefix, "sampleids.txt"), sep="\t")
sdiv <- estimate_richness(serse_phylo, measures="Shannon") %>%
  rownames_to_column("identifier.y")
named <- readRDS("data/data_from_josh/ici_butyrate/genedata2-May3.RDS")
cleaned_clinical_data <- readRDS("data/updated_clinical_data.rds")
updated_ecogs <- read.csv("data/data_from_josh/ecogs.csv")


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
  dplyr::slice(1) %>%
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
ps <- ps_filter(serse_phylo, experiments %in% tpt1$WMS_SGPID)
bacterial_abundances <- ps %>%
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
  ) 

tpt1_a <- tpt1 %>%
  left_join(genename, by=c("WMS_SGPID")) %>%
  left_join(bacterial_abundances, by=c("WMS_SGPID" = "sample")) %>%
  filter(!is.na(pyruvate)) %>%
  select(-ecog) %>% # we have an updated list of ecogs with 14 fewer unknown values.  Use that instead:
  left_join(updated_ecogs %>% mutate(id_int = as.character(id_int))) %>%
  left_join(cleaned_clinical_data %>% select(c(blinded_subject_id, bmi, smoking_status, imputed_smoking_status))) %>%
  mutate(
    bmi = as.numeric(bmi),
    log_acoa = log10(pyruvate),
    med_p = cut(pyruvate, quantile(pyruvate, probs=seq(0,1,0.5)), c("Below Median", "Above Median")),
    tt_os_months = tt_os_d/30.44,
    tt_pfs_months = tt_pfs_d/30.44, 
    imputed_ecog = ifelse(is.na(ecog), get_mode(ecog), ecog)) %>%
  arrange(med_p)



write_rds(tpt1_a, "data/mixed_solid_tumor.rds")

