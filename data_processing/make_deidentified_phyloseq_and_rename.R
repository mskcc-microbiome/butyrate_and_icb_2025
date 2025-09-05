library(tidyverse)
library(microViz)
library(phyloseq)

prefix = "data/data_from_josh/gu_project/"

serse_phylo <- readRDS(paste0(prefix, "serse_phylo.rds"))
acoa_df <- readRDS("data/cleaned_data/acoa_data.rds")
ps <- ps_filter(serse_phylo, experiments %in% acoa_df$WMS_SGPID)

sample_data(ps) <- data.frame(sample_data(ps)) %>% 
  rownames_to_column("tmp_rownames") %>%
  left_join(acoa_df %>% select(id_int, `pyruvate`), by = "id_int") %>%
  mutate(log_total = log(total)) %>%
  mutate(log_acoa = log(pyruvate)) %>%
  select(-c(age, impact_tmb_score, cpb_drug, ecog, best_overall_response,
            tt_pfs_d, pfs_event, tt_os_d, event_os, TMPT, identifier,
            analysis_id, experiments, lb)) %>%
  as.data.frame() %>%
  column_to_rownames("tmp_rownames")%>%
  sample_data()

# Next we will add information about top 10 families to the sample data for this phyloseq:

#Step 1: get family level abundance
ps_family <- tax_glom(ps, taxrank = "Family")

taxa_names(ps_family) <- as.character(tax_table(ps_family)[, "Family"])

# Step 2: Transform to compositional (relative abundance)
ps_family_rel <- transform_sample_counts(ps_family, function(x) x / sum(x))

# Step 3: Compute mean abundance per family across all samples
mean_abund <- taxa_sums(ps_family_rel) / nsamples(ps_family_rel)

# Step 4: Identify top 10 families by mean abundance
top10_families <- names(sort(mean_abund, decreasing = TRUE)[1:10])

# Step 5: Create a sample_data data frame with columns for each top family
abund_df <- as(otu_table(ps_family_rel), "matrix")
if (!taxa_are_rows(ps_family_rel)) {
  abund_df <- t(abund_df)
}
abund_df <- as.data.frame(abund_df)

# Extract only the top 10 families
top10_df <- abund_df[top10_families, ]
top10_df <- t(top10_df)


# Step 6: Add to sample metadata
sample_metadata <- as.data.frame(sample_data(ps_family_rel))
sample_metadata <- cbind(sample_metadata, top10_df)

# Step 5: Assign updated metadata back to the species-level object
sample_data(ps) <- sample_data(sample_metadata)

ps %>%
  ps_filter(SAMPID %in% acoa_df$SAMPID) %>%
  write_rds("public_data/sequencing_data/plotting_phyloseq_deidentified.rds")

