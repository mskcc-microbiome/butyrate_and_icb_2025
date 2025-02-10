library(tidyverse)

prefix = "data/data_from_josh/gu_project/"

serse_phylo <- readRDS(paste0(prefix, "serse_phylo.rds"))
acoa_df <- readRDS("data/cleaned_data/acoa_data.rds")
ps <- ps_filter(serse_phylo, experiments %in% acoa_df$WMS_SGPID)

josh_ps2 <- readRDS("data/data_from_josh/PS2_2025-02-10.RDS")


sample_df <- data.frame(sample_data(ps))
merged_df <- left_join(sample_df, acoa_df %>% select(id_int, `pyruvate`), by = "id_int")
merged_df$log_total <- log(merged_df$total)
merged_df$log_acoa <- log(merged_df$pyruvate)
new_sample_data <- sample_data(as(merged_df, "data.frame"))
rownames(new_sample_data) <- rownames(sample_data(ps))
sample_data(ps) <- new_sample_data
# Extract OTU matrix and convert it to a tibble
otu_df <- otu_table(ps) %>%
  as.data.frame() %>%
  rownames_to_column("Taxa") %>%
  as_tibble()

# Ensure the taxonomy data is also accessible
taxa_df <- tax_table(ps) %>%
  as.data.frame() %>%
  rownames_to_column("Taxa") %>%
  as_tibble()

# Aggregate the data
bacteroidales_aggregate <- otu_df %>%
  # Use left_join to include taxonomy information
  left_join(taxa_df, by = "Taxa") %>%
  # Filter rows based on your criteria
  filter(str_detect(Family, "Bacteroidaceae") | str_detect(Order, "Bacteroidales")) %>%
  # Sum across all samples (assuming samples are columns starting from the 2nd column)
  summarise(across(starts_with("SGP"), sum, na.rm = TRUE)) %>% 
  # Add a new Taxa row for the aggregate
  mutate(Taxa = "k__Bacteria|p__Bacteroidetes|c__Bacteroidia|o__Bacteroidales|f__Bacteroidales_order|g__Bac|s__Bac") %>%
  # Select only the necessary columns to match the original OTU matrix format
  select(c(Taxa, starts_with("SGP")))

# Optionally, you might want to remove the original taxa
otu_df <- otu_df %>%
  filter(!str_detect(Taxa, "Bacteroidaceae") & !str_detect(Taxa, "Bacteroidales"))


# Append this new row to the original OTU data frame
otu_df <- bind_rows(otu_df, bacteroidales_aggregate)

# Convert back to OTU matrix
new_otu_table <- otu_df %>%
  column_to_rownames("Taxa") %>%
  as.matrix()




# Assuming you have created an entry for 'Bacteroidales_phylum' in the OTU table
# First, create a new row for the taxonomy table
new_taxa_row <- data.frame(
  Kingdom = "Bacteria",
  Phylum = "Bacteroidetes",
  Class = NA,
  Order = "Bacteroidales",
  Family = "Bacteroidales_order",
  Genus = "Bac",
  Species = "Bac"
)

# Convert it to the same format as the existing taxonomy table
new_taxa_row <- as(new_taxa_row, "matrix")

# Add this new row to the existing taxonomy table
taxa_matrix <- as.matrix(tax_table(ps))
taxa_matrix <- rbind(taxa_matrix, new_taxa_row)
rownames(taxa_matrix)[nrow(taxa_matrix)] <- "k__Bacteria|p__Bacteroidetes|c__Bacteroidia|o__Bacteroidales|f__Bacteroidales_order|g__Bac|s__Bac" # Ensure the rownames match those in the OTU table

# Replace the old taxonomy table with the new one
tax_table(ps) <- tax_table(taxa_matrix)

ps2 <- phyloseq(otu_table(new_otu_table, taxa_are_rows=T), tax_table(taxa_matrix), sample_data(ps))

write_rds(ps2, "data/cleaned_data/plotting_phyloseq.rds")

