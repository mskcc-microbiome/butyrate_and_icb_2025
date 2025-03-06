library(tidyverse)
library(Biostrings)

raw_mpa_phy <- readRDS("data/data_from_josh/ici_butyrate/out_phyloseq80.RDS")
wargo <- read.delim("data/data_from_josh/ici_butyrate/combined_butyrate_952024.tsv")
w_meta <- read_csv("data/data_from_josh/ici_butyrate/SraRunTable (1).txt")
ici_folder = "P:\\Josh\\ICI_butyrate\\"


# Function to read FASTA file and process headers
process_fasta_headers <- function(fasta_file) {
  # Read the FASTA file
  fasta_data <- readDNAStringSet(fasta_file)
  
  # Extract headers
  headers <- names(fasta_data)
  
  # Split headers and store in a data frame
  split_headers <- strsplit(headers, " ", fixed = TRUE)
  header_df <- data.frame(
    gene_family_id = sapply(split_headers, `[`, 1),
    gene_info = sapply(split_headers, function(x) paste(x[-1], collapse = " ")),
    stringsAsFactors = FALSE
  )
  
  return(header_df)
}


wargo <- wargo %>%
  dplyr::rename(sample_id = sampleid)

# Replace with the path to your FASTA file
fasta_file_path <- paste0( ici_folder, "data/raw/butyrate_genes.fa")

# Process the FASTA file
result_df <- process_fasta_headers(fasta_file_path)

pyruvate <- read_xlsx(paste0( ici_folder, "genelist.xlsx"), sheet = 1)
glutarate <- read_xlsx(paste0( ici_folder, "genelist.xlsx"), sheet = 2)
fouraminobutyrate <- read_xlsx(paste0( ici_folder, "genelist.xlsx"), sheet = 3)
lysine <- read_xlsx(paste0( ici_folder, "genelist.xlsx"), sheet = 4)

l_pyruvate <- unlist(pyruvate[,1:10], use.names=F)
l_pyruvate <- l_pyruvate[which(!is.na(l_pyruvate))]


l_glutarate <- unlist(glutarate[,1:7], use.names=F)
l_glutarate <- l_glutarate[which(!is.na(l_glutarate))]

l_fouraminobutyrate <- unlist(fouraminobutyrate[,1:3], use.names=F)
l_fouraminobutyrate <- l_fouraminobutyrate[which(!is.na(l_fouraminobutyrate))]

l_lysine <- unlist(lysine[,1:8], use.names=F)
l_lysine <- l_lysine[which(!is.na(l_lysine))]


genelist <- data.frame(rbind(cbind(l_pyruvate, "pyruvate"),
                             cbind(l_glutarate, "gluatarate"),
                             cbind(l_fouraminobutyrate, "fouraminobutyrate"),
                             cbind(l_lysine, "lysine")))
genelist <- genelist %>%
  rename("l_pyruvate" = "gene_family_id","V2" = "gene_path_true") %>%
  mutate(gene_family_id = as.numeric(gene_family_id))


result_df <- result_df %>%
  mutate(gene_family_id = as.numeric(gene_family_id)) %>%
  left_join(genelist)


named_wargo <- wargo %>%
  left_join(result_df, by=c("Family"="gene_family_id"))

genename_wargo <- named_wargo %>%
  mutate(gene_name = if_else(grepl("^[^(\\[]", gene_info), # If string doesn't start with ( or [
                             sub("\\s*\\[.*", "", gene_info), # Remove starting from [
                             gene_info)) %>%
  mutate(gene_name = if_else(grepl("^\\(", gene_name), # If cleaned string starts with (
                             sub("^\\(.*?\\)\\s*", "", gene_name), # Remove the first ()
                             gene_name)) %>%
  mutate(gene_name = sub("\\s*\\(.*", "", gene_name)) %>% # Finally, remove starting from
  mutate(gene_name = sub("\\s*[\\[\\(].*", "", gene_name)) %>%
  group_by(sample_id, gene_path_true) %>%
  dplyr::filter(gene_path_true != "other" & !is.na(gene_path_true)) %>%
  dplyr::summarise(total_count = sum(Count)) %>%
  ungroup() %>%
  pivot_wider(id_cols= sample_id, names_from=gene_path_true, values_from=total_count)

w_meta_s <- w_meta %>%
  select(Run, pfs_d, pfsevent, fiber_cat, DSQfib, treatment, AGE, adv_substage, Antibiotics) %>%
  filter(Run %in% genename_wargo$sample_id)

pfs_wargo <- genename_wargo %>%
  left_join(w_meta_s, by=c("sample_id"="Run"))


sampletable <- sample_data(raw_mpa_phy)
sampletable <- sampletable %>%
  as.data.frame()
sampletable$sample_id <- rownames(sampletable)
sampletable <- sampletable %>%
  as_tibble() %>%
  left_join(pfs_wargo, by = "sample_id") %>%
  select(-dummymetadata) %>%
  mutate(log_acoa = log(pyruvate))

sampletable <- sampletable %>%
  as.data.frame() %>%
  column_to_rownames("sample_id")

sample_data(raw_mpa_phy) <- sample_data(sampletable)


taxa_table <- as.data.frame(tax_table(raw_mpa_phy))
taxa_table <- taxa_table %>%
  mutate(Family = if_else(Order == "Bacteroidales", "Bacteroidales (order)", Family))
tax_table(raw_mpa_phy) <- tax_table(as.matrix(taxa_table))
raw_mpa_phy_icb <- subset_samples(raw_mpa_phy, treatment != "other systemic", )

saveRDS(raw_mpa_phy_icb, "data/cleaned_data/wargo_phylo.RDS")

result <- raw_mpa_phy_icb %>%
  get.otu.melt() %>%
  select(sample, pfs_d, pfsevent, pyruvate, Class, Order, Family, Genus, Species, numseqs) %>%
  group_by(sample, pfs_d, pfsevent, pyruvate) %>%
  summarise(
    rum = sum(numseqs[Family == "Ruminococcaceae"], na.rm=T),
    rum_lach = sum(numseqs[Family %in% c("Ruminococcaceae", "Lachnospiraceae")], na.rm=T),
    clostridiales = sum(numseqs[Order == "Clostridiales"], na.rm=T),
    osc = sum(numseqs[Order == "Oscillospirales"], na.rm=T),
    clostridia = sum(numseqs[Class == "Clostridia"], na.rm=T),
    faecalibacterium = sum(numseqs[Genus == "Faecalibacterium"], na.rm=T),
    faecalibacterium_prausnitzii = sum(numseqs[Species == "Faecalibacterium_prausnitzii"], na.rm=T)
  ) %>%
  mutate(
    rum = replace_na(rum, 0),
    osc = replace_na(osc, 0),
    rum_lach = replace_na(rum_lach, 0),
    clostridiales = replace_na(clostridiales, 0),
    clostridia = replace_na(clostridia, 0),
    faecalibacterium = replace_na(faecalibacterium, 0),
    faecalibacterium_prausnitzii = replace_na(faecalibacterium_prausnitzii, 0)
  )

saveRDS(result, "data/cleaned_data/wargo_pfs_data_for_univariate_model.RDS")
