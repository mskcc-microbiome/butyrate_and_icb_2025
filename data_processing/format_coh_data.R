library(readxl)
library(tidyverse)
dotenv::load_dot_env(".env")

pyruvate <- read_xlsx("data_processing/shortbred_analysis/genelist.xlsx", sheet = 1)
glutarate <- read_xlsx("data_processing/shortbred_analysis/genelist.xlsx", sheet = 2)
fouraminobutyrate <- read_xlsx("data_processing/shortbred_analysis/genelist.xlsx", sheet = 3)
lysine <- read_xlsx("data_processing/shortbred_analysis/genelist.xlsx", sheet = 4)

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
                             cbind(l_lysine, "lysine"))) %>%
  unique() %>%
  rename("gene_family_id" = "l_pyruvate" ,"gene_path_true" = "V2") %>%
  mutate(gene_family_id = as.numeric(gene_family_id))
result_df <- readRDS("data/data_from_josh/ici_butyrate/genedata2-May3.RDS") %>%
  select(c(Family, gene_info, TotMarkerLength)) %>% 
  left_join(genelist , by = c("Family" = "gene_family_id")) %>%
  unique()


####
coh <- list.files(Sys.getenv("COH_SHORTBRED_ANALYSIS_FOLDER"), pattern="*.txt")

cohbuty <- data.frame(Family=as.numeric(), Count = as.numeric(), Hits = as.numeric(), TotMarkerLength = as.numeric(),
                      sample_id = as.character())
for (file in coh){
  filepath <- paste0(Sys.getenv("COH_SHORTBRED_ANALYSIS_FOLDER"), "/", file)
  butyout <- read_delim(file=filepath, delim="\t")
  butyout$sample_id <- gsub(".txt", "", file)
  cohbuty <- rbind(cohbuty, butyout)
}

named_coh <- cohbuty %>%
  left_join(result_df, by=c("Family", "TotMarkerLength"))


genename_coh <- named_coh %>%
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

cohmeta <- readxl::read_xlsx(paste0(Sys.getenv("COH_SHORTBRED_ANALYSIS_FOLDER"),"\\..\\18523_21133_metadata_shortbred.xlsx")) %>%
  janitor::clean_names() %>%
  head(115)

c_meta_s <- genename_coh %>%
  left_join(cohmeta, by="sample_id")

coh_base <- c_meta_s %>%
  filter(!is.na(response)) %>%
  filter(collection_cat == "Baseline") %>%
  select(-include_exclude) #relevant to COH analysis not to this project

coh_base_ni <- coh_base %>%
  filter(treatment == "Nivo-Ipi")

write_rds(coh_base_ni, "data/coh_butyrate.rds")

