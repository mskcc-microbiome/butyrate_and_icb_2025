library(readxl)
library(tidyverse)
dotenv::load_dot_env(".env")

pyruvate <- read_xlsx("~/Library/CloudStorage/Box-Box/Joshua Fein (Internal)/ICI_butyrate/genelist.xlsx", sheet = 1)
glutarate <- read_xlsx("~/Library/CloudStorage/Box-Box/Joshua Fein (Internal)/ICI_butyrate/genelist.xlsx", sheet = 2)
fouraminobutyrate <- read_xlsx("~/Library/CloudStorage/Box-Box/Joshua Fein (Internal)/ICI_butyrate/genelist.xlsx", sheet = 3)
lysine <- read_xlsx("~/Library/CloudStorage/Box-Box/Joshua Fein (Internal)/ICI_butyrate/genelist.xlsx", sheet = 4)

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
result_df <- genelist %>%
  rename("gene_family_id" = "l_pyruvate" ,"gene_path_true" = "V2") %>%
  mutate(gene_family_id = as.numeric(gene_family_id))


####
coh <- list.files(Sys.getenv("COH_SHORTBRED_ANALYSIS_FODLER"), pattern="*.txt")

cohbuty <- data.frame(Family=as.numeric(), Count = as.numeric(), Hits = as.numeric(), TotMarkerLength = as.numeric(),
                      sample_id = as.character())
for (file in coh){
  filepath <- paste0(cohpath, "/", file)
  butyout <- read_delim(file=filepath, delim="\t")
  butyout$sample_id <- gsub(".txt", "", file)
  cohbuty <- rbind(cohbuty, butyout)
}

result_df$gene_family_id <- as.numeric(result_df$gene_family_id)

named_coh <- cohbuty %>%
  left_join(result_df, by=c("Family"="gene_family_id"))

as_tibble(named_coh)
named_coh %>%
  group_by(gene_path_true, sample_id) %>%
  summarise(total_count = sum(Count)) %>%
  filter(!is.na(gene_path_true) & gene_path_true != "NA" & gene_path_true != "other") %>%
  ggplot() +
  geom_point(aes(x=fct_reorder(sample_id, total_count, sum), y=total_count, fill=gene_path_true), shape=21, size=4, alpha=0.6)+
  scale_fill_manual(values=colors)+
  scale_y_continuous(trans="sqrt")+
  theme_classic()+
  theme(axis.text.x = element_blank(), axis.ticks.x=element_blank(), legend.position="bottom", axis.text.y=element_text(size=18), legend.text=element_text(size=18))

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

cohmeta <- read_csv(paste0(cohpath,"/../coh_meta.csv")) %>% janitor::clean_names()

c_meta_s <- genename_coh %>%
  left_join(cohmeta, by="sample_id")

coh_base <- c_meta_s %>%
  #filter(include_exclude == "Include") %>% -- discussed with CoH, these were excluded from the original analysis but for reasons not relevant to the current analysis
  filter(!is.na(response)) %>%
  filter(collection_cat == "Baseline")

coh_base_ni <- coh_base %>%
  filter(treatment == "Nivo-Ipi")

write_rds(tpt1_a, "data/cleaned_data/acoa_data.rds")

