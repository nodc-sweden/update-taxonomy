library(SHARK4R)
library(tidyverse)

# Read latest Taxon.csv
taxonomy_table<-read_tsv("export/Taxon.csv")

# Get taxa information
shark_taxon <- get_shark_data(tableView = "report_taxon")

# Filter taxa without ID
no_id <- shark_taxon %>%
  filter(is.na(dyntaxa_id))

# Filter rows where taxonId does not match acceptedNameUsageID
mismatched_rows <- taxonomy_table[taxonomy_table$taxonId != taxonomy_table$acceptedNameUsageID, ]

# Perform a self-join to get the scientificName for the acceptedNameUsageID
translation_list <- merge(
  mismatched_rows, 
  taxonomy_table[, c("taxonId", "scientificName")], 
  by.x = "acceptedNameUsageID", 
  by.y = "taxonId", 
  suffixes = c("_taxonID", "_acceptedNameUsageID")
)

# Select and rename relevant columns
translation_list <- translation_list[, c("scientificName_taxonID", "scientificName_acceptedNameUsageID")]
colnames(translation_list) <- c("taxon_name_from", "taxon_name_to")

# Remove identical names and duplicates
translation_list <- translation_list %>%
  filter(!taxon_name_from == taxon_name_to) %>%
  filter(taxon_name_from %in% no_id$scientific_name)%>%
  distinct() %>%
  arrange(taxon_name_from)

write_tsv(translation_list,"export/translate_to_dyntaxa.txt", na = "")
