# 2024-11-20
# Purpose of script is to create a new updated Artdatabanken-Dyntaxa taxa file (Taxon.csv) for use in SHARK, Swedish Ocean Archive, shark.smhi.se

# Workflow
# 0. Sign up and get a DYNTAXA_APIKEY at api-portal.artdatabanken.se
# 1. Get list of All taxa from SHARK API. get_shark_options(), select taxa
# 2. Match scientific_name with Dyntaxa API. match_taxon_name()
# 3. Remove taxa using black list
# 4. Get parent taxa form Dyntaxa API. get_dyntaxa_parent_ids()
# 5. Build a new Taxon.csv file. construct_dyntaxa_table()


# Help
# help(get_shark_options)
# help(get_shark_data)
# help(match_taxon_name)
# help(get_dyntaxa_parent_ids)
# help(construct_dyntaxa_table)


### Preparation ###

# Install packages
library(SHARK4R)
library(here)
library(tidyverse)

# Set path
path <-here()
setwd(path)

# Define Dyntaxa API subscription key from .Renviron
subscription_key <- Sys.getenv("DYNTAXA_APIKEY")

# Import black list for removal of taxa
black_list <- read.table("resources/black_list_shark.txt", header = TRUE, fill = TRUE, sep = "\t", quote = "", fileEncoding = "UTF-8")  


### Operation ###

# Get SHARK API options
options <- get_shark_options(prod = TRUE) # FALSE = TEST API, TRUE = PROD API

# Get All scientific_names in SHARK database
taxa <- data.frame(scientific_name = unlist(options$taxa))

# Clean up taxon names
source("code/cleanup-v2.R")

# Match SHARK taxa to Dyntaxa API
match <- match_taxon_name(taxa_adj, subscription_key, multiple_options = TRUE) #multiple_options = TRUE for All alternatives, example "Nitzschia"

# Remove IDs based on black list
match_adj <- match %>%
  anti_join(black_list, by = "taxon_id")
  
# Select ID column, remove NAs
taxon_id <- na.omit(unique(match_adj$taxon_id))
              
# Collecting parent taxa from ID list
# parents <- get_dyntaxa_parent_ids(taxon_id, subscription_key) # No longer necessary
  
# Build new Taxon file for SHARK
taxonomy_table <- construct_dyntaxa_table(taxon_id, subscription_key, shark_output = FALSE, add_synonyms = FALSE, add_descendants = TRUE, add_missing_taxa = TRUE) # Extend the table with genus children

# Select cols

taxonomy_table_selected <- taxonomy_table %>%
  select(taxonId,
         acceptedNameUsageID,
         parentNameUsageID,
         scientificName,
         taxonRank,
         scientificNameAuthorship,
         taxonomicStatus,
         nomenclaturalStatus,
         taxonRemarks,
         kingdom,
         phylum,
         class,
         order,
         family,
         genus,
         species
        # hierarchy
  )



taxonomy_table_x <- taxonomy_table_selected %>%
   filter(!grepl("×", scientificName)) %>%
  filter(!grepl("×", species))
  
  !filter(taxonRank =="SpeciesComplex") %>%
  arrange(taxonRank)
 # filter(taxonRank =="Cultivar")


  select(taxonRank) %>%
  distinct()

# Print Taxon file
write_tsv(taxonomy_table_selected, "export/Taxon.csv", na = "")

write.table(
  taxonomy_table_x, 
  "export/Taxon3.csv",
  sep = "\t",           # Tab-separated values
  row.names = FALSE,    # No row names
  col.names = TRUE,     # Include column names
  na = "",              # Empty string for NA values
  eol = "\n",          # Ensure CRLF line endings
  #append = TRUE,        # Append to include BOM
  quote = FALSE,
  fileEncoding = "UTF-8" # Ensure UTF-8 encoding
)


# Done!




### Extras ###


# Create SHARK genus white list
white_list <- taxonomy_table %>%
  filter(taxonRank  == "Genus") %>%
  select(scientificName) %>%
  arrange(scientificName) %>%
  unique() %>%
  na.omit() %>%
  rename(scientific_name = scientificName) %>%
  mutate(rank = "genus")

# Print
write.table(white_list,
            "export/dyntaxa_whitelist.txt",
            na = "",
            quote = FALSE,
            sep = "\t",
            fileEncoding = "latin1",
            row.names = FALSE)
  


# Get duplicate taxa, support for creating black list
duplicate_match <- match %>% # Find duplicate rows in best_match column
  group_by(best_match) %>%
  filter(n() > 1) %>%
  ungroup()

duplicate_match_adj <- duplicate_match %>% # Find duplicates in best_match and taxon_id columns, keep only if they are different: I.e when author and/or valid name has changed. This removes duplicate rows where Id is the same, i.e when valid name is the same but the author has changed
  add_count(best_match, taxon_id) %>%
  filter(n == 1) %>%
  select(-n)

problem_duplicates <- duplicate_match_adj %>% # Remove rows with non-duplicate valid_name, keep only taxa with same valid name i.e. Nitzschia, Ctenophora
  group_by(valid_name) %>%
  filter(n() > 1) %>%
  ungroup()        


# Find out taxa with no match, handle in translate_to_dyntaxa or similar
no_match <- match %>%
  filter(is.na(taxon_id))

# Print
write.table(problem_duplicates,
            "export/problem_duplicates.txt",
            na = "",
            quote = FALSE,
            sep = "\t",
            fileEncoding = "latin1",
            row.names = FALSE)



