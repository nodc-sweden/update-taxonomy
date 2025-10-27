#update-worms
# 2025-10-23
# Purpose of script is to create a new updated World Register of Marine Species - WoRMS - taxa file (taxa_worms.txt) for use in SHARK, Swedish Ocean Archive, shark.smhi.se

# Workflow
# 1. Get list of All reported taxa from SHARK API. get_shark_data()
# 2. Clean up misspellings using translate_to_worms.txt.
# 3. Match scientific_name with WoRMS API. match_worms_taxa()
# 4. Build a new taxon file. get_worms_taxonomy_tree()

# Help
# help(get_shark_options)
# help(match_worms_taxa)
# help(add_worms_taxonomy)
# help(get_worms_taxonomy_tree)


# Load packages
library(SHARK4R)
library(tidyverse)

# Get reported taxon names from SHARK API
reported_scientific_name <- get_shark_data(tableView = "report_taxon") %>%
  select(reported_scientific_name) %>%
  rename(scientific_name_adj = reported_scientific_name) %>%
  unique()

# Read SHARK translate config file for misspellings etc
translate_to_worms <- read.table("resources/translate_to_worms.txt", header = TRUE, fill = TRUE, sep = "\t", quote = "", fileEncoding = "cp1252")  %>%
  select(scientific_name_from, scientific_name_to) %>%
  filter(scientific_name_from != scientific_name_to) %>%
  rename(scientific_name_adj = scientific_name_from)

# Prepare taxon list before WoRMS match 
shark_taxa <- left_join(reported_scientific_name, translate_to_worms, by = "scientific_name_adj") %>%
  unique() %>%
  mutate(scientific_name = if_else(is.na(scientific_name_to), scientific_name_adj, scientific_name_to)) %>%
  select(scientific_name) %>%
  filter(!(is.na(scientific_name) | scientific_name == ""))

# Test with only 50 rows
#shark_taxa_50 <- head(shark_taxa, 50)

# Match names with WoRMS
worms_match <- match_worms_taxa(shark_taxa$scientific_name, 
                                fuzzy = TRUE, 
                                best_match_only = TRUE, 
                                max_retries = 3, 
                                sleep_time = 10, 
                                marine_only = FALSE, 
                                verbose = TRUE)

# Get a list of taxa not in WoRMS
problem_taxa <- worms_match %>%
  filter(status == "no content")

# Remove taxa without WoRMS match
worms_match <- worms_match %>%
  filter(status != "no content")

# Fetch higher taxonomy records and genus children, parent name and species column
worms_tree <- get_worms_taxonomy_tree(
  worms_match$AphiaID, 
  add_descendants = TRUE,           # Adds all species from the genera present in the data
  add_hierarchy = TRUE,             # Adds hierarchy, species and parentName columns
  add_rank_to_hierarchy = TRUE,     # Adds [rank] to hierarchy data
  add_synonyms = FALSE              # Include all synonyms
  )

# Join with matched names to add name column (containing input name, when available)
worms_tree_joined <- worms_tree %>%
  left_join(select(distinct(worms_match), AphiaID, name)) %>%
  relocate(name) %>%
  mutate(name = ifelse(is.na(name), scientificname, name)) # Prioritize reported name

# Rename to SHARK internal_names
worms_shark_format <- worms_tree_joined %>%
  rename(scientific_name = name,
         NameMatchWorms = scientificname,
         aphia_id = AphiaID,
         parent_id = parentNameUsageID,
         parent_name = parentName,
         valid_aphia_id = valid_AphiaID,
         classification = hierarchy
  )  

# Select cols taxa_worms file
taxa_worms_selected <- worms_shark_format %>%
  select(scientific_name,
         authority,
         rank,
         aphia_id,
         url,
         parent_name,
         parent_id,
         status,
         valid_aphia_id,
         valid_name,
         valid_authority,
         kingdom,
         phylum,
         class,
         order,
         family,
         genus,
         species,
         classification,
         lsid
  )

# Save Taxon file
write.table(taxa_worms_selected, 
            "export/taxa_worms.txt", 
            na = "",
            sep = "\t",
            quote = FALSE,
            fileEncoding = "cp1252",
            row.names = FALSE)



# Print problem_taxa
#write.table(problem_taxa, 
#            "export/problem_taxa.txt", 
#            na = "",
#            sep = "\t",
#            quote = FALSE,
#            fileEncoding = "cp1252",
#            row.names = FALSE)

