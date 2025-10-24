#update-worms
# 2025-10-23
# Purpose of script is to create a new updated World Register of Marine Species - WoRMS - taxa file (taxa_worms.txt) for use in SHARK, Swedish Ocean Archive, shark.smhi.se

# Workflow
# 1. Get list of All reported taxa from SHARK API. get_shark_data()
# 2. Clean up misspellings.
# 3. Match scientific_name with WoRMS API. match_worms_taxa()
# 4. Build a new taxon file. add_worms_taxonomy()

# Help
# help(get_shark_options)
# help(match_worms_taxa)
# help(add_worms_taxonomy)


# Install packages
library(SHARK4R)
library(here)
library(tidyverse)

# Set path
path <-here()
setwd(path)

# Get reported taxon names from SHARK API
reported_scientific_name <- get_shark_data(tableView = "report_taxon", row_limit = 500) %>%
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

# Rename to SHARK internal_names
worms_match <- worms_match %>%
                              rename(scientific_name = name,
                                    NameMatchWorms = scientificname,
                                    aphia_id = AphiaID,
                                    parent_id = parentNameUsageID,
                                    valid_aphia_id = valid_AphiaID
                                    )  

# Get taxonomy based on Aphia IDs
taxonomy <- add_worms_taxonomy(worms_match$aphia_id, 
                                    add_rank_to_hierarchy = TRUE,
                                    scientific_name = NULL, 
                                    verbose = TRUE) %>%
                                    select(aphia_id, 
                                           worms_species, 
                                           worms_hierarchy) %>%
                                    rename(species = worms_species,
                                           classification = worms_hierarchy) %>%
                                    unique() %>%
                                    filter(!is.na(aphia_id))

# Get parent names based on parent IDs 
parents <- get_worms_records(worms_match$parent_id, 
                                    max_retries = 3, 
                                    sleep_time = 10, 
                                    verbose = TRUE) %>%
                             rename(parent_name = scientificname,
                                    parent_id = AphiaID) %>%
                             select(parent_name,
                                    parent_id) %>%
                             unique() %>%
                             filter(!is.na(parent_id))

# Build the taxa_worms file
taxa_worms_file <- left_join(worms_match, taxonomy, by = "aphia_id")

taxa_worms_file <- left_join(taxa_worms_file, parents, by = "parent_id")


# Select cols taxa_worms file
taxa_worms_selected <- taxa_worms_file %>%
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


# Print Taxon file
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

