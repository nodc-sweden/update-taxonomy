#update-worms

# Workflow
# 1. Get list of All taxa from SHARK API. get_shark_options(), select taxa
# 2. Match scientific_name with WoRMS API. match_worms_taxa()
# 3. Build a new taxon file. add_worms_taxonomy()

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


# Get SHARK API options
options <- get_shark_options(prod = TRUE) # FALSE = TEST API, TRUE = PROD API


# Get All scientific_names in SHARK database
taxa <- data.frame(scientific_name = unlist(options$taxa))
taxa_vector <- taxa$scientific_name

# test with only 50 rows
#taxa_50 <- head(taxa, 50)
#taxa_vector <- taxa_50$scientific_name

# match names with WoRMS
worms_match <- match_worms_taxa(taxa_vector, 
                                      fuzzy = TRUE, 
                                      best_match_only = TRUE, 
                                      max_retries = 3, 
                                      sleep_time = 10, 
                                      marine_only = FALSE, 
                                      verbose = TRUE)

# taxa not in WoRMS
problem_taxa <- worms_match %>%
  filter(status == "no content")

# rename
worms_data <- worms_match %>%
  rename(scientific_name = name,
         NameMatchWorms = scientificname,
         aphia_id = AphiaID,
         parent_id = parentNameUsageID,
         valid_aphia_id = valid_AphiaID
         ) %>%
  
# select cols
  select(scientific_name,
         NameMatchWorms,
         authority,
         rank,
         aphia_id,
         url,
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
         lsid
         ) %>%
  
# remove problem taxa
  filter(status != "no content")

# select IDs
aphia_id_vector <- worms_data$aphia_id

# create taxonomy table
taxonomy_match <- add_worms_taxonomy(aphia_id_vector, 
                               scientific_name = NULL, 
                               verbose = TRUE)

# clean up
taxonomy_select <- taxonomy_match %>%
  select(aphia_id, 
         worms_species, 
         worms_hierarchy) %>%
  
  rename(species = worms_species,
         classification = worms_hierarchy) %>%
  
  unique() %>%
  
  filter(!is.na(aphia_id))


# select parent ids
parent_ids <- worms_data$parent_id

# match parent ids 
parents <- get_worms_records(parent_ids, 
                             max_retries = 3, 
                             sleep_time = 10, 
                             verbose = TRUE)

# clean up
parents_select <- parents %>%
  rename(
    parent_name = scientificname,
    parent_id = AphiaID) %>%
  
  select(
    parent_name,
    parent_id) %>%
  
  unique() %>%
  
  filter(!is.na(parent_id))

# join data
taxa_worms_file <- left_join(worms_data, taxonomy_select, by = "aphia_id")

taxa_worms_file <- left_join(taxa_worms_file, parents_select, by = "parent_id")


# Select cols
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
