
# SHARK problems 2024-11-15
remove <- c("COMNT_SAMPLE", # remove obvious nonmatch 
            "COVER_CLAY",
            "COVER_HARD_BOTTOM",
            "COVER_HARD_BOTTOM",
            "COVER_SOFT_BOTTOM",
            "COVER_SOFT_BOTTOM",
            "COVER_STONE_20_60",
            "COVER_STONE_20_60",
            "COVER_STONE_20_60",
            "Other_unicellular_plankton",
            "Other_unicellular_plankton",
            "Unicells_and_flagellates",
            "Unicells_and_flagellates")

# Remove from PlanktonBarcoding
taxa_adj <- taxa %>%
  filter(!if_any(everything(), ~ str_detect(.x, "\\|"))) %>% 
  filter(!if_any(everything(), ~ str_detect(.x, "Bacteria-"))) %>% 
  filter(!if_any(everything(), ~ str_detect(.x, "Archaea-"))) %>% 
  filter(!if_any(everything(), ~ str_detect(.x, "Unassigned-"))) %>% 
  
  filter(!scientific_name %in% remove) # remove obvious nonmatch 




