library(SHARK4R)
library(here)


path <-here()
setwd(path)

dyntaxa_key <- Sys.getenv("DYNTAXA_APIKEY")

#help("match_taxon_name")




taxon_in_SHARK #Ladda hem artlista från SHARK. API endpoint i framtiden


match_taxon_name("Navicula", dyntaxa_key)

dyntaxa_id_list <- c("106665", "3000107", "1010447")

taxon_list <- update_dyntaxa_taxonomy(dyntaxa_id_list, dyntaxa_key)

# 
# alla felstav eller ickematch måste vi få en lista på. 
# kolumn med match type
# paraent kolumn
# GUID = urn:okdpoakdsposak:123099


#skapa en output i csv enligt Taxon.csv


write_tsv(taxon_list, "export/Taxon.csv", na = "")




