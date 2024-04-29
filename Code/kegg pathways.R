library(tidyverse)
library(janitor)
# library('biomaRt')
# library(GTExR)


# melanogenesis <- read_csv('kegg_pathway.csv') |> 
#   separate_wider_delim(Information, delim = ';', names = c('geneSymbol', 'information')) |> 
#   separate_wider_delim(information, 
#                        delim = '[', 
#                        names = c('gene_discription', 'KO', 'EC'), 
#                        too_few = 'align_start')

region_4q24 <- read_csv('4q24region.csv')

gene_list <- unique(region_4q24$geneSymbol)

modified_gene_list <- ifelse(startsWith(gene_list, "ENSG"), paste0(gene_list, "%"), gene_list)
sql_modified_list <- paste0("('", paste(modified_gene_list, collapse = "','"), "')")

known_genes <- c('TYR', 'OCA2', 'TYRP1', 'SLC45A2', 'SLC24A5', 'LRMDA', 'TYRP2', 'ENSG00000148655.14')
sql_known_genes <- paste0("('", paste(known_genes, collapse = "','"), "')")

sql_gene_list <- paste0("('", paste(gene_list, collapse = "','"), "')")

short_list <- head(gene_list)
sql_short_list <- paste0("('", paste(short_list, collapse = "','"), "')")

# transcript_data <- getTranscripts(gene_ids = gene_list, tissue = "Skin")

known_genes_exp <- known_genes_exp |> 
  distinct(Name, .keep_all = TRUE)

C4q24_genes_exp <- C4q24_genes_exp |> 
  distinct(Name, .keep_all = TRUE)

C4q24_genes_fix <- result |> 
  distinct(Name, .keep_all = TRUE)

filtered_data <- C4q24_genes_fix |> 
  filter(if_any(3:56, ~ .x != 0))

top_skin <- filtered_data |> 
  arrange(desc(`Skin - Sun Exposed (Lower leg)`)) |> 
  head(5)

top_thyroid <- filtered_data |> 
  arrange(desc(Thyroid)) |> 
  head(5)

all_data <- bind_rows(filtered_data, known_genes_exp)
