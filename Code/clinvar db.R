library(DBI)
library(duckdb)
library(tidyverse)

con <- dbConnect(duckdb(), dbdir = "duckdb")

file_path <- "variant_summary.txt.gz"
table_name <- "clinvar_df"

# Function to read and stream data into DuckDB
stream_data_to_duckdb <- function(file_path, con, table_name) {
  # Open a gzfile connection for reading
  con_gz <- gzfile(file_path, "rb")
  
  # Initialize variable to check if table creation is the first time
  first_chunk <- TRUE
  
  readr::read_delim_chunked(
    file = con_gz,
    chunk_size = 10000,  # Adjust chunk size based on your system's memory
    callback = function(chunk, pos) {
      # Handle the overwrite logic for the first chunk
      dbWriteTable(con, table_name, chunk, append = !first_chunk, row.names = FALSE, overwrite = first_chunk)
      first_chunk <<- FALSE  # Update first_chunk to FALSE after the first run
      TRUE  # Must return TRUE to keep reading chunks
    },
    delim = "\t",  # Set the correct delimiter based on your file format
    col_names = TRUE,  # Ensure column names are read from the first line of the file
    col_types = cols()  # Optionally specify column types to improve performance
  )
  
  # Close the gzfile connection
  close(con_gz)
}

# Execute the function to stream data
if (!dbIsValid(con)) {
  con <- dbConnect(duckdb::duckdb(), dbdir = "duckdb")
}

stream_data_to_duckdb(file_path, con, table_name)


# query <- "
# SELECT 
  \"RS# (dbSNP)\" AS dbSNP_RS, 
  Type, 
  Name, 
  GeneSymbol, 
  ClinicalSignificance, 
  PhenotypeList 
FROM clinvar_df 
WHERE GeneSymbol = 'TET2'
"

gene_list <- c('TYR', 'OCA2', 'TYRP1', 'SLC45A2', 'SLC24A5', 'LRMDA', 'DCT')

gene_dataframes <- lapply(gene_list, function(gene) {
  query <- sprintf("
        SELECT 
          \"RS# (dbSNP)\" AS dbSNP_RS, 
          Type, 
          Name, 
          GeneSymbol, 
          ClinicalSignificance, 
          PhenotypeList 
        FROM clinvar_df 
        WHERE GeneSymbol = '%s'
    ", gene)
  dbGetQuery(con, query)
})

# Naming the list for easier access
names(gene_dataframes) <- gene_list
