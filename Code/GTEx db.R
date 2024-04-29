library(DBI)
library(duckdb)
library(tidyverse)

con <- dbConnect(duckdb(), dbdir = "duckdb")

file_path <- "GTEx_Analysis_2017-06-05_v8_RNASeQCv1.1.9_gene_median_tpm.gct.gz"
table_name <- "GTEx_df"

# Function to read and stream data into DuckDB
stream_data_to_duckdb <- function(file_path, con, table_name) {
  # Open a gzfile connection for reading
  con_gz <- gzfile(file_path, "rb")
  
  readr::read_delim_chunked(
    file = con_gz, 
    chunk_size = 10000,  # Adjust chunk size based on your system's memory
    callback = function(chunk, pos) {
      # Use dbWriteTable from DBI to insert each chunk into DuckDB
      dbWriteTable(con, table_name, chunk, append = TRUE, row.names = FALSE, overwrite = FALSE)
      TRUE  # Must return TRUE to keep reading chunks
    },
    delim = "\t",  # Set the correct delimiter based on your file format
    col_types = cols(),  # Optionally specify column types to improve performance
    skip = 2  # Adjust this if your file includes headers that should be skipped
  )
  
  # Close the gzfile connection
  close(con_gz)
}

# Execute the function to stream data
if (!dbIsValid(con)) {
  con <- dbConnect(duckdb::duckdb(), dbdir = "duckdb")
}

stream_data_to_duckdb(file_path, con, table_name)

patterns <- paste0(gene_list, "%")
#query <- sprintf("SELECT * FROM GTEx_df WHERE Name IN %s OR Description IN %s", sql_known_genes, sql_known_genes)
#query <- sprintf("SELECT * FROM GTEx_df WHERE Name LIKE ANY (ARRAY[%s]) OR Description LIKE ANY (ARRAY[%s])", sql_modified_list, sql_modified_list)
like_conditions <- paste(sprintf("Name LIKE '%s' OR Description LIKE '%s'", patterns, patterns), collapse = " OR ")
query <- sprintf("SELECT * FROM GTEx_df WHERE %s", like_conditions)
result <- dbGetQuery(con, query)

dbDisconnect(con)