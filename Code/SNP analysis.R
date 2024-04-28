library(tidyverse)

SNPs_TYR <- read_delim('snp_result TYR.txt') |> 
  filter(!is.na(pos))

CDS_variant <- SNPs_TYR |> 
  filter(str_detect(function_class, ';?coding_sequence_variant;?|initiator_codon_variant|splice_|terminator_codon_variant|UTR_variant'))

Intron_variant <- SNPs_TYR |> 
  filter(str_detect(function_class, ';?intron_variant;?|upstream_transcript_variant|downstream_'))
