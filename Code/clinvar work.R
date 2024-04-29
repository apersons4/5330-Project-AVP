library(tidyverse)

TYR_clinvar_cleaned <- TYR_clinvar |> 
  filter(!PhenotypeList %in% c('not provided', 'not specified', "not specified/|not provided", '-' )) |> 
  mutate(PhenotypeList = str_replace_all(PhenotypeList, "\\|", ",")) |> 
  mutate(PhenotypeList = str_replace_all(
    PhenotypeList, 
    "not provided,?\\b|not specified,?\\b", "" # Regex to match 'not provided' and 'not provided,' without affecting subsequent characters
  )) |> 
  mutate(PhenotypeList = str_replace_all(
    PhenotypeList, 
    ",\\s*$", ""  # Regex to match a comma followed by any whitespace at the end of the string
  )) |> 
  filter(PhenotypeList != '') |> 
  mutate(
    Disease = case_when(
      str_detect(PhenotypeList, "albinism|PIGMENTATION") & str_detect(PhenotypeList, ",|;") ~ "Albinism Condition",
      !str_detect(PhenotypeList, "[,;]") ~ PhenotypeList,
      TRUE ~ "Other"  # Default case if none of the above conditions are met
    )
  ) |> 
  mutate(Disease = case_when(
    str_detect(Disease, 'albinism') ~ 'Albinism Condition',
    TRUE ~ Disease))

OCA2_clinvar_cleaned <- OCA2_clinvar |> 
  filter(!PhenotypeList %in% c('not provided', 'not specified', "not specified/|not provided", '-', 'See cases' )) |> 
  mutate(PhenotypeList = str_replace_all(PhenotypeList, "\\|", ",")) |> 
  mutate(PhenotypeList = str_replace_all(
    PhenotypeList, 
    "not provided,?\\b|not specified,?\\b", "" # Regex to match 'not provided' and 'not provided,' without affecting subsequent characters
  )) |> 
  mutate(PhenotypeList = str_replace_all(
    PhenotypeList, 
    ",\\s*$", ""  # Regex to match a comma followed by any whitespace at the end of the string
  ))|> 
  filter(PhenotypeList != '') |> 
  mutate(
    Disease = case_when(
      str_detect(PhenotypeList, regex('albinism', ignore_case = TRUE)) ~ 'Albinism Condition',
      TRUE ~ PhenotypeList
    )
  )

TYRP1_clinvar_cleaned <- TYRP1_clinvar |> 
  filter(!PhenotypeList %in% c('not provided', 'not specified', "not specified/|not provided", '-', 'See cases' )) |> 
  mutate(PhenotypeList = str_replace_all(PhenotypeList, "\\|", ",")) |> 
  mutate(PhenotypeList = str_replace_all(
    PhenotypeList, 
    "not provided,?\\b|not specified,?\\b", "" # Regex to match 'not provided' and 'not provided,' without affecting subsequent characters
  )) |> 
  mutate(PhenotypeList = str_replace_all(
    PhenotypeList, 
    ",\\s*$", ""  # Regex to match a comma followed by any whitespace at the end of the string
  ))|> 
  filter(PhenotypeList != '') |> 
  mutate(
    Disease = case_when(
      str_detect(PhenotypeList, regex('albinism', ignore_case = TRUE)) ~ 'Albinism Condition',
      TRUE ~ PhenotypeList
    )
  )

SLC45A2_clinvar_cleaned <- SLC45A2_clinvar |> 
  filter(!PhenotypeList %in% c('not provided', 'not specified', "not specified/|not provided", '-', 'See cases' )) |> 
  mutate(PhenotypeList = str_replace_all(PhenotypeList, "\\|", ",")) |> 
  mutate(PhenotypeList = str_replace_all(
    PhenotypeList, 
    "not provided,?\\b|not specified,?\\b", "" # Regex to match 'not provided' and 'not provided,' without affecting subsequent characters
  )) |> 
  mutate(PhenotypeList = str_replace_all(
    PhenotypeList, 
    ",\\s*$", ""  # Regex to match a comma followed by any whitespace at the end of the string
  ))|> 
  filter(PhenotypeList != '') |> 
  mutate(
    Disease = case_when(
      str_detect(PhenotypeList, regex('albinism', ignore_case = TRUE)) ~ 'Albinism Condition',
      TRUE ~ PhenotypeList
    )
  )

SLC24A5_clinvar_cleaned <- SLC24A5_clinvar |> 
  filter(!PhenotypeList %in% c('not provided', 'not specified', "not specified/|not provided", '-', 'See cases' )) |> 
  mutate(PhenotypeList = str_replace_all(PhenotypeList, "\\|", ",")) |> 
  mutate(PhenotypeList = str_replace_all(
    PhenotypeList, 
    "not provided,?\\b|not specified,?\\b", "" # Regex to match 'not provided' and 'not provided,' without affecting subsequent characters
  )) |> 
  mutate(PhenotypeList = str_replace_all(
    PhenotypeList, 
    ",\\s*$", ""  # Regex to match a comma followed by any whitespace at the end of the string
  ))|> 
  filter(PhenotypeList != '') |> 
  mutate(
    Disease = case_when(
      str_detect(PhenotypeList, regex('albinism', ignore_case = TRUE)) ~ 'Albinism Condition',
      TRUE ~ PhenotypeList
    )
  )

LRMDA_clinvar_cleaned <- LRMDA_clinvar |> 
  filter(!PhenotypeList %in% c('not provided', 'not specified', "not specified/|not provided", '-', 'See cases' )) |> 
  mutate(PhenotypeList = str_replace_all(PhenotypeList, "\\|", ",")) |> 
  mutate(PhenotypeList = str_replace_all(
    PhenotypeList, 
    "not provided,?\\b|not specified,?\\b", "" # Regex to match 'not provided' and 'not provided,' without affecting subsequent characters
  )) |> 
  mutate(PhenotypeList = str_replace_all(
    PhenotypeList, 
    ",\\s*$", ""  # Regex to match a comma followed by any whitespace at the end of the string
  ))|> 
  filter(PhenotypeList != '') |> 
  mutate(
    Disease = case_when(
      str_detect(PhenotypeList, regex('albinism', ignore_case = TRUE)) ~ 'Albinism Condition',
      TRUE ~ PhenotypeList
    )
  )

DCT_clinvar_cleaned <- DCT_clinvar |> 
  filter(!PhenotypeList %in% c('not provided', 'not specified', "not specified/|not provided", '-', 'See cases' )) |> 
  mutate(PhenotypeList = str_replace_all(PhenotypeList, "\\|", ",")) |> 
  mutate(PhenotypeList = str_replace_all(
    PhenotypeList, 
    "not provided,?\\b|not specified,?\\b", "" # Regex to match 'not provided' and 'not provided,' without affecting subsequent characters
  )) |> 
  mutate(PhenotypeList = str_replace_all(
    PhenotypeList, 
    ",\\s*$", ""  # Regex to match a comma followed by any whitespace at the end of the string
  ))|> 
  filter(PhenotypeList != '') |> 
  mutate(
    Disease = case_when(
      str_detect(PhenotypeList, regex('albinism', ignore_case = TRUE)) ~ 'Albinism Condition',
      TRUE ~ PhenotypeList
    )
  )

all_genes <- bind_rows(TYR_clinvar_cleaned, 
                       OCA2_clinvar_cleaned, 
                       TYRP1_clinvar_cleaned, 
                       SLC45A2_clinvar_cleaned,
                       SLC24A5_clinvar_cleaned,
                       LRMDA_clinvar_cleaned,
                       DCT_clinvar_cleaned)
summarized_disease <- all_genes |> 
  group_by(GeneSymbol, Disease) |> 
  summarise(count = n(),
            .groups = 'drop_last')

num_albinism_vars <- summarized_disease |> 
  filter(Disease == 'Albinism Condition') |> 
  mutate(OCA_Type = case_when(
    str_detect(GeneSymbol, 'TYR$') ~ 'OCA1',
    str_detect(GeneSymbol, 'OCA2') ~ 'OCA2',
    str_detect(GeneSymbol, 'TYRP1') ~ 'OCA3',
    str_detect(GeneSymbol, 'SLC45A2') ~ 'OCA4',
    str_detect(GeneSymbol, 'SLC24A5') ~ 'OCA6',
    str_detect(GeneSymbol, 'LRMDA') ~ 'OCA7',
    str_detect(GeneSymbol, 'DCT') ~ 'OCA8'
  )) |> 
  arrange(OCA_Type)

write_csv(num_albinism_vars, 'Figures/Table 2.csv')
