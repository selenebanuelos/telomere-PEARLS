## Author: Selene Banuelos
## Date: 6/17/2026
## Description: Merge buccal DNA QC and telomere length assay data

# setup
library(dplyr)

# import data ------------------------------------------------------------------
tel_data <- read.csv('data-processed/merged-runs-tel-36b4.csv')
dna_qc <- read.csv('data-processed/buccal-dna-qc.csv')

# data wrangling ---------------------------------------------------------------
# clean up buccal DNA QC data
clean_qc <- dna_qc %>%
  filter(visitnum == 5) %>%
  select(pearls_id,
         specimenid,
         visitnum, 
         os_dna_conc, 
         purity,
         dna_qc_passed) %>%
  # rename for joining
  rename(Sample = specimenid) %>%
  # change data type for joining
  mutate(Sample = as.character(Sample))

# merge buccal DNA QC data to telomere length assay data
merged <- left_join(tel_data, clean_qc, by = 'Sample')

# output------------------------------------------------------------------------
write.csv(merged, 'data-processed/merged-runs-tel-36b4.csv', row.names = FALSE)