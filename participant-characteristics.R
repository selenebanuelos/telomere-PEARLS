## Author: Selene Banuelos
## Date: 12/5/2025
## Description: Compare participant characteristics between those who's buccal
## DNA passed QC in timepoints 2 and 5

# setup
library(dplyr)
library(gtsummary)

# import data ##################################################################
# PEARLS demographics data
pearls <- read.csv('data-raw/pearls_dataset_2022-07-08.csv')

# participant sex data
sex <- read.csv('data-raw/pearlsbio_data_SeleneBanuelos_2025_08_06.csv') %>%
  rename('pearls_id' = 'subjectid')

# import buccal DNA QC data
dna_qc <- read.csv('data-processed/dna-qc.csv')


# merge demographics data with buccal DNA info
merged <- pearls %>%
  filter(visitnum == 1 | visitnum == 2) %>%
  select(c(pearls_id, 
           visitnum,
           child_age_years_demo,
           child_age_years_exam,
           today_s_date_r
           )
         ) %>%
  full_join(.,
            dna_qc,
            by = c('pearls_id', 'visitnum')
  ) %>%
  full_join(., sex, by = 'pearls_id')

# create summary table
merged %>%
  filter(visitnum == 2 | visitnum == 5) %>%
  tbl_summary(by = visitnum, # stratify table by timepoint
              include = c(dna_qc_passed,
                          child_age_years_exam,
                          sex)
              )

# create reasons-for-missing table
merged %>%
  filter(dna_qc_passed == 0) %>%
  tbl_summary(by = visitnum,
               include = c(dna_conc_5,
                           dna_pure))
