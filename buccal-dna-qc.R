## Author: Selene Banuelos
## Date: 6/15/2026
## Description: Assess DNA quality across t2, t4, and t5 buccal samples and pull
## out t5 participants who fail DNA QC

# setup 
library(dplyr)
library(janitor)

# import data ------------------------------------------------------------------
# buccal DNA concentration and purity (260/280) data
t2 <- read.csv('data-raw/pearls_dataset_Austin_Le_2022-07-14.csv')
t4 <- read.csv('data-raw/T4 Buccal Swab DNA Dilutions.csv')
t5 <- read.csv('data-raw/PEARLSBio-T5sWithBuccal_DATA_2025-12-02_1043.csv')

# data wrangling ---------------------------------------------------------------
# clean up t2 data (this t2 data does not contain reruns)
t2_clean <- t2 %>%
    select(c(pearls_id, 
           specimenid, 
           contains(c('conc_', 'purity_')))
           ) %>%
    remove_empty('rows') %>% 
    # add in visitnum
    mutate(visitnum = 2)

# clean up t4 data
t4_clean <- t4 %>%
    remove_empty('cols') %>%
    # add in visit number
    mutate(visitnum = 4) %>%
    # remove 'average' columns
    select(-contains('Ave')) %>%
    # rename columns for downstream joining
    rename(purity_DZ = DZ_purity,
           purity_RD = RD_purity,
           conc_DZ = DZ_Conc,
           conc_RD = RD_Conc)

# merge t2, t4, and t5 buccal DNA data
merged <- t5 %>%
    # rename t5 participant ID variable for joining
    rename('pearls_id' = 'subjectid') %>%
    # join t2 data
    full_join(., 
              t2_clean, 
              by = c('specimenid', 'pearls_id', 'visitnum')) %>%
    # join t4 data
    full_join(., 
              t4_clean,
              by = c('specimenid', 
                     'visitnum',
                     'purity_DZ',
                     'purity_RD',
                     'conc_DZ',
                     'conc_RD'))
# t5 had an extra participant outside of the 555 in the cohort?

# QC DNA -----------------------------------------------------------------------
# DNA purity check: 1.7 <= 260/280 <= 2.0
is_pure <- function(x) {1.7 <= x & x <= 2.0} 

# assess QC metrics (DNA concentration and purity)
dna_qc <- merged %>%
    # check that any purity measure (260/280) is between 1.7-2.0
    mutate(dna_pure = case_when(is_pure(purity) == TRUE ~ 1,
                                is_pure(purity_DZ) == TRUE ~ 1,
                                is_pure(purity_RD) == TRUE ~ 1,
                                is_pure(purity) == FALSE ~ 0,
                                is_pure(purity_DZ) == FALSE ~ 0,
                                is_pure(purity_RD) == FALSE ~ 0,
                                .default = NA)) %>%
    # check that any DNA concentration measure is >= 5ng/ul
    mutate(dna_conc_5 = case_when(os_dna_conc >= 5 ~ 1,
                                  conc_DZ >= 5 ~ 1,
                                  conc_RD >= 5 ~ 1,
                                  os_dna_conc < 5 ~ 0,
                                  conc_DZ < 5 ~ 0,
                                  conc_RD < 5 ~ 0,
                                  .default = NA)) %>%
    # check that DNA concentration and purity pass metrics
    mutate(dna_qc_passed = case_when(dna_pure == 1 & dna_conc_5 == 1 ~ 1,
                                     dna_pure == 0 | dna_conc_5 == 0 ~ 0,
                                     is.na(dna_pure) | is.na(dna_conc_5) ~ 0))

# pull out all t5 samples that did not pass QC
t5_qc_fail <- dna_qc %>%
    filter(visitnum == 5,
           dna_qc_passed == 0 | is.na(dna_qc_passed))

# output -----------------------------------------------------------------------
write.csv(dna_qc,
          'data-processed/buccal-dna-qc.csv',
          row.names = FALSE)

write.csv(t5_qc_fail,
          'data-processed/buccal-dna-qc-failed-t5.csv',
          row.names = FALSE)