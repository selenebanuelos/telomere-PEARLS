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

# explore t2
t2_no_sample <- dna_qc %>%
  filter(visitnum == 2,
         is.na(specimenid))

t2_low_dna <- dna_qc %>%
  filter(visitnum ==2,
         !is.na(specimenid)
  ) %>%
  filter(dna_conc_5 == 0)

t2_contaminated <- dna_qc %>%
  filter(visitnum ==2,
         !is.na(specimenid)
  ) %>%
  filter(dna_pure == 0)

t2_passed <- dna_qc %>%
  filter(visitnum == 2, 
         dna_qc_passed ==1)
  
# explore t5
t5_no_sample <- dna_qc %>%
  filter(visitnum == 5,
         is.na(specimenid))

t5_low_dna <- dna_qc %>%
  filter(visitnum ==5,
         !is.na(specimenid)
  ) %>%
  filter(dna_conc_5 == 0)

t5_contaminated <- dna_qc %>%
  filter(visitnum ==5,
         !is.na(specimenid)
  ) %>%
  filter(dna_pure == 0)

t5_passed <- dna_qc %>%
  filter(visitnum == 5, 
         dna_qc_passed ==1)

# for t2 samples that passed DNA QC, average concentration and 260/280 measures
t2 <- dna_qc %>%
  filter(visitnum ==2) %>%
  mutate(dna_conc = signif( ( (conc_DZ+conc_RD) / 2), digits = 4 ),
         dna_purity = signif( ( (purity_DZ+purity_RD) / 2), digits = 3)
  ) %>%
  select(c(pearls_id, specimenid, visitnum, dna_conc, dna_purity, dna_qc_passed))

# clean up t5 data for merge with t2
t5 <- dna_qc %>%
  filter(visitnum == 5) %>%
  select(c(pearls_id, specimenid, visitnum, os_dna_conc, purity, dna_qc_passed)) %>%
  rename('dna_conc' = 'os_dna_conc',
         'dna_purity' = 'purity')

t2_t5_passed <- rbind(filter(t2, dna_qc_passed == 1),
                      filter(t5, dna_qc_passed == 1)
                      )

# merge demographics data with buccal DNA info
merged <- pearls %>%
  filter(visitnum == 2) %>%
  select(c(pearls_id,
           visitnum,
           child_age_years_exam
           )
         ) %>%
  full_join(.,
            t2_t5_passed,
            by = c('pearls_id', 'visitnum')
  ) %>%
  full_join(., sex, by = 'pearls_id') %>%
  mutate(missing_sample = case_when(is.na(specimenid) ~ 1,
                                    .default = 0)
         )

# create summary table of those with buccal DNA samples
merged %>%
  tbl_summary(by = visitnum, # stratify table by timepoint
              include = c(child_age_years_exam,
                          sex),
              missing_text = 'Missing'
              )

# create summary table of those missing buccal DNA samples
merged %>%
  filter(visitnum == 2 | visitnum == 5,
         dna_qc_passed == 0 | is.na(dna_qc_passed)
         ) %>%
  tbl_summary(by = visitnum,
              include = c(dna_conc_5,
                          dna_pure,
                          missing_sample),
              statistic = list(
                all_categorical() ~ '{n} ({p}%)'
              ),
              missing_text = 'Missing'
  )