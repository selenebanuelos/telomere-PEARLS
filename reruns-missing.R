## Author: Selene Banuelos
## Date: 11/25/2025
## Description: Identify timepoint 5 participants that have buccal telomere 
## probe data and those who were assayed more than once

# setup 
library(dplyr)
library(stringr)
library(purrr)

# import data ------------------------------------------------------------------
# save telomere raw data file names as list
run1_names <- list.files(path = 'data-raw/t5-buccal/run1/',
                         pattern = '.csv',
                         full.names = TRUE)

run2_names <- list.files(path = 'data-raw/t5-buccal/run2-tel/',
                         pattern = '.csv',
                         full.names = TRUE)

# import all raw data files and combine into one dataframe
run1_data <- map_dfr(run1_names, 
                   # import .csv as df and add column with original file name
                   function(x) read.csv(x) %>% mutate(file_name = x, run = 1))

run2_data <- map_dfr(run2_names, 
                     # import .csv as df and add column with original file name
                     function(x) read.csv(x) %>% mutate(file_name = x, run = 2))

# import master list of all T5 participants with buccal DNA samples
all_t5 <- read.csv('data-raw/PEARLSBio-T5sWithBuccal_DATA_2025-12-02_1043.csv')

# data wrangling ---------------------------------------------------------------
avail_data <- rbind(run1_data, run2_data) %>%
  # create batch column (rundate_plate)
  mutate(batch = str_extract(
    file_name, 
    '(?<=TEL_)(.*)(?= - Quant)') # (?<=prefix)(keep)(?=suffix)
    ) %>%
  select(Sample, Well, Content, batch, run, contains('Cq')) %>%
  # remove standards, positive controls, no template controls
  filter(!grepl('Std|Pos Ctrl|NTC', Content)) %>%
  # strip any leading zeros from sample names
  mutate(Sample = str_remove(Sample,'^0+'))

# identify reruns with batch names
rerun_ids <- avail_data %>%
  group_by(Sample) %>%
  # keep samples that were run in > 1 batch
  filter(n_distinct(batch) >1) %>% 
  # remove triplicates for each sample
  distinct(Sample, batch) %>%
  # rename sample ID for joining
  rename(specimenid = Sample) %>%
  # create vector of sample names
  pull(specimenid) %>%
  # remove triplicates
  unique(.)

# create dataset with missing data and rerun information
reruns_missing <- avail_data %>%
  # keep sample IDs and run ID
  select(Sample, Well, batch, run) %>%
  # remove triplicates
  distinct(.) %>%
  # rename sample for joining
  rename(specimenid = Sample) %>%
  # change variable type for joining
  mutate(specimenid = as.integer(specimenid),
         # create indicator of available telomere data
         tel_data = 1) %>%
  # join all t5 sample data to telomere data availability 
  right_join(., all_t5, by = 'specimenid') %>%
  # fill in indicator of available telomere data with 0's for any missing data
  mutate_at(vars(tel_data), ~replace(., is.na(.), 0)) %>%
  # fill in indicator of rerun status with 0's for any available telomere data
  mutate(rerun = case_when(specimenid %in% rerun_ids & tel_data == 1 ~ 1,
                           !specimenid %in% rerun_ids & tel_data == 1 ~0,
                           is.na(tel_data) ~ NA)) %>%
  # rename participant ID for consistency with other datasets
  rename(pearls_id = subjectid)

# output -----------------------------------------------------------------------
write.csv(reruns_missing,
          'data-processed/reruns-missing-tel-T5.csv',
          row.names = FALSE)