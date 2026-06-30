## Author: Selene Banuelos
## Date: 11/25/2025
## Description: Identify timepoint 5 participants that have telomere-specific
## and 36B4-specific probe data from telomere length assay of buccal DNA samples
## that were assayed more than once

# setup 
library(dplyr)
library(stringr)
library(purrr)

# import data ------------------------------------------------------------------
# save telomere raw data file names as list
tel_36b4 <- read.csv('data-processed/merged-runs-tel-36b4.csv')

# import master list of all T5 participants with buccal DNA samples
all_t5 <- read.csv('data-raw/PEARLSBio-T5sWithBuccal_DATA_2025-12-02_1043.csv')

# data wrangling ---------------------------------------------------------------
# identify which samples have been assayed more than once
rerun_ids <- tel_36b4 %>%
  # remove standards, positive controls, no template controls
  filter(!grepl('Std|Pos Ctrl|NTC', Content)) %>%
  group_by(Sample, Target) %>%
  # keep samples with data in more than 1 file
  filter(n_distinct(file_name) > 1) %>% 
  # remove triplicates for each sample
  distinct(Sample, file_name) %>%
  # rename sample ID for joining
  rename(specimenid = Sample) %>%
  # remove triplicates
  # unique(.) %>%
  # add in rerun indicator column
  mutate(rerun = 1) %>%
  ungroup(.)

  # # create vector of sample names
  # pull(specimenid) %>%


# # identify which samples have data in either run 1 or run 2
# tel_ids <- tel_36b4 %>%
#   # remove standards, positive controls, no template controls
#   filter(!grepl('Std|Pos Ctrl|NTC', Content)) %>%
#   # rename sample ID for joining
#   rename(specimenid = Sample) %>%
#   # add in data availability indicator column
#   mutate(data_avail = 1) %>%
#   # keep only necessary colums
#   select(specimenid, Target, file_name, data_avail)
  # # create vector of sample names
  # pull(specimenid) %>%
  # remove within plate triplicates/run duplicates/probe-type duplicates
  # unique(.) %>%


# remove unnecessary columns from all_t5
all_t5 <- select(all_t5, specimenid, subjectid, visitnum)
  
# create dataset with data availability and rerun information
avail_reruns <- tel_36b4 %>%
  # remove standards, positive controls, no template controls
  filter(!grepl('Std|Pos Ctrl|NTC', Content)) %>%
  # rename sample for joining
  rename(specimenid = Sample) %>%
  # keep sample IDs and run ID
  select(specimenid, Target, Well, file_name, date, set, run) %>%
  # add in data availability indicator column
  mutate(data_avail = 1) %>%
  # change variable type for joining
  mutate(specimenid = as.integer(specimenid)) %>%
  # join all t5 sample data to telomere data availability 
  right_join(., all_t5, by = 'specimenid') %>%
  # add in 0's where data is not available for sample
  mutate(data_avail = ifelse(is.na(data_avail), 0, data_avail)) %>%
  # change variable type for joining
  mutate(specimenid = as.character(specimenid)) %>%
  # join with rerun indicator column
  left_join(., rerun_ids, by = c('Target', 'specimenid', 'file_name')) %>%
  # add in 0's where sample is not rerun
  mutate(rerun = ifelse(is.na(rerun), 0, rerun))


original_added <- rerun_ids %>%
  rename(Sample = specimenid) %>%
  right_join(., tel_36b4, by = c('Target', 'Sample', 'file_name')) %>%
  # add in 0's where sample is not rerun
  mutate(rerun = ifelse(is.na(rerun), 0, rerun))

  # # create indicator of available telomere data
  # mutate(tel_data = case_when(specimenid %in% tel_ids ~ 1,
  #                             !specimenid %in% tel_ids ~ 0)) %>%
  # # rename participant ID for consistency with other datasets
  # rename(pearls_id = subjectid)

  # # add rerun indicator column
  # left_join(., rerun_ids, by = c('Target', 'specimenid', 'file_name'))
  # 
  # # fill in indicator of rerun status with 0's for any available telomere data
  # mutate(rerun = case_when(specimenid %in% rerun_ids & tel_data == 1 ~ 1,
  #                          !specimenid %in% rerun_ids & tel_data == 1 ~ 0,
  #                          is.na(tel_data) ~ NA)) %>%


# # add rerun variable into original telomere/36B4 dataset
# avail_rerun <- avail %>%
#   # keep only specimen ID and telomere data availability indicator
#   select(specimenid, tel_data) %>%
#   # change variable type for joining
#   mutate(specimenid = as.character(specimenid)) %>%
#   # rename for joining
#   rename(Sample = specimenid) %>%
#   # remove triplicates
#   unique(.) %>%
#   # join telomere availability back to original dataset
#   right_join(., tel_36b4, by = 'Sample') %>%
#   # join rerun column back into original dataset
#   left_join(., rerun_ids, by = c('Target', 'Sample', 'file_name'))

# output -----------------------------------------------------------------------
# create file with telomere data availability and rerun information
write.csv(avail_reruns,
          'data-processed/tel-avail-reruns-T5.csv',
          row.names = FALSE)

# add rerun variable into original telomere/36B4 dataset
write.csv(original_added, 
          'data-processed/merged-runs-tel-36b4.csv', 
          row.names = FALSE)