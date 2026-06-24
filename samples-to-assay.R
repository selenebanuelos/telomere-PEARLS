## Author: Selene Banuelos
## Date: 1/30/2026
## Description: Make dataset of T5 PEARLS samples that need to be nanodropped
## and/or assayed for telomere length

# setup
library(dplyr)
library(janitor)
library(tidyr)

# import data ------------------------------------------------------------------
# T5 samples that have tel-length data available and assayed more than once
reruns_avail <- read.csv('data-processed/tel-avail-reruns-T5.csv')

# information about plate contamination
cont <- read.csv('data-processed/merged-runs-tel-36b4.csv')

# information about buccal DNA quality
dna <- read.csv('data-processed/buccal-dna-qc.csv')

# data wrangling ---------------------------------------------------------------
# rename participant ID for downstream joining
reruns_avail <- reruns_avail %>% rename(pearls_id = subjectid)

# clean up DNA QC data
dna_clean <- dna %>%
  # only keep data from T5
  filter(visitnum == 5) %>%
  # remove empty columns
  remove_empty('cols') %>%
  # remove unecessary columns
  select(-c(dna_pure, dna_conc_5))
  
# merge data on reruns, availability, DNA quality, and plate contamination
merged <- cont %>%
  # remove standards, positive controls, no template controls
  filter(!grepl('Std|Pos Ctrl|NTC', Content)) %>%
  # join to info on rerun status and data availability
  full_join(., reruns_avail, by = intersect(names(.), names(reruns_avail))) %>%
  # removed incomplete DNA QC columns from already assayed samples
  select(-c(os_dna_conc, purity, dna_qc_passed)) %>%
  # re-add in complete data on DNA QC
  left_join(., dna_clean, by = intersect(names(.), names(dna_clean)))

# separate samples into categories ---------------------------------------------
# samples that were not included in Run 1 or Run 2
no_data <- merged %>% filter(data_avail == 0)

# samples that have telomere and 36B4 data available from non-contaminated plates
data_avail <- merged %>% 
  # data available and run in plates with no sign of contamination
  filter(data_avail == 1 & ntc_cont == 0) %>%
  # keep only sample ID and participant ID
  select(pearls_id, Sample, Target, data_avail) %>%
  # remove triplicates
  distinct(.) %>%
  # make data wider to check data availability across both probes
  pivot_wider(id_cols = c('pearls_id', 'Sample'),
              names_from = Target,
              names_glue = 'avail_{Target}',
              values_from = data_avail) %>%
  # keep samples that have data across both probes only
  filter(!is.na(avail_TEL) & !is.na(avail_36B4))

# samples included in Run 2 ONLY, in contaminated plates
cont_plates <- merged %>% 
  # assayed only once in Run 2 on plate with signs of contamination
  filter(rerun == 0, run == 2, ntc_cont == 1) %>%
  # either telomere or 36B4 data was on contaminated plate (Target doesn't matter)
  select(-Target) %>%
  # keep only relevant columns
  select(specimenid, 
         pearls_id, 
         run, 
         ntc_cont, 
         os_dna_conc, 
         purity, 
         dna_qc_passed) %>%
  # remove triplicates
  distinct(.) 

# samples included in Run 1 or Run 2 that did not pass DNA QC
qc_fail_run <- merged %>%
  # keep DNA buccal samples that failed QC
  filter(dna_qc_passed == 0) %>%
  # only keep relevant variables 
  select(specimenid, 
         pearls_id, 
         run, 
         data_avail, 
         os_dna_conc, 
         purity, 
         dna_qc_passed)

# make sure all samples have been accounted for
nrow(dna_clean) == (nrow(data_avail) + nrow(no_data) + nrow(cont_plates)) # TRUE

# merge all samples that need to be run ----------------------------------------
# select 5 samples with highest concentrations in Run 1 to include in next run
# to adjust for run batches downstream
five <- merged %>%
  # take 5 samples from Run 1 that passed DNA QC
  filter(run == 1 & dna_qc_passed == 1) %>%
  # keep only relevant columns
  select(specimenid, 
         pearls_id, 
         run, 
         ntc_cont, 
         os_dna_conc, 
         purity, 
         dna_qc_passed) %>%
  # remove triplicates
  distinct(.) %>%
  # order by DNA concentration in descending order
  arrange(desc(os_dna_conc)) %>%
  # keep top 5 samples with highest concentrations
  slice(1:5)
  
# create data set of all samples that should be run next
next_run <- no_data %>% # samples not included in Run 1 or Run 2
  # keep only relevant columns
  select(specimenid, 
         pearls_id, 
         run, 
         ntc_cont, 
         os_dna_conc, 
         purity, 
         dna_qc_passed) %>%
  # samples from Run 2 ONLY, that were from contaminated plates
  full_join(., cont_plates, by = intersect(names(.), names(cont_plates))) %>%
  # add in 5 samples from Run 1
  rbind(five)
  
# create data set of samples that should be re-nanodropped
nanodrop <- merged %>% 
  # keep only samples that failed DNA QC
  filter(dna_qc_passed == 0) %>%
  # keep only relevant columns
  select(specimenid, 
         pearls_id, 
         run, 
         ntc_cont, 
         os_dna_conc, 
         purity, 
         dna_qc_passed) %>%
  # create indicator if sample was previously included in Run 1 or Run 2
  mutate(run_before = case_when(run == 1 | run == 2 ~ 1,
                                is.na(run) ~ 0)) %>%
  select(-c(run, ntc_cont, dna_qc_passed)) %>%
  # remove triplicates
  distinct(.) 
  
# create data dictionary -------------------------------------------------------
# variable names
var_names <- names(next_run)

# data types
data_type <- sapply(next_run, class)

# variable descriptions
descriptions <- c("Sample ID",
                  "Participant PEARLS ID",
                  "Assay run batch",
                  'No template control well with Cq<35, indicating contamination',
                  "DNA concentration (ng/ul)",
                  '260/280 ratio',
                  "Passed DNA QC metrics: 1.7 <= 260/280 <= 2.0 and at least 5ng/ul of DNA"
                  )

# possible values for variables
values <- c("3 digit integer",
            "PXXXX",
            "1=Run1 | 2=Run2 | NA=not yet run",
            "1=yes | 0=no | NA=no yet run",
            "numeric >= 0",
            "numeric >= 0",
            "1=yes| 0=no"
            )
            
# create dictionary
dict <- data.frame(var_names, 
                   data_type, 
                   descriptions, 
                   values,
                   row.names = NULL)

# output -----------------------------------------------------------------------
# save list of samples to run next
write.csv(next_run,
          "data-processed/samples-to-assay.csv",
          row.names = FALSE)

# save list of samples to re-nandrop
write.csv(nanodrop,
          "data-processed/samples-to-nanodrop.csv",
          row.names = FALSE)

# save data dictionary
write.csv(dict,
          "data-processed/samples-to-assay-DATADICTIONARY.csv",
          row.names = FALSE)
