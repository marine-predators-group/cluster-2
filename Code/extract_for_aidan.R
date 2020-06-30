## blue and mako data for Aidan vertical habitat use analysis

library(googledrive)
library(tidyverse)

## get meta & filter
meta <- read.table('../nip_drake/RawData/all_tag_meta.csv', sep=',', header=T, blank.lines.skip = F, skip=0, stringsAsFactors = F)
meta_sub <- meta %>% filter(instrument_type == 'popup' & end_details != 'DNR' &
                              platform %in% c('Isurus oxyrinchus',
                                              'Prionace glauca') &
                              person_owner %in% c('Camrin Braun', 
                                                  'Gregory Skomal'))

meta_sub$instrument_name
#[1] "160424_2016_141255" "160424_2016_133017" "160424_2016_141258" "160424_2016_106754" "160424_2016_133016" "160424_2015_141259"
#[7] "160424_2016_133021" "160424_2016_141247" "160424_2016_154096" "160424_2016_133018" "160424_2015_141256" "160424_2015_141254"
#[13] "160424_2016_163097" "159924_2015_141257" "159924_2017_163096" "159924_2017_163098" "159924_2008_78680"  "159924_2008_78682" 
#[19] "159924_2008_78683" 

## provide dir where data lives
#existing_dir <- '~/ebs/Data/data_org/'
#fList <- list.files(existing_dir)

for (i in 1:nrow(meta_sub)){
  
  system(paste('aws s3 cp s3://braun-data/Data/data_org/', meta_sub$instrument_name[i], '/', meta_sub$instrument_name[i], '_eTUFF.txt ~/Downloads/aidan/', sep=''))
  
}

## then just drop to google drive at /MPG/data/
## can't get the googledrive package code to play nice with the shared (team?) MPG drive

#drive_ls('MPG')
#drive_find(n_max = 30)

#existing_dir <- '~/Downloads/aidan/'
#for (i in 1:nrow(meta_sub)){
  
#  etuff_file <- paste(existing_dir, meta_sub$instrument_name[i], '_eTUFF.txt', sep='')
#  drive_upload(etuff_file, "MPG/data")
  
#}



