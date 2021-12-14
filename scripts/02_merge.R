## Merges Tobii, Subject, and AOI data for Novel project

## Load libraries
library(eyetrackingR)
library(tidyverse)
library(forcats)
library(here)
library(readxl)
library(lubridate)
library(janitor)


## Read Data Files
aoi <- read_csv(here("data/Novel_AOI.csv")) %>% rename(trial_type = trial.type)
order <- read_csv(here("./data/Novel_TrialOrders.csv"))
participants <- read_csv(here("./data/2021-11-04_participant_data_for_upload.csv"))

load(here("data/all_novel_eyetracking.Rda"))

## add unique stimulus column
dataset <- dataset %>%
  clean_names() %>%
  mutate(stimulus = as.factor(paste(study_order, media_name, trial_number, sep = "_")))  

## Prep order
order <- order %>%
  mutate(stimulus = as.factor(paste(study_order, media_name, trial_number, sep = "_")))


## Prep participant dataframe

participants <- participants %>%
  #filter(keeper_traditional == 1) %>%
 # mutate(gender_written = as.factor(gender_written)) %>%
 # mutate(do_birth = ymd(do_birth)) %>%
 # mutate(do_participation = ymd(do_participation)) %>%
  #mutate(age_days = do_participation - do_birth) %>%
  #mutate(gender_coded = case_when(gender_written == "female" ~ 0,
                                  #gender_written == "male" ~ 1)) %>%
  rename(id = recording_name)

## Check if we have the same levels for media_name in dataset and orders

setdiff(order$media_name, dataset$media_name)
setdiff(dataset$media_name, order$media_name)
setdiff(dataset$stimulus, order$stimulus)

## Check if all participants are in the dataset

setdiff(dataset$id, participants$id) #these are ids in the dataset not in the MSL. Should be 0
setdiff(participants$id, dataset$id) #these are ids in the MSL not in the dataset. OK if not 0—some kids don't have recordings

#Fixfor merging

participants <- participants %>%
  mutate(id = case_when(
    id == "Novel_14_S145_47720" ~ "Novel14_S145_47720",
    id == "Novel_14_S148_48417" ~ "Novel14_S148_48417",
    id == "Novel_14_S163_48886" ~ "Novel14_S163_48886",
    id == "Novel_14_S164_49191" ~ "Novel14_S164_49191",
    id == "NovelMono_14_S33_49028" ~ "NovelMono14_S33_49028",
    id == "Novel_14_s18" ~ "Novel_14_S18",
    id == "Novel_14_S142_45497" ~ "Novel14_S142_45497",
    id == "NovelMono_14_S23_48346" ~ "NovelMono14_S23_48346",     
    TRUE ~ id
  ))


setdiff(dataset$id, participants$id) #should be 0 now
setdiff(participants$id, dataset$id) # these kids (6 of them) had technical problems with Tobii, so no videos

dataset <- dataset %>% 
  select(-stimulus, -study_id) #no longer needed, will affect merge later

## Prep and merge the AOI files

PrepAOI <- function(aoi) {
  # Function to add coordinates for the AOIs
  # Calculates further coordinates based on x.topleft, y.topleft, and x and y lengths
  # Fixes coordinates so that the origin not top left, but is bottom left (cartesian)
  
  aoi <- aoi %>%
    rename(L = x.topleft, `T` = y.topleft) %>%
    mutate(R = L + x.length) %>%
    mutate(B = `T` + y.length) %>%
    select(object, location, trial_type, `T`,B,L,R)
  
  return(aoi)
}

aoi <- PrepAOI(aoi)


aoi_target <-  order %>%
  filter(trial_type == "test") %>%
  left_join(aoi, by = c("target_word" = "object", "target_side" = "location")) %>% 
  select(study_order, trial_number, stimulus_set, media_name, `T`, B, L, R) %>%
  distinct()

aoi_distractor <- order %>%
  filter(trial_type == "test") %>%
  left_join(aoi, by = c("distractor_word" = "object", "distractor_side" = "location")) %>%
  select(study_order, trial_number, stimulus_set, media_name, `T`, B, L, R) %>%
  distinct()

aoi_training <- order %>%
  filter(trial_type == "training") %>%
  left_join(aoi, by = c("target_word" = "object", "target_side" = "location")) %>%
  select(study_order, trial_number, stimulus_set, media_name, `T`, B, L, R) %>%
  distinct()


## Merge AOIS


#Join participants dataset to TOBII data, then join order dataset to that
dataset <- dataset %>%
  inner_join(participants, by = c("id", "study_order")) %>%
  inner_join(order, by = c("study_order", "media_name", "stimulus_set", "trial_number"))


#Add AOI columns to the dataset with eyetrackingR -- WARNING column names must match for this to work, and also the AOI dataframes must not have any extra columns except those used for matching
dataset <- dataset %>%
  add_aoi(aoi_dataframe = aoi_target, aoi_name = "target_AOI", x_col = "gaze_point_x", y_col = "gaze_point_y") %>%
  add_aoi(aoi_dataframe = aoi_distractor, aoi_name = "distractor_AOI", x_col = "gaze_point_x", y_col = "gaze_point_y") %>%
  add_aoi(aoi_dataframe = aoi_training, aoi_name = "training_AOI", x_col = "gaze_point_x", y_col = "gaze_point_y") %>%
  mutate(training_away_AOI = case_when(trial_type == "training" & training_AOI == FALSE & trackloss == FALSE ~ TRUE,
                                       trial_type == "training" & training_AOI == TRUE & trackloss == FALSE ~ FALSE))


## Determine if video was in dominant language ---COMMENT: THE FOLLOWING CODE LEAVES SOME PARTICIPANTS WITH NA VALUES (probably kids who don't have Eng or French as best language)

dataset <- dataset %>%
  mutate(trial_lang = case_when(best_lang == "french" & carrier_language == "french" ~ "best",
                                best_lang == "english" & carrier_language == "english" ~ "best",
                                best_lang == "french" & carrier_language == "english" ~ "non-best",
                                best_lang == "english" & carrier_language == "french" ~ "non-best",
                                TRUE ~ "foreign"))


dataset <- dataset %>%
  mutate(per_carrier_lang = case_when(
    carrier_language == "english" ~ per_eng,
    carrier_language == "french" ~ per_fr,
    TRUE ~ NA_real_
  ))


## Saving full dataset
save(dataset, file = "./data/2021-11-04_data_ready_for_analysis.Rda")

