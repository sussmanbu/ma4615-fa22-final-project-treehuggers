library(tidyverse)

### EXAMPLE ###
loan_data <- read_csv(here::here("dataset", "loan_refusal.csv"))
## CLEAN the data
loan_data_clean <- loan_data
write_csv(loan_data_clean, file = here::here("dataset", "loan_refusal_clean.csv"))
save(loan_data_clean, file = here::here("dataset/loan_refusal.RData"))
### EXAMPLE ENDS ###


### CLEAN PARKS DATA ###
CAcountytract <- read_csv(here::here("dataset", "CA_countytract.csv"), show_col_types=FALSE)
CAcountytract_clean <- CAcountytract %>% rename("remaining" = "TYPE; FULLCODE; STATE; COUNTY; TRACT; SHEETS") %>% 
  separate(col="remaining", into = c("TYPE", "remains1"), (5)) %>% 
  separate(col="remains1", into = c("semi1", "remains11"), sep=(1)) %>% 
  separate(col="remains11", into = c("FULLCODE", "remains2"), sep = (11)) %>% 
  separate(col="remains2", into = c("semi2", "remains22"), sep=(1)) %>% 
  separate(col="remains22", into = c("STATE", "remains3"), sep = (2)) %>% 
  separate(col="remains3", into = c("semi3", "remains33"), sep=(1)) %>% 
  separate(col="remains33", into = c("COUNTY", "remains4"), sep = (3)) %>% 
  separate(col="remains4", into = c("semi4", "remains44"), sep=(1)) %>% 
  separate(col="remains44", into = c("TRACT", "SHEETS"), sep = ";") %>% 
  unite(col = "COUNTY_CODE", c("STATE", "COUNTY"), sep="") %>% 
  select(-semi1, -semi2, -semi3, -semi4, -"...3", -"...4", -"...5", -"...6")

### CLEAN ASTHMA DATA ###
asthma <- read_csv(here::here("dataset", "asthma.csv"), show_col_types=FALSE)

# The cleaned "kids" data features asthma data on people who are from ages 0-17.
asthmaCA_kids <- asthma %>% filter(AGE_GROUP == "0-17 years") %>%
  filter(!is.na(AGE_ADJUSTED_HOSPITALIZATION_RATE)) %>%
  filter(!is.na(NUMBER_OF_HOSPITALIZATIONS)) %>%
  select(-COMMENT)

# The below section looks at the racial and ethnic backgrounds of asthma sufferers in California. 
# Since air pollution is often a bigger issue for those who experience 
# environmental injustice, this could help shed light on where that is happening. 
# If some of the counties like up with those with high rates of childhood asthma, 
# it becomes even more convincing.
asthmaCA_race_ethnicity <- asthma %>% filter(STRATA == "Race/ethnicity") %>%
  filter(!is.na(NUMBER_OF_HOSPITALIZATIONS)) %>%
  filter(!is.na(AGE_ADJUSTED_HOSPITALIZATION_RATE)) %>%
  select(-COMMENT)

# The following "kids 2" shows the asthma rate for two different age classes of children. 
asthmaCA_kids_2 <- asthma %>% filter(STRATA == "Age groups") %>%
  filter(AGE_GROUP == c("0-4 years", "5-17")) %>%
  filter(!is.na(NUMBER_OF_HOSPITALIZATIONS)) %>%
  filter(!is.na(AGE_ADJUSTED_HOSPITALIZATION_RATE)) %>%
  select(-COMMENT)

# This compares asthma data between children and adults. 
asthmaCA_kids_v_adults <- asthma %>% filter(STRATA == "Child vs. adult") %>%
  filter(!is.na(NUMBER_OF_HOSPITALIZATIONS)) %>%
  filter(!is.na(AGE_ADJUSTED_HOSPITALIZATION_RATE)) %>%
  select(-COMMENT)

### JOINED DATA ###
# Joining Cleaned Parks data with cleaned asthma data for kids
load(here::here("dataset", "clean_parks_data.RData"))
colnames(asthmaCA_kids) <- c("county_name", "year", "strata", "strata_name", "age_group", "number_hospitalizations", "age_adjusted_hospitalization_rate")
asthmaCA_kids <- asthmaCA_kids %>% mutate(county_name = str_to_upper(county_name))
joined1 <- clean_parks_data %>% inner_join(asthmaCA_kids, by = "county_name")

# Joining parks with asthma for kids2
colnames(asthmaCA_kids_2) <- c("county_name", "year", "strata", "strata_name", "age_group", "number_hospitalizations", "age_adjusted_hospitalization_rate")
asthmaCA_kids_2 <- asthmaCA_kids_2 %>% mutate(county_name = str_to_upper(county_name))
joined2 <- clean_parks_data %>% inner_join(asthmaCA_kids_2, by = "county_name")

# Joining parks with asthma kids vs. adults
colnames(asthmaCA_kids_v_adults) <- c("county_name", "year", "strata", "strata_name", "age_group", "number_hospitalizations", "age_adjusted_hospitalization_rate")
asthmaCA_kids_v_adults <- asthmaCA_kids_v_adults %>% mutate(county_name = str_to_upper(county_name))
joined3 <- clean_parks_data %>% inner_join(asthmaCA_kids_v_adults, by = "county_name")

# Joining parks with asthma race/ethnicity
colnames(asthmaCA_race_ethnicity) <- c("county_name", "year", "strata", "race_ethnicity", "age_group", "number_hospitalizations", "age_adjusted_hospitalization_rate")
asthmaCA_race_ethnicity <- asthmaCA_race_ethnicity %>% mutate(county_name = str_to_upper(county_name))
joined4 <- clean_parks_data %>% inner_join(asthmaCA_race_ethnicity, by = "county_name")
