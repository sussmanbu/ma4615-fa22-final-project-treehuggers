
library(tidyverse)

CAcountytract <- read_csv("CA_countytract.csv") %>% 
  rename("remaining" = "TYPE; FULLCODE; STATE; COUNTY; TRACT; SHEETS") %>%
  separate(col="remaining", into = c("TYPE", "remains1"), (5)) %>%
  separate(col="remains1", into = c("semi1", "remains11"), sep=(1)) %>%
  separate(col="remains11", into = c("FULLCODE", "remains2"), sep = (11)) %>%
  separate(col="remains2", into = c("semi2", "remains22"), sep=(1)) %>%
  separate(col="remains22", into = c("STATE", "remains3"), sep = (2)) %>%
  separate(col="remains3", into = c("semi3", "remains33"), sep=(1)) %>%
  separate(col="remains33", into = c("COUNTY", "remains4"), sep = (3)) %>%
  separate(col="remains4", into = c("semi4", "remains44"), sep=(1)) %>%
  separate(col="remains44", into = c("TRACT", "SHEETS"), sep = ";") %>%
  unite(col = "COUNTY_CODE", c("STATE", "COUNTY"), sep="", remove = FALSE) %>%
  select(-semi1, -semi2, -semi3, -semi4, -"...3", -"...4", -"...5", -"...6")

saveRDS(CAcountytract.Rdata, ".../dataset/CAcountytract.Rdata")