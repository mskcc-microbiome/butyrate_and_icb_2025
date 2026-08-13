dotenv::load_dot_env()
library(tidyverse)
mixed_solid_tumor_cohort <- readRDS("data/mixed_solid_tumor.rds")

patient_list <- read.csv(Sys.getenv("PATIENT_LIST")) %>% 
  janitor::clean_names() %>%
  filter(blinded_subject_id %in% mixed_solid_tumor_cohort$blinded_subject_id) %>%
  mutate(c1d1 = lubridate::ymd(as.Date(c1d1, format = "%d/%m/%Y")))

print(paste("Range of Treatment Start Dates: ", base::format(min(pateint_list$c1d1), "%m/%y"), base::format(max(pateint_list$c1d1), "%m/%y")))
