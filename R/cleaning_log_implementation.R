library(tidyverse)
library(lubridate)
library(glue)


# read data ---------------------------------------------------------------

# main dataset
cols_to_escape <- c("index", "start", "end", "today", "starttime",	"endtime", "_submission_time", "_submission__submission_time")
vars_to_remove_from_main_data <- c("deviceid", "audit", "audit_URL", "instance_name", "individual_name",
                                   "_validation_status", "_notes", "_tags", "check_ptno_insamples", "validate_ptno",
                                   "pt_num_msg", "pt_num_validation_message", "geopoint", "_geopoint_latitude",
                                   "_geopoint_longitude", "_geopoint_altitude", "_geopoint_precision")

data_nms <- names(readxl::read_excel(path = "inputs/UGA2305_land_and_energy_data.xlsx", n_max = 500))
c_types <- ifelse(str_detect(string = data_nms, pattern = "_other$"), "text", "guess")

df_raw_data <- readxl::read_excel(path = "inputs/UGA2305_land_and_energy_data.xlsx", col_types = c_types) %>% 
  mutate(across(.cols = -c(contains(cols_to_escape)))) %>% 
  mutate(start = as_datetime(start), end = as_datetime(end), today = as_date(as_datetime(today)))


# cleaning log
df_cleaning_log <- read_csv("inputs/combined_checks.csv") %>%
  filter(!adjust_log %in% c("delete_log")) %>% 
  mutate(adjust_log = ifelse(is.na(adjust_log), "apply_suggested_change", adjust_log),
         value = ifelse(is.na(value) & str_detect(string = issue_id, pattern = "logic_c_"), "blank", value),
         value = ifelse(type %in% c("remove_survey"), "blank", value),
         name = ifelse(name %in% c("hh_id"), "individual_name", name),
         name = ifelse(is.na(name) & type %in% c("remove_survey"), "individual_name", name)
  ) %>%
  filter(!is.na(value), !is.na(uuid)) %>%
  mutate(value = ifelse(value %in% c("blank"), NA, value),
         sheet = NA,
         index = NA,
         relevant = NA) %>%
  select(uuid, type, name, value, issue_id, sheet, index, relevant, issue)

# survey tool
df_survey <- readxl::read_excel(path = "inputs/UGA2305_land_and_energy_data.xlsx", sheet = "survey")
df_choices <- readxl::read_excel(path = "inputs/UGA2305_land_and_energy_data.xlsx", sheet = "choices")

# main dataset

df_cleaning_log_dataset <- df_cleaning_log %>% 
  filter(name %in% colnames(df_raw_data)) # NOTE: if there is "remove_survey" without "name" filled, it will be filtered out


df_cleaned_data <- supporteR::cleaning_support(input_df_raw_data = df_raw_data, 
                                              input_df_survey = df_survey, 
                                              input_df_choices = df_choices, 
                                              input_df_cleaning_log = df_cleaning_log_dataset,
                                              input_vars_to_remove_from_data = vars_to_remove_from_main_data)

df_cleaned_data_final <- df_cleaned_data %>% 
    janitor::remove_empty(which = "cols") 
  # select(-c(vars_to_remove_from_main_data)) %>% 
    
  # mutate(across(.cols = everything(), .fns = ~ ifelse(str_detect(string = ., 
  #                                                       pattern = fixed("FALSE", ignore_case = TRUE)), "1", .))) %>% 
  # mutate(across(.cols = everything(), .fns = ~ ifelse(str_detect(string = ., 
  #                                                       pattern = fixed("TRUE", ignore_case = TRUE)), "0", .)))
  
openxlsx::write.xlsx(x = df_cleaned_data_final,
                     file = paste0("outputs/", butteR::date_file_prefix(), 
                                   "_clean_data_land_energy.xlsx"), 
                     overwrite = TRUE, keepNA = TRUE, na.string = "")
