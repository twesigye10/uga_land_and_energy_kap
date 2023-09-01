library(tidyverse)
library(lubridate)

# main data ---------------------------------------------------------------

df_main_data <- readxl::read_excel("inputs/UGA2305_land_and_energy_data.xlsx") |> 
    rename_with(~str_replace(string = .x, pattern = "meta_", replacement = "")) |> 
    mutate(start = lubridate::as_datetime(start),
           end = lubridate::as_datetime(end),
           main_survey_time_interval = lubridate::time_length(end - start, unit = "min"), 
           main_survey_time_interval = ceiling(main_survey_time_interval))

# ********************* consider data that is not in deletion log // to be handled later
df_main_data_support_audit <- df_main_data |> 
    select(uuid = `_uuid`, enumerator_id, district_name, point_number, main_survey_time_interval)

# read audit files --------------------------------------------------------

my_audit_list <- list.files("inputs/audit_files", pattern = "\\.csv$", recursive = T, full.names = T)

df_audit_raw <- readr::read_csv(my_audit_list, id = "file_name")

df_audit_data <- df_audit_raw |> 
    filter(event %in% c("question")) |> 
    mutate(i.check.audit_uuid = str_replace_all(string = file_name, pattern = "^inputs\\/audit_files\\/|\\/audit\\.csv$", replacement = ""),
           # i.check.start = start,
           # i.check.end = end,
           i.check.audit_qn = str_extract(string = node, pattern = "[\\w]+$"),
           i.check.qn_time_interval = round(x = (end - start)/1000, digits = 0)) |> 
    supporteR::batch_select_rename()


# outliers in the audit data ----------------------------------------------

df_check_audit_outliers <- cleaningtools::check_outliers(dataset = df_audit_data |> mutate(audit_uuid = paste0(audit_uuid, " * ", audit_qn)), 
                                                         uuid_column = "audit_uuid", strongness_factor = 3) 

df_potential_audit_outliers <- df_check_audit_outliers$potential_outliers|> 
    separate_wider_delim(cols = uuid, delim = " * ", names = c("audit_uuid", "audit_qn")) |> 
    select(-question) |> 
    rename(qn_time_interval = old_value)
# cols to add enumerator, district, location/settlement

# outlier issues per survey
df_time_interval_outliers_per_survey <- df_potential_audit_outliers |> 
    group_by(audit_uuid) |> 
    summarise(time_interval_outliers_per_survey = n())

# outlier issues per enumerator
df_time_interval_outliers_per_enumerator <- df_potential_audit_outliers |>
    left_join(y = df_main_data_support_audit, by = c("audit_uuid" = "uuid")) |>
    group_by(enumerator_id) |>
    summarise(time_interval_outliers_per_enumerator = n())

# outlier issues per location// also group by host or settlement

# mean audit time ---------------------------------------------------------

# mean question times per enumerator//outliers
df_qn_time_enum_means <- df_audit_data |> 
    left_join(y = df_main_data_support_audit, by = c("audit_uuid" = "uuid")) |>
    group_by(enumerator_id, audit_qn) |>
    summarise(enum_qn_average_time = round(x = mean(qn_time_interval, na.rm = TRUE), digits = 0))

# main survey and audit time comparison -----------------------------------
df_main_and_audit_times <- df_audit_data |> 
    group_by(audit_uuid) |>
    summarise(audit_survey_time_interval = ceiling(sum(qn_time_interval, na.rm = TRUE) / 60)) |> 
    left_join(y = df_main_data_support_audit, by = c("audit_uuid" = "uuid")) |>
    mutate(main_and_audit_timme_diff = main_survey_time_interval - audit_survey_time_interval) |> 
    relocate(audit_survey_time_interval, .after = point_number)

# list of datasets
list_audit_summary_data <- list("interval outliers" = df_potential_audit_outliers,
                           "interval outliers per survey" = df_time_interval_outliers_per_survey,
                           "interval outliers per enum" = df_time_interval_outliers_per_enumerator,
                           "average time per qn and enum" = df_qn_time_enum_means,
                           "time diff main and audit" = df_main_and_audit_times)

openxlsx::write.xlsx(x = list_audit_summary_data,
                     file = paste0("outputs/", butteR::date_file_prefix(), 
                                   "_audit_summary_data.xlsx"), 
                     overwrite = TRUE, keepNA = TRUE, na.string = "")
