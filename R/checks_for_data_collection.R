# load libraries
library(supporteR)
library(tidyverse)
library(lubridate)
library(glue)
library(butteR)
library(sf)
library(cluster)
library(kobold)

source("R/support_functions.R")

# load data
df_tool_data <- readxl::read_excel("inputs/UGA2305_land_and_energy_data.xlsx") %>% 
    rename_with(~str_replace(string = .x, pattern = "meta_", replacement = "")) %>% 
    mutate(i.check.uuid = `_uuid`,
           i.check.start_date = as_date(start),
           i.check.enumerator_id = as.character(enumerator_id),
           i.check.district_name = district_name,
           i.check.point_number = point_number,
           start = as_datetime(start),
           end = as_datetime(end)) %>% 
    mutate(i.check.point_number = case_when(i.check.point_number %in% c("bud_03") ~ "test_bud_03",
                                            i.check.point_number %in% c("bud_17") ~ "test_bud_17",
                                                                    TRUE ~ i.check.point_number))

df_survey <- readxl::read_excel("inputs/land_and_energy_tool.xlsx", sheet = "survey") 

df_choices <- readxl::read_excel("inputs/land_and_energy_tool.xlsx", sheet = "choices")

df_sample_data <- sf::st_read("inputs/land_energy_settlement_host_samples.gpkg", quiet = TRUE)

# output holder -----------------------------------------------------------
checks <- list()

# time checks -------------------------------------------------------------
# check survey time
survey_time_duration <-  check_survey_time(
    input_tool_data = df_tool_data,
    input_enumerator_id_col = "enumerator_id",
    input_location_col = "district_name",
    input_min_time = 20,
    input_max_time = 120)

add_checks_data_to_list(input_list_name = "checks", input_df_name = "survey_time_duration")

# check time between surveys
time_between_surveys <-  check_time_interval_btn_surveys(
    input_tool_data = df_tool_data,
    input_enumerator_id_col = "enumerator_id",
    input_location_col= "district_name",
    input_min_time = 5)

add_checks_data_to_list(input_list_name = "checks", input_df_name = "time_between_surveys")

# no consent --------------------------------------------------------------
# check no consent but data submitted
df_no_consent <- df_tool_data %>% 
    filter(consent == "no") %>% 
    mutate(i.check.type = "remove_survey",
           i.check.name = "consent",
           i.check.current_value = as.character(consent),
           i.check.value = "",
           i.check.issue_id = "logic_m_requirement_no_consent",
           i.check.issue = "no_consent",
           i.check.other_text = "",
           i.check.checked_by = "MT",
           i.check.checked_date = as_date(today()),
           i.check.comment = "", 
           i.check.reviewed = "1",
           i.check.adjust_log = "",
           i.check.so_sm_choices = "") %>% 
    batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")
add_checks_data_to_list(input_list_name = "checks", input_df_name = "df_no_consent")

# testing_data
df_testing_data <- df_tool_data %>%
    filter(str_detect(string = point_number, pattern = fixed('test', ignore_case = TRUE))) %>%
    mutate(i.check.type = "remove_survey",
           i.check.name = "",
           i.check.current_value = "",
           i.check.value = "",
           i.check.issue_id = "logic_m_testing_data",
           i.check.issue = "testing_data",
           i.check.other_text = "",
           i.check.checked_by = "",
           i.check.checked_date = as_date(today()),
           i.check.comment = "",
           i.check.reviewed = "1",
           i.check.adjust_log = "",
           i.check.uuid_cl = "",
           i.check.so_sm_choices = "") %>%
    batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")
add_checks_data_to_list(input_list_name = "checks", input_df_name = "df_testing_data")


# training_data
df_training_data <- df_tool_data %>%
    filter(i.check.uuid %in% c("44ef2446-0afb-4c05-a7cc-ba6560d02afa", "0df3336d-0bf0-4e40-b703-952d3285cec1",
                               "95f06c28-3b2c-4f6a-a706-ce479f8ac900", "e542c894-c93c-4430-b547-4bf3173f17d9",
                               "45534f22-04fc-4c8e-bf9c-4a9a9fb18e4a")) %>% 
    mutate(i.check.type = "remove_survey",
           i.check.name = "",
           i.check.current_value = "",
           i.check.value = "",
           i.check.issue_id = "logic_m_training_data",
           i.check.issue = "training_data",
           i.check.other_text = "",
           i.check.checked_by = "",
           i.check.checked_date = as_date(today()),
           i.check.comment = "",
           i.check.reviewed = "1",
           i.check.adjust_log = "",
           i.check.uuid_cl = "",
           i.check.so_sm_choices = "") %>%
    batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")
add_checks_data_to_list(input_list_name = "checks", input_df_name = "df_training_data")


# checking  outliers ------------------------------------------------------

df_c_outliers <- supporteR::check_outliers_cleaninginspector(input_tool_data = df_tool_data %>% 
                                        select(-c("respondent_age", "hoh_age")) %>% 
 filter(!kap_cost_of_cheapest_improved_stove %in% c(666, 999, 9999, 99999)),
                                                             input_enumerator_id_col = "enumerator_id",
                                                             input_location_col = "district_name")

add_checks_data_to_list(input_list_name = "checks", input_df_name = "df_c_outliers")

# filtering '999' etc from stove price 
df_stove_data_price <- df_tool_data %>%
    filter(kap_cost_of_cheapest_improved_stove %in% c(666, 999, 9999, 99999, 0)) %>% 
    mutate(i.check.type = "change_response",
           i.check.name = "kap_cost_of_cheapest_improved_stove",
           i.check.current_value = as.character(kap_cost_of_cheapest_improved_stove),
           i.check.value = "NA",
           i.check.issue_id = "logic_c_outlier",
           i.check.issue = "stove_price_outliers",
           i.check.other_text = "",
           i.check.checked_by = "",
           i.check.checked_date = as_date(today()),
           i.check.comment = "",
           i.check.reviewed = "1",
           i.check.adjust_log = "",
           i.check.uuid_cl = "",
           i.check.so_sm_choices = "") %>%
    batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")
add_checks_data_to_list(input_list_name = "checks", input_df_name = "df_stove_data_price")


# check duplicate uuids ---------------------------------------------------

df_duplicate_uuids <- checks_duplicate_uuids(input_tool_data = df_tool_data)

add_checks_data_to_list(input_list_name = "checks", input_df_name = "df_duplicate_uuids")

# other specify -----------------------------------------------------------
df_others_data <- extract_other_specify_data(input_tool_data = df_tool_data, 
                                             input_enumerator_id_col = "enumerator_id",
                                             input_location_col = "district_name",
                                             input_survey = df_survey, 
                                             input_choices = df_choices)

df_tool_other <- df_tool_data %>% 
    select(i.check.uuid, point_number) %>% 
    rename(uuid = i.check.uuid)

df_other_wih_point <- df_others_data %>% 
    left_join(df_tool_other, by = "uuid") %>% 
    relocate(point_number, .after = district_name)
    
add_checks_data_to_list(input_list_name = "checks", input_df_name = "df_other_wih_point")


# spatial checks ----------------------------------------------------------

# if("status" %in% colnames(df_sample_data)){
#     sample_pt_nos <- df_sample_data %>% 
#         mutate(unique_pt_number = paste0(status, "_", Name)) %>% 
#         pull(unique_pt_number) %>% 
#         unique()
# }else{
#     sample_pt_nos <- df_sample_data %>% 
#         mutate(unique_pt_number = Name) %>% 
#         pull(unique_pt_number) %>% 
#         unique()
# }
# 
# # duplicate point numbers
# df_duplicate_pt_nos <- check_duplicate_pt_numbers(input_tool_data = df_tool_data,
#                                                   input_enumerator_id_col = "enumerator_id",
#                                                   input_location_col = "district_name",
#                                                   input_point_id_col = "point_number",
#                                                   input_sample_pt_nos_list = sample_pt_nos)
# 
# add_checks_data_to_list(input_list_name = "checks", input_df_name = "df_duplicate_pt_nos")
# 

# point number does not exist in sample

# df_pt_number_not_in_sample <- check_pt_number_not_in_samples(input_tool_data = df_tool_data,
#                                                              input_enumerator_id_col = "enumerator_id",
#                                                              input_location_col = "district_name",
#                                                              input_point_id_col = "point_number",
#                                                              input_sample_pt_nos_list = sample_pt_nos)
# 
# add_checks_data_to_list(input_list_name = "checks", input_df_name = "df_pt_number_not_in_sample")


# check for exceeded threshold distance

# df_greater_thresh_distance <- check_threshold_distance(input_sample_data = df_sample_data,
#                                                        input_tool_data = df_tool_data,
#                                                        input_enumerator_id_col = "enumerator_id",
#                                                        input_location_col = "district_name",
#                                                        input_point_id_col = "point_number",
#                                                        input_threshold_dist = 150)
# 
# add_checks_data_to_list(input_list_name = "checks", input_df_name = "df_greater_thresh_distance")


# logical checks ----------------------------------------------------------

# The respondent says they use the three stone fire, but neither charcoal nor wood are their main fuels. i.e,
# kap_stove_type_owned = "three_stone_fire", and kap_fuels_mostly_used = "everything except wood and charcoal"
df_kap_stove_type_owned_1 <- df_tool_data %>% 
    filter(str_detect(string = kap_stove_type_owned, pattern = "three_stone_fire"),
           kap_fuels_mostly_used %in% c("briquettes", "bio_waste", "gas", "kerosene", "paraffin",
                                        "electricity", "other")) %>% 
    mutate(i.check.type = "change_response",
           i.check.name = "kap_stove_type_owned", 
           i.check.current_value = kap_stove_type_owned,
           i.check.value = "", 
           i.check.issue_id = "logic_c_kap_stove_type_owned_1",
           i.check.issue = glue("kap_stove_type_owned: {kap_stove_type_owned}, 
                              kap_fuels_mostly_used: {kap_fuels_mostly_used}"),
           i.check.other_text = "",
           i.check.checked_by = "MT",
           i.check.checked_date = as_date(today()),
           i.check.comment = "",
           i.check.reviewed = "",
           i.check.adjust_log = "",
           i.check.so_sm_choices = "") %>% 
    batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")
add_checks_data_to_list(input_list_name = "checks", input_df_name = "df_kap_stove_type_owned_1")

# The respondent knows how to make an improved stove but does not own one. i.e,
# kap_knowledge_to_build_improved_stoves = "yes", and kap_stove_type_owned = "everything except the improved stove"
# df_kap_knowledge_to_build_improved_stoves_2 <- df_tool_data %>% 
#     filter(kap_knowledge_to_build_improved_stoves %in% c("yes"), 
#            !str_detect(string = kap_stove_type_owned, pattern = "gasifier_stove|kerosene_stove|impr_mud_stove|impr_charcoal_stove|
#                impr_ceramic_stove|solar_cookstove"))%>% 
#     mutate(i.check.type = "change_response",
#            i.check.name = "kap_knowledge_to_build_improved_stoves", 
#            i.check.current_value = kap_knowledge_to_build_improved_stoves,
#            i.check.value = "", 
#            i.check.issue_id = "logic_c_kap_knowledge_to_build_improved_stoves_2",
#            i.check.issue = glue("kap_knowledge_to_build_improved_stoves: {kap_knowledge_to_build_improved_stoves}, 
#                               kap_stove_type_owned: {kap_stove_type_owned}"),
#            i.check.other_text = "",
#            i.check.checked_by = "MT",
#            i.check.checked_date = as_date(today()),
#            i.check.comment = "",
#            i.check.reviewed = "",
#            i.check.adjust_log = "",
#            i.check.so_sm_choices = "") %>% 
#     batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")
# add_checks_data_to_list(input_list_name = "checks", input_df_name = "df_kap_knowledge_to_build_improved_stoves_2")


# The respondent says they are not familiar with the concept of briquettes, but reported using briquettes as one of their main fuels for cooking. i.e,
# kap_briquettes_familiarity = "no", and kap_fuels_mostly_used = "briquettes"
df_kap_briquettes_familiarity_3 <- df_tool_data %>% 
    filter(kap_briquettes_familiarity %in% c("no"), 
           str_detect(string = kap_fuels_mostly_used, pattern = "briquettes"))%>% 
    mutate(i.check.type = "change_response",
           i.check.name = "kap_briquettes_familiarity", 
           i.check.current_value = kap_briquettes_familiarity,
           i.check.value = "", 
           i.check.issue_id = "logic_c_kap_briquettes_familiarity_3",
           i.check.issue = glue("kap_briquettes_familiarity: {kap_briquettes_familiarity}, 
                              kap_fuels_mostly_used: {kap_fuels_mostly_used}"),
           i.check.other_text = "",
           i.check.checked_by = "MT",
           i.check.checked_date = as_date(today()),
           i.check.comment = "",
           i.check.reviewed = "",
           i.check.adjust_log = "",
           i.check.so_sm_choices = "") %>% 
    batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")
add_checks_data_to_list(input_list_name = "checks", input_df_name = "df_kap_briquettes_familiarity_3")


# The respondent says they use briquettes regularly, but did not report briquettes as one of their main cooking fuels.i.e,
# kap_regular_briquette_usage = "yes", and kap_fuels_mostly_used != "briquettes"
df_kap_regular_briquette_usage_4 <- df_tool_data %>% 
    filter(kap_regular_briquette_usage %in% c("yes"), 
           !str_detect(string = kap_fuels_mostly_used, pattern = "briquettes"))%>% 
    mutate(i.check.type = "change_response",
           i.check.name = "kap_regular_briquette_usage", 
           i.check.current_value = kap_regular_briquette_usage,
           i.check.value = "", 
           i.check.issue_id = "logic_c_kap_regular_briquette_usage_4",
           i.check.issue = glue("kap_regular_briquette_usage: {kap_regular_briquette_usage},
                              kap_fuels_mostly_used: {kap_fuels_mostly_used}"),
           i.check.other_text = "",
           i.check.checked_by = "MT",
           i.check.checked_date = as_date(today()),
           i.check.comment = "",
           i.check.reviewed = "",
           i.check.adjust_log = "",
           i.check.so_sm_choices = "") %>% 
    batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")
add_checks_data_to_list(input_list_name = "checks", input_df_name = "df_kap_regular_briquette_usage_4")


# The respondent says briquettes are not one of their main cooking fuels, but says they do use briquettes regularly (at least once a week).i.e,
# kap_regular_briquette_usage = "no", and kap_fuels_mostly_used = "briquettes"
df_kap_regular_briquette_usage_no_5 <- df_tool_data %>% 
    filter(kap_regular_briquette_usage %in% c("no"), 
           str_detect(string = kap_fuels_mostly_used, pattern = "briquettes"))%>% 
    mutate(i.check.type = "change_response",
           i.check.name = "kap_regular_briquette_usage", 
           i.check.current_value = kap_regular_briquette_usage,
           i.check.value = "", 
           i.check.issue_id = "logic_c_kap_regular_briquette_usage_no_5",
           i.check.issue = glue("kap_regular_briquette_usage: {kap_regular_briquette_usage},
                              kap_fuels_mostly_used: {kap_fuels_mostly_used}"),
           i.check.other_text = "",
           i.check.checked_by = "MT",
           i.check.checked_date = as_date(today()),
           i.check.comment = "",
           i.check.reviewed = "",
           i.check.adjust_log = "",
           i.check.so_sm_choices = "") %>% 
    batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")
add_checks_data_to_list(input_list_name = "checks", input_df_name = "df_kap_regular_briquette_usage_no_5")


# The respondent says there is a shortage of wood, but does not think the environment around them is at risk.i.e,
# kap_why_use_briquettes = "there_is_a_shortage_of_wood", and kap_environment_at_risk = "no"
df_kap_environment_at_risk_6 <- df_tool_data %>% 
    filter(kap_environment_at_risk %in% c("no"), 
           str_detect(string = kap_why_use_briquettes, pattern = "there_is_a_shortage_of_wood"))%>% 
    mutate(i.check.type = "change_response",
           i.check.name = "kap_environment_at_risk", 
           i.check.current_value = kap_environment_at_risk,
           i.check.value = "", 
           i.check.issue_id = "logic_c_kap_environment_at_risk_6",
           i.check.issue = glue("kap_environment_at_risk: {kap_environment_at_risk}, 
                              kap_why_use_briquettes: {kap_why_use_briquettes}"),
           i.check.other_text = "",
           i.check.checked_by = "MT",
           i.check.checked_date = as_date(today()),
           i.check.comment = "",
           i.check.reviewed = "",
           i.check.adjust_log = "",
           i.check.so_sm_choices = "") %>% 
    batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")
add_checks_data_to_list(input_list_name = "checks", input_df_name = "df_kap_environment_at_risk_6")


# The respondent says deforestation is an issue, but does not think the environment is at risk. i.e,
# kap_deforestation_an_issue = "yes", and kap_environment_at_risk = "no"
df_kap_deforestation_an_issue_7 <- df_tool_data %>% 
    filter(kap_deforestation_an_issue %in% c("yes"), 
           kap_environment_at_risk %in% c("no"))%>% 
    mutate(i.check.type = "change_response",
           i.check.name = "kap_deforestation_an_issue", 
           i.check.current_value = kap_deforestation_an_issue,
           i.check.value = "", 
           i.check.issue_id = "logic_c_kap_deforestation_an_issue_7",
           i.check.issue = glue("kap_deforestation_an_issue: {kap_deforestation_an_issue}, 
                              kap_environment_at_risk: {kap_environment_at_risk}"),
           i.check.other_text = "",
           i.check.checked_by = "MT",
           i.check.checked_date = as_date(today()),
           i.check.comment = "",
           i.check.reviewed = "",
           i.check.adjust_log = "",
           i.check.so_sm_choices = "") %>% 
    batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")
add_checks_data_to_list(input_list_name = "checks", input_df_name = "df_kap_deforestation_an_issue_7")


# The respondent says climate change has had in impact on them in the last 10 years, yet doesn't think it will have an impact in the coming year. i.e,
# kap_rank_climate_change_impact = "all yes options", and kap_rank_climate_change_negative_impact_next_year = "no"
df_kap_rank_climate_change_negative_impact_next_year_8 <- df_tool_data %>% 
    filter(kap_rank_climate_change_negative_impact_next_year %in% c("no"), 
           # kap_rank_climate_change_impact %in% c("yes_a_lot", "yes_somewhat", "yes_a_little")) %>% 
           str_detect(kap_rank_climate_change_impact, "^yes"))%>% 
    mutate(i.check.type = "change_response",
           i.check.name = "kap_rank_climate_change_negative_impact_next_year", 
           i.check.current_value = kap_rank_climate_change_negative_impact_next_year,
           i.check.value = "", 
           i.check.issue_id = "logic_c_kap_rank_climate_change_negative_impact_next_year_8",
           i.check.issue = glue("kap_rank_climate_change_negative_impact_next_year: {kap_rank_climate_change_negative_impact_next_year}, 
                              kap_rank_climate_change_impact: {kap_rank_climate_change_impact}"),
           i.check.other_text = "",
           i.check.checked_by = "MT",
           i.check.checked_date = as_date(today()),
           i.check.comment = "",
           i.check.reviewed = "",
           i.check.adjust_log = "",
           i.check.so_sm_choices = "") %>% 
    batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")
add_checks_data_to_list(input_list_name = "checks", input_df_name = "df_kap_rank_climate_change_negative_impact_next_year_8")


# combine checks ----------------------------------------------------------
df_combined_checks <- bind_rows(checks)


# adding the label column to the log
df_combined_checks_plus_label <- df_combined_checks %>%
    mutate(int.name = ifelse(str_detect(string = name, pattern = "_rank_.*"), 
                             str_replace(string = name, pattern = "_rank_.*", replacement = ""), name)) %>%
    left_join(df_survey %>% select(name, label), by = c("int.name" = "name")) %>% 
    select(-int.name) %>%
    relocate(label, .after = name) %>% 
    mutate(FO_comment = "") %>% 
           # location = ifelse(status = "refugee", refugee_settlement, sub_county_div)) %>% 
    relocate(FO_comment, .after = comment) 

# Add location
df_data_location  <- df_tool_data %>%  
    select(i.check.uuid, status, sub_county_div, refugee_settlement) %>% 
    mutate(location = ifelse(!is.na(refugee_settlement), refugee_settlement, sub_county_div)) %>% 
    rename(uuid = i.check.uuid) %>% 
    select(uuid, location)

df_location <- df_combined_checks_plus_label %>% 
    left_join(df_data_location, by = "uuid") %>% 
    relocate(location, .after = district_name)
    

# contact details for hhs agreed for IDI (independent file)
contact_details <- df_tool_data %>% 
    filter(land_agree_to_idi_interview %in% c("yes")) %>% 
    mutate(i.check.start_date = as_date(start),
           i.check.enumerator_id = enumerator_id,
           i.check.district_name = district_name,
           i.check.refugee_settlement = refugee_settlement,
           i.check.sub_county_div = sub_county_div,
           i.check.point_number = point_number,
           i.check.hh_has_access_to_additional_land = land_hh_have_access_to_additional_land,
           i.check.hh_ever_attempt_access_additional_land = land_hh_ever_attempt_access_additional_land,
           i.check.respondent_name = land_respondent_name,
           i.check.respondent_phone_number = land_respondent_phone_number) %>%
    batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "") %>% 
    rename_with(~str_replace(string = .x, pattern = "land", replacement = "")) %>% 
    select(-c(uuid))


# write output

list_of_output_files <- list("UGA2305_land _and_energy" = df_location,
                             "contact_details" = contact_details)


write_csv(x = df_location, file = paste0("outputs/", butteR::date_file_prefix(), 
                                                           "_combined_checks_land_energy.csv"), na = "")

openxlsx::write.xlsx(x = list_of_output_files,
                     file = paste0("outputs/", butteR::date_file_prefix(), 
                                   "_combined_checks_land_energy.xlsx"), 
                     overwrite = TRUE, keepNA = TRUE, na.string = "")



# similarity and silhouette analysis --------------------------------------

# silhouette analysis

# NOTE: the column for "col_admin" is kept in the data

omit_cols_sil <- c("start", "end", "today", "duration", "duration_minutes",
                   "instruction_note", "consent_note",  "consent","note", "land_livelihoods",
                   "deviceid", "audit", "audit_URL", "instance_name", "end_survey", "kap_climate_change_and_adaptation_note",
                   "demo_check", "kap_cooking_or_stove_note","kap_briquettes_usage_note","kap_dry_cell_battery_usage_note", "kap_solar_usage_note","kap_environemnt_degradation_note",
                   "mdd_note", "kap_climate_change_observations_note", "kap_understanding_climate_change_note", "interview_feedback_note",
                   "end_note", "geopoint", "_geopoint_latitude", "_geopoint_altitude", "_geopoint_precision", "_id" ,"_submission_time","_validation_status","_notes","_status","_submitted_by","_tags","_index","Too short", "pmi_issues",
                   "i.check.enumerator_id")

data_similartiy_sil <- df_tool_data %>% 
    mutate(location = ifelse(status == "refugee", refugee_settlement, sub_county_div)) %>% 
    select(- any_of(omit_cols_sil))

df_sil_data <- calculateEnumeratorSimilarity(data = data_similartiy_sil,
                                             input_df_survey = df_survey, 
                                             col_enum = "enumerator_id",
                                             col_admin = "location") %>% 
    mutate(si2= abs(si))

df_sil_data[order(df_sil_data$`si2`, decreasing = TRUE),!colnames(df_sil_data)%in%"si2"] %>%  
    openxlsx::write.xlsx(paste0("outputs/", butteR::date_file_prefix(), "_silhouette_analysis_land_energy.xlsx"))


# similarity analysis

data_similartiy <- df_tool_data %>% 
    select(- any_of(c(omit_cols_sil, "location")))

df_sim_data <- calculateDifferences(data = data_similartiy, 
                                    input_df_survey = df_survey) %>% 
    openxlsx::write.xlsx(paste0("outputs/", butteR::date_file_prefix(), 
                                "_most_similar_analysis_land_energy.xlsx"))



