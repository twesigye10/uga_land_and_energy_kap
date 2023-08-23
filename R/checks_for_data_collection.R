# load libraries
library(supporteR)
library(tidyverse)
library(lubridate)
library(glue)
library(butteR)

# load data
df_tool_data <- readxl::read_excel("inputs/UGA2305_land__and_energy_tool_testing.xlsx") %>% 
    rename_with(~str_replace(string = .x, pattern = "meta_", replacement = "")) %>% 
    rename_with(~str_replace(string = .x, pattern = "KAP", replacement = "kap")) %>% 
    mutate(i.check.uuid = `_uuid`,
           i.check.start_date = as_date(start),
           i.check.enumerator_id = as.character(enumerator_id),
           i.check.district_name = district_name,
           i.check.point_number = point_number,
           start = as_datetime(start),
           end = as_datetime(end)) 

df_survey <- readxl::read_excel("inputs/land_and_energy_tool.xlsx", sheet = "survey") %>% 
    mutate(across(where(is.character), str_to_lower))
   
df_choices <- readxl::read_excel("inputs/land_and_energy_tool.xlsx", sheet = "choices")

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
     filter(consent == "no_consent") %>% 
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
            i.check.uuid_cl = "",
            i.check.so_sm_choices = "") %>% 
     batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")
 add_checks_data_to_list(input_list_name = "checks", input_df_name = "df_no_consent")
 
 
# other specify -----------------------------------------------------------
 df_others_data <- extract_other_specify_data(input_tool_data = df_tool_data, 
                                              input_enumerator_id_col = "enumerator_id",
                                              input_location_col = "district_name",
                                              input_survey = df_survey, 
                                              input_choices = df_choices)
 add_checks_data_to_list(input_list_name = "checks", input_df_name = "df_others_data")
 
 
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
           i.check.uuid_cl = "",
           i.check.so_sm_choices = "") %>% 
     batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")
 add_checks_data_to_list(input_list_name = "checks", input_df_name = "df_kap_stove_type_owned_1")
 
# The respondent knows how to make an improved stove but does not own one. i.e,
# kap_knowledge_to_build_improved_stoves = "yes", and kap_stove_type_owned = "everything except the improved stove"
df_kap_knowledge_to_build_improved_stoves_2 <- df_tool_data %>% 
    filter(kap_knowledge_to_build_improved_stoves %in% c("yes"), 
   !str_detect(string = kap_stove_type_owned, pattern = "a_fuel_efficient_cookstove"))%>% 
    mutate(i.check.type = "change_response",
           i.check.name = "kap_knowledge_to_build_improved_stoves", 
           i.check.current_value = kap_knowledge_to_build_improved_stoves,
           i.check.value = "", 
           i.check.issue_id = "logic_c_kap_knowledge_to_build_improved_stoves_2",
           i.check.issue = glue("kap_knowledge_to_build_improved_stoves: {kap_knowledge_to_build_improved_stoves}, 
                              kap_stove_type_owned: {kap_stove_type_owned}"),
           i.check.other_text = "",
           i.check.checked_by = "MT",
           i.check.checked_date = as_date(today()),
           i.check.comment = "",
           i.check.reviewed = "",
           i.check.adjust_log = "",
           i.check.uuid_cl = "",
           i.check.so_sm_choices = "") %>% 
    batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")
add_checks_data_to_list(input_list_name = "checks", input_df_name = "df_kap_knowledge_to_build_improved_stoves_2")

 
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
            i.check.uuid_cl = "",
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
            i.check.uuid_cl = "",
            i.check.so_sm_choices = "") %>% 
     batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")
 add_checks_data_to_list(input_list_name = "checks", input_df_name = "df_kap_regular_briquette_usage_4")
 
 
 # The respondent says briquettes are one of their main cooking fuels, but says they do not use briquettes regularly (at least once a week).i.e,
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
            i.check.uuid_cl = "",
            i.check.so_sm_choices = "") %>% 
     batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")
 add_checks_data_to_list(input_list_name = "checks", input_df_name = "df_kap_regular_briquette_usage_no_5")
 
 
 # The respondent says there is a shortage of wood, but does not think the environment around them is at risk.i.e,
 # kap_why_use_briquettes = "there_is_a_shortage_of_wood", and kap_environment_at_risk = "no"
 df_kap_environment_at_risk_6 <- df_tool_data %>% 
     filter(kap_environment_at_risk %in% c("no"), 
            str_detect(string = kap_why_use_briquttes, pattern = "there_is_a_shortage_of_wood"))%>% 
     mutate(i.check.type = "change_response",
            i.check.name = "kap_environment_at_risk", 
            i.check.current_value = kap_environment_at_risk,
            i.check.value = "", 
            i.check.issue_id = "logic_c_kap_environment_at_risk_6",
            i.check.issue = glue("kap_environment_at_risk: {kap_environment_at_risk}, 
                              kap_why_use_briquttes: {kap_why_use_briquttes}"),
            i.check.other_text = "",
            i.check.checked_by = "MT",
            i.check.checked_date = as_date(today()),
            i.check.comment = "",
            i.check.reviewed = "",
            i.check.adjust_log = "",
            i.check.uuid_cl = "",
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
            i.check.uuid_cl = "",
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
            i.check.uuid_cl = "",
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
     relocate(label, .after = name) 
     
 # write output
 write_csv(x = df_combined_checks_plus_label, file = paste0("outputs/", butteR::date_file_prefix(), "_combined_checks_land_energy.csv"), na = "")
 
 
# contact details for hhs agreed for IDI (independent file)
 contact_details <- df_tool_data %>% 
     filter(land_agree_to_idi_interview %in% c("yes")) %>% 
     mutate(i.check.start_date = as_date(start),
            i.check.enumerator_id = enumerator_id,
            i.check.enumerator_id = enumerator_id,
            i.check.district_name = district_name,
            i.check.refugee_settlement = refugee_settlement,
            i.check.sub_county_div = sub_county_div,
            i.check.point_number = point_number) %>% 
            # i.check.respondent_phone_name = respondent_phone_name,
            # i.check.respondent_phone_number = respondent_phone_number) %>% 
     batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")

