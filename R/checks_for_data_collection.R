# load libraries
library(supporteR)
library(tidyverse)
library(lubridate)
library(glue)
library(butteR)

# load data
df_tool_data <- readxl::read_excel("inputs/UGA2305_land__and_energy_tool_testing.xlsx") %>% 
    rename_with(~str_replace(string = .x, pattern = "meta_", replacement = "")) %>% 
    mutate(i.check.uuid = `_uuid`,
           i.check.start_date = as_date(start),
           i.check.enumerator_id = enumerator_id,
           i.check.district_name = district_name,
           i.check.point_number = point_number,
           start = as_datetime(start),
           end = as_datetime(end))

df_survey <- readxl::read_excel("inputs/land_and_energy_tool.xlsx", sheet = "survey")
df_choices <- readxl::read_excel("inputs/land_and_energy_tool.xlsx", sheet = "choices")

# Logical checks

# The respondent says they use the three stone fire, but neither charcoal nor wood are their main fuels. i.e,
# KAP_stove_type_owned = "three_stone_fire", and KAP_fuels_mostly_used = "everything except wood and charcoal"
df_KAP_stove_type_owned_1 <- df_tool_data %>% 
    filter(str_detect(string = KAP_stove_type_owned, pattern = "three_stone_fire"),
           KAP_fuels_mostly_used %in% c("briquettes", "bio_waste", "gas", "kerosene", "paraffin",
                                        "electricity", "other")) %>% 
    mutate(i.check.type = "change_response",
           i.check.name = "KAP_stove_type_owned", 
           i.check.current_value = KAP_stove_type_owned,
           i.check.value = "", 
           i.check.issue_id = "logic_c_KAP_stove_type_owned_1",
           i.check.issue = glue("KAP_stove_type_owned: {KAP_stove_type_owned}, 
                              KAP_fuels_mostly_used: {KAP_fuels_mostly_used}"),
           i.check.other_text = "",
           i.check.checked_by = "MT",
           i.check.checked_date = as_date(today()),
           i.check.comment = "",
           i.check.reviewed = "",
           i.check.adjust_log = "",
           i.check.uuid_cl = "",
           i.check.so_sm_choices = "") %>% 
    dplyr::select(starts_with("i.check")) %>%
    rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

# The respondent knows how to make an improved stove but does not own one. i.e,
# KAP_knowledge_to_build_improved_stoves = "yes", and KAP_stove_type_owned = "everything except the improved stove"
df_KAP_knowledge_to_build_improved_stoves_2 <- df_tool_data %>% 
    filter(KAP_knowledge_to_build_improved_stoves %in% c("yes"), 
   !str_detect(string = KAP_stove_type_owned, pattern = "a_fuel_efficient_cookstove"))%>% 
    mutate(i.check.type = "change_response",
           i.check.name = "KAP_knowledge_to_build_improved_stoves", 
           i.check.current_value = KAP_knowledge_to_build_improved_stoves,
           i.check.value = "", 
           i.check.issue_id = "logic_c_KAP_knowledge_to_build_improved_stoves_2",
           i.check.issue = glue("KAP_knowledge_to_build_improved_stoves: {KAP_knowledge_to_build_improved_stoves}, 
                              KAP_stove_type_owned: {KAP_stove_type_owned}"),
           i.check.other_text = "",
           i.check.checked_by = "MT",
           i.check.checked_date = as_date(today()),
           i.check.comment = "",
           i.check.reviewed = "",
           i.check.adjust_log = "",
           i.check.uuid_cl = "",
           i.check.so_sm_choices = "") %>% 
    dplyr::select(starts_with("i.check")) %>%
    rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

 
 # The respondent says they are not familiar with the concept of briquettes, but reported using briquettes as one of their main fuels for cooking. i.e,
 # KAP_knowledge_to_build_improved_stoves = "yes", and KAP_stove_type_owned = "everything except the improved stove"
 df_KAP_knowledge_to_build_improved_stoves_2 <- df_tool_data %>% 
     filter(KAP_knowledge_to_build_improved_stoves %in% c("yes"), 
            !str_detect(string = KAP_stove_type_owned, pattern = "a_fuel_efficient_cookstove"))%>% 
     mutate(i.check.type = "change_response",
            i.check.name = "KAP_knowledge_to_build_improved_stoves", 
            i.check.current_value = KAP_knowledge_to_build_improved_stoves,
            i.check.value = "", 
            i.check.issue_id = "logic_c_KAP_knowledge_to_build_improved_stoves_2",
            i.check.issue = glue("KAP_knowledge_to_build_improved_stoves: {KAP_knowledge_to_build_improved_stoves}, 
                              KAP_stove_type_owned: {KAP_stove_type_owned}"),
            i.check.other_text = "",
            i.check.checked_by = "MT",
            i.check.checked_date = as_date(today()),
            i.check.comment = "",
            i.check.reviewed = "",
            i.check.adjust_log = "",
            i.check.uuid_cl = "",
            i.check.so_sm_choices = "") %>% 
     dplyr::select(starts_with("i.check")) %>%
     rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))
 
 write_csv(x = df_KAP_knowledge_to_build_improved_stoves_2, file = "outputs/fuel.csv")
 






