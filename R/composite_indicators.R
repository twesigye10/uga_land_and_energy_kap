# function for creating composite indicators
create_composite_indicators <- function(input_df) {
    input_df %>% 
        mutate(i.meta_region = case_when(meta_district_name %in% c("isingiro", "kamwenge", "kikuube", "kyegegwa") ~ "south_west",
                                         TRUE ~ "west_nile"),
               i.meta_hoh_gender = ifelse(meta_hoh %in% c("yes"), meta_respondent_gender, meta_hoh_gender), 
               i.meta_hoh_age = ifelse(meta_hoh %in% c("yes"), meta_respondent_age, meta_hoh_age), 
               i.meta_hoh_educ_level = ifelse(meta_hoh %in% c("yes"), meta_respodent_educ_level, meta_hoh_educ_level), 
               i.meta_respondent_age = case_when(meta_respondent_age <= 18 ~ "age_12_18",
                                                 meta_respondent_age <= 59 ~ "age_19_59",
                                                 meta_respondent_age > 59 ~ "age_greater_59",
                                                 TRUE ~ "NA"),
               i.land_access_attempts_extra_land = case_when(land_hh_have_access_to_additional_land %in% c("yes") ~ "Yes, additional land",
                                                             land_hh_have_access_to_additional_land %in% c("no") & land_hh_ever_attempt_access_additional_land %in% c("yes") ~ "Attempted unsuccessfuly",
                                                             land_hh_have_access_to_additional_land %in% c("no") & land_hh_ever_attempt_access_additional_land %in% c("no") ~ "Never attempted")
        )
}