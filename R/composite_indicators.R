# function for creating composite indicators
create_composite_indicators <- function(input_df) {
    input_df %>% 
        mutate(i.meta_region = case_when(meta_district_name %in% c("isingiro", "kamwenge", "kikuube", "kyegegwa") ~ "south_west",
                                         TRUE ~ "west_nile"),
               i.meta_hoh_gender = ifelse(meta_hoh %in% c("yes"), meta_respondent_gender, meta_hoh_gender), 
               i.meta_respondent_age = case_when(meta_respondent_age <= 18 ~ "age_12_18",
                                                 meta_respondent_age <= 59 ~ "age_19_59",
                                                 meta_respondent_age > 59 ~ "age_greater_59",
                                                 TRUE ~ "NA")
        )
}