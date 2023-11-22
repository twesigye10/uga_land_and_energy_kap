library(tidyverse)
library(srvyr)
library(supporteR)  

source("R/composite_indicators.R")
source("R/make_weights.R")

# packages to install incase
# devtools::install_github("zackarno/butteR")
# devtools::install_github("twesigye10/supporteR")

# script running start time
start <- Sys.time()

df_pop_data_settlement <- read_csv("inputs/refugee_population.csv") %>% 
    dplyr::mutate(strata = janitor::make_clean_names(strata))
df_pop_data_host <- read_csv("inputs/host_population.csv") %>% 
    dplyr::mutate(strata = janitor::make_clean_names(strata))

# clean data
data_path <- "inputs/clean_data_land_energy.xlsx"

data_nms <- names(readxl::read_excel(path = data_path, n_max = 2000))
c_types <- case_when(str_detect(string = data_nms, pattern = "_other$") ~ "text", 
                     str_detect(string = data_nms, pattern = "/") ~ "logical",
                     TRUE ~ "guess")

df_main_clean_data <- readxl::read_excel(path = data_path, col_types = c_types, na = "NA")

# tool
loc_tool <- "inputs/land_and_energy_tool.xlsx"
df_survey <- readxl::read_excel(loc_tool, sheet = "survey")
df_choices <- readxl::read_excel(loc_tool, sheet = "choices")

df_tool_data_support <- df_survey %>% 
    select(type, name, label) %>% 
    filter(str_detect(string = type, pattern = "integer|date|select_one|select_multiple")) %>% 
    separate(col = type, into = c("select_type", "list_name"), sep =" ", remove = TRUE, extra = "drop" )

df_tool_settlement_host_support <- df_choices %>% 
    filter(list_name %in% c("refugee_settlement_list")) %>% 
    mutate(i.host_settlement_name = paste0(district, "_", name)) %>% 
    select(name, label, district, i.host_settlement_name)

# dap
dap <- read_csv("inputs/r_dap_land_energy.csv")

# add composites and strata
df_data_with_composites <- df_main_clean_data %>% 
    create_composite_indicators() %>% 
    dplyr::mutate(kap_cost_of_cheapest_improved_stove = as.numeric(kap_cost_of_cheapest_improved_stove),
                  i.meta_hoh_age = as.numeric(i.meta_hoh_age),
                  meta_district_name = case_when(meta_status  %in% c("host_community") & meta_district_name %in% c("isingiro") & str_detect(string = point_number, pattern = "^oru_|^kak_|^kgt_" ) ~ "isingiro_oruchinga",
                                                 meta_status  %in% c("host_community") & meta_district_name %in% c("isingiro") & str_detect(string = point_number, pattern = "^nak_|kna_|^ksh_|^rug_|^rus_" ) ~ "isingiro_nakivale",
                                                 TRUE ~ meta_district_name),
                  strata = case_when(meta_status == "refugee" ~ paste0(meta_refugee_settlement, "_refugee"),
                                     meta_status == "host_community" ~ paste0(meta_district_name,"_host"),
                                     TRUE ~ meta_status))

# split data into host and refugee
df_ref <- df_data_with_composites %>% 
    filter(meta_status == "refugee") %>% 
    select(-where(function(x) all(is.na(x))))

df_host <- df_data_with_composites %>% 
    filter(meta_status == "host_community") %>% 
    select(-where(function(x) all(is.na(x))))

# analysis for refugees ---------------------------------------------------

# weights table refugees
ref_weight_table <- make_refugee_weight_table(input_df_ref = df_ref, 
                                              input_refugee_pop = df_pop_data_settlement)
# add weights to data
df_ref_with_weights <- df_ref %>%  
    left_join(ref_weight_table, by = "strata")

# set up design object
ref_svy <- as_survey(.data = df_ref_with_weights, strata = strata, weights = weights)

# analysis

df_ref_analysis <- analysis_after_survey_creation(input_svy_obj = ref_svy,
                                                   input_dap = dap %>% filter(variable %in% colnames(df_ref), !subset_1 %in% c("meta_district_name")) ) %>% 
    dplyr::mutate(population = "refugee")


# analysis for host -------------------------------------------------------

# weights table refugees
host_weight_table <- make_host_weight_table(input_df_host = df_host, 
                                            input_host_pop = df_pop_data_host)
# add weights to data
df_host_with_weights <- df_host %>%  
    left_join(host_weight_table, by = "strata")

# set up design object
host_svy <- as_survey(.data = df_host_with_weights, strata = strata, weights = weights)

# analysis

df_host_analysis <- analysis_after_survey_creation(input_svy_obj = host_svy,
                                                  input_dap = dap %>% filter(variable %in% colnames(df_host),
                                                                             subset_1 %in% colnames(df_host)) ) %>% 
    dplyr::mutate(population = "host_community") %>% 
    mutate(subset_1_val =  ifelse(subset_1_name %in% c("meta_district_name") & !subset_1_val %in% c("isingiro_nakivale",
                                                                                                   "isingiro_oruchinga"), 
                                  recode(subset_1_val, !!!setNames(df_tool_settlement_host_support$i.host_settlement_name, 
                                                                   df_tool_settlement_host_support$district)), subset_1_val) )


# merge and format analysis ----------------------------------------------------------

combined_analysis <- bind_rows(df_ref_analysis, df_host_analysis)

# script running end time
end <- Sys.time()

print(paste("Sript running time: ", (end-start)/60, "minutes"))


integer_cols_i <- c("i.meta_hoh_age", "i.meta_respondent_age")
integer_cols_int <- c("int.meta_hoh_age", "int.meta_respondent_age")

# formatting the analysis, adding question labels
full_analysis_long <- combined_analysis %>% 
  mutate(variable = ifelse(is.na(variable) | variable %in% c(""), variable_val, variable),
         int.variable = ifelse(str_detect(string = variable, pattern = "^i\\."), str_replace(string = variable, pattern = "^i\\.", replacement = ""), variable)) %>% 
  left_join(df_tool_data_support, by = c("int.variable" = "name")) %>% 
  relocate(label, .after = variable) %>% 
  mutate(variable = ifelse(variable %in% integer_cols_i, str_replace(string = variable, pattern = "i.", replacement = "int."), variable),
         select_type = ifelse(variable %in% integer_cols_int, "integer", select_type),
         label = ifelse(is.na(label), variable, label),
         `mean/pct` = ifelse(select_type %in% c("integer") & !variable %in% integer_cols_i & !str_detect(string = variable, pattern = "^i\\."), `mean/pct`, `mean/pct`*100),
         `mean/pct` = round(`mean/pct`, digits = 3)) %>% 
  mutate(variable = ifelse(variable %in% integer_cols_int, str_replace(string = variable, pattern = "int.", replacement = "i."), variable),
         label = ifelse(label %in% integer_cols_int, str_replace(string = label, pattern = "int.", replacement = "i."), label)) %>% 
  select(`Question`= label, 
         variable, 
         `choices/options` = variable_val, 
         `Results(mean/percentage)` = `mean/pct`, 
         n_unweighted, 
         population, 
         subset_1_name, 
         subset_1_val, 
         select_type
         )

# output analysis
write_csv(full_analysis_long, paste0("outputs/", butteR::date_file_prefix(), "_full_analysis_lf_uga_land_and_energy.csv"), na="")
write_csv(full_analysis_long, paste0("outputs/full_analysis_lf_uga_land_and_energy.csv"), na="")
write_csv(combined_analysis, paste0("outputs/combined_analysis_lf_uga_land_and_energy.csv"), na="")

