library(tidyverse)
library(openxlsx)

# global options can be set to further simplify things
options("openxlsx.borderStyle" = "thin")
options("openxlsx.withFilter" = FALSE)

# tool
loc_tool <- "inputs/land_and_energy_tool.xlsx"
df_survey <- readxl::read_excel(loc_tool, sheet = "survey")
df_choices <- readxl::read_excel(loc_tool, sheet = "choices") %>% 
    select(list_name, choice_name = name,   choice_label =label)

# extract select types
df_tool_select_type <- df_survey %>% 
    select(type, qn_name = name, label) %>% 
    filter(str_detect(string = type, pattern = "integer|date|select_one|select_multiple")) %>% 
    separate(col = type, into = c("select_type", "list_name"), sep =" ", remove = TRUE, extra = "drop" )

# extract choice ids and labels
df_choices_support <- df_choices %>% 
    left_join(df_tool_select_type) %>% 
    unite("survey_choice_id", qn_name, choice_name, sep = "_", remove = FALSE) %>% 
    select(survey_choice_id, choice_label) 

# extract groups
df_tool_groups <- df_survey %>% 
    mutate(int.group = ifelse(str_detect(string = type, pattern = "begin_group"), label, NA_character_),
           i.group = int.group) %>%  
    fill(i.group) %>% 
    filter(!str_detect(string = name, pattern = "_other$"),
           !str_detect(string = type, pattern = "group|repeat|text|geopoint|^gps$|^note$"),
           !is.na(i.group)
    ) %>% 
    mutate(qn_number = row_number()) %>% 
    select(type, name, label, indicator_group_sector = i.group, qn_number)

# support composite grps and labels
df_support_composite_grps <- readxl::read_excel("support_files/support composite grps and labels.xlsx")
# df_support_integer_col_labs <- readxl::read_excel("support_files/support integer column names.xlsx") %>% 
#     filter(!is.na(integer_column_label))

# analysis

integer_cols_i <- c("i.meta_hoh_age", "i.meta_respondent_age")
integer_cols_int <- c("int.meta_hoh_age", "int.meta_respondent_age")

df_unformatted_analysis <- read_csv("outputs/combined_analysis_lf_uga_land_and_energy.csv") %>% 
    filter(!str_detect(string = variable, pattern = "^land_")) %>% 
    mutate(variable = ifelse(is.na(variable) | variable %in% c(""), variable_val, variable),
           int.variable = ifelse(str_detect(string = variable, pattern = "^i\\."), str_replace(string = variable, pattern = "^i\\.", replacement = ""), variable)) %>% 
    left_join(df_tool_select_type, by = c("int.variable" = "qn_name")) %>% 
    relocate(label, .after = variable) %>% 
    mutate(variable = ifelse(variable %in% integer_cols_i, str_replace(string = variable, pattern = "i.", replacement = "int."), variable),
           select_type = ifelse(variable %in% integer_cols_int, "integer", select_type),
           label = ifelse(is.na(label), variable, label)) %>% 
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
           subset_2_name,
           subset_2_val,
           select_type
    )

df_analysis <- df_unformatted_analysis %>% 
    mutate(analysis_choice_id = case_when(select_type %in% c("select_multiple", "select multiple") ~ str_replace(string = `choices/options`, 
                                                                                                                 pattern = "\\/", replacement = "_"),
                                          select_type %in% c("select_one", "select one") ~ paste0(variable, "_", `choices/options`)),
           Question = ifelse(variable %in% df_support_composite_grps$composite_code, recode(variable, !!!setNames(df_support_composite_grps$composite_qn_label, df_support_composite_grps$composite_code)), Question)
    )

# format the data ---------------------------------------------------------

df_analysis_formatting <- df_analysis %>% 
    left_join(df_tool_groups %>% 
                  select(name, indicator_group_sector, qn_number), by = c("variable" = "name")) %>% 
    mutate(response_label = recode(analysis_choice_id, !!!setNames(df_choices_support$choice_label, df_choices_support$survey_choice_id)),
           choices = ifelse(is.na(response_label), `choices/options`, response_label),
           # subset_1_val_label = recode(subset_1_val, !!!setNames(df_choices$choice_label, df_choices$choice_name)),
           # subset_1_val_label =  ifelse(is.na(subset_1_val_label), "National", subset_1_val_label),
           # subset_2_val_label = recode(subset_2_val, !!!setNames(df_choices$choice_label, df_choices$choice_name)),
           # subset_2_val_label =  ifelse(is.na(subset_2_val_label), "National", subset_2_val_label)
    ) #%>% 
    # select(-c(n_unweighted, subset_1_name, subset_1_val))  %>%  
    # filter(!is.na(indicator_group_sector))

# make wider and reorder columns

df_analysis_wide <- df_analysis_formatting %>% 
    mutate(subset_1_val = ifelse(subset_1_name %in% c("i.meta_hoh_gender"), paste0("hoh_", subset_1_val), subset_1_val)) %>% # there were 2 subsets with gender
    filter(is.na(subset_2_val)) %>%
    # filter(is.na(subset_2_val), population %in% c("refugee")) %>% 
    # select(-c(subset_1_name, subset_2_name, subset_2_val, n_unweighted)) %>% 
    select(-c(subset_1_name, subset_2_name, subset_2_val)) %>% 
    mutate(subset_1_val = ifelse(is.na(subset_1_val), "National", subset_1_val)) %>% 
    pivot_wider(names_from = c(subset_1_val, population), values_from = c(`Results(mean/percentage)`, n_unweighted), values_fill = 0) %>%
    # pivot_wider(names_from = c(subset_1_val), values_from = c(`Results(mean/percentage)`)) %>% 
    arrange(qn_number) %>% 
    mutate(row_id = row_number()) %>% 
    rename_with(.fn = ~str_replace(string = .x, pattern = "Results\\(mean\\/percentage\\)_|_unweighted", replacement = ""))

# A. Settlement level
settln_host_cols <- c("adjumani_refugee", "bidibidi_refugee", "imvepi_refugee", "kiryandongo_refugee", "kyaka_ii_refugee", "kyangwali_refugee", "lobule_refugee", "nakivale_refugee", "oruchinga_refugee", "palabek_refugee", "palorinya_refugee", "rhino_camp_refugee", "rwamwanja_refugee", "adjumani_adjumani_host_community", "isingiro_nakivale_host_community", "isingiro_oruchinga_host_community", "kamwenge_rwamwanja_host_community", "kikuube_kyangwali_host_community", "kiryandongo_kiryandongo_host_community", "koboko_lobule_host_community", "kyegegwa_kyaka_ii_host_community", "lamwo_palabek_host_community", "madi_okollo_rhino_camp_host_community", "obongi_palorinya_host_community", "terego_imvepi_host_community", "yumbe_bidibidi_host_community")
settln_host_cols_n <- c("n_adjumani_refugee", "n_bidibidi_refugee", "n_imvepi_refugee", "n_kiryandongo_refugee", "n_kyaka_ii_refugee", "n_kyangwali_refugee", "n_lobule_refugee", "n_nakivale_refugee", "n_oruchinga_refugee", "n_palabek_refugee", "n_palorinya_refugee", "n_rhino_camp_refugee", "n_rwamwanja_refugee", "n_adjumani_adjumani_host_community", "n_isingiro_nakivale_host_community", "n_isingiro_oruchinga_host_community", "n_kamwenge_rwamwanja_host_community", "n_kikuube_kyangwali_host_community", "n_kiryandongo_kiryandongo_host_community", "n_koboko_lobule_host_community", "n_kyegegwa_kyaka_ii_host_community", "n_lamwo_palabek_host_community", "n_madi_okollo_rhino_camp_host_community", "n_obongi_palorinya_host_community", "n_terego_imvepi_host_community", "n_yumbe_bidibidi_host_community")
settlement_columns <- tibble(settln_host_cols, settln_host_cols_n) %>% 
    pivot_longer(cols = c(settln_host_cols, settln_host_cols_n), names_to = "entries", values_to = "columns") %>% 
    pull(columns)

# B. Regional
regional_cols <- c("south_west_refugee", "west_nile_refugee", "south_west_host_community", "west_nile_host_community")
regional_cols_n <- c("n_south_west_refugee", "n_west_nile_refugee", "n_south_west_host_community", "n_west_nile_host_community")
regional_columns <- tibble(regional_cols, regional_cols_n) %>% 
    pivot_longer(cols = c(regional_cols, regional_cols_n), names_to = "entries", values_to = "columns") %>% 
    pull(columns)

# C. National
national_cols <- c("National_refugee", "National_host_community", "hoh_female_refugee", "hoh_male_refugee", "hoh_female_host_community", "hoh_male_host_community", "female_refugee", "male_refugee", "female_host_community", "male_host_community")
national_cols_n <- c("n_National_refugee", "n_National_host_community", "n_hoh_female_refugee", "n_hoh_male_refugee", "n_hoh_female_host_community", "n_hoh_male_host_community", "n_female_refugee", "n_male_refugee", "n_female_host_community", "n_male_host_community")
national_columns <- tibble(national_cols, national_cols_n) %>% 
    pivot_longer(cols = c(national_cols, national_cols_n), names_to = "entries", values_to = "columns") %>% 
    pull(columns)

# reorder

df_analysis_wide_reodered <- df_analysis_wide %>% 
    relocate(any_of(national_columns), .after = "choices") %>% 
    relocate(any_of(regional_columns), .after = "choices") %>% 
    relocate(any_of(settlement_columns), .after = "choices") %>% 
    relocate(select_type, .after = choices) 

cols_for_num_pct_formatting <- df_analysis_wide_reodered %>% 
    select(adjumani_refugee:n_male_host_community) %>% 
    select(!matches("^n_")) %>% 
    colnames()

# extract header data

df_to_extract_header = df_analysis_wide_reodered %>% 
    select(-c(Question, `choices/options`, 
              analysis_choice_id, 
              indicator_group_sector,response_label,
              row_id, variable, qn_number)) %>% 
    colnames()

df_extracted_header_data <- tibble("old_cols" = df_to_extract_header) %>% 
    mutate("new_cols" = paste0("x", row_number())) %>% 
    mutate(old_cols = str_replace(string = old_cols, pattern = "_host_community$|_refugee$", replacement = "")) %>% 
    mutate(old_cols = str_replace(string = old_cols, pattern = "^n_.+", replacement = "n")) %>% 
    pivot_wider(names_from = new_cols, values_from = old_cols)

df_extracted_modified_data <- tibble("old_cols" = df_to_extract_header) %>% 
    mutate("new_cols" = paste0("x", row_number())) %>% 
    pivot_wider(names_from = new_cols, values_from = old_cols)


df_extracted_header <- bind_rows(df_extracted_header_data, df_extracted_modified_data) %>% 
    mutate(x1 = NA_character_)


# create workbook ---------------------------------------------------------

wb <- createWorkbook()

hs1 <- createStyle(fgFill = "#EE5859", halign = "CENTER", textDecoration = "Bold", fontColour = "white", fontSize = 14, wrapText = T)
hs2 <- createStyle(fgFill = "#808080", halign = "LEFT", textDecoration = "Bold", fontColour = "white", wrapText = T)
hs3 <- createStyle(fgFill = "#EE5859", halign = "CENTER", textDecoration = "Bold", border = "Bottom", fontColour = "white")

# numbers
number_2digit_style <- openxlsx::createStyle(numFmt = "0.00")
number_1digit_style <- openxlsx::createStyle(numFmt = "0.0")
number_style <- openxlsx::createStyle(numFmt = "0")


addWorksheet(wb, sheetName="Analysis")

# add header to sheet

mergeCells(wb, sheet = "Analysis", rows = 2, cols = 3:54)
addStyle(wb, sheet = "Analysis", hs1, rows = 2, cols = 3:54, gridExpand = TRUE)
writeData(wb, sheet = "Analysis", "Settlement", startCol = 3, startRow = 2, headerStyle = hs1)
mergeCells(wb, sheet = "Analysis", rows = 2, cols = 55:62)
addStyle(wb, sheet = "Analysis", hs1, rows = 2, cols = 55:62, gridExpand = TRUE)
writeData(wb, sheet = "Analysis", "Regional", startCol = 55, startRow = 2, headerStyle = hs1)
mergeCells(wb, sheet = "Analysis", rows = 2, cols = 63:82)
addStyle(wb, sheet = "Analysis", hs1, rows = 2, cols = 63:82, gridExpand = TRUE)
writeData(wb, sheet = "Analysis", "National", startCol = 63, startRow = 2, headerStyle = hs1)
# header showing results headings
writeData(wb, sheet = "Analysis", df_extracted_header %>% head(1), startCol = 1, startRow = 3, headerStyle = hs2, colNames = FALSE)
addStyle(wb, sheet = "Analysis", hs2, rows = 3, cols = 1:82, gridExpand = TRUE)

setColWidths(wb = wb, sheet = "Analysis", cols = 1, widths = 60)
setColWidths(wb = wb, sheet = "Analysis", cols = 2, widths = 10)
setColWidths(wb = wb, sheet = "Analysis", cols = 3:82, widths = 8)

# split variables to be written in different tables with in a sheet
sheet_variables_data <- split(df_analysis_wide_reodered, factor(df_analysis_wide_reodered$variable, levels = unique(df_analysis_wide_reodered$variable)))

previous_row_end <- 3

for (i in 1:length(sheet_variables_data)) {
    
    current_variable_data <- sheet_variables_data[[i]]
    
    get_question <- current_variable_data %>% select(Question) %>% unique() %>% pull()
    get_qn_type <- current_variable_data %>% select(select_type) %>% unique() %>% pull()
    
    if(get_qn_type %in% c("select_one", "select one", "select_multiple", "select multiple")){
        for(n in cols_for_num_pct_formatting){class(current_variable_data[[n]])= "percentage"}
    }else{
        for(n in cols_for_num_pct_formatting){class(current_variable_data[[n]])= "numeric"}
    }
    
    current_row_start <- previous_row_end + 3
    
    print(current_row_start)
    
    # add header for variable
    writeData(wb, sheet = "Analysis", get_question, startCol = 1, startRow = previous_row_end + 2)
    addStyle(wb, sheet = "Analysis", hs2, rows = previous_row_end + 2, cols = 1, gridExpand = TRUE)
    writeData(wb, sheet = "Analysis", get_qn_type, startCol = 2, startRow = previous_row_end + 2)
    addStyle(wb, sheet = "Analysis", hs2, rows = previous_row_end + 2, cols = 2, gridExpand = TRUE)
    
    current_data_length <- max(current_variable_data$row_id) - min(current_variable_data$row_id)
    
    writeData(wb = wb, 
                   sheet = "Analysis", 
                   x = current_variable_data %>% 
                       select(-c(Question, `choices/options`, 
                                 analysis_choice_id, 
                                 indicator_group_sector,response_label,
                                 row_id, variable, qn_number)
                       ) %>% mutate(select_type = NA_character_), 
                   startRow = current_row_start, 
                   startCol = 1, 
                   colNames = FALSE)
    
    previous_row_end <- current_row_start + 1 + current_data_length
}
# hide grid lines
showGridLines(wb,  "Analysis", showGridLines = FALSE)  

# freeze pane
freezePane(wb, "Analysis", firstActiveRow = 4, firstActiveCol = 3)

openXL(wb)

# saveWorkbook(wb, paste0("outputs/", butteR::date_file_prefix(),"_formatted_analysis_uga_kap_draft.xlsx"), overwrite = TRUE)
# openXL(file = paste0("outputs/", butteR::date_file_prefix(),"_formatted_analysis_uga_kap_draft.xlsx"))

