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
           !is.na(i.group)) %>% 
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
           select_type )

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
    ) %>% 
    filter(!subset_1_name %in% c("i.meta_hoh_gender")) %>% 
    filter(!(subset_2_name %in% c("i.meta_region") & subset_1_name %in% c("meta_refugee_settlement", "meta_district_name")))
    

# make wider and reorder columns

df_analysis_wide <- df_analysis_formatting %>% 
    mutate(subset_1_val = ifelse(subset_1_name %in% c("i.meta_hoh_gender"), paste0("hoh_", subset_1_val), subset_1_val)) %>% # there were 2 subsets with gender
    select(-c(subset_1_name, subset_2_name)) %>% 
    mutate(subset_1_val = ifelse(is.na(subset_1_val), "National", subset_1_val)) %>% 
    pivot_wider(names_from = c(subset_1_val, population, subset_2_val), values_from = c(`Results(mean/percentage)`, n_unweighted), values_fill = 0) %>%
    arrange(qn_number) %>% 
    mutate(row_id = row_number(),
           select_type = str_to_sentence(str_replace_all(string = select_type, pattern = "_", replacement = " "))) 

# start_cols <- c("Question", "variable", "choices/options", "select_type", "analysis_choice_id", "indicator_group_sector", "qn_number", "response_label", "choices")
# openxlsx::write.xlsx(df_analysis_wide %>% select(any_of(start_cols), ends_with("_NA"), everything()), "outputs/test_wide_format.xlsx")

df_cols_for_ordering <- readxl::read_excel("outputs/column_ordering_with_regional.xlsx", sheet = "combined")

reordered_columns <- df_cols_for_ordering %>%
    pivot_longer(cols = c(result_col, n_unweighted), names_to = "entries", values_to = "columns") %>%
    pull(columns)

# reorder

df_analysis_wide_reodered <- df_analysis_wide %>%
    relocate(any_of(reordered_columns), .after = "choices") %>%
    relocate(select_type, .after = choices)

cols_for_num_pct_formatting <- df_analysis_wide_reodered %>% 
    select(`Results(mean/percentage)_adjumani_refugee_NA`:row_id) %>% 
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
    mutate(old_cols = str_replace(string = old_cols, pattern = "Results\\(mean\\/percentage\\)_|_host_community$|_refugee$", replacement = "")) %>% 
    mutate(old_cols = str_replace(string = old_cols, pattern = "_host_community_NA$|_refugee_NA$", replacement = "")) %>% 
    mutate(old_cols = str_replace(string = old_cols, pattern = "^n_.+", replacement = "n")) %>% 
    pivot_wider(names_from = new_cols, values_from = old_cols)

df_header_labels_info <- readxl::read_excel("outputs/column_ordering_with_regional.xlsx", sheet = "header_info")
df_extracted_header_label <- tibble("old_cols" = df_to_extract_header) %>% 
    mutate("new_cols" = paste0("x", row_number())) %>% 
    mutate(old_cols = str_replace(string = old_cols, pattern = "Results\\(mean\\/percentage\\)_|_host_community$|_refugee$", replacement = "")) %>% 
    mutate(old_cols = str_replace(string = old_cols, pattern = "_host_community_NA$|_refugee_NA$", replacement = "")) %>% 
    mutate(old_cols = str_replace(string = old_cols, pattern = "^n_.+", replacement = "n"),
           old_cols = recode(old_cols, !!!setNames(df_header_labels_info$header_label, df_header_labels_info$header_code))) %>% 
    pivot_wider(names_from = new_cols, values_from = old_cols)

df_extracted_modified_data <- tibble("old_cols" = df_to_extract_header) %>% 
    mutate("new_cols" = paste0("x", row_number())) %>% 
    pivot_wider(names_from = new_cols, values_from = old_cols)

df_extracted_host_refugee <- tibble("old_cols" = df_to_extract_header) %>% 
    mutate("new_cols" = paste0("x", row_number()),
           old_cols = case_when(str_detect(string = old_cols, pattern = "refugee") ~ "Refugee",
                                str_detect(string = old_cols, pattern = "host_community") ~ "Host community",)) %>% 
    
    pivot_wider(names_from = new_cols, values_from = old_cols)


df_extracted_header <- bind_rows(df_extracted_header_label, df_extracted_host_refugee, df_extracted_header_data, df_extracted_modified_data) %>% 
    mutate(x1 = NA_character_)


# create workbook ---------------------------------------------------------

wb <- createWorkbook()

hs1 <- createStyle(fgFill = "#EE5859", halign = "CENTER", textDecoration = "Bold", fontColour = "white", fontSize = 14, wrapText = T, 
                   border = "TopBottomLeftRight", borderStyle = "medium", borderColour = "#000000")
hs2 <- createStyle(fgFill = "#808080", halign = "LEFT", textDecoration = "Bold", fontColour = "white", wrapText = F)
hs2_no_bold <- createStyle(fgFill = "#808080", halign = "LEFT", textDecoration = "", fontColour = "white", wrapText = F)
hs3 <- createStyle(fgFill = "#808080", halign = "CENTER", fontColour = "white", textDecoration = "Bold", 
                   border = "TopBottomLeftRight", borderStyle = "medium", borderColour = "#000000")

# numbers
number_2digit_style <- openxlsx::createStyle(numFmt = "0.00")
number_1digit_style <- openxlsx::createStyle(numFmt = "0.0")
number_style <- openxlsx::createStyle(numFmt = "0")
# percent
pct = createStyle(numFmt="0.0%") # not working

addWorksheet(wb, sheetName="Analysis")

# add disaggregations
mergeCells(wb, sheet = "Analysis", rows = 1, cols = 3:54)
writeData(wb, sheet = "Analysis", "Settlement", startCol = 3, startRow = 1, headerStyle = hs1)
mergeCells(wb, sheet = "Analysis", rows = 1, cols = 55:78)
writeData(wb, sheet = "Analysis", "Regional", startCol = 55, startRow = 1, headerStyle = hs1)
mergeCells(wb, sheet = "Analysis", rows = 1, cols = 79:86)
writeData(wb, sheet = "Analysis", "Respondent Gender", startCol = 79, startRow = 1, headerStyle = hs1)
mergeCells(wb, sheet = "Analysis", rows = 1, cols = 87:90)
writeData(wb, sheet = "Analysis", "National", startCol = 87, startRow = 1, headerStyle = hs1)

addStyle(wb, sheet = "Analysis", hs1, rows = 1, cols = 3:90, gridExpand = TRUE)

# specify status of the analysis
mergeCells(wb, sheet = "Analysis", rows = 2, cols = 3:28)
writeData(wb, sheet = "Analysis", "Refugee", startCol = 3, startRow = 2)
mergeCells(wb, sheet = "Analysis", rows = 2, cols = 29:54)
writeData(wb, sheet = "Analysis", "Host Community", startCol = 29, startRow = 2)
mergeCells(wb, sheet = "Analysis", rows = 2, cols = 55:58)
writeData(wb, sheet = "Analysis", "Refugee", startCol = 55, startRow = 2)
mergeCells(wb, sheet = "Analysis", rows = 2, cols = 59:62)
writeData(wb, sheet = "Analysis", "Host Community", startCol = 59, startRow = 2)
mergeCells(wb, sheet = "Analysis", rows = 2, cols = 63:70)
writeData(wb, sheet = "Analysis", "Refugee", startCol = 63, startRow = 2)
mergeCells(wb, sheet = "Analysis", rows = 2, cols = 71:78)
writeData(wb, sheet = "Analysis", "Host Community", startCol = 71, startRow = 2)
mergeCells(wb, sheet = "Analysis", rows = 2, cols = 79:82)
writeData(wb, sheet = "Analysis", "Refugee", startCol = 79, startRow = 2)
mergeCells(wb, sheet = "Analysis", rows = 2, cols = 83:86)
writeData(wb, sheet = "Analysis", "Host Community", startCol = 83, startRow = 2)
mergeCells(wb, sheet = "Analysis", rows = 2, cols = 87:88)
writeData(wb, sheet = "Analysis", "Refugee", startCol = 87, startRow = 2)
mergeCells(wb, sheet = "Analysis", rows = 2, cols = 89:90)
writeData(wb, sheet = "Analysis", "Host Community", startCol = 89, startRow = 2)

addStyle(wb, sheet = "Analysis", hs3, rows = 2, cols = 3:90, gridExpand = TRUE)

# refugee or host


# header showing results headings
writeData(wb, sheet = "Analysis", df_extracted_header %>% head(1), startCol = 1, 
          startRow = 3, headerStyle = hs2, colNames = FALSE, 
          borders = "all", borderColour = "#000000", borderStyle = "thin")
# addStyle(wb, sheet = "Analysis", hs2, rows = 3, cols = 1:90, gridExpand = TRUE)
# addStyle(wb, sheet = "Analysis", hs2_no_bold, rows = 4, cols = 1:90, gridExpand = TRUE)

setColWidths(wb = wb, sheet = "Analysis", cols = 1, widths = 70)
setColWidths(wb = wb, sheet = "Analysis", cols = 2, widths = 10)
setColWidths(wb = wb, sheet = "Analysis", cols = 3:90, widths = 8)

# split variables to be written in different tables with in a sheet
sheet_variables_data <- split(df_analysis_wide_reodered, factor(df_analysis_wide_reodered$variable, levels = unique(df_analysis_wide_reodered$variable)))

previous_row_end <- 4

for (i in 1:length(sheet_variables_data)) {
    
    current_variable_data <- sheet_variables_data[[i]]
    
    get_question <- current_variable_data %>% select(Question) %>% unique() %>% pull()
    get_qn_type <- current_variable_data %>% select(select_type) %>% unique() %>% pull()
    
    if(get_qn_type %in% c("select_one", "Select one", "select_multiple", "Select multiple")){
        for(n in cols_for_num_pct_formatting){class(current_variable_data[[n]])= "percentage"}
    }else{
        for(n in cols_for_num_pct_formatting){class(current_variable_data[[n]])= "numeric"}
    }
    
    current_row_start <- previous_row_end + 2
    
    print(current_row_start)
    
    # add header for variable
    writeData(wb, sheet = "Analysis", get_question, startCol = 1, startRow = previous_row_end + 1)
    writeData(wb, sheet = "Analysis", get_qn_type, startCol = 2, startRow = previous_row_end + 1)
    addStyle(wb, sheet = "Analysis", hs2, rows = previous_row_end + 1, cols = 1:2, gridExpand = TRUE)

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
    
    previous_row_end <- current_row_start + current_data_length
}
# hide grid lines
# showGridLines(wb,  "Analysis", showGridLines = FALSE)  

# freeze pane
freezePane(wb, "Analysis", firstActiveRow = 5, firstActiveCol = 3)

openXL(wb)

# saveWorkbook(wb, paste0("outputs/", butteR::date_file_prefix(),"_formatted_analysis_uga_kap_draft.xlsx"), overwrite = TRUE)
# openXL(file = paste0("outputs/", butteR::date_file_prefix(),"_formatted_analysis_uga_kap_draft.xlsx"))

