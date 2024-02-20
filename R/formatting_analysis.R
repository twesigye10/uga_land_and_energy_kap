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



# create workbook ---------------------------------------------------------

wb <- createWorkbook()

hs1 <- createStyle(fgFill = "#EE5859", halign = "CENTER", textDecoration = "Bold", fontColour = "white", fontSize = 14, wrapText = T)
hs2 <- createStyle(fgFill = "#808080", halign = "CENTER", textDecoration = "Bold", fontColour = "white", wrapText = T)
hs3 <- createStyle(fgFill = "#EE5859", halign = "CENTER", textDecoration = "Bold", border = "Bottom", fontColour = "white")

# numbers
number_2digit_style <- openxlsx::createStyle(numFmt = "0.00")
number_1digit_style <- openxlsx::createStyle(numFmt = "0.0")
number_style <- openxlsx::createStyle(numFmt = "0")


addWorksheet(wb, sheetName="Analysis")

# add header to sheet
mergeCells(wb, sheet = "Analysis", rows = 1, cols = 1:10)
writeData(wb, sheet = "Analysis", "Analysis", startCol = 1, startRow = 1, headerStyle = hs1)
addStyle(wb, sheet = "Analysis", hs1, rows = 1, cols = 1:10, gridExpand = TRUE)

setColWidths(wb = wb, sheet = "Analysis", cols = 2, widths = 60)

# get current data for the group or sector
current_sheet_data <- df_analysis_formatting %>% 
    mutate(subset_1_val = ifelse(subset_1_name %in% c("i.meta_hoh_gender"), paste0("hoh_", subset_1_val), subset_1_val)) %>% # there were 2 subsets with gender
    filter(is.na(subset_2_val)) %>%
    # filter(is.na(subset_2_val), population %in% c("refugee")) %>% 
    # select(-c(subset_1_name, subset_2_name, subset_2_val, n_unweighted)) %>% 
    select(-c(subset_1_name, subset_2_name, subset_2_val)) %>% 
    mutate(subset_1_val = ifelse(is.na(subset_1_val), "National", subset_1_val)) %>% 
    pivot_wider(names_from = c(subset_1_val, population), values_from = c(`Results(mean/percentage)`, n_unweighted), values_fill = 0) %>%
    # pivot_wider(names_from = c(subset_1_val), values_from = c(`Results(mean/percentage)`)) %>% 
    arrange(qn_number) %>% 
    mutate(row_id = row_number())

# split variables to be written in different tables with in a sheet
sheet_variables_data <- split(current_sheet_data, factor(current_sheet_data$variable, levels = unique(current_sheet_data$variable)))

previous_row_end <- 2

for (j in 1:length(sheet_variables_data)) {
    
    current_variable_data <- sheet_variables_data[[j]]
    
    get_question <- current_variable_data %>% select(Question) %>% unique() %>% pull()
    get_qn_type <- current_variable_data %>% select(select_type) %>% unique() %>% pull()
    
    if(get_qn_type %in% c("select_one", "select one", "select_multiple", "select multiple")){
        class(current_variable_data$Zonal) <- "percentage"
        class(current_variable_data$Gasera) <- "percentage"
        class(current_variable_data$Sinana) <- "percentage"
        class(current_variable_data$Goba) <- "percentage"
        class(current_variable_data$`Harena Buluk`) <- "percentage"
        class(current_variable_data$`Delo Mena`) <- "percentage"
        class(current_variable_data$Berbere) <- "percentage"
        class(current_variable_data$Goro) <- "percentage"
    }else{
        class(current_variable_data$Zonal) <- "numeric"
        class(current_variable_data$Gasera) <- "numeric"
        class(current_variable_data$Sinana) <- "numeric"
        class(current_variable_data$Goba) <- "numeric"
        class(current_variable_data$`Harena Buluk`) <- "numeric"
        class(current_variable_data$`Delo Mena`) <- "numeric"
        class(current_variable_data$Berbere) <- "numeric"
        class(current_variable_data$Goro) <- "numeric"
    }
    
    current_row_start <- previous_row_end + 3
    
    print(current_row_start)
    
    # add header for variable
    mergeCells(wb, sheet = "Analysis", rows = previous_row_end + 2, cols = 1:10)
    writeData(wb, sheet = "Analysis", get_question, startCol = 1, startRow = previous_row_end + 2)
    addStyle(wb, sheet = "Analysis", hs2, rows = previous_row_end + 2, cols = 1:10, gridExpand = TRUE)
    
    current_data_length <- max(current_variable_data$row_id) - min(current_variable_data$row_id)
    
    addStyle(wb, sheet = "Analysis", number_1digit_style, rows = current_row_start + 1 : current_row_start + 1 + current_data_length, cols = 1:10, gridExpand = TRUE)
    
    writeDataTable(wb = wb, 
                   sheet = "Analysis", 
                   x = current_variable_data %>% 
                       select(-c(Question, `choices/options`, 
                                 population, analysis_choice_id, 
                                 indicator_group_sector,response_lable,
                                 row_id, variable, select_type, indicator_variable, qn_number)
                       ), 
                   startRow = current_row_start, 
                   startCol = 1, 
                   tableStyle = "TableStyleLight9", 
                   headerStyle = hs3)
    
    previous_row_end <- current_row_start + 1 + current_data_length
}
# hide grid lines
showGridLines(wb,  "Analysis", showGridLines = FALSE)  

saveWorkbook(wb, paste0("outputs/", butteR::date_file_prefix(),"_formatted_analysis_uga_kap.xlsx"), overwrite = TRUE)
openXL(file = paste0("outputs/", butteR::date_file_prefix(),"_formatted_analysis_uga_kap.xlsx"))
