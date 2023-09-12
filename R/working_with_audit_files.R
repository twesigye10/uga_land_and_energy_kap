library(tidyverse)
library(lubridate)
library(openxlsx)

# global options can be set to further simplify things
options("openxlsx.borderStyle" = "thin")
# options("openxlsx.borderColour" = "#4F81BD")
options("openxlsx.withFilter" = TRUE)

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
           i.check.audit_start = as_datetime(as.POSIXct(start/1000, origin = "1970-01-01", tz = "Africa/Nairobi")),
           i.check.audit_end = as_datetime(as.POSIXct(end/1000, origin = "1970-01-01", tz = "Africa/Nairobi")),
           i.check.audit_qn = str_extract(string = node, pattern = "[\\w]+$"),
           i.check.qn_time_interval = round(x = (end - start)/1000, digits = 0)) |> 
    supporteR::batch_select_rename()


# outliers in the audit data ----------------------------------------------

df_check_audit_outliers <- cleaningtools::check_outliers(dataset = df_audit_data |> mutate(audit_uuid = paste0(audit_uuid, " * ", audit_qn)), 
                                                         uuid_column = "audit_uuid", strongness_factor = 3) 

df_potential_audit_outliers <- df_check_audit_outliers$potential_outliers|> 
    separate_wider_delim(cols = uuid, delim = " * ", names = c("audit_uuid", "audit_qn")) |> 
    filter(!str_detect(string = audit_qn, pattern = "_note$|_other$")) |> 
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


# export audit data -------------------------------------------------------

openxlsx::write.xlsx(x = df_audit_data,
                     file = paste0("outputs/", butteR::date_file_prefix(), 
                                   "_audit_raw_data_land_energy.xlsx"), 
                     overwrite = TRUE, keepNA = TRUE, na.string = "")

# formatting --------------------------------------------------------------

wb <- createWorkbook()

hs1 <- createStyle(fgFill = "#EE5859", halign = "CENTER", textDecoration = "Bold", fontColour = "white", fontSize = 14, wrapText = T)
hs2 <- createStyle(fgFill = "#808080", halign = "CENTER", textDecoration = "Bold", fontColour = "white", wrapText = T)
hs3 <- createStyle(fgFill = "#EE5859", halign = "CENTER", textDecoration = "Bold", border = "Bottom", fontColour = "white")
rowhs <- createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")

# write data
var_sheet_name <- "time diff main and audit"
addWorksheet(wb, sheetName = var_sheet_name)
setColWidths(wb = wb, sheet = var_sheet_name, cols = 1, widths = 37)
setColWidths(wb = wb, sheet = var_sheet_name, cols = 2:4, widths = 18)
setColWidths(wb = wb, sheet = var_sheet_name, cols = 5:7, widths = 30)
writeDataTable(wb = wb, 
               sheet = var_sheet_name, 
               x = df_main_and_audit_times, 
               startRow = 1, 
               startCol = 1, 
               tableStyle = "TableStyleLight9", 
               headerStyle = hs3)
conditionalFormatting(wb = wb, sheet = var_sheet_name, cols=1:7,rows = 2:nrow(df_main_and_audit_times), rule="AND($E2<20, $F2>20)", style = rowhs)

var_sheet_name <- "average time per qn and enum"
addWorksheet(wb, sheetName = var_sheet_name)
setColWidths(wb = wb, sheet = var_sheet_name, cols = 1, widths = 18)
setColWidths(wb = wb, sheet = var_sheet_name, cols = 2, widths = 56)
setColWidths(wb = wb, sheet = var_sheet_name, cols = 3, widths = 26)
writeDataTable(wb = wb, 
               sheet = var_sheet_name, 
               x = df_qn_time_enum_means, 
               startRow = 1, 
               startCol = 1, 
               tableStyle = "TableStyleLight9", 
               headerStyle = hs3)

var_sheet_name <- "interval outliers"
addWorksheet(wb, sheetName = var_sheet_name)
setColWidths(wb = wb, sheet = var_sheet_name, cols = 1, widths = 37)
setColWidths(wb = wb, sheet = var_sheet_name, cols = 2, widths = 50)
setColWidths(wb = wb, sheet = var_sheet_name, cols = 3, widths = 30)
setColWidths(wb = wb, sheet = var_sheet_name, cols = 4, widths = 20)
writeDataTable(wb = wb, 
               sheet = var_sheet_name, 
               x = df_potential_audit_outliers, 
               startRow = 1, 
               startCol = 1, 
               tableStyle = "TableStyleLight9", 
               headerStyle = hs3)

var_sheet_name <- "interval outliers per survey"
addWorksheet(wb, sheetName = var_sheet_name)
setColWidths(wb = wb, sheet = var_sheet_name, cols = 1, widths = 37)
setColWidths(wb = wb, sheet = var_sheet_name, cols = 2, widths = 36)
writeDataTable(wb = wb, 
               sheet = var_sheet_name, 
               x = df_time_interval_outliers_per_survey, 
               startRow = 1, 
               startCol = 1, 
               tableStyle = "TableStyleLight9", 
               headerStyle = hs3)

var_sheet_name <- "interval outliers per enum"
addWorksheet(wb, sheetName = var_sheet_name)
setColWidths(wb = wb, sheet = var_sheet_name, cols = 1, widths = 18)
setColWidths(wb = wb, sheet = var_sheet_name, cols = 2, widths = 40)
writeDataTable(wb = wb, 
               sheet = var_sheet_name, 
               x = df_time_interval_outliers_per_enumerator, 
               startRow = 1, 
               startCol = 1, 
               tableStyle = "TableStyleLight9", 
               headerStyle = hs3)

saveWorkbook(wb, paste0("outputs/", butteR::date_file_prefix(),"_formatted_audit_summary_land_and_energy.xlsx"), overwrite = TRUE)
openXL(file = paste0("outputs/", butteR::date_file_prefix(),"_formatted_audit_summary_land_and_energy.xlsx"))
