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


