library(tidyverse)
library(readxl)
library(fs)
library(sf)

dir_create("app/clean_data")

path_1 <- "raw_data/2007-2018-PIT-Counts-by-State.xlsx"
path_2 <- "raw_data/2007-2018-HIC-Counts-by-State.xlsx"

data_1 <- path_1 %>%
  excel_sheets() %>%
  set_names() %>%
  map_df(read_excel, path = path_1, .id = "year")

data_2 <- path_2 %>%
  excel_sheets() %>%
  set_names() %>%
  map_df(read_excel, path = path_2, .id = "year") 

write_rds(x = data_1, path = "app/clean_data/data_1.rds")
write_rds(x = data_2, path = "app/clean_data/data_2.rds")
