library("tidyverse")
library("readxl")

info <- read_excel("data-raw/FIdata.xlsx",
                   range = cell_cols("A:C")) # import all records in columns A to C

write_csv(info, path = "data/HNdata.csv")

