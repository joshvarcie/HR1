library(ipumsr)
library(tidyverse)
library(haven)

atus_cps <- read_dta('/Users/juliaheinzel/Documents/Back-up/Princeton/Fall 2025/591E/Report/Data/atuscps_2024.dta')
asec <- read.csv('/Users/juliaheinzel/Documents/Back-up/Princeton/Fall 2025/591E/Report/Data/hhpub25.csv')
atus_resp <- read_dta('/Users/juliaheinzel/Documents/Back-up/Princeton/Fall 2025/591E/Report/Data/atusresp_2024.dta')
atus_act <- read_dta('/Users/juliaheinzel/Documents/Back-up/Princeton/Fall 2025/591E/Report/Data/atusact_2024.dta')
atus_sum <- read_dta('/Users/juliaheinzel/Documents/Back-up/Princeton/Fall 2025/591E/Report/Data/atussum_2024.dta')
atus_rost <- read_dta('/Users/juliaheinzel/Documents/Back-up/Princeton/Fall 2025/591E/Report/Data/atusrost_2024.dta')
#ddi <- read_ipums_ddi('/Users/juliaheinzel/Documents/Back-up/Princeton/Fall 2025/591E/Report/Data/atus_00002.xml')
#atus_cps <- read_ipums_micro(ddi)

# Merge files
atus <- atus_resp |> 
  full_join(atus_sum, by = "tucaseid") |> 
  full_join(atus_rost, by = c("tucaseid", "tulineno")) |> 
  full_join(atus_cps, by = c("tucaseid", "tulineno")) 

atus_cps_OR <- atus |> 
  mutate(hh_size = case_when(
    trsppres.x == 3 & trchildnum.x == 0 ~ 1,
    trsppres.x != 3 & trchildnum.x == 0 ~ 2,
    trsppres.x != 3 & trchildnum.x != 0 ~ 2 + trchildnum.x,
    .default = 1
  ),
  max_poverty = case_when(
    hh_size == 1 & hefaminc <= 6 ~ 1,
    hh_size == 2 & hefaminc <= 7 ~ 1,
    hh_size == 3 & hefaminc <= 8 ~ 1,
    hh_size == 4 & hefaminc <= 9 ~ 1,
    hh_size == 5 & hefaminc <= 10 ~ 1,
    hh_size == 6 & hefaminc <= 11 ~ 1,
    hh_size == 7 & hefaminc <= 11 ~ 1,
    hh_size == 8 & hefaminc <= 12 ~ 1,
    .default = 0
  ),
  min_poverty = case_when(
    hh_size == 1 & hefaminc < 6 ~ 1,
    hh_size == 2 & hefaminc < 7 ~ 1,
    hh_size == 3 & hefaminc < 8 ~ 1,
    hh_size == 4 & hefaminc < 9 ~ 1,
    hh_size == 5 & hefaminc < 10 ~ 1,
    hh_size == 6 & hefaminc < 11 ~ 1,
    hh_size == 7 & hefaminc < 11 ~ 1,
    hh_size == 8 & hefaminc < 12 ~ 1,
    .default = 0
  ),
  monthly_hours_volunteering = 30*(rowSums(across(starts_with("t15")), na.rm = TRUE))/60,
  monthly_hours_program = 30*(sum(c_across(cols = t060301:t060302), na.rm = TRUE))/60,
  monthly_hours_working = 30*(sum(c_across(cols = t060101:t060103), na.rm = TRUE))/60)

# Program
atus_cps_OR |> 
  filter(gestfips == 41 & teage.x > 18 & teage.x < 65 & (max_poverty == 1 | min_poverty == 1)) |> 
  filter(!is.na(monthly_hours_program)) |> 
  filter(monthly_hours_program >= 80) |> 
  group_by(max_poverty, min_poverty) |> 
  summarize(num = sum(tufinlwgt.x))

# Volunteering
atus_cps_OR |> 
  filter(gestfips == 41 & teage.x > 18 & teage.x < 65 & (max_poverty == 1 | min_poverty == 1)) |> 
  filter(!is.na(monthly_hours_program)) |> 
  filter(monthly_hours_volunteering >= 80) |> 
  group_by(max_poverty, min_poverty) |> 
  summarize(num = sum(tufinlwgt.x))
