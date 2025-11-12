library(ipumsr)
library(tidyverse)
library(haven)

ddi <- read_ipums_ddi(r"{C:\R\workshop\hr1_ACS1yr_86var.xml}")
acs <- read_ipums_micro(ddi)

acs<- acs|> mutate(
  # First, recode all difficulties to 0 or 1
  DIFFPHYS = case_when(DIFFPHYS == 2 ~ 1, .default = 0),
  DIFFMOB = case_when(DIFFMOB == 2 ~ 1, .default = 0),
  DIFFCARE = case_when(DIFFCARE == 2 ~ 1, .default = 0), 
  DIFFEYE = case_when(DIFFEYE == 2 ~ 1, .default = 0),
  DIFFHEAR = case_when(DIFFHEAR == 2 ~ 1, .default = 0))|>
  
  mutate(
  # Count the number of ADLS for an individual
 #  adls = DIFFREM + DIFFPHYS + DIFFMOB + DIFFCARE + DIFFEYE + DIFFHEAR, # Should we include memory problems? 
    adls = DIFFPHYS + DIFFMOB + DIFFCARE + DIFFEYE + DIFFHEAR,
  
  # Count an individual as disabled if they have at least 1 ADL. 
  disabled = adls > 0)

acs_by_hh <- acs|>
  group_by(SERIAL)|>
  summarize(disabled_in_hh = sum(disabled))
  
acs <- left_join(acs, acs_by_hh, by= 'SERIAL')
  

acs<- acs|>
  mutate(
  # Identify individual who is exempt because they are an american indian
    
  exempt_american_indian = ifelse(RACAMIND==2, 1, 0),
    
  # Identify if respondent has a child under 13 in the home by using the age of youngest child variable,
  # which includes own children, step-children and adopted children. 
  exempt_parent_child_under13 = ifelse(YNGCH <= 13, 1, 0),

# Identify if respondent is a caretaker for an individual who has a disability based on if there is a 
# person with a disability in the home that is not themselves and they do not work. 
  num_disabled_in_hh_minus_self = ifelse(disabled, disabled_in_hh - 1, disabled),
  exempt_potential_caretaker_disabled = ifelse(num_disabled_in_hh_minus_self > 0 , 1, 0), 

# Identify individual who is exempt due to caretaker obligations.
  exempt_caretaker = case_when(exempt_parent_child_under13 ==1 | exempt_potential_caretaker_disabled == 1 ~1,
                               .default = 0),

# Identify individual who is exempt because of total disability rating (we are counting >.7 as total,
# so it's a bit too expansive, but it's the best we can do. )
  exempt_vet_totaldis = case_when(VETDISAB == 6 ~1, .default = 0),

# Identify individual who is exempt because they are disabled

  exempt_disabled = ifelse(disabled ==1, 1, 0),

# Identify individual who is exempt because they are receiving SNAP

  exempt_SNAP = ifelse(FOODSTMP == 2, 1, 0),

# Identify individual who is exempt because they are receiving postpartum coverage
  exempt_postpartum = ifelse(FERTYR == 2, 1, 0),

# Unemployment data


# Binary variable for any exemption: 
  exempt_any = case_when(exempt_parent_child_under13 == 1 | 
                         exempt_potential_caretaker_disabled == 1 |
                         exempt_vet_totaldis == 1 |
                         exempt_disabled == 1 |
                         exempt_SNAP == 1 |
                         exempt_postpartum == 1 ~ 1,
                         .default = 0
                           )
  )
# 
  
  


# Define potential population who is subject to work requirements, which includes anyone in the
# ACA expansion group (i.e., anyone who is between the ages of 19-64 and has income between 0 and 138% FPL.)
# We use this group as our "universe" of people who might need to comply with the law.
ohp_adultMAGI <-
  acs|>
  filter(
    (AGE > 18 & AGE < 65 & HIUHHSPOV <=138))
# 
# 
# 
# 
# 
# 
# Within the adultMAGI population, define key groups of people that we are interested in for work requirements:

# Summarize the weighted population in this group.
ohp_adultMAGI_exempt<- ohp_adultMAGI|>
  mutate(num_exempt_parent_child_under13 = exempt_parent_child_under13*PERWT, 
         num_exempt_potential_caretaker_disabled = exempt_potential_caretaker_disabled*PERWT,
         num_exempt_vet_totaldis = exempt_vet_totaldis*PERWT,
         num_exempt_disabled = exempt_disabled*PERWT,
         num_exempt_SNAP = exempt_SNAP*PERWT,
         num_exempt_postpartum = exempt_postpartum*PERWT,
         num_exempt_any = exempt_any*PERWT)|>
  summarize(across(where(is.numeric), ~sum(.x, na.rm = TRUE)))

ohp_adultMAGI |>
  select(PERWT)|>
  sum()

ohp_elig <- ohp_adultMAGI |>
  mutate(
    american_indian = case_when(
      RACAMIND == 2 ~ 1,
      .default = 0),
    w2 = if_else(CLASSWKRD %in% 22:28, 1, 0),
    self_employed = if_else(CLASSWKRD %in% 10:14, 1, 0),
    likely_gig = if_else(w2 == 0 & IND %in% c(6380, 6190, 8564, 8470), 1, 0),
    ui_receipt = if_else(EMPSTAT == 2, 1, 0),
    education_programs = if_else(SCHOOL == 2, 1, 0),
    income_compliant = case_when(
      (INCTOT / 12) > 0 ~ 1,
      .default = 0
      )
  ) 

ohp_elig <- ohp_elig |>
  mutate(exempt_or_compliant = ifelse(income_compliant==1 | exempt_any==1, 1, 0))

# Print totals
for (var in c("american_indian", "w2", "self_employed", "likely_gig", "ui_receipt", "education_programs", "income_compliant", "exempt_or_compliant")){
 print(var)
  total <- ohp_elig |>
    filter(.data[[var]] == 1) |>
    summarise(total = sum(PERWT, na.rm = TRUE)) |>
    pull(total)
  
  print(total)
}

# 
# ohp_enrl<- ohp_elig |>
#   filter(HINSCAID == 2)|>
#   mutate(PERWT = as.numeric(PERWT))
# 
# all_enrl <- data |>
#   filter(HINSCAID == 2)|>
#   mutate(PERWT = as.numeric(PERWT))
# 
# ohp_enrl|>select(PERWT)|>sum()
# all_enrl|>select(PERWT)|>sum()
# 
# 
# ohp_elig <- ohp_elig|>
#   mutate(
#     american_indian = case_when(
#       RACAMIND == 2 ~ 1,
#       .default = 0),
#     income_compliant = case_when((INCTOT / 12) > 0 ~1,
#     .default = 0),
#     income_compliant_due_to_work = case_when(income_compliant = 1 & )
#     edu_compliant = case_when())
# 
# 
# 
# ohp_elig|>filter(income_compliant==1)|>select(PERWT)|>sum()
# 
# ohp_elig|>select(PERWT)|>sum()



