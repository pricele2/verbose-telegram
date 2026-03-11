# Module 1: Downloading & Importing from URL 
# https://rfortherestofus.com/courses/going-deeper/lessons/downloading-and-importing-data
# command-shift-R shortcut ------------------------------------------------

# Load Packages -----------------------------------------------------------

library(tidyverse)
library(fs)
library(readxl)
# install.packages("janitor")
library(janitor)

# Create Directories ------------------------------------------------------
# dir_create("data-raw") # from the FS package 

# Download Math Data (web ex) -------------------------------------------------
# https://www.oregon.gov/ode/educator-resources/assessment/Pages/Assessment-Group-Reports.aspx
# NB: no files for 1920 or 2021 due to Covid; 2324 and 2425 are online but not in the content
# screenshot for the published report itself is at 6:48 of video

download.file("https://www.oregon.gov/ode/educator-resources/assessment/Documents/TestResults2324/pagr_schools_math_tot_raceethnicity_2324.xlsx",
              mode = "wb",
              destfile = "data-raw/pagr_schools_math_tot_raceethnicity_2324.xlsx")

download.file("https://www.oregon.gov/ode/educator-resources/assessment/Documents/TestResults2223/pagr_schools_math_tot_raceethnicity_2223.xlsx",
              mode = "wb",
              destfile = "data-raw/pagr_schools_math_tot_raceethnicity_2223.xlsx")

download.file("https://www.oregon.gov/ode/educator-resources/assessment/Documents/TestResults2122/pagr_schools_math_tot_raceethnicity_2122.xlsx",
              mode = "wb",
              destfile = "data-raw/pagr_schools_math_tot_raceethnicity_2122.xlsx")

download.file("https://www.oregon.gov/ode/educator-resources/assessment/Documents/TestResults2122/TestResults2019/pagr_schools_math_tot_raceethnicity_1819.xlsx",
              mode = "wb",
              destfile = "data-raw/pagr_schools_math_tot_raceethnicity_1819.xlsx")

download.file("https://www.oregon.gov/ode/educator-resources/assessment/TestResults2018/pagr_schools_math_raceethnicity_1718.xlsx",
              mode = "wb",
              destfile = "data-raw/pagr_schools_math_raceethnicity_1718.xlsx")

download.file("https://www.oregon.gov/ode/educator-resources/assessment/TestResults2017/pagr_schools_math_raceethnicity_1617.xlsx",
              mode = "wb",
              destfile = "data-raw/pagr_schools_math_raceethnicity_1617.xlsx")

download.file("https://www.oregon.gov/ode/educator-resources/assessment/TestResults2016/pagr_schools_math_raceethnicity_1516.xlsx",
              mode = "wb",
              destfile = "data-raw/pagr_schools_math_raceethnicity_1516.xlsx")

# Import Math from data-raw ----------------------------------------------------
math_2324 <-
  read_excel(path = "data-raw/pagr_schools_math_tot_raceethnicity_2324.xlsx") |> 
  clean_names()

math_2223 <-
  read_excel(path = "data-raw/pagr_schools_math_tot_raceethnicity_2223.xlsx") |> 
  clean_names()

math_2122 <-
  read_excel(path = "data-raw/pagr_schools_math_tot_raceethnicity_2122.xlsx") |> 
  clean_names()

math_1819 <-
  read_excel(path = "data-raw/pagr_schools_math_tot_raceethnicity_1819.xlsx") |> 
  clean_names()

math_1718 <- # note path namechange
  read_excel(path = "data-raw/pagr_schools_math_raceethnicity_1718.xlsx") |> 
  clean_names()

math_1617 <- # note path namechange
  read_excel(path = "data-raw/pagr_schools_math_raceethnicity_1617.xlsx") |> 
  clean_names()

math_1516 <- # note path namechange
  read_excel(path = "data-raw/pagr_schools_math_raceethnicity_1516.xlsx") |> 
  clean_names()

# VERBOSE Import Fall Membership from URL -------------------------------------
# Needs the INDEX 
download.file("https://www.oregon.gov/ode/reports-and-data/students/Documents/fallmembershipreport_20232024.xlsx",
              mode = "wb",
              destfile = "data-raw/membership_2324.xlsx")

download.file("https://www.oregon.gov/ode/reports-and-data/students/Documents/fallmembershipreport_20222023.xlsx",
              mode = "wb",
              destfile = "data-raw/membership_2223.xlsx")

download.file("https://www.oregon.gov/ode/reports-and-data/students/Documents/fallmembershipreport_20212022.xlsx",
              mode = "wb",
              destfile = "data-raw/membership_2122.xlsx")

#MEMBERSHIP does exist for 1920 and 2021
download.file("https://www.oregon.gov/ode/reports-and-data/students/Documents/fallmembershipreport_20202021.xlsx",
              mode = "wb",
              destfile = "data-raw/membership_2021.xlsx")

download.file("https://www.oregon.gov/ode/reports-and-data/students/Documents/fallmembershipreport_20192020.xlsx",
              mode = "wb",
              destfile = "data-raw/membership_1920.xlsx")

download.file("https://www.oregon.gov/ode/reports-and-data/students/Documents/fallmembershipreport_20182019.xlsx",
              mode = "wb",
              destfile = "data-raw/membership_1819.xlsx")

download.file("https://www.oregon.gov/ode/reports-and-data/students/Documents/fallmembershipreport_20172018.xlsx",
              mode = "wb",
              destfile = "data-raw/membership_1718.xlsx")

download.file("https://www.oregon.gov/ode/reports-and-data/students/Documents/fallmembershipreport_20162017.xlsx",
              mode = "wb",
              destfile = "data-raw/membership_1617.xlsx")

download.file("https://www.oregon.gov/ode/reports-and-data/students/Documents/fallmembershipreport_20152016.xlsx",
              mode = "wb",
              destfile = "data-raw/membership_1516.xlsx")

# Import Membership verbose -----------------------------------------------

enroll_2223 = 
  read_excel(path = "data-raw/membership_2223.xlsx", sheet = "School 2022-23") |> 
  clean_names()

enroll_2122 = 
  read_excel(path = "data-raw/membership_2122.xlsx", sheet = "School 2021-22") |> 
  clean_names()

# Tidy Data 101 -----------------------------------------------------------
# Lots of very familiar ish and the 'three rules'
# Looking at Math_2122 -- violates these principles for sure -- it's wide by var 
# Same with enroll_* files, wildly wide and just long by district wide by var

# lect 3 ------------------------------------------------------------------
## Tidy Rule 1 - Every column is a variable --------------------------------
# using pivot_longer() from tidyr
# excellent cheat: pivot_longer(cols = -state) # name them via negativa

## Third grade math in 2122
math_gr3_2122 = math_2122 |> 
  filter(student_group == "Total Population (All Students)") |> 
  filter(grade_level == "Grade 3") |> 
  select(academic_year, school_id, contains("number_level_")) |>  # 762 obs by 6
  pivot_longer(cols = contains("number_"),
               names_to = "proficiency_level",
               values_to = "number_of_students") # 3048 by 4 vars
  
# check with 
glimpse(math_gr3_2122)

## exercise 3 --------------------------------------------------------------
ex_3 = enroll_2223 |> 
  select(!c(contains("percent"), contains("grade"), contains("total"), contains("name"))) |> 
  pivot_longer(cols = contains("x2022_"), 
               names_to = "race_ethnicity",
               values_to = "number_of_students") |> 
  filter(race_ethnicity != "kindergarten")

# if I were doing this myself I'd:
# add var YEAR = 2223 and regex out the 'x2022_23_' prefixes 
# convert number_of to type = double
# also it looks like he kept TOTAL so I may need to add that back in [he used : indexing]

# lect 4 - every cell is a single value ---------------------------
# Use mutate(newvar = recode(varname, old = new))
# or mutate(newvar = case_when() 
# or mutate(year = parse_number(year)) which will do both name and typeof

## example 4 ----
math_ex4 = math_gr3_2122 |> 
  mutate(prof_level = parse_number(proficiency_level)) # no quotes within function

## assignment 4 ----
# clean up ex_3 using recode, if_else, or case_when

ex_4 = ex_3 |> 
  mutate(temp = str_remove(race_ethnicity, "x2022_23_")) |> 
  select(!race_ethnicity) |> 
  mutate(race_ethnicity = 
    case_when(
      temp == "american_indian_alaska_native" ~ "AIAN",
      temp == "asian" ~ "ASIA",
      temp == "black_african_american" ~ "AFAM",
      temp == "hispanic_latino" ~ "HISP",
      temp == "multi_racial" ~ "MLTI",
      temp == "native_hawaiian_pacific_islander" ~ "NHPI",             
      temp == "white" ~ "WHIT")) |> 
  select(!temp)

# lect 5: tidy rule 2: every row is an observation --------------------------------

library(tidyverse)

gss_cat |> view()
# write code to count the number of unique responses in the partyid variable
#use the separate_longer_delim() and count() functions

gss_1 = gss_cat |> select(year, partyid)  # this part is probably unnecessary
gss_2 = separate_longer_delim(gss_1, partyid, delim = ",")
length(unique(gss_2$partyid)) # 11 including NA and don't know 

# lect 6 - change var types -----------------------------------------------
# use typeof(x$y)

# Ex 6: Convert the number_of_students variable to numeric by using as.numeric() and parse_number().

ex_6 = ex_4 |> 
  mutate(number_of_students = as.numeric(number_of_students)) |>  # 264 NAs
  summarize(total_students = sum(number_of_students, na.rm = TRUE))

# lect 7 - deal with missing data  ----------------------------------------
# read_excel incldues option for na = c("*", "-") 
# can also use na_if() from... dplyr
# going other way -- replace_na("*", "0") and then convert to numeric

ex_7 = ex_4 |> 
  mutate(number_of_students = na_if(number_of_students, "-")) |> 
  mutate(number_of_students = replace_na(number_of_students, "0"))
# GOTTA wrap it in mutate!! 

# lect 8 group_by mutate() ---------------------------------------------
# group_by(year) |>  mutate(pct = number / sum(number, na.rm = TRUE))
# dont forget to ungroup ! otherwise slice_max(order_by = pct, n= 1 ) acts up

## ex 8 calc % studetns at each school at each prof level of math ----
colnames(math_gr3_2122)
glimpse(math_ex4)

math_lec8 = math_ex4 |> as_tibble() |> 
  mutate(n_students = as.numeric(number_of_students)) |> 
  select(-c(proficiency_level, number_of_students)) |> 
  group_by(school_id) |> 
  mutate(pct = n_students / sum(n_students, na.rm = TRUE)) |> 
  ungroup()

## exercise 8 calc % create %var for each race/eth as % of district enrollmnt
glimpse(ex_7)

ex_8 = ex_7 |> as_tibble() |> 
  mutate(number_of_students = as.numeric(number_of_students)) |> 
  group_by(district_institution_id, race_ethnicity) |> 
  summarize(n_students = sum(number_of_students, na.rm = TRUE)) |> 
  ungroup() |> 
  group_by(district_institution_id) |> 
  mutate(pct = n_students / sum(n_students)) |> 
  ungroup()

# lect 9 binding data frames -----------------------------------


## Load Packages -----------------------------------------------------------

library(tidyverse)
library(fs)
library(readxl)
library(janitor)

## Create Directories ------------------------------------------------------

# dir_create("data-raw")

## Download Data -----------------------------------------------------------

# https://www.oregon.gov/ode/reports-and-data/students/Pages/Student-Enrollment-Reports.aspx

# download.file("https://www.oregon.gov/ode/reports-and-data/students/Documents/fallmembershipreport_20222023.xlsx",
#               mode = "wb",
#               destfile = "data-raw/fallmembershipreport_20222023.xlsx")
# 
# download.file("https://www.oregon.gov/ode/reports-and-data/students/Documents/fallmembershipreport_20212022.xlsx",
#               mode = "wb",
#               destfile = "data-raw/fallmembershipreport_20212022.xlsx")
# 
# download.file("https://www.oregon.gov/ode/reports-and-data/students/Documents/fallmembershipreport_20202021.xlsx",
#               mode = "wb",
#               destfile = "data-raw/fallmembershipreport_20202021.xlsx")
# 
# download.file("https://www.oregon.gov/ode/reports-and-data/students/Documents/fallmembershipreport_20192020.xlsx",
#               mode = "wb",
#               destfile = "data-raw/fallmembershipreport_20192020.xlsx")
# 
# download.file("https://www.oregon.gov/ode/reports-and-data/students/Documents/fallmembershipreport_20182019.xlsx",
#               mode = "wb",
#               destfile = "data-raw/fallmembershipreport_20182019.xlsx")

## Import Data -------------------------------------------------------------

enrollment_2022_2023 <- read_excel(path = "data-raw/membership_2223.xlsx", sheet = "School 2022-23") |> 
  clean_names() 
colnames(enrollment_2022_2023)

enrollment_2021_2022 <- read_excel(path = "data-raw/membership_2122.xlsx",
                                   sheet = "School 2021-22") |> 
  clean_names()
colnames(enrollment_2021_2022)

## Tidy and Clean Data -----------------------------------------------------

enrollment_by_race_ethnicity_2022_2023 <-
  enrollment_2022_2023 |> 
  select(district_institution_id, school_institution_id,
         x2022_23_american_indian_alaska_native:x2022_23_multi_racial) |> 
  select(-contains("percent")) |> 
  pivot_longer(cols = -c(district_institution_id, school_institution_id),
               names_to = "race_ethnicity",
               values_to = "number_of_students") |> 
  mutate(race_ethnicity = str_remove(race_ethnicity, pattern = "x2022_23_")) |> 
  mutate(race_ethnicity = case_when(
    race_ethnicity == "american_indian_alaska_native" ~ "AIAN",
    race_ethnicity == "asian" ~ "Asian",
    race_ethnicity == "black_african_american" ~ "AFAM",
    race_ethnicity == "hispanic_latino" ~ "HISP/LTX",
    race_ethnicity == "multiracial" ~ "MULTI",
    race_ethnicity == "native_hawaiian_pacific_islander" ~ "NHPI",
    race_ethnicity == "white" ~ "WHIT",
    race_ethnicity == "multi_racial" ~ "MULTI"
  )) |> 
  mutate(number_of_students = parse_number(number_of_students, na = c("-"))) |> 
  group_by(district_institution_id, race_ethnicity) |> 
  summarize(number_of_students = sum(number_of_students, na.rm = TRUE)) |> 
  ungroup() |> 
  group_by(district_institution_id) |> 
  mutate(pct = number_of_students / sum(number_of_students)) |> 
  ungroup() |> 
  mutate(year = "22-23")

enrollment_by_race_ethnicity_2021_2022 <-
  enrollment_2021_2022 |> 
  select(attending_district_institution_id, attending_school_institution_id,
         x2021_22_american_indian_alaska_native:x2021_22_multi_racial) |> 
  rename(district_institution_id = attending_district_institution_id,
         school_institution_id = attending_school_institution_id) |> 
  select(-contains("percent")) |> 
  pivot_longer(cols = -c(district_institution_id, school_institution_id),
               names_to = "race_ethnicity",
               values_to = "number_of_students") |> 
  mutate(race_ethnicity = str_remove(race_ethnicity, pattern = "x2021_22_")) |> 
    mutate(race_ethnicity = case_when(
      race_ethnicity == "american_indian_alaska_native" ~ "AIAN",
      race_ethnicity == "asian" ~ "Asian",
      race_ethnicity == "black_african_american" ~ "AFAM",
      race_ethnicity == "hispanic_latino" ~ "HISP/LTX",
      race_ethnicity == "multiracial" ~ "MULTI",
      race_ethnicity == "native_hawaiian_pacific_islander" ~ "NHPI",
      race_ethnicity == "white" ~ "WHIT",
      race_ethnicity == "multi_racial" ~ "MULTI"
    )) |>
  mutate(number_of_students = parse_number(number_of_students, na = c("-"))) |> 
  group_by(district_institution_id, race_ethnicity) |> 
  summarize(number_of_students = sum(number_of_students, na.rm = TRUE)) |> 
  ungroup() |> 
  group_by(district_institution_id) |> 
  mutate(pct = number_of_students / sum(number_of_students)) |> 
  ungroup() |> 
  mutate(year = "21-22")

enrollment_by_race_ethnicity <-
  bind_rows(enrollment_by_race_ethnicity_2021_2022,
            enrollment_by_race_ethnicity_2022_2023)

# lect 10 FUNCTIONS -------------------------------------------------------
# Note: Functions have to be told explicitly to return the output and create it as an object # set_names() may also be super helpful here

# Load Packages -----------------------------------------------------------

library(tidyverse)
library(fs)
library(readxl)
library(janitor)

# Create Raw and Download XLSXs -------------------------------------------
dir_create("data-raw")

## Download Data -----------------------------------------------------------

# https://www.oregon.gov/ode/reports-and-data/students/Pages/Student-Enrollment-Reports.aspx

# fn() to Import, Reshape, and Clean Data --------------------------------------
# test values
# path = "data-raw/membership_2223.xlsx", sheet = "School 2022-23", prefix = "x2021_22_"
# don't trip yourself up on double quotes -- leave the quotes out of the function

clean_enrollment_file <- function(excel_file, sheet_name, prefix) 
  {
read_excel(path = excel_file, sheet = sheet_name) |> 
  clean_names() |> 
select(!c(contains("percent"), contains("grade"), contains("total"), contains("name"), contains("kinder"))) |> 
   rename(district_id = 1, school_id = 2) |> 
  pivot_longer(cols = -c(district_id, school_id),
               names_to = "race_ethnicity",
               values_to = "number_of_students") |> 
  mutate(race_ethnicity = str_remove(race_ethnicity, pattern = prefix)) |>
    mutate(race_ethnicity = case_when(
      race_ethnicity == "american_indian_alaska_native" ~ "AIAN",
      race_ethnicity == "asian" ~ "Asian",
      race_ethnicity == "black_african_american" ~ "AFAM",
      race_ethnicity == "hispanic_latino" ~ "HISP/LTX",
      race_ethnicity == "multiracial" ~ "MULTI",
      race_ethnicity == "native_hawaiian_pacific_islander" ~ "NHPI",
      race_ethnicity == "white" ~ "WHIT",
      race_ethnicity == "multi_racial" ~ "MULTI"
    )) |>
  mutate(number_of_students = parse_number(number_of_students, na = c("-"))) |> 
    group_by(district_id, race_ethnicity) |> 
    summarize(number_of_students = sum(number_of_students, na.rm = TRUE)) |> 
    ungroup() |> 
    group_by(district_id) |> 
    mutate(pct = number_of_students / sum(number_of_students)) |> 
    ungroup() |> 
    mutate(year = sheet_name) |> 
    mutate(year = str_remove(year, "School "))
  }

enr_byrace_2122 = 
  clean_enrollment_file(excel_file = "data-raw/membership_2122.xlsx",
                            sheet_name = "School 2021-22",
                            prefix = "x2021_22_")

enr_byrace_2223 = 
  clean_enrollment_file(excel_file = "data-raw/membership_2223.xlsx",
                        sheet_name = "School 2022-23",
                        prefix = "x2022_23_")

enr_twoyears = bind_rows(enr_byrace_2122, enr_byrace_2223)

# lect 11 Joins!  -----------------------------------------------------

# good ol dplyr again: left, right, anti, semi, inner, full 

download.file("https://github.com/rfortherestofus/going-deeper-v2/raw/main/data-raw/oregon-districts.xlsx",
              mode = "wb",
              destfile = "data-raw/oregon-districts.xlsx")

oregon_districts = 
  read_excel(path = "data-raw/oregon-districts.xlsx", sheet = 1) |> 
  clean_names() |> 
  rename(district_id = 1, district_name = 2)

dist_enr_race_twoyrs = left_join(enr_twoyears, oregon_districts, by = "district_id")

# lect 12 Export to CSV and RDS ---------------------------------------------------

dir_create("data")

write_csv(dist_enr_race_twoyrs, 
          file = "data/enroll_bydist_byrace_2years.csv")

write_rds(dist_enr_race_twoyrs, 
          file = "data/enroll_bydist_byrace_2years.rds")

# lect 13 adv wrangling ---------------------------------------------------

# does a project top to bottom 
# best practice guidance: do your cleaning and wrangling in an R script and *then* 
# export the RDS into the directory for quarto to pick it up 


# Work through additional years -------------------------------------------

## 1516 
# Deleted M/F and cols 2 & 4 (names), replaced '-   ' 0 with '- ; renamed tab
enr_byrace_1516 = 
  clean_enrollment_file(excel_file = "data-raw/membership_1516.xlsx",
                        sheet_name = "School 2015-16",
                        prefix = "x2015_16_")

## 1617 
enr_byrace_1617 = 
  clean_enrollment_file(excel_file = "data-raw/membership_1617.xlsx",
                        sheet_name = "School 2016-17",
                        prefix = "x2016_17_")
  
## 1718 
enr_byrace_1718 = 
  clean_enrollment_file(excel_file = "data-raw/membership_1718.xlsx",
                        sheet_name = "School 2017-18",
                        prefix = "x2017_18_")

## 1819
enr_byrace_1819 = 
  clean_enrollment_file(excel_file = "data-raw/membership_1819.xlsx",
                        sheet_name = "School 2018-19",
                        prefix = "x2018_19_")

## 1920
enr_byrace_1920 = 
  clean_enrollment_file(excel_file = "data-raw/membership_1920.xlsx",
                        sheet_name = "School 2019-20",
                        prefix = "x2019_20_")

## 2021
enr_byrace_2021 = 
  clean_enrollment_file(excel_file = "data-raw/membership_2021.xlsx",
                        sheet_name = "School 2020-21",
                        prefix = "x2020_21_")
## 2122
enr_byrace_2122 = 
  clean_enrollment_file(excel_file = "data-raw/membership_2122.xlsx",
                        sheet_name = "School 2021-22",
                        prefix = "x2021_22_")

## 2223
enr_byrace_2223 = 
  clean_enrollment_file(excel_file = "data-raw/membership_2223.xlsx",
                        sheet_name = "School 2022-23",
                        prefix = "x2022_23_")

## 2324
enr_byrace_2324 = 
  clean_enrollment_file(excel_file = "data-raw/membership_2324.xlsx",
                        sheet_name = "School 2023-24",
                        prefix = "x2023_24_")

## Combine ----
enr_allyears = bind_rows(enr_byrace_1516, enr_byrace_1617, enr_byrace_1718, enr_byrace_1819, enr_byrace_1920, enr_byrace_2021, enr_byrace_2122, enr_byrace_2223, enr_byrace_2324)

dist_enr_all = left_join(enr_allyears, oregon_districts, by = "district_id")

write_csv(dist_enr_all, 
          file = "data/enroll_bydist_byrace_allyears.csv")

write_rds(dist_enr_all, 
          file = "data/enroll_bydist_byrace_allyears.rds")

