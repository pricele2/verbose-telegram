# Additional practice with MATH and ELA datasets -------------------------------
# Membership files go from 1516 to 2324 

# Packages ----------------------------------------------------------------
library(tidyverse)
library(fs)
library(readxl)
library(janitor)

# Download Math Data (web ex) -------------------------------------------------
# https://www.oregon.gov/ode/educator-resources/assessment/Pages/Assessment-Group-Reports.aspx
# File URL at "School Level, Grades 3-8 & HS, Race/Ethnicity, Total Population (All Students)"
# Ideally I could do this as a nested function via purrr BUT there is too much variation in filenaming within the URLs 

# 2324
download.file("https://www.oregon.gov/ode/educator-resources/assessment/Documents/TestResults2324/pagr_schools_math_tot_raceethnicity_2324.xlsx",
              mode = "wb",
              destfile = "data-raw/math2324.xlsx")
math2324 <- read_excel(path = "data-raw/math2324.xlsx") |> clean_names()
col.names = colnames(math2324) # Assign vector

# 2223
download.file("https://www.oregon.gov/ode/educator-resources/assessment/Documents/TestResults2223/pagr_schools_math_tot_raceethnicity_2223.xlsx",
              mode = "wb",
              destfile = "data-raw/math2223.xlsx")
math2223 <- read_excel(path = "data-raw/math2223.xlsx") |> clean_names()
colnames(math2223) = col.names # Standardize colnames to 2324 

# 2122
download.file("https://www.oregon.gov/ode/educator-resources/assessment/Documents/TestResults2122/pagr_schools_math_tot_raceethnicity_2122.xlsx",
              mode = "wb",
              destfile = "data-raw/math2122.xlsx")
math2122 <- read_excel(path = "data-raw/math2122.xlsx") |> clean_names()
colnames(math2122) = col.names # Standardize colnames to 2324 

# No 1920 or 2021 files 

# 1819
download.file("https://www.oregon.gov/ode/educator-resources/assessment/Documents/TestResults2122/TestResults2019/pagr_schools_math_tot_raceethnicity_1819.xlsx",
              mode = "wb",
              destfile = "data-raw/math1819.xlsx")
math1819 = read_excel(path = "data-raw/math1819.xlsx") |> clean_names()
colnames(math1819) = col.names # Standardize colnames to 2324 

# 1718
download.file("https://www.oregon.gov/ode/educator-resources/assessment/TestResults2018/pagr_schools_math_raceethnicity_1718.xlsx",
              mode = "wb",
              destfile = "data-raw/math1718.xlsx")
math1718 = read_excel(path = "data-raw/math1718.xlsx") |> clean_names()
colnames(math1718) = col.names # Standardize colnames to 2324 

# 1617
download.file("https://www.oregon.gov/ode/educator-resources/assessment/TestResults2017/pagr_schools_math_raceethnicity_1617.xlsx",
              mode = "wb",
              destfile = "data-raw/math1617.xlsx")
math1617 = read_excel(path = "data-raw/math1617.xlsx") |> clean_names()
  colnames(math1617) = col.names # Standardize colnames to 2324 

# 1516
download.file("https://www.oregon.gov/ode/educator-resources/assessment/TestResults2016/pagr_schools_math_raceethnicity_1516.xlsx",
              mode = "wb",
              destfile = "data-raw/math1516.xlsx")
math1516 = read_excel(path = "data-raw/math1516.xlsx") |> clean_names() 
  colnames(math1516) = col.names # Standardize colnames to 2324 

# Rowbind all, and keep 3rd grade math ---------------------------
# DAMN there are inconsistent colnames over number_proficient and percent_proficient

math_allyears = bind_rows(math1516, math1617, math1718, math1819, math2122, math2223, math2324) |> rename(percent_participation = participation_rate)
  # length(unique(math_allyears$school)) #1337 vs 1308 for school_id
  # length(unique(math_allyears$district)) #198 vs 198 district_id
  
# NB from the na_if documentation: # If you have multiple problematic values that you'd like to replace with`NA`, then `replace_values()` is a better choice than `na_if()`

math_gr3 = math_allyears |> 
  mutate(across(
    starts_with(c("number_", "percent_")),
    # ~ replace_values(.x, from = c("-", "--", "*", "< 5.0%", "> 95.0%"), to = NA))) 
    ~ parse_number(.x, na = c("-", "--", "*", "< 5.0%", "> 95.0%")))) |> 
  filter(grade_level == "Grade 3") |> 
  mutate(year = 
           recode_values(academic_year, 
                         "2023-2024" ~ 2324, 
                         "2022-2023" ~ 2223,
                         "2021-2022" ~ 2122,
                         "2018-2019" ~ 1819,
                         "2017-2018" ~ 1718,
                         "2016-2017" ~ 1617,
                         "2015-2016" ~ 1516)) |> 
  relocate(year, .after = grade_level) |> 
  select(-c(academic_year))
colnames(math_gr3)

math3_tidy = pivot_longer(math_gr3, cols = 9:20, 
                          names_to = "element", values_to = "value", 
                          values_drop_na = TRUE)
