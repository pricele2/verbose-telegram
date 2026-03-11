
# Advanced Data Visualization (15 modules) ----------------------------------

# lect 3 | Pipe data into ggplot ------------------------------------------

library(palmerpenguins)
data(package = 'palmerpenguins')
# penguins %>% 
#   count(species)

penguins |> # NB! NOT an assign! 
  filter(year == 2008) |> 
  count(island) |> 
  ggplot(aes(x = n,
             y = island)) + 
  geom_col()

# Best practices include: 
# Highlighting elements of interest in a different color
# De-cluttered! Grid lines are at major points, and not everywhere
# Annotated, with the conclusion spelled out in words
# Title gives a big-picture takeaway 
# Brand guidelines, for NYT and Financial Times specifically 

# GGPLOT requires tidy data: every row is an observation (long by X by Y)

## Load packages -----------------------------------------------------------

library(tidyverse)
library(fs)

## Create Dir safely -------------------------------------------------------

# try(dir_create("data"))
dir_exists("data")

## Recreate example from tutorial ------------------------------------------

### Download import clean and confirm tidy shape 
download.file("https://github.com/rfortherestofus/going-deeper-v2/raw/main/data/third_grade_math_proficiency.rds",
              mode = "wb",
              destfile = "data/third_grade_math_proficiency.rds")

third_grade_math_proficiency |> 
  read_rds("data/third_grade_math_proficiency.rds") |> 
  select(academic_year, school, school_id, district, 
         proficiency_level, number_of_students) |> 
  mutate(is_proficient = 
           case_when(
            proficiency_level >= 3 ~ TRUE,
            .default = FALSE)) |>
  group_by(academic_year, school, district, school_id, is_proficient) |> 
  summarize(number_of_students = 
              sum(number_of_students, na.rm = TRUE)) |> 
  ungroup() |>
  group_by(academic_year, school, district, school_id) |> 
  mutate(percent_proficient = number_of_students / 
           sum(number_of_students, na.rm = TRUE)) |> 
  ungroup() |> 
  mutate(percent_proficient = 
           case_when(
            is.nan(percent_proficient) ~ NA,
            .default = percent_proficient)) |> 
  filter(is_proficient == TRUE) |> 
  select(academic_year, school, district, percent_proficient) |> 
  rename(year = academic_year) 
#dim now 1523 by 4 cols 

### And plot 
third_grade_math_proficiency |> 
  filter(year == "2018-2019") |> 
  filter(district == "Portland SD 1J") |> 
  ggplot(aes(x = percent_proficient, 
             y = school)) +
  geom_col()

# my turn 1 ------------------------------------
# Download the enrollment data by race/ethnicity and create a data frame called enrollment_by_race_ethnicity using the starter code below.
# Pipe your data into a bar chart that shows the breakdown of race/ethnicity among students in Beaverton SD 48J in 2022-2023.

## Load Pkgs ----
library(dplyr)
library(readr)
library(ggplot2)
library(forcats)
library(tidyr)
library(fs)

## Download from github and import/clean ----
# download.file("https://github.com/rfortherestofus/going-deeper-v2/raw/main/data/enrollment_by_race_ethnicity.rds",
#               mode = "wb",
#               destfile = "data/enrollment_by_race_ethnicity.rds")

enrollment_by_race_ethnicity = 
  read_rds("data/enrollment_by_race_ethnicity.rds") |> 
  select(-district_institution_id)  |> 
  select(year, district, everything()) |> 
  mutate(year = case_when(
    year == "School 2021-22" ~ "2021-2022",
    year == "School 2022-23" ~ "2022-2023")) |> # nb - template had an extra comma
  filter(year == "2022-2023") |> 
  filter(district == "Beaverton SD 48J") |> 
  ggplot(aes(x = pct, 
             y = race_ethnicity)) +
  geom_col()
## NB this would be pretty easy to loop over for different values of DISTRICT 

# L4 Reorder plots to highlight findings -----------------------------------
# in PENGUINS, with y = reorder(island, n)) *or* invoke fct_reorder
library(palmerpenguins)

penguins |> 
  filter(year == 2008) |> 
  count(island) |> 
  ggplot(aes(x = n,
             y = reorder(island, n))) + 
  geom_col()

penguins |> 
  count(island) |> 
  mutate(island = fct_reorder(island, n)) |> 
  ggplot(aes(x = n, 
             y = island)) +
  geom_col()

## using third_math from above 
third_grade_math_proficiency |> 
  filter(year == "2021-2022") |> 
  filter(district == "Portland SD 1J") |> 
  ggplot(aes(x = percent_proficient, 
             y = reorder(school, percent_proficient))) +
  geom_col()

## my turn 2 --------------------------------------------------------------

enrollment_by_race_ethnicity =  
  read_rds("data/enrollment_by_race_ethnicity.rds") 

## First way, with y = reorder()
enrollment_by_race_ethnicity |> 
  select(-district_institution_id)  |> 
  select(year, district, everything()) |> 
  mutate(year = case_when(
    year == "School 2021-22" ~ "2021-2022",
    year == "School 2022-23" ~ "2022-2023")) |> 
  filter(year == "2022-2023") |> 
  filter(district == "Beaverton SD 48J") |> 
  ggplot(aes(x = pct, 
             y = reorder(race_ethnicity, pct))) +
  geom_col()

## second option with fct_reorder from forcats
# turns out it matters where you put mutate, within the filtering!

enrollment_by_race_ethnicity |> 
  select(-district_institution_id)  |> 
  select(year, district, everything()) |> 
  mutate(year = case_when(
    year == "School 2021-22" ~ "2021-2022",
    year == "School 2022-23" ~ "2022-2023")) |> 
  filter(year == "2022-2023") |> 
  filter(district == "Beaverton SD 48J") |> 
  mutate(race_ethnicity = fct_reorder(race_ethnicity, pct)) |>  
  ggplot(aes(x = pct, 
             y = race_ethnicity)) +
  geom_col()

# Line charts & group aes -------------------------------------------------
## in Penguins 
penguins |> 
  count(year, island) |> 
  ggplot(aes(x = year,
             y = n,
             group = island)) +
  geom_line()

## for 3rd grade math, chaotic lol 
third_grade_math_proficiency |>
  filter(district == "Portland SD 1J") |>
  ggplot(aes(x = year,
             y = percent_proficient,
             group = school)) +
  geom_line()

## my turn 3 ------------------------------------------------------------

# Task: Make a line chart that shows the change in the Hispanic/Latino population in school districts from 2021-2022 to 2022-2023.
glimpse(enrollment_by_race_ethnicity)

enrollment_by_race_ethnicity |> 
  filter(race_ethnicity == "Hispanic/Latino") |> 
  mutate(year = case_when(
    year == "School 2021-22" ~ "2021-2022",
    year == "School 2022-23" ~ "2022-2023")) |> 
  ggplot(aes(x = year,
             y = pct,
             group = district)) + 
  geom_line()

# NB - geom_area() is also an option
# also, slopegraphs made via ggbump package 
# https://github.com/davidsjoberg/ggbump

