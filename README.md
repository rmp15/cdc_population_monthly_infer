# Generate monthly population by sex, 5- and 10-year age group, race, ethnicity, county in the United States 1982-2020

Work in progress by Robbie M Parks

Currently monthly population (vintage 2020) during 1982 - 2020 for females and males by five-year (0-4, 5-9,.....,85+ years) and ten-year age group (0-4,5-14,15-24,...,65-74,75-84,85+ years) for all counties (by FIPS code) in US except Alaska (working on it).

## Data for use by year and sex

[5-year age groups](output/population_processed/5_year_age_groups)

[10-year age groups](output/population_processed/10_year_age_groups)

[Race groups](output/population_processed/race_groups)

[Ethnicity groups](output/population_processed/ethnicity_groups)

## Important details

a_03 contains a look-up of FIPS Codes which have been changed over time to be consistent over time.

## Instructions on downloading raw origin files

**2021-2022**

Data for Vintage 2022 downloaded from CDC Wonder at <https://wonder.cdc.gov/single-race-population.html>

1.  By sex and age group: Download for each year as .txt file in two sexes (F, M) by sex, age group, state, county
2.  By race/ethnicity: Download for each year as .txt file in two sexes (F, M) by sex, race/ethnicity, state, county

**1990-2020**

**Note: NCHS ceased releasing bridged-race population estimates after release of the bridged-race intercensal population estimates for July 1, 2010 - July 2019. With the release of the intercensal population estimates, the series will be discontinued.**

Data for Vintage 2020 downloaded from CDC Wonder at <https://wonder.cdc.gov/bridged-race-population.html>

1.  By sex and age group: Download for each year as .txt file in two sexes (F, M) by sex, age group, state, county
2.  By race/ethnicity: Download for each year as .txt file in two sexes (F, M) by sex, race/ethnicity, state, county

Data for pre-1990 downloaded from census.gov at <https://www.census.gov/data/tables/time-series/demo/popest/1980s-county.html>

## Data preparation (data_prep) list

a_01_prepare_raw_bridged_race_population_data - initial processing to get from downloaded raw files to something that can be loaded

a_01_prepare_raw_single_race_population_data - initial processing to get from downloaded raw files to something that can be loaded

a_02_process_and_match_population_data - process to match format of older years (pre-1990 from US Census Bureau)

a_03_fix_county_codes - explore and fix counties which change codes to unify over time

a_04_fix_county_codes_race - explore and fix counties which change codes to unify over time for race groups

a_05_fix_county_codes_ethnicity - explore and fix counties which change codes to unify over time for ethnicity groups

a_06_infer_monthly_population_5_year_groups - infer population by month from annual data for 5-year groups

a_07_infer_monthly_population_10_year_groups - infer population by month from annual data for 10-year groups

a_08_infer_monthly_population_race - infer population by month from annual data for race groups

a_09_infer_monthly_population_ethnicity - infer population by month from annual data for ethnicity groups

## Data exploration (data_exploration) list

b_01_investigate_year_coverage - initial exploration of which county fips codes don't run the entire time

b_02_compare_old_against_new - compare originally downloaded vintage (2010) with current vintage (2020)

b_03_explore_population_over_time - load and plot time series of population for sanity checks

## Personal running note

Please run 0_00_create_folder_structure.R first to create folders which may not be there when first loaded. note: to run an R Markdown file from command line, run  Rscript -e "rmarkdown::render('SCRIPT_NAME.Rmd')"
