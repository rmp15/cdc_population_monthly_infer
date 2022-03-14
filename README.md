# Generate monthly population by sex, age group, county in the United States 1982-2020
Work in progress by Robbie M Parks\

Currenty monthly population (vintage 2020) during 1982 - 2020 for females and males by ten-year age group (0-4,5-14,15-24,...,65-74,75-84,85+ years) for all counties (by FIPS code) in US except Alaska (working on it).

note: please run 0_00_create_folder_structure.R first to create folders which may not be there when first loaded.\
note: a_03 contains a lookup of FIPS Codes which have been changed over time to be consistent over time. 

Data for Vintage 2020 downloaded from CDC Wonder at https://wonder.cdc.gov/bridged-race-population.html \
Instructions on downloading raw files from link above: Download for each year as .txt file in two sexes (F, M) by sex, age group, state, county

Data preparation (data_prep) list:

a_01_prepare_raw_population_data               - initial processing to get from downloaded raw files to something that can be loaded\
a_02_process_and_match_population_data         - process to match format of older years (pre-1990 from US Census Bureau)\
a_03_fix_county_codes                          - explore and fix counties which change codes to unify over time\
a_04_infer_monthly_population                  - infer population by month from annual data

Data exploration (data_exploration) list:

b_01_investigate_year_coverage                 - initial exploration of which county fips codes don't run the entire time\
b_02_compare_old_against_new                   - compare originally downloaded vintage (2010) with current vintage (2020)\
b_03_explore_population_over_time              - load and plot time series of population for sanity checks

note: to run an R Markdown file from command line, run\ 
Rscript -e "rmarkdown::render('SCRIPT_NAME.Rmd')"
