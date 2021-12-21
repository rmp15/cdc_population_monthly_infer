# Generate monthly population by sex, age group, county in the United States
Work in progress by Robbie M Parks

note: please run 0_00_create_folder_structure.R first to create folders which may not be there when first loaded.

note: to run an R Markdown file from command line, run\ 
Rscript -e "rmarkdown::render('SCRIPT_NAME.Rmd')"

Data downloaded from CDC Wonder at https://wonder.cdc.gov/bridged-race-population.html \
Instructions: Download for each year as .txt file in two sexes (F, M) by sex, age group, state, county

Data preparation (data_prep) list:

a_01_prepare_raw_population_data               - initial processing to get from downloaded raw files to something that can be loaded\
a_02_process_and_match_population_data         - process to match format of older years (pre-1990 from US Census Bureau)\
a_03_fix_county_codes                          - explore and fix counties which change codes to unify to current coding\
a_04_infer_monthly_population                  - infer population by month from annual data

Data exploration (data_exploration) list:

b_01_investigate_year_coverage                 - initial exploration of which county fips codes don't run the entire time
b_02_compare_old_against_new                   - compare originally downloaded vintages with current vintage\
b_03_explore_population_over_time              - load and plot time series of population for sanity checks