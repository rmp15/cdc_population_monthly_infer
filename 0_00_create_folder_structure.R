rm(list=ls())

# Create Folder Structure
# Hurricanes, tropical cyclones and cause-specific mortality in the United States 
# Robbie M Parks et al.

####***********************
#### Table of Contents #### 
####***********************

# D: Description
# 0: Preparation 
# 1: Create Folder Structure

####********************
#### D: Description ####
####********************

# Script to initially set up all the folder structure if not already there

####********************
#### 0: Preparation #### 
####********************

# 0a Load Packages
library(here)

####********************************
#### 1: Create Folder Structure #### 
####********************************

# 1a Declare directories
project.folder <- paste0(print(here::here()),'/')
  code.folder <- paste0(project.folder, "code/")
    data.prep.code.folder <- paste0(code.folder, "data_prep/")
    data.exploration.folder <- paste0(code.folder, "data_exploration/")
    packages.folder <- paste0(code.folder, "packages/")
    functions.folder <- paste0(code.folder, "functions/")
  data.folder <- paste0(project.folder, "data/")
    population.folder <- paste0(data.folder, "population/")
      population.raw.folder <- paste0(population.folder, "raw/")
      population.cleaned.folder <- paste0(population.folder, "cleaned/")
      population.documentation.folder <- paste0(population.folder, "documentation/")
        population.raw.vintage.folder <- paste0(population.raw.folder, "vintage_2020/")  
      population.na.folder <- paste0(population.folder, "na/")
    file.locations.folder <- paste0(data.folder, "file_locations/")
    objects.folder <- paste0(data.folder, "objects/")
  output.folder <- paste0(project.folder, "output/")
    population.processed.folder <- paste0(output.folder, "population_processed/")
  figures.folder <- paste0(project.folder, "figures/")
    population.over.time.folder <- paste0(figures.folder,"population_over_time/")
    population.compare.folder <- paste0(figures.folder,"population_compare/")
  reports.folder <- paste0(project.folder, "reports/")
  tables.folder <- paste0(project.folder, "tables/")

# 1b Identify list of folder locations which have just been created above
folders.names <- grep(".folder",names(.GlobalEnv),value=TRUE)

# 1c Create function to create list of folders
# note that the function will not create a folder if it already exists 
create_folders <- function(name){
  ifelse(!dir.exists(get(name)), dir.create(get(name), recursive=TRUE), FALSE)
}

# 1d Create the folders named above
lapply(folders.names, create_folders)
