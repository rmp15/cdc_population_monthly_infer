############ DATA LOADING FUNCTIONS ############

# load appropriate tropical cyclone data 
load_tc_data <- function(type){
  print(paste0('Loading ',type,' data'))
  counties.wind.rolling = readRDS(paste0(tc.folder,'tc_monthly_county_',start_year,'_',end_year,'.rds'))
  if(type %in% c('hurricane','tropical storm')){
  counties.wind.rolling = subset(counties.wind.rolling,state_fips%in%states_to_load&year%in%years_analysis&category==type)}
  if(type=='all'){
    counties.wind.rolling = subset(counties.wind.rolling,state_fips%in%states_to_load&year%in%years_analysis)}
  counties.wind.rolling = counties.wind.rolling[,c('fips','count','Date')] %>%
    group_by(fips, Date) %>%
    mutate(count = sum(count)) %>%
    mutate(fips = as.character(fips))
  counties.wind.rolling = unique(counties.wind.rolling)
  print(paste0('Loaded ',type,' data'))
  
  return(counties.wind.rolling)
}

# load appropriate temperature data 
load_temperature_data <- function(time_unit){
  print(paste0('Loading temperature data'))
  
  dat.all = data.frame()
  for(year_current in years_analysis){
    dat.temp = readRDS(paste0(tmean.folder,'weighted_area_raster_fips_tmean_daily_',as.character(year_current),'.rds'))
    dat.all = data.table::rbindlist(list(dat.all,dat.temp))
  }
  
  # summarise by selected time unit
  if(time_unit=='month'){
    dat.summarized = dat.all %>%
      group_by(fips, year, month) %>%
      summarize(tmean = mean(tmean)) %>%
      mutate(fips = as.character(fips)) %>%
      filter(fips%in%fips_to_include)
  }
  if(time_unit=='year'){
    dat.summarized = dat.all %>%
      group_by(fips, year) %>%
      summarize(tmean = mean(tmean)) %>%
      mutate(fips = as.character(fips)) %>%
      filter(fips%in%fips_to_include)
  }
  
  # add date information to tempearture data
  dat.summarized$Date = zoo::as.yearmon(paste(dat.summarized$year, dat.summarized$month), "%Y %m")
  dat.summarized = dat.summarized[,c('fips','tmean','Date')]
  dat.summarized$tmean=round(dat.summarized$tmean,1)
  
  print(paste0('Loaded temperature data'))
  
  return(dat.summarized)
}

# establish which county fips are included in analysis
figure_out_fips_to_include <- function(data){
  dat.event.multiple.test = ddply(subset(data), .(fips), summarize, NumSubs = length(unique(count)))
  dat.event.multiple.test = subset(dat.event.multiple.test,NumSubs>1)
  fips_to_include = (as.character(dat.event.multiple.test$fips))
  
  return(fips_to_include)
}

# load appropriate mortality data 
load_mortality_data <- function(cause_selected,sex_separate=0,age_separate=0){
  
  # loop through all states in all years
  print('Loading mortality data')
  dat = data.frame()
  for(year_current in years_analysis){
    dat.year = data.frame()
    mortality.year.folder = paste0(mortality.folder,as.character(year_current),'/')
    for(state in states_to_load){
      if(cause_selected %in% broad_causes_included){dat.temp = readRDS(paste0(mortality.year.folder,'broad_cause_county_monthly_',state,'_',year_current))}
      if(cause_selected %in% injuries_subcauses_included){
        dat.temp = readRDS(paste0(mortality.year.folder,'injuries_county_monthly_',state,'_',year_current))
        dat.temp$cause.group = dat.temp$cause.sub
      }
      dat.year = data.table::rbindlist(list(dat.year,dat.temp))
    }
    dat = data.table::rbindlist(list(dat,dat.year))
  }
  
  if(cause_selected %in% original_broad_causes_included){
    # combine unwanted groups into 'other' and put Diabetes Mellitus into Endocrine Disorders
    dat = dat %>%
      mutate(cause.group = gsub("Diabetes mellitus", "Endocrine disorders", cause.group)) %>%
      #mutate(cause.group = gsub("\\?.*", "Other", cause.group)) %>%
      #mutate(cause.group = gsub("Congenital anomalies", "Other", cause.group)) %>%
      #mutate(cause.group = gsub("Ill-defined", "Other", cause.group)) %>%
      #mutate(cause.group = gsub("Maternal conditions", "Other", cause.group)) %>%
      #mutate(cause.group = gsub("Oral conditions", "Other", cause.group)) %>%
      #mutate(cause.group = gsub("Perinatal conditions", "Other", cause.group)) %>%
      #mutate(cause.group = gsub("Sense organ diseases", "Other", cause.group)) %>%
      #mutate(cause.group = gsub("Sudden infant death syndrome", "Other", cause.group)) %>%
      mutate(cause.group = gsub("HIV/AIDS", "Infectious and parasitic diseases", cause.group)) #%>%
    #mutate(cause.group = gsub("Nutritional deficiencies", "Other", cause.group)) %>%
    #mutate(cause.group = gsub("Musculoskeletal diseases", "Other", cause.group)) %>%
    #mutate(cause.group = gsub("Skin diseases", "Other", cause.group))
  }
  
  if(cause_selected %in% additional_broad_causes_included){
    dat = dat %>%
      mutate(cause.group = gsub("Diabetes mellitus", "Other NCDs", cause.group)) %>%
      mutate(cause.group = gsub("Digestive diseases", "Other NCDs", cause.group)) %>%
      mutate(cause.group = gsub("Endocrine disorders", "Other NCDs", cause.group)) %>%
      mutate(cause.group = gsub("Genitourinary diseases", "Other NCDs", cause.group)) %>%
      mutate(cause.group = gsub("HIV/AIDS", "Infectious and parasitic diseases", cause.group)) %>%
      mutate(cause.group = gsub("Maternal conditions", "Maternal, neonatal, perinatal", cause.group)) %>%
      mutate(cause.group = gsub("Musculoskeletal diseases", "Other NCDs", cause.group)) %>%
      mutate(cause.group = gsub("Nutritional deficiencies", "Other NCDs", cause.group)) %>%
      mutate(cause.group = gsub("Oral conditions", "Other NCDs", cause.group)) %>%
      mutate(cause.group = gsub("Perinatal conditions", "Maternal, neonatal, perinatal", cause.group)) %>%
      mutate(cause.group = gsub("Sense organ diseases", "Other NCDs", cause.group)) %>%
      mutate(cause.group = gsub("Skin diseases", "Other NCDs", cause.group)) %>%
      mutate(cause.group = gsub("Sudden infant death syndrome", "Maternal, neonatal, perinatal", cause.group)) 
  }
  
  # total number of broad cause deaths in dataset
  deaths_total = round(dat %>%
    filter(cause.group==cause_selected) %>%
    summarize(deaths.adj=sum(deaths.adj)))
  
  # develop broad age groups
  # (0-64, 65+ years)
  if(age_separate==1){
    dat$age = ifelse(dat$age<65,0,65)
  }
  # develop finest age groups for
  # (0-4,5-14,15-24,25-34,35-44,45-54,55-64,65-74,75-84,85+ years)
  if(age_separate==2){
    dat = dat # already in the data structure so no need to change
  }
  # develop finer age groups 
  # (0-24,25-44,45-64,64-84,85+ years)
  if(age_separate==3){
    dat = dat  %>%
    mutate(age=recode(age,`0`="0",`5`="0",
                      `15`="0",`25`="25",
                      `35`="25",`45`="45",
                      `55`="45",`65`="65",
                      `75`="65",`85`="85")) %>%
    mutate(age=as.numeric(age))  
  }
  # sum over broad cause deaths separately
  if(sex_separate==0&age_separate==0){
    dat.summary = dat %>%
      group_by(cause.group,fips,year,month) %>%
      summarize(deaths.adj=sum(deaths.adj)) %>%
      filter(cause.group==cause_selected) %>%
      filter(fips%in%fips_to_include)
  }
  if(sex_separate==1&age_separate==0){
    dat.summary = dat %>%
      group_by(sex,cause.group,fips,year,month) %>%
      summarize(deaths.adj=sum(deaths.adj)) %>%
      filter(cause.group==cause_selected) %>%
      filter(fips%in%fips_to_include)
  }
  if(sex_separate==0&age_separate%in%c(1,2,3)){
    dat.summary = dat %>%
      group_by(age,cause.group,fips,year,month) %>%
      summarize(deaths.adj=sum(deaths.adj)) %>%
      filter(cause.group==cause_selected) %>%
      filter(fips%in%fips_to_include)
  }
  if(sex_separate==1&age_separate%in%c(1,2,3)){
    dat.summary = dat %>%
      group_by(sex,age,cause.group,fips,year,month) %>%
      summarize(deaths.adj=sum(deaths.adj)) %>%
      filter(cause.group==cause_selected) %>%
      filter(fips%in%fips_to_include)
  }
  
  dat.summary$cause.group = NULL
  
  # number of deaths in counties with at least one hurricane
  deaths_included = round(sum(dat.summary$deaths.adj))
  
  # proportion of deaths included
  proportion_included = round( 100 * (deaths_included / deaths_total) , 1)
  
  print(paste0(deaths_included,' deaths in states and dataset included based on included fips'))
  print(paste0(deaths_total,' deaths in states and dataset included in total'))
  print(paste0(proportion_included,'% of all deaths in states and dataset included'))
  
  print('Loaded mortality data')
  
  return(dat.summary)
  
}

# load population data
load_population_data <- function(sex_separate=0,age_separate=0){
  print('Loading population data')
  pop.county <- readRDS(paste0(population_data,'countyPopulations_infer_by_days_new_years'))
  
  # develop broad age groups
  # (0-64, 65+ years)
  if(age_separate==1){
    pop.county$age = ifelse(pop.county$age<65,0,65)
  }
  # develop finest age groups for
  # (0-4,5-14,15-24,25-34,35-44,45-54,55-64,65-74,75-84,85+ years)
  if(age_separate==2){
    pop.county = pop.county # already in the data structure so no need to change
  }
  # develop finer age groups 
  # (0-24,25-44,45-64,64-84,85+ years)
  if(age_separate==3){
    pop.county = pop.county  %>%
      mutate(age=recode(age,`0`="0",`5`="0",
                        `15`="0",`25`="25",
                        `35`="25",`45`="45",
                        `55`="45",`65`="65",
                        `75`="65",`85`="85")) %>%
      mutate(age=as.numeric(age))  
  }
  
  # sum over chosen parameters
  if(sex_separate==0&age_separate==0){
    pop.county = pop.county %>%
      group_by(fips,year,month) %>%
      summarize(pop=sum(pop),pop.adj=sum(pop.adj)) %>%
      filter(fips%in%c(fips_to_include,'12025')) # temporarily add 12025 in manually
  }
  if(sex_separate==1&age_separate==0){
    pop.county = pop.county %>%
      group_by(sex,fips,year,month) %>%
      summarize(pop=sum(pop),pop.adj=sum(pop.adj)) %>%
      filter(fips%in%c(fips_to_include,'12025')) # temporarily add 12025 in manually
  }
  if(sex_separate==0&age_separate%in%c(1,2,3)){
    pop.county = pop.county %>%
      group_by(age,fips,year,month) %>%
      summarize(pop=sum(pop),pop.adj=sum(pop.adj)) %>%
      filter(fips%in%c(fips_to_include,'12025')) # temporarily add 12025 in manually
  }
  if(sex_separate==1&age_separate%in%c(1,2,3)){
    pop.county = pop.county %>%
      group_by(sex,age,fips,year,month) %>%
      summarize(pop=sum(pop),pop.adj=sum(pop.adj)) %>%
      filter(fips%in%c(fips_to_include,'12025')) # temporarily add 12025 in manually
  }
  
  # temporary: fix 12025 -> 12086 for hurricane analysis (delete once fixed in population processing
  pop.county$pop = ifelse(pop.county$fips=='12086'&pop.county$year==1990,NA,pop.county$pop)
  pop.county$pop.adj = ifelse(pop.county$fips=='12086'&pop.county$year==1990,NA,pop.county$pop.adj)
  pop.county$fips = ifelse(pop.county$fips=='12025','12086',pop.county$fips)
  
  # temporary: fix 13053 for tc analysis (delete once fixed in population processing
  pop.county$pop = ifelse(pop.county$fips=='13053'&pop.county$year%in%c(2002,2003,2004),NA,pop.county$pop)
  pop.county$pop.adj = ifelse(pop.county$fips=='13053'&pop.county$year%in%c(2002,2003,2004),NA,pop.county$pop.adj)
  
  # provide missing values in population with value just below (delete once fixed in population processing)
  if(sex_separate==0&age_separate==0){
    pop.county = pop.county %>%
      group_by(fips) %>%
      fill(pop, .direction = "up") %>%
      fill(pop.adj, .direction = "up")
  }
  if(sex_separate==1&age_separate==0){
    pop.county = pop.county %>%
      group_by(sex,fips) %>%
      fill(pop, .direction = "up") %>%
      fill(pop.adj, .direction = "up")
  }
  if(sex_separate==0&age_separate%in%c(1,2,3)){
    pop.county = pop.county %>%
      group_by(age,fips) %>%
      fill(pop, .direction = "up") %>%
      fill(pop.adj, .direction = "up")
  }
  if(sex_separate==1&age_separate%in%c(1,2,3)){
    pop.county = pop.county %>%
      group_by(sex,age,fips) %>%
      fill(pop, .direction = "up") %>%
      fill(pop.adj, .direction = "up")
  }
  
  print('Loaded population data')
  
  return(pop.county)
  
}

# prep data for model input
data_model_prep <- function(sex_separate=0,age_separate=0){
  
  print('Preparing data for model')
  
  # create complete grid of fips, year, month, sex, age
  fips = fips_to_include
  years = years_analysis
  months = months
  
  if(sex_separate==0&age_separate==0){
    complete.grid = expand.grid(fips=as.character(fips),year=years,month=months)
  }
  if(sex_separate==1&age_separate==0){
    complete.grid = expand.grid(sex=sexes,fips=as.character(fips),year=years,month=months)
  }
  if(sex_separate==0&age_separate==1){
    complete.grid = expand.grid(age=ages.broad,fips=as.character(fips),year=years,month=months)
  }
  if(sex_separate==1&age_separate==1){
    complete.grid = expand.grid(sex=sexes,age=ages.broad,fips=as.character(fips),year=years,month=months)
  }
  if(sex_separate==0&age_separate==2){
    complete.grid = expand.grid(age=ages,fips=as.character(fips),year=years,month=months)
  }
  if(sex_separate==1&age_separate==2){
    complete.grid = expand.grid(sex=sexes,age=ages,fips=as.character(fips),year=years,month=months)
  }
  if(sex_separate==0&age_separate==3){
    complete.grid = expand.grid(age=ages.finer,fips=as.character(fips),year=years,month=months)
  }
  if(sex_separate==1&age_separate==3){
    complete.grid = expand.grid(sex=sexes,age=ages.finer,fips=as.character(fips),year=years,month=months)
  }
  
  # merge complete grid with population values
  complete.grid.columns = names(complete.grid)
  complete.grid = left_join(complete.grid,pop.county,by=complete.grid.columns)
  
  # merge to complete grid and then make all missing values zeroes
  dat.complete = left_join(complete.grid,dat.summary,by=complete.grid.columns)
  
  # fix NAs in deaths as 0
  dat.complete = dat.complete %>%
    mutate(deaths.adj = coalesce(deaths.adj, 0))
  
  # which values have NAs?
  dat.na = dat.complete %>%
    filter_all(any_vars(is.na(.)))  %>%
    distinct(fips)
  
  # add date information to mortality data
  dat.complete$Date = zoo::as.yearmon(paste(dat.complete$year, dat.complete$month), "%Y %m")
  
  # add adjusted death rates
  dat.complete$rate.adj = ifelse(dat.complete$pop.adj==0,0,with(dat.complete,deaths.adj/pop.adj))
  
  # merge tropical cyclone exposure
  dat.complete = left_join(dat.complete,counties.wind.rolling, by=c('fips','Date'))
  
  # merge temperature exposure
  dat.complete = left_join(dat.complete,dat.temp, by=c('fips','Date'))
  
  if(sex_separate==0&age_separate==0){
    dat.complete$stratum = as.factor(as.factor(dat.complete$fips):as.factor(dat.complete$month))
  }
  if(sex_separate==1&age_separate==0){
    dat.complete$stratum = as.factor(as.factor(dat.complete$sex):as.factor(dat.complete$fips):as.factor(dat.complete$month))
  }
  if(sex_separate==0&age_separate%in%c(1,2,3)){
    dat.complete$stratum = as.factor(as.factor(dat.complete$age):as.factor(dat.complete$fips):as.factor(dat.complete$month))
  }
  if(sex_separate==1&age_separate%in%c(1,2,3)){
    dat.complete$stratum = as.factor(as.factor(dat.complete$sex):as.factor(dat.complete$age):as.factor(dat.complete$fips):as.factor(dat.complete$month))
  }
    
  # year.month variable for time along series
  dat.year.month = unique(dat.complete[,c('year', 'month')])
  dat.year.month = dat.year.month[order(dat.year.month$year,dat.year.month$month),]
  dat.year.month$year.month = seq(nrow(dat.year.month))
  dat.complete = left_join(dat.complete,dat.year.month, by=c('year','month'))
  
  # order data
  if(sex_separate==0&age_separate==0){
  dat.complete = dat.complete[order(dat.complete$fips,dat.complete$Date),]}
  if(sex_separate==1&age_separate==0){
    dat.complete = dat.complete[order(dat.complete$sex,dat.complete$fips,dat.complete$Date),]}
  if(sex_separate==0&age_separate%in%c(1,2,3)){
    dat.complete = dat.complete[order(dat.complete$age,dat.complete$fips,dat.complete$Date),]}
  if(sex_separate==1&age_separate%in%c(1,2,3)){
    dat.complete = dat.complete[order(dat.complete$sex,dat.complete$age,dat.complete$fips,dat.complete$Date),]}
  
  # create lag structure for TC exposures
  dat.complete$event_lag0 = dat.complete$count
  
  if(sex_separate==0&age_separate==0){
    dat.complete =
      dat.complete %>%
      group_by(fips) %>%
      mutate(event_lag1=dplyr::lag(event_lag0), event_lag2=dplyr::lag(event_lag0,2),
             event_lag3=dplyr::lag(event_lag0,3), event_lag4=dplyr::lag(event_lag0,4), 
             event_lag5=dplyr::lag(event_lag0,5), event_lag6=dplyr::lag(event_lag0,6),         
             event_lag7=dplyr::lag(event_lag0,7), event_lag8=dplyr::lag(event_lag0,8),
             event_lag9=dplyr::lag(event_lag0,9), event_lag10=dplyr::lag(event_lag0,10),
             event_lag11=dplyr::lag(event_lag0,11), event_lag12=dplyr::lag(event_lag0,12))
  }
  if(sex_separate==1&age_separate==0){
    dat.complete =
      dat.complete %>%
      group_by(sex,fips) %>%
      mutate(event_lag1=dplyr::lag(event_lag0), event_lag2=dplyr::lag(event_lag0,2),
             event_lag3=dplyr::lag(event_lag0,3), event_lag4=dplyr::lag(event_lag0,4), 
             event_lag5=dplyr::lag(event_lag0,5), event_lag6=dplyr::lag(event_lag0,6),         
             event_lag7=dplyr::lag(event_lag0,7), event_lag8=dplyr::lag(event_lag0,8),
             event_lag9=dplyr::lag(event_lag0,9), event_lag10=dplyr::lag(event_lag0,10),
             event_lag11=dplyr::lag(event_lag0,11), event_lag12=dplyr::lag(event_lag0,12))
  }
  if(sex_separate==0&age_separate%in%c(1,2,3)){
    dat.complete =
      dat.complete %>%
      group_by(age,fips) %>%
      mutate(event_lag1=dplyr::lag(event_lag0), event_lag2=dplyr::lag(event_lag0,2),
             event_lag3=dplyr::lag(event_lag0,3), event_lag4=dplyr::lag(event_lag0,4), 
             event_lag5=dplyr::lag(event_lag0,5), event_lag6=dplyr::lag(event_lag0,6),         
             event_lag7=dplyr::lag(event_lag0,7), event_lag8=dplyr::lag(event_lag0,8),
             event_lag9=dplyr::lag(event_lag0,9), event_lag10=dplyr::lag(event_lag0,10),
             event_lag11=dplyr::lag(event_lag0,11), event_lag12=dplyr::lag(event_lag0,12))
  }
  if(sex_separate==1&age_separate%in%c(1,2,3)){
    dat.complete =
      dat.complete %>%
      group_by(sex,age,fips) %>%
      mutate(event_lag1=dplyr::lag(event_lag0), event_lag2=dplyr::lag(event_lag0,2),
             event_lag3=dplyr::lag(event_lag0,3), event_lag4=dplyr::lag(event_lag0,4), 
             event_lag5=dplyr::lag(event_lag0,5), event_lag6=dplyr::lag(event_lag0,6),         
             event_lag7=dplyr::lag(event_lag0,7), event_lag8=dplyr::lag(event_lag0,8),
             event_lag9=dplyr::lag(event_lag0,9), event_lag10=dplyr::lag(event_lag0,10),
             event_lag11=dplyr::lag(event_lag0,11), event_lag12=dplyr::lag(event_lag0,12))
  }
  
  # fix rownames
  rownames(dat.complete) = 1:nrow(dat.complete)
  
  # round adjusted death counts and population
  dat.complete$deaths.adj = round(dat.complete$deaths.adj)
  dat.complete$pop.adj = round(dat.complete$pop.adj)
  
  # variables for INLA model
  dat.complete$year.month4 = dat.complete$year.month3 = dat.complete$year.month2 = dat.complete$year.month
  dat.complete$month7 = dat.complete$month6 = dat.complete$month5 = dat.complete$month4 = dat.complete$month3 = dat.complete$month2 = dat.complete$month
  dat.complete$fips3 = dat.complete$fips2 =  dat.complete$fips = as.integer(dat.complete$fips)
  dat.complete$e = 1:nrow(dat.complete)
  
  # variables for frequentist model
  dat.complete$logpop=log(dat.complete$pop.adj)
  
  print('Data prepared for model')
  
  return(dat.complete)
  
}

# add svi data
svi_data <- function(n){
  # all tropical cyclone counties
  fips_to_include_tc <- as.integer(figure_out_fips_to_include(load_tc_data('all')))
  
  dat.svi <- read.csv(paste0(cdc.svi.folder,'SVI2016_US_COUNTY.csv')) %>%
    select(FIPS,RPL_THEMES) %>%
    filter(FIPS%in%fips_to_include_tc) %>%
    mutate(percentile=ntile(RPL_THEMES, n)) %>%
    select(-RPL_THEMES)
    
  names(dat.svi) = c('fips','percentile')
  
  dat.complete <- left_join(dat.complete,dat.svi,by=c('fips'))
  
  return(dat.complete)
                    
}

# knots for spline term
prepare_knots <- function(knot_interval){
  
  # year.month variable for time along series
  dat.year.month = unique(dat.complete[,c('year', 'month')])
  dat.year.month = dat.year.month[order(dat.year.month$year,dat.year.month$month),]
  dat.year.month$year.month = seq(nrow(dat.year.month))
  dat.complete = left_join(dat.complete,dat.year.month, by=c('year','month'))
  
  knots = seq(knot_interval,nrow(dat.year.month)-knot_interval,by=knot_interval)
  
  return(knots)
}

############ DATA PROCESSING FUNCTIONS ############

# Function to add year, month, and day to dataset from lubridate function
add_date_info = function(dat){
  dat$year     = lubridate::year(dat$closest_date)
  dat$month    = lubridate::month(dat$closest_date)
  dat$day      = lubridate::day(dat$closest_date)
  
  dat = dat[
    with(dat, order(year,month,day)),
  ]
  
  return(dat)
}

# Function to summarise a year's data for broad causes
# Uses GHE and http://www.wolfbane.com/icd/ when GHE is missing
yearsummary_broad  <- function(x=2000) {

  print(paste0('year ',x,' now being processed'))

  # load year of deaths
  raw_mortalty_data <- '~/data/mortality/US/state/processed/cod/'
  dat.name <- paste0(raw_mortalty_data,"deathscod",x,".dta")
  dat <- read.dta(dat.name)

  # fix sex classification if in certain years
  if(x %in% c(2003:2010,2012)){
    dat$sex = as.integer(plyr::mapvalues(dat$sex,from=sort(unique(dat$sex)),to=c(2,1)))
  }

  # add extra label for CODs based on relevant ICD year
  start_year = 1999
  if(x<start_year) {
    # ICD 9 coding for broad cod coding
    dat$cause[nchar(dat$cause)==3] <- paste0(dat$cause[nchar(dat$cause)==3],'0')
    dat$cause.numeric = as.numeric(dat$cause)
    dat$cause.group =
		ifelse(dat$cause.numeric>=0   &dat$cause.numeric<=1399,'Infectious and parasitic diseases',
		ifelse(dat$cause.numeric>=1400&dat$cause.numeric<=2399,'Cancers',
	    ifelse(dat$cause.numeric>=2400&dat$cause.numeric<=2429,'Endocrine disorders',
	    ifelse(dat$cause.numeric>=2430&dat$cause.numeric<=2439,'Nutritional deficiencies',
   		ifelse(dat$cause.numeric>=2440&dat$cause.numeric<=2469,'Endocrine disorders',
	    ifelse(dat$cause.numeric>=2470&dat$cause.numeric<=2499,'?????',
	    ifelse(dat$cause.numeric>=2500&dat$cause.numeric<=2509,'Endocrine disorders', # 'Diabetes mellitus'
        ifelse(dat$cause.numeric>=2510&dat$cause.numeric<=2599,'Endocrine disorders',
	    ifelse(dat$cause.numeric>=2600&dat$cause.numeric<=2699,'Nutritional deficiencies',
		ifelse(dat$cause.numeric>=2700&dat$cause.numeric<=2739,'Endocrine disorders',
		ifelse(dat$cause.numeric>=2740&dat$cause.numeric<=2749,'Musculoskeletal diseases',
	    ifelse(dat$cause.numeric>=2750&dat$cause.numeric<=2794,'Endocrine disorders',
		ifelse(dat$cause.numeric>=2795&dat$cause.numeric<=2796,'HIV/AIDS', # 'Infectious and parasitic diseases'
		ifelse(dat$cause.numeric>=2797&dat$cause.numeric<=2799,'Endocrine disorders',
	    ifelse(dat$cause.numeric>=2800&dat$cause.numeric<=2819,'Nutritional deficiencies',
		ifelse(dat$cause.numeric>=2820&dat$cause.numeric<=2858,'Endocrine disorders',
		ifelse(dat$cause.numeric>=2859&dat$cause.numeric<=2859,'Nutritional deficiencies',
		ifelse(dat$cause.numeric>=2860&dat$cause.numeric<=2899,'Endocrine disorders',
		ifelse(dat$cause.numeric>=2900&dat$cause.numeric<=3199,'Neuropsychiatric conditions',
		ifelse(dat$cause.numeric>=3200&dat$cause.numeric<=3239,'Infectious and parasitic diseases',
		ifelse(dat$cause.numeric>=3240&dat$cause.numeric<=3599,'Neuropsychiatric conditions',
		ifelse(dat$cause.numeric>=3600&dat$cause.numeric<=3809,'Sense organ diseases',
	    ifelse(dat$cause.numeric>=3810&dat$cause.numeric<=3829,'Respiratory diseases', # Otitis Media
		ifelse(dat$cause.numeric>=3830&dat$cause.numeric<=3899,'Sense organ diseases',
        ifelse(dat$cause.numeric>=3900&dat$cause.numeric<=4599,'Cardiovascular diseases',
        ifelse(dat$cause.numeric>=4600&dat$cause.numeric<=5199,'Respiratory diseases',
	    ifelse(dat$cause.numeric>=5200&dat$cause.numeric<=5299,'Oral conditions',
		ifelse(dat$cause.numeric>=5300&dat$cause.numeric<=5799,'Digestive diseases',
		ifelse(dat$cause.numeric>=5800&dat$cause.numeric<=6119,'Genitourinary diseases',
		ifelse(dat$cause.numeric>=6120&dat$cause.numeric<=6169,'Genitourinary diseases', # TO CONFIRM AS MISSING FROM GHE
		ifelse(dat$cause.numeric>=6170&dat$cause.numeric<=6299,'Genitourinary diseases',
		ifelse(dat$cause.numeric>=6300&dat$cause.numeric<=6769,'Maternal conditions',
		ifelse(dat$cause.numeric>=6770&dat$cause.numeric<=6799,'Maternal conditions', # Other maternal and fetal complications
		ifelse(dat$cause.numeric>=6800&dat$cause.numeric<=7099,'Skin diseases',
		ifelse(dat$cause.numeric>=7100&dat$cause.numeric<=7399,'Musculoskeletal diseases',
		ifelse(dat$cause.numeric>=7400&dat$cause.numeric<=7599,'Congenital anomalies',
		ifelse(dat$cause.numeric>=7600&dat$cause.numeric<=7712,'Perinatal conditions',
		ifelse(dat$cause.numeric>=7713&dat$cause.numeric<=7713,'Infectious and parasitic diseases',
		ifelse(dat$cause.numeric>=7714&dat$cause.numeric<=7799,'Perinatal conditions',
		ifelse(dat$cause.numeric>=7800&dat$cause.numeric<=7979,'Ill-defined',
		ifelse(dat$cause.numeric>=7980&dat$cause.numeric<=7980,'Sudden infant death syndrome',
		ifelse(dat$cause.numeric>=7981&dat$cause.numeric<=7999,'Ill-defined',
        ifelse(dat$cause.numeric>=8000&dat$cause.numeric<=9999,'Injuries',
        'NA')))))))))))))))))))))))))))))))))))))))))))

    dat$cause.group = as.character(dat$cause.group)

    dat.merged = dat

    # dummy cause subgroups
    dat.merged$cause.sub = ''

    # dummy cause subsubgroups
    dat.merged$cause.sub.sub = ''

    # dummy letter
    dat.merged$letter = ' '

  }

  if(x>=start_year){
    # merge cod in ICD 10 coding for broad letter coding
    dat$cause[nchar(dat$cause)==3] <- paste0(dat$cause[nchar(dat$cause)==3],'0')
    dat$letter = substr(dat$cause,1,1)

    dat.merged = dat

    # numerical cause
    dat.merged$cause.numeric = as.numeric(as.character(substr(dat.merged$cause,2,4)))

    # cause groups (some taken from http://www.wolfbane.com/icd/)
    dat.merged$cause.group =
		ifelse(dat.merged$letter=='A'&dat.merged$cause.numeric>=0&dat.merged$cause.numeric<=999,'Infectious and parasitic diseases',
	    ifelse(dat.merged$letter=='B'&dat.merged$cause.numeric>=0&dat.merged$cause.numeric<=199,'Infectious and parasitic diseases',
	    ifelse(dat.merged$letter=='B'&dat.merged$cause.numeric>=200&dat.merged$cause.numeric<=249,'HIV/AIDS', # 'Infectious and parasitic diseases'
		ifelse(dat.merged$letter=='B'&dat.merged$cause.numeric>=250&dat.merged$cause.numeric<=999,'Infectious and parasitic diseases',
	    ifelse(dat.merged$letter=='C'&dat.merged$cause.numeric>=0&dat.merged$cause.numeric<=979,'Cancers',
	    ifelse(dat.merged$letter=='C'&dat.merged$cause.numeric>=980&dat.merged$cause.numeric<=999,'Cancers', # TO CONFIRM AS MISSING FROM GHE
		ifelse(dat.merged$letter=='D'&dat.merged$cause.numeric>=0&dat.merged$cause.numeric<=489,'Cancers',
	   	ifelse(dat.merged$letter=='D'&dat.merged$cause.numeric>=490&dat.merged$cause.numeric<=499,'?????',
		ifelse(dat.merged$letter=='D'&dat.merged$cause.numeric>=500&dat.merged$cause.numeric<=539,'Nutritional deficiencies',
	   	ifelse(dat.merged$letter=='D'&dat.merged$cause.numeric>=540&dat.merged$cause.numeric<=549,'?????',
		ifelse(dat.merged$letter=='D'&dat.merged$cause.numeric>=550&dat.merged$cause.numeric<=648,'Endocrine disorders',
        ifelse(dat.merged$letter=='D'&dat.merged$cause.numeric>=640&dat.merged$cause.numeric<=649,'Nutritional deficiencies',
        ifelse(dat.merged$letter=='D'&dat.merged$cause.numeric>=650&dat.merged$cause.numeric<=899,'Endocrine disorders',
		ifelse(dat.merged$letter=='D'&dat.merged$cause.numeric>=900&dat.merged$cause.numeric<=999,'?????',
		ifelse(dat.merged$letter=='E'&dat.merged$cause.numeric>=0&dat.merged$cause.numeric<=29,'Nutritional deficiencies',
	    ifelse(dat.merged$letter=='E'&dat.merged$cause.numeric>=30&dat.merged$cause.numeric<=79,'Endocrine disorders',
	    ifelse(dat.merged$letter=='E'&dat.merged$cause.numeric>=80&dat.merged$cause.numeric<=99,'?????',
		ifelse(dat.merged$letter=='E'&dat.merged$cause.numeric>=100&dat.merged$cause.numeric<=149,'Endocrine disorders', # 'Diabetes mellitus'
		ifelse(dat.merged$letter=='E'&dat.merged$cause.numeric>=150&dat.merged$cause.numeric<=169,'Endocrine disorders',
		ifelse(dat.merged$letter=='E'&dat.merged$cause.numeric>=170&dat.merged$cause.numeric<=199,'?????',
	    ifelse(dat.merged$letter=='E'&dat.merged$cause.numeric>=200&dat.merged$cause.numeric<=359,'Endocrine disorders', # ADAPTED FROM GHE TO INCLUDE E35
	    ifelse(dat.merged$letter=='E'&dat.merged$cause.numeric>=360&dat.merged$cause.numeric<=399,'?????',
		'NA'))))))))))))))))))))))

    dat.merged$cause.group =
		ifelse(dat.merged$letter=='E'&dat.merged$cause.numeric>=400&dat.merged$cause.numeric<=469,'Nutritional deficiencies',
	   	ifelse(dat.merged$letter=='E'&dat.merged$cause.numeric>=470&dat.merged$cause.numeric<=499,'?????',
	    ifelse(dat.merged$letter=='E'&dat.merged$cause.numeric>=500&dat.merged$cause.numeric<=649,'Nutritional deficiencies', # TO CONFIRM AS MISSING FROM GHE
		ifelse(dat.merged$letter=='E'&dat.merged$cause.numeric>=650&dat.merged$cause.numeric<=889,'Endocrine disorders',
		ifelse(dat.merged$letter=='E'&dat.merged$cause.numeric>=890&dat.merged$cause.numeric<=899,'Endocrine disorders', # TO CONFIRM AS MISSING FROM GHE
	    ifelse(dat.merged$letter=='E'&dat.merged$cause.numeric>=900&dat.merged$cause.numeric<=909,'Nutritional deficiencies', # TO CONFIRM AS MISSING FROM GHE
	    ifelse(dat.merged$letter=='E'&dat.merged$cause.numeric>=910&dat.merged$cause.numeric<=999,'?????', # TO CONFIRM AS MISSING FROM GHE
	    ifelse(dat.merged$letter=='F'&dat.merged$cause.numeric>=0&dat.merged$cause.numeric<=999,'Neuropsychiatric conditions', # F00 ADDED
		ifelse(dat.merged$letter=='G'&dat.merged$cause.numeric>=0&dat.merged$cause.numeric<=9,'Infectious and parasitic diseases',
		ifelse(dat.merged$letter=='G'&dat.merged$cause.numeric>=10&dat.merged$cause.numeric<=29,'Infectious and parasitic diseases', # TO CONFIRM AS MISSING FROM GHE
		ifelse(dat.merged$letter=='G'&dat.merged$cause.numeric>=30&dat.merged$cause.numeric<=49,'Infectious and parasitic diseases',
		ifelse(dat.merged$letter=='G'&dat.merged$cause.numeric>=50&dat.merged$cause.numeric<=59,'Infectious and parasitic diseases', # TO CONFIRM AS MISSING FROM GHE
		ifelse(dat.merged$letter=='G'&dat.merged$cause.numeric>=60&dat.merged$cause.numeric<=139,'Neuropsychiatric conditions',
		ifelse(dat.merged$letter=='G'&dat.merged$cause.numeric>=140&dat.merged$cause.numeric<=149,'Infectious and parasitic diseases',
		ifelse(dat.merged$letter=='G'&dat.merged$cause.numeric>=150&dat.merged$cause.numeric<=989,'Neuropsychiatric conditions',
		ifelse(dat.merged$letter=='G'&dat.merged$cause.numeric>=990&dat.merged$cause.numeric<=999,'?????', # Other disorders of nervous system in diseases classified elsewhere
		ifelse(dat.merged$letter=='H'&dat.merged$cause.numeric>=0&dat.merged$cause.numeric<=619,'Sense organ diseases',
		ifelse(dat.merged$letter=='H'&dat.merged$cause.numeric>=620&dat.merged$cause.numeric<=649,'Respiratory diseases', # TO CONFIRM AS MISSING FROM GHE
		ifelse(dat.merged$letter=='H'&dat.merged$cause.numeric>=650&dat.merged$cause.numeric<=669,'Respiratory diseases',
	    ifelse(dat.merged$letter=='H'&dat.merged$cause.numeric>=670&dat.merged$cause.numeric<=679,'Respiratory diseases', # TO CONFIRM AS MISSING FROM GHE
		dat.merged$cause.group))))))))))))))))))))

dat.merged$cause.group =
		ifelse(dat.merged$letter=='H'&dat.merged$cause.numeric>=680&dat.merged$cause.numeric<=939,'Sense organ diseases', # TO CONFIRM AS MISSING FROM GHE
		ifelse(dat.merged$letter=='H'&dat.merged$cause.numeric>=940&dat.merged$cause.numeric<=999,'Sense organ diseases',
	    ifelse(dat.merged$letter=='I'&dat.merged$cause.numeric>=0&dat.merged$cause.numeric<=999,'Cardiovascular diseases',
		ifelse(dat.merged$letter=='J'&dat.merged$cause.numeric>=0&dat.merged$cause.numeric<=69,'Respiratory diseases',
		ifelse(dat.merged$letter=='J'&dat.merged$cause.numeric>=70&dat.merged$cause.numeric<=89,'Respiratory diseases', # TO CONFIRM AS MISSING FROM GHE
	    ifelse(dat.merged$letter=='J'&dat.merged$cause.numeric>=90&dat.merged$cause.numeric<=189,'Respiratory diseases',
		ifelse(dat.merged$letter=='J'&dat.merged$cause.numeric>=190&dat.merged$cause.numeric<=199,'Respiratory diseases', # TO CONFIRM AS MISSING FROM GHE
		ifelse(dat.merged$letter=='J'&dat.merged$cause.numeric>=200&dat.merged$cause.numeric<=229,'Respiratory diseases',
		ifelse(dat.merged$letter=='J'&dat.merged$cause.numeric>=230&dat.merged$cause.numeric<=299,'Respiratory diseases', # TO CONFIRM AS MISSING FROM GHE
		ifelse(dat.merged$letter=='J'&dat.merged$cause.numeric>=300&dat.merged$cause.numeric<=989,'Respiratory diseases',
		ifelse(dat.merged$letter=='J'&dat.merged$cause.numeric>=990&dat.merged$cause.numeric<=999,'Respiratory diseases', # TO CONFIRM AS MISSING FROM GHE
		ifelse(dat.merged$letter=='K'&dat.merged$cause.numeric>=0&dat.merged$cause.numeric<=149,'Oral conditions',
	    ifelse(dat.merged$letter=='K'&dat.merged$cause.numeric>=150&dat.merged$cause.numeric<=199,'?????',
		ifelse(dat.merged$letter=='K'&dat.merged$cause.numeric>=200&dat.merged$cause.numeric<=929,'Digestive diseases',
		ifelse(dat.merged$letter=='K'&dat.merged$cause.numeric>=930&dat.merged$cause.numeric<=999,'Digestive diseases', # TO CONFIRM AS MISSING FROM GHE
		ifelse(dat.merged$letter=='L'&dat.merged$cause.numeric>=0&dat.merged$cause.numeric<=989,'Skin diseases',
		ifelse(dat.merged$letter=='L'&dat.merged$cause.numeric>=900&dat.merged$cause.numeric<=999,'Skin diseases', # TO CONFIRM AS MISSING FROM GHE
		ifelse(dat.merged$letter=='M'&dat.merged$cause.numeric>=0&dat.merged$cause.numeric<=999,'Musculoskeletal diseases',
		ifelse(dat.merged$letter=='N'&dat.merged$cause.numeric>=0&dat.merged$cause.numeric<=649,'Genitourinary diseases',
		ifelse(dat.merged$letter=='N'&dat.merged$cause.numeric>=650&dat.merged$cause.numeric<=699,'?????',
		dat.merged$cause.group))))))))))))))))))))

dat.merged$cause.group =
		ifelse(dat.merged$letter=='N'&dat.merged$cause.numeric>=700&dat.merged$cause.numeric<=739,'Infectious and parasitic diseases',
		ifelse(dat.merged$letter=='N'&dat.merged$cause.numeric>=740&dat.merged$cause.numeric<=749,'Infectious and parasitic diseases', # TO CONFIRM AS MISSING FROM GHE
		ifelse(dat.merged$letter=='N'&dat.merged$cause.numeric>=750&dat.merged$cause.numeric<=989,'Genitourinary diseases',
		ifelse(dat.merged$letter=='N'&dat.merged$cause.numeric>=990&dat.merged$cause.numeric<=999,'Genitourinary diseases', # TO CONFIRM AS MISSING FROM GHE
		ifelse(dat.merged$letter=='O'&dat.merged$cause.numeric>=0&dat.merged$cause.numeric<=999,'Maternal conditions',
		ifelse(dat.merged$letter=='P'&dat.merged$cause.numeric>=0&dat.merged$cause.numeric<=969,'Perinatal conditions',
	   	ifelse(dat.merged$letter=='P'&dat.merged$cause.numeric>=970&dat.merged$cause.numeric<=999,'?????',
		ifelse(dat.merged$letter=='Q'&dat.merged$cause.numeric>=0&dat.merged$cause.numeric<=999,'Congenital anomalies',
		ifelse(dat.merged$letter=='R'&dat.merged$cause.numeric>=0&dat.merged$cause.numeric<=949,'Ill-defined',
		ifelse(dat.merged$letter=='R'&dat.merged$cause.numeric>=950&dat.merged$cause.numeric<=959,'Sudden infant death syndrome',
		ifelse(dat.merged$letter=='R'&dat.merged$cause.numeric>=960&dat.merged$cause.numeric<=999,'Ill-defined',
		ifelse(dat.merged$letter=='S'&dat.merged$cause.numeric>=0&dat.merged$cause.numeric<=999,'Injuries',
		ifelse(dat.merged$letter=='T'&dat.merged$cause.numeric>=0&dat.merged$cause.numeric<=999,'Injuries',
		ifelse(dat.merged$letter=='U'&dat.merged$cause.numeric>=0&dat.merged$cause.numeric<=999,'?????', # 'U' is reserved for new diseases, U011 is September 11th (2922 deaths in 2011)
		ifelse(dat.merged$letter=='V'&dat.merged$cause.numeric>=0&dat.merged$cause.numeric<=899,'Injuries',
		ifelse(dat.merged$letter=='V'&dat.merged$cause.numeric>=900&dat.merged$cause.numeric<=999,'Injuries', # TO CONFIRM AS MISSING FROM GHE
		ifelse(dat.merged$letter=='W'&dat.merged$cause.numeric>=0&dat.merged$cause.numeric<=999,'Injuries',
		ifelse(dat.merged$letter=='X'&dat.merged$cause.numeric>=0&dat.merged$cause.numeric<=409,'Injuries',
		ifelse(dat.merged$letter=='X'&dat.merged$cause.numeric>=410&dat.merged$cause.numeric<=429,'Neuropsychiatric conditions',
		ifelse(dat.merged$letter=='X'&dat.merged$cause.numeric>=430&dat.merged$cause.numeric<=449,'Injuries', # Poisoning
		ifelse(dat.merged$letter=='X'&dat.merged$cause.numeric>=450&dat.merged$cause.numeric<=459,'Neuropsychiatric conditions',
		ifelse(dat.merged$letter=='X'&dat.merged$cause.numeric>=460&dat.merged$cause.numeric<=999,'Injuries',
		ifelse(dat.merged$letter=='Y'&dat.merged$cause.numeric>=0&dat.merged$cause.numeric<=899,'Injuries',
		ifelse(dat.merged$letter=='Y'&dat.merged$cause.numeric>=900&dat.merged$cause.numeric<=999,'?????', # Supplementary factors related to causes of morbidity and mortality classified elsewhere
		dat.merged$cause.group))))))))))))))))))))))))

    # dummy cause subgroups
    dat.merged$cause.sub = ''

    # dummy cause subsubgroups
    dat.merged$cause.sub.sub = ''

  }

  # add agegroup groupings
  dat.merged$agegroup <-
    ifelse (dat.merged$age<5,   0,
            ifelse (dat.merged$age<15,  5,
                    ifelse (dat.merged$age<25,  15,
                            ifelse (dat.merged$age<35,  25,
                                    ifelse (dat.merged$age<45,  35,
                                            ifelse (dat.merged$age<55,  45,
                                                    ifelse (dat.merged$age<65,  55,
                                                            ifelse (dat.merged$age<75,  65,
                                                                    ifelse (dat.merged$age<85,  75,
                                                                            85)))))))))

  # # summarise by county, year, month, sex, agegroup
  dat.summarised <- dplyr::summarise(group_by(dat.merged,cause.group,cause.sub,cause.sub.sub,fips,year,monthdth,sex,agegroup),deaths=sum(deaths))
  names(dat.summarised)[1:9] <- c('cause.group','cause.sub','cause.sub.sub','fips','year','month','sex','age','deaths')
  dat.summarised <- na.omit(dat.summarised)

  # add year
  dat.summarised$year <- x

  # add state fips
  dat.summarised$state.fips <- substr(dat.summarised$fips,1,2)

  print(paste0('total deaths in year ',sum(dat$deaths),', total deaths after processing is ',sum(dat.merged$deaths),' and with na rows removed with ',sum(dat.summarised$deaths)))

  print(head(dat.summarised),20)
  print(sort(unique(dat.summarised$cause.group)))

  return(dat.summarised)
}

# Function to summarise a year's data for cardiorespiratory diseases
yearsummary_cardioresp  <- function(x=2000) {
  
  print(paste0('year ',x,' now being processed'))
  
  # load year of deaths
  raw_mortalty_data <- '~/data/mortality/US/state/processed/cod/'
  dat.name <- paste0(raw_mortalty_data,"deathscod",x,".dta")
  dat <- read.dta(dat.name)
  
  # fix sex classification if in certain years
  if(x %in% c(2003:2010,2012)){
    dat$sex = as.integer(plyr::mapvalues(dat$sex,from=sort(unique(dat$sex)),to=c(2,1)))
  }

  # add extra label for CODs based on relevant ICD year
  start_year = 1999
  if(x<start_year) {
    # ICD 9 coding for broad cod coding
    dat$cause[nchar(dat$cause)==3] <- paste0(dat$cause[nchar(dat$cause)==3],'0')
    dat$cause.numeric = as.numeric(dat$cause)
    dat$cause.group =
		ifelse(dat$cause.numeric>=1400&dat$cause.numeric<=2399,'Cancers',
	    ifelse(dat$cause.numeric>=3810&dat$cause.numeric<=3829,'Respiratory diseases', # Otitis Media addition
        ifelse(dat$cause.numeric>=3900&dat$cause.numeric<=4599,'Cardiovascular diseases',
        ifelse(dat$cause.numeric>=4600&dat$cause.numeric<=5199,'Respiratory diseases',
        ifelse(dat$cause.numeric>=8000&dat$cause.numeric<=9999,'Injuries',
        'Other')))))
    
    dat$cause.group = as.character(dat$cause.group)
    
    # only filter for cardiorespiratory
    dat.merged = subset(dat,cause.group%in%c('Respiratory diseases', 'Cardiovascular diseases'))

    # cause subgroups
    dat.merged$cause.sub =
						 # Ottis media (other respiratory diseases)
						 ifelse(dat.merged$cause.numeric>=3810&dat.merged$cause.numeric<=3829, 'Respiratory infections', #'Otitis media',
						 # Cardiovascular diseases (3900-4599)
						 ifelse(dat.merged$cause.numeric>=3900&dat.merged$cause.numeric<=3989, 'Other cardiovascular diseases', #'Rheumatic heart disease',
						 ifelse(dat.merged$cause.numeric>=3990&dat.merged$cause.numeric<=4009, 'Other cardiovascular diseases', #'?',
						 ifelse(dat.merged$cause.numeric>=4010&dat.merged$cause.numeric<=4059, 'Other cardiovascular diseases', #'Hypertensive heart disease',
						 ifelse(dat.merged$cause.numeric>=4060&dat.merged$cause.numeric<=4099, 'Other cardiovascular diseases', #'?',
						 ifelse(dat.merged$cause.numeric>=4100&dat.merged$cause.numeric<=4149, 'Ischaemic heart disease', #'Ischaemic heart disease',
						 ifelse(dat.merged$cause.numeric>=4150&dat.merged$cause.numeric<=4199, 'Other cardiovascular diseases', #'?',
						 ifelse(dat.merged$cause.numeric>=4200&dat.merged$cause.numeric<=4229, 'Other cardiovascular diseases', #'Inflammatory heart diseases',
						 ifelse(dat.merged$cause.numeric>=4230&dat.merged$cause.numeric<=4249, 'Other cardiovascular diseases', #'?',
						 ifelse(dat.merged$cause.numeric>=4250&dat.merged$cause.numeric<=4259, 'Other cardiovascular diseases', #'Inflammatory heart diseases',
						 ifelse(dat.merged$cause.numeric>=4260&dat.merged$cause.numeric<=4299, 'Other cardiovascular diseases', #'?',
						 ifelse(dat.merged$cause.numeric>=4300&dat.merged$cause.numeric<=4389, 'Cerebrovascular disease', #'Cerebrovascular disease',
						 ifelse(dat.merged$cause.numeric>=4390&dat.merged$cause.numeric<=4599, 'Other cardiovascular diseases', #'?',
						 # Respiratory diseases and infections (4600-5199)
						 ifelse(dat.merged$cause.numeric>=4600&dat.merged$cause.numeric<=4659, 'Respiratory infections', #'Upper respiratory infections',
						 ifelse(dat.merged$cause.numeric>=4660&dat.merged$cause.numeric<=4669, 'Respiratory infections', #'Lower respiratory infections',
						 ifelse(dat.merged$cause.numeric>=4670&dat.merged$cause.numeric<=4799, 'Other respiratory diseases', #'?',
						 ifelse(dat.merged$cause.numeric>=4800&dat.merged$cause.numeric<=4879, 'Respiratory infections', #'Lower respiratory infections',
						 ifelse(dat.merged$cause.numeric>=4880&dat.merged$cause.numeric<=4899, 'Other respiratory diseases', #'?',
						 ifelse(dat.merged$cause.numeric>=4900&dat.merged$cause.numeric<=4929, 'Chronic obstructive pulmonary disease', #'Chronic obstructive pulmonary disease',
						 ifelse(dat.merged$cause.numeric>=4930&dat.merged$cause.numeric<=4939, 'Other respiratory diseases', #'Asthma',
						 ifelse(dat.merged$cause.numeric>=4940&dat.merged$cause.numeric<=4949, 'Other respiratory diseases', #'?',
						 ifelse(dat.merged$cause.numeric>=4950&dat.merged$cause.numeric<=4969, 'Chronic obstructive pulmonary disease', #'Chronic obstructive pulmonary disease',
						 ifelse(dat.merged$cause.numeric>=4970&dat.merged$cause.numeric<=5199, 'Other respiratory diseases', #'?',
						 'NA')))))))))))))))))))))))
    
    # cause subsubgroups
    dat.merged$cause.sub.sub =
						 # Ottis media (other respiratory diseases)
						 ifelse(dat.merged$cause.numeric>=3810&dat.merged$cause.numeric<=3829, 'Otitis media',
						 # Cardiovascular diseases (3900-4599)
						 ifelse(dat.merged$cause.numeric>=3900&dat.merged$cause.numeric<=3989, 'Rheumatic heart disease',
						 ifelse(dat.merged$cause.numeric>=3990&dat.merged$cause.numeric<=4009, 'Other',
						 ifelse(dat.merged$cause.numeric>=4010&dat.merged$cause.numeric<=4059, 'Hypertensive heart disease',
						 ifelse(dat.merged$cause.numeric>=4060&dat.merged$cause.numeric<=4099, 'Other',
						 ifelse(dat.merged$cause.numeric>=4100&dat.merged$cause.numeric<=4149, 'Ischaemic heart disease',
						 ifelse(dat.merged$cause.numeric>=4150&dat.merged$cause.numeric<=4199, 'Other',
						 ifelse(dat.merged$cause.numeric>=4200&dat.merged$cause.numeric<=4229, 'Inflammatory heart diseases',
						 ifelse(dat.merged$cause.numeric>=4230&dat.merged$cause.numeric<=4249, 'Other',
						 ifelse(dat.merged$cause.numeric>=4250&dat.merged$cause.numeric<=4259, 'Inflammatory heart diseases',
						 ifelse(dat.merged$cause.numeric>=4260&dat.merged$cause.numeric<=4299, 'Other',
						 ifelse(dat.merged$cause.numeric>=4300&dat.merged$cause.numeric<=4389, 'Cerebrovascular disease',
						 ifelse(dat.merged$cause.numeric>=4390&dat.merged$cause.numeric<=4599, 'Other',
						 # Respiratory diseases and infections (4600-5199)
						 ifelse(dat.merged$cause.numeric>=4600&dat.merged$cause.numeric<=4659, 'Upper respiratory infections',
						 ifelse(dat.merged$cause.numeric>=4660&dat.merged$cause.numeric<=4669, 'Lower respiratory infections',
						 ifelse(dat.merged$cause.numeric>=4670&dat.merged$cause.numeric<=4799, 'Other',
						 ifelse(dat.merged$cause.numeric>=4800&dat.merged$cause.numeric<=4879, 'Lower respiratory infections',
						 ifelse(dat.merged$cause.numeric>=4880&dat.merged$cause.numeric<=4899, 'Other respiratory diseases',
						 ifelse(dat.merged$cause.numeric>=4900&dat.merged$cause.numeric<=4929, 'Chronic obstructive pulmonary disease',
						 ifelse(dat.merged$cause.numeric>=4930&dat.merged$cause.numeric<=4939, 'Asthma',
						 ifelse(dat.merged$cause.numeric>=4940&dat.merged$cause.numeric<=4949, 'Other',
						 ifelse(dat.merged$cause.numeric>=4950&dat.merged$cause.numeric<=4969, 'Chronic obstructive pulmonary disease',
						 ifelse(dat.merged$cause.numeric>=4970&dat.merged$cause.numeric<=5199, 'Other',
						 'NA')))))))))))))))))))))))
    
    # also add cardiovascular or respiratory diseases
    dat.merged$cause.group = 	ifelse(dat.merged$cause.numeric>=3810&dat.merged$cause.numeric<=3829,'Respiratory diseases', #'Ottis media'
                                     ifelse(dat.merged$cause.numeric>=3900&dat.merged$cause.numeric<=4599,'Cardiovascular diseases', #'Cardiovascular' proper
                                            ifelse(dat.merged$cause.numeric>=4600&dat.merged$cause.numeric<=5199,'Respiratory diseases', #'Respiratory diseases and infections proper'
                                                   'Other')))
    
    dat.merged$letter = ' '
    
  }
  
  if(x>=start_year){
    # merge cod in ICD 10 coding for broad letter coding
    dat$cause[nchar(dat$cause)==3] <- paste0(dat$cause[nchar(dat$cause)==3],'0')
    dat$letter = substr(dat$cause,1,1)
    dat.merged = merge(dat,cod.lookup.10,by.x='letter',by.y='letter',all.x=1)
    
    dat.merged$cause.group = as.character(dat.merged$cause.group)
    
    # move deaths due to weather-based heat/cold to 'Other'
    #dat.merged$cause.group = ifelse((dat.merged$cause=='X300'|dat.merged$cause=='X310'),'Other',as.character(dat.merged$cause.group))
    
    # numerical cause
    dat.merged$cause.numeric = as.numeric(as.character(substr(dat.merged$cause,2,4)))
    
    # only filter for cardiorespiratory AND include otitis media
    dat.merged.append = subset(dat.merged,letter=='H'&cause.numeric>=650&cause.numeric<=669) # otitis media
    dat.merged.append$cause.group = 'Respiratory diseases'
    dat.merged = subset(dat.merged,cause.group%in%c('Respiratory diseases', 'Cardiovascular diseases'))
    dat.merged = rbind(dat.merged,dat.merged.append)

    # cause subgroups
    dat.merged$cause.sub =
      # Cardiovascular diseases (I00-I99)
      ifelse(dat.merged$letter=='I'&dat.merged$cause.numeric>=0&dat.merged$cause.numeric<=9,		'Other cardiovascular diseases', # 'Other cardiovascular diseases'
             ifelse(dat.merged$letter=='I'&dat.merged$cause.numeric>=10&dat.merged$cause.numeric<=99,	'Other cardiovascular diseases', #'Rheumatic heart disease',
                    ifelse(dat.merged$letter=='I'&dat.merged$cause.numeric>=100&dat.merged$cause.numeric<=139,	'Other cardiovascular diseases', #'Hypertensive heart disease'
                           ifelse(dat.merged$letter=='I'&dat.merged$cause.numeric>=140&dat.merged$cause.numeric<=199,	'Other cardiovascular diseases', # 'Other cardiovascular diseases'
                                  ifelse(dat.merged$letter=='I'&dat.merged$cause.numeric>=200&dat.merged$cause.numeric<=259,	'Ischaemic heart disease', #'Ischaemic heart disease',
                                         ifelse(dat.merged$letter=='I'&dat.merged$cause.numeric>=260&dat.merged$cause.numeric<=299,	'Other cardiovascular diseases', # 'Other cardiovascular diseases'
                                                ifelse(dat.merged$letter=='I'&dat.merged$cause.numeric>=300&dat.merged$cause.numeric<=339,	'Other cardiovascular diseases', #'Inflammatory heart diseases',
                                                       ifelse(dat.merged$letter=='I'&dat.merged$cause.numeric>=340&dat.merged$cause.numeric<=379,	'Other cardiovascular diseases', # 'Other cardiovascular diseases'
                                                              ifelse(dat.merged$letter=='I'&dat.merged$cause.numeric>=380&dat.merged$cause.numeric<=389,	'Other cardiovascular diseases', #'Inflammatory heart diseases',
                                                                     ifelse(dat.merged$letter=='I'&dat.merged$cause.numeric>=390&dat.merged$cause.numeric<=399,	'Other cardiovascular diseases', # 'Other cardiovascular diseases'
                                                                            ifelse(dat.merged$letter=='I'&dat.merged$cause.numeric>=400&dat.merged$cause.numeric<=409,	'Other cardiovascular diseases', #'Inflammatory heart diseases',
                                                                                   ifelse(dat.merged$letter=='I'&dat.merged$cause.numeric>=410&dat.merged$cause.numeric<=419,	'Other cardiovascular diseases', # 'Other cardiovascular diseases'
                                                                                          ifelse(dat.merged$letter=='I'&dat.merged$cause.numeric>=420&dat.merged$cause.numeric<=429,	'Other cardiovascular diseases', #'Inflammatory heart diseases',
                                                                                                 ifelse(dat.merged$letter=='I'&dat.merged$cause.numeric>=430&dat.merged$cause.numeric<=599,	'Other cardiovascular diseases', # 'Other cardiovascular diseases'
                                                                                                        ifelse(dat.merged$letter=='I'&dat.merged$cause.numeric>=600&dat.merged$cause.numeric<=699,	'Cerebrovascular disease', #'Cerebrovascular disease',
                                                                                                               ifelse(dat.merged$letter=='I'&dat.merged$cause.numeric>=700&dat.merged$cause.numeric<=999,	'Other cardiovascular diseases', # 'Other cardiovascular diseases'
                                                                                                                      # Respiratory diseases (J00-J99)
                                                                                                                      ifelse(dat.merged$letter=='J'&dat.merged$cause.numeric>=0&dat.merged$cause.numeric<=69,		'Respiratory infections', #'Upper respiratory infections',
                                                                                                                             ifelse(dat.merged$letter=='J'&dat.merged$cause.numeric>=70&dat.merged$cause.numeric<=89,	'Other respiratory diseases', #'Other respiratory diseases',
                                                                                                                                    ifelse(dat.merged$letter=='J'&dat.merged$cause.numeric>=90&dat.merged$cause.numeric<=189,	'Respiratory infections', #'Lower respiratory infections',
                                                                                                                                           ifelse(dat.merged$letter=='J'&dat.merged$cause.numeric>=190&dat.merged$cause.numeric<=199,	'Other respiratory diseases', #'Other respiratory diseases',
                                                                                                                                                  ifelse(dat.merged$letter=='J'&dat.merged$cause.numeric>=200&dat.merged$cause.numeric<=229,	'Respiratory infections', #'Lower respiratory infections',
                                                                                                                                                         ifelse(dat.merged$letter=='J'&dat.merged$cause.numeric>=230&dat.merged$cause.numeric<=399,	'Other respiratory diseases', #'Other respiratory diseases',
                                                                                                                                                                ifelse(dat.merged$letter=='J'&dat.merged$cause.numeric>=400&dat.merged$cause.numeric<=449,	'Chronic obstructive pulmonary disease', #'Chronic obstructive pulmonary disease',
                                                                                                                                                                       ifelse(dat.merged$letter=='J'&dat.merged$cause.numeric>=450&dat.merged$cause.numeric<=469,	'Other respiratory diseases', #'Asthma',
                                                                                                                                                                              ifelse(dat.merged$letter=='J'&dat.merged$cause.numeric>=470&dat.merged$cause.numeric<=999,	'Other respiratory diseases', #'Other respiratory diseases',
                                                                                                                                                                                     ifelse(dat.merged$letter=='H'&dat.merged$cause.numeric>=650&dat.merged$cause.numeric<=669,	'Other respiratory diseases', #'Otitis media',
                                                                                                                                                                                            'NA'))))))))))))))))))))))))))
    
    # cause sub sub groups
    dat.merged$cause.sub.sub =
      # Cardiovascular diseases (I00-I99)
      ifelse(dat.merged$letter=='I'&dat.merged$cause.numeric>=0&dat.merged$cause.numeric<=9,		'Other',
             ifelse(dat.merged$letter=='I'&dat.merged$cause.numeric>=10&dat.merged$cause.numeric<=99,	'Rheumatic heart disease',
                    ifelse(dat.merged$letter=='I'&dat.merged$cause.numeric>=100&dat.merged$cause.numeric<=139,	'Hypertensive heart disease',
                           ifelse(dat.merged$letter=='I'&dat.merged$cause.numeric>=140&dat.merged$cause.numeric<=199,	'Other',
                                  ifelse(dat.merged$letter=='I'&dat.merged$cause.numeric>=200&dat.merged$cause.numeric<=259,	'Ischaemic heart disease',
                                         ifelse(dat.merged$letter=='I'&dat.merged$cause.numeric>=260&dat.merged$cause.numeric<=299,	'Other',
                                                ifelse(dat.merged$letter=='I'&dat.merged$cause.numeric>=300&dat.merged$cause.numeric<=339,	'Inflammatory heart diseases',
                                                       ifelse(dat.merged$letter=='I'&dat.merged$cause.numeric>=340&dat.merged$cause.numeric<=379,	'Other',
                                                              ifelse(dat.merged$letter=='I'&dat.merged$cause.numeric>=380&dat.merged$cause.numeric<=389,	'Inflammatory heart diseases',
                                                                     ifelse(dat.merged$letter=='I'&dat.merged$cause.numeric>=390&dat.merged$cause.numeric<=399,	'Other',
                                                                            ifelse(dat.merged$letter=='I'&dat.merged$cause.numeric>=400&dat.merged$cause.numeric<=409,	'Inflammatory heart diseases',
                                                                                   ifelse(dat.merged$letter=='I'&dat.merged$cause.numeric>=410&dat.merged$cause.numeric<=419,	'Other',
                                                                                          ifelse(dat.merged$letter=='I'&dat.merged$cause.numeric>=420&dat.merged$cause.numeric<=429,	'Inflammatory heart diseases',
                                                                                                 ifelse(dat.merged$letter=='I'&dat.merged$cause.numeric>=430&dat.merged$cause.numeric<=599,	'Other',
                                                                                                        ifelse(dat.merged$letter=='I'&dat.merged$cause.numeric>=600&dat.merged$cause.numeric<=699,	'Cerebrovascular disease',
                                                                                                               ifelse(dat.merged$letter=='I'&dat.merged$cause.numeric>=700&dat.merged$cause.numeric<=999,	'Other',
                                                                                                                      # Respiratory diseases (J00-J99)
                                                                                                                      ifelse(dat.merged$letter=='J'&dat.merged$cause.numeric>=0&dat.merged$cause.numeric<=69,		'Upper respiratory infections',
                                                                                                                             ifelse(dat.merged$letter=='J'&dat.merged$cause.numeric>=70&dat.merged$cause.numeric<=89,	'Other',
                                                                                                                                    ifelse(dat.merged$letter=='J'&dat.merged$cause.numeric>=90&dat.merged$cause.numeric<=189,	'Lower respiratory infections',
                                                                                                                                           ifelse(dat.merged$letter=='J'&dat.merged$cause.numeric>=190&dat.merged$cause.numeric<=199,	'Other',
                                                                                                                                                  ifelse(dat.merged$letter=='J'&dat.merged$cause.numeric>=200&dat.merged$cause.numeric<=229,	'Lower respiratory infections',
                                                                                                                                                         ifelse(dat.merged$letter=='J'&dat.merged$cause.numeric>=230&dat.merged$cause.numeric<=399,	'Other',
                                                                                                                                                                ifelse(dat.merged$letter=='J'&dat.merged$cause.numeric>=400&dat.merged$cause.numeric<=449,	'Chronic obstructive pulmonary disease',
                                                                                                                                                                       ifelse(dat.merged$letter=='J'&dat.merged$cause.numeric>=450&dat.merged$cause.numeric<=469,	'Asthma',
                                                                                                                                                                              ifelse(dat.merged$letter=='J'&dat.merged$cause.numeric>=470&dat.merged$cause.numeric<=999,	'Other',
                                                                                                                                                                                     ifelse(dat.merged$letter=='H'&dat.merged$cause.numeric>=650&dat.merged$cause.numeric<=669,	'Otitis media',
                                                                                                                                                                                            'NA'))))))))))))))))))))))))))
    
    
    # merge cod in ICD 10 coding
    dat.merged$cause.group = 	ifelse(dat.merged$letter=='I','Cardiovascular diseases',
                                     ifelse(dat.merged$letter=='J','Respiratory diseases',
                                            ifelse(dat.merged$letter=='H','Respiratory diseases',
                                                   'NA')))

  }
  
  # find unique values of causes of death and sub-groupings and save
  dat.unique = unique(dat.merged[c('cause.group','cause.sub','cause.sub.sub')])
  saveRDS(dat.unique,paste0(cods.folder,'cods_cardio_sub_',x))
  
  # add agegroup groupings
  dat.merged$agegroup <-
    ifelse (dat.merged$age<5,   0,
            ifelse (dat.merged$age<15,  5,
                    ifelse (dat.merged$age<25,  15,
                            ifelse (dat.merged$age<35,  25,
                                    ifelse (dat.merged$age<45,  35,
                                            ifelse (dat.merged$age<55,  45,
                                                    ifelse (dat.merged$age<65,  55,
                                                            ifelse (dat.merged$age<75,  65,
                                                                    ifelse (dat.merged$age<85,  75,
                                                                            85)))))))))
  
  # # summarise by county, year, month, sex, agegroup
  dat.summarised <- dplyr::summarise(group_by(dat.merged,cause.group,cause.sub,cause.sub.sub,fips,year,monthdth,sex,agegroup),deaths=sum(deaths))
  names(dat.summarised)[1:9] <- c('cause.group','cause.sub','cause.sub.sub','fips','year','month','sex','age','deaths')
  dat.summarised <- na.omit(dat.summarised)
  
  # add year
  dat.summarised$year <- x 
  
  # add state fips
  dat.summarised$state.fips <- substr(dat.summarised$fips,1,2)
  
  print(paste0('total deaths in year ',sum(dat$deaths),', total deaths for cardiorespiratory ',sum(dat.merged$deaths),' and with na rows removed with ',sum(dat.summarised$deaths)))
  
  print(head(dat.summarised),20)
  
  return(dat.summarised)
}

# Function to summarise a year's data for injuries
yearsummary_injuries  <- function(x=2000) {

	print(paste0('year ',x,' now being processed'))

	# load year of deaths
  	raw_mortalty_data <- '~/data/mortality/US/state/processed/cod/'
  	dat.name <- paste0(raw_mortalty_data,"deathscod",x,".dta")
  	dat <- read.dta(dat.name)

  # fix sex classification if in certain years
  if(x %in% c(2003:2010,2012)){
    dat$sex = as.integer(plyr::mapvalues(dat$sex,from=sort(unique(dat$sex)),to=c(2,1)))
  }

  # add extra label for CODs based on relevant ICD year
  start_year = 1999
  if(x<start_year) {
    # ICD 9 coding for broad cod coding
    dat$cause[nchar(dat$cause)==3] <- paste0(dat$cause[nchar(dat$cause)==3],'0')
    dat$cause.numeric = as.numeric(dat$cause)
    dat$cause.group =
		ifelse(dat$cause.numeric>=1400&dat$cause.numeric<=2399,'Cancers',
	    ifelse(dat$cause.numeric>=3810&dat$cause.numeric<=3829,'Respiratory diseases', # Otitis Media addition
        ifelse(dat$cause.numeric>=3900&dat$cause.numeric<=4599,'Cardiovascular diseases',
        ifelse(dat$cause.numeric>=4600&dat$cause.numeric<=5199,'Respiratory diseases',
        ifelse(dat$cause.numeric>=8000&dat$cause.numeric<=9999,'Injuries',
        'Other')))))

    dat$cause.group = as.character(dat$cause.group)

    # only filter for injuries
    dat.merged = subset(dat,cause.group %in% 'Injuries')

    # cause subgroups
	dat.merged$cause.sub =
                            ifelse(dat.merged$cause.numeric>=8000&dat.merged$cause.numeric<=8079, 'Unintentional',#'Railway Accidents',
							ifelse(dat.merged$cause.numeric>=8100&dat.merged$cause.numeric<=8199, 'Unintentional',#'Motor Vehicle Traffic Accidents',
							ifelse(dat.merged$cause.numeric>=8200&dat.merged$cause.numeric<=8259, 'Unintentional',#'Motor Vehicle Nontraffic Accidents',
							ifelse(dat.merged$cause.numeric>=8260&dat.merged$cause.numeric<=8299, 'Unintentional',#'Other Road Vehicle Accidents',
							ifelse(dat.merged$cause.numeric>=8300&dat.merged$cause.numeric<=8389, 'Unintentional',#'Water Transport Accidents',
							ifelse(dat.merged$cause.numeric>=8400&dat.merged$cause.numeric<=8459, 'Unintentional',#'Air and Space Transport Accidents',
							ifelse(dat.merged$cause.numeric>=8460&dat.merged$cause.numeric<=8499, 'Unintentional',#'Vehicle Accidents, Not Elsewhere Classifiable',
							ifelse(dat.merged$cause.numeric>=8500&dat.merged$cause.numeric<=8589, 'Unintentional',#'Accidental Poisoning By Drugs, Medicinal Substances, And Biologicals',
							ifelse(dat.merged$cause.numeric>=8600&dat.merged$cause.numeric<=8699, 'Unintentional',#'Accidental Poisoning By Other Solid And Liquid Substances, And Biologicals',
							ifelse(dat.merged$cause.numeric>=8700&dat.merged$cause.numeric<=8769, 'Other',#'Misadventures To Patients During Surgical And Medical Care',
							ifelse(dat.merged$cause.numeric>=8780&dat.merged$cause.numeric<=8799, 'Other',#'Non-Misadventures To Patients During Surigcal And Medical Care',
							ifelse(dat.merged$cause.numeric>=8800&dat.merged$cause.numeric<=8889, 'Unintentional', #'Accidental Falls',
							ifelse(dat.merged$cause.numeric>=8900&dat.merged$cause.numeric<=8999, 'Unintentional',#'Accidents Caused By Fire and Flames',
							ifelse(dat.merged$cause.numeric>=9000&dat.merged$cause.numeric<=9099, 'Unintentional',#'Accidents Due To Natural And Environmental Factors',
							ifelse(dat.merged$cause.numeric>=9100&dat.merged$cause.numeric<=9109, 'Unintentional',#'Accidents Caused By Submersion',
							ifelse(dat.merged$cause.numeric>=9110&dat.merged$cause.numeric<=9159, 'Unintentional',#'Accidents Caused By Suffocation And Foreign Bodies',
							ifelse(dat.merged$cause.numeric>=9160&dat.merged$cause.numeric<=9289, 'Unintentional',#'Other Accidents',
							ifelse(dat.merged$cause.numeric>=9290&dat.merged$cause.numeric<=9299, 'Other',#'Late Effects Of Accidental Injury',
							ifelse(dat.merged$cause.numeric>=9300&dat.merged$cause.numeric<=9499, 'Other',#'Complications of medical and surgical care',
							ifelse(dat.merged$cause.numeric>=9500&dat.merged$cause.numeric<=9599, 'Intentional',#'Suicide And Self-Inflicted Injury',
							ifelse(dat.merged$cause.numeric>=9600&dat.merged$cause.numeric<=9699, 'Intentional',#'Homicide And Injury Purposely Inflicted By Other Persons',
							ifelse(dat.merged$cause.numeric>=9700&dat.merged$cause.numeric<=9799, 'Other',#'Legal Intervention',
							ifelse(dat.merged$cause.numeric>=9800&dat.merged$cause.numeric<=9899, 'Other',#'Injury Undetemined Whether Accidentlally Or Purposely Inflicted',
							ifelse(dat.merged$cause.numeric>=9900&dat.merged$cause.numeric<=9999, 'Other',#'Injury Resulting From Operations Of War',
							'NA'))))))))))))))))))))))))

    # cause subsubgroups
	dat.merged$cause.sub.sub =
                            ifelse(dat.merged$cause.numeric>=8000&dat.merged$cause.numeric<=8079, 'Transport',#'Railway Accidents',
							ifelse(dat.merged$cause.numeric>=8100&dat.merged$cause.numeric<=8199, 'Transport',#'Motor Vehicle Traffic Accidents',
							ifelse(dat.merged$cause.numeric>=8200&dat.merged$cause.numeric<=8259, 'Transport',#'Motor Vehicle Nontraffic Accidents',
							ifelse(dat.merged$cause.numeric>=8260&dat.merged$cause.numeric<=8299, 'Transport',#'Other Road Vehicle Accidents',
							ifelse(dat.merged$cause.numeric>=8300&dat.merged$cause.numeric<=8389, 'Transport',#'Water Transport Accidents',
							ifelse(dat.merged$cause.numeric>=8400&dat.merged$cause.numeric<=8459, 'Transport',#'Air and Space Transport Accidents',
							ifelse(dat.merged$cause.numeric>=8460&dat.merged$cause.numeric<=8499, 'Transport',#'Vehicle Accidents, Not Elsewhere Classifiable',
							ifelse(dat.merged$cause.numeric>=8500&dat.merged$cause.numeric<=8589, 'Other unintentional',#'Accidental Poisoning By Drugs, Medicinal Substances, And Biologicals',
							ifelse(dat.merged$cause.numeric>=8600&dat.merged$cause.numeric<=8699, 'Other unintentional',#'Accidental Poisoning By Other Solid And Liquid Substances, And Biologicals',
							ifelse(dat.merged$cause.numeric>=8700&dat.merged$cause.numeric<=8769, 'Complications of medical and surgical care',#'Misadventures To Patients During Surgical And Medical Care',
							ifelse(dat.merged$cause.numeric>=8780&dat.merged$cause.numeric<=8799, 'Complications of medical and surgical care',#'Non-Misadventures To Patients During Surigcal And Medical Care',
							ifelse(dat.merged$cause.numeric>=8800&dat.merged$cause.numeric<=8889, 'Falls', #'Accidental Falls',
							ifelse(dat.merged$cause.numeric>=8900&dat.merged$cause.numeric<=8999, 'Other unintentional',#'Accidents Caused By Fire and Flames',
							ifelse(dat.merged$cause.numeric>=9000&dat.merged$cause.numeric<=9099, 'Other unintentional',#'Accidents Due To Natural And Environmental Factors',
							ifelse(dat.merged$cause.numeric>=9100&dat.merged$cause.numeric<=9109, 'Drownings',#'Accidents Caused By Submersion',
							ifelse(dat.merged$cause.numeric>=9110&dat.merged$cause.numeric<=9159, 'Other unintentional',#'Accidents Caused By Suffocation And Foreign Bodies',
							ifelse(dat.merged$cause.numeric>=9160&dat.merged$cause.numeric<=9289, 'Other unintentional',#'Other Accidents',
							ifelse(dat.merged$cause.numeric>=9290&dat.merged$cause.numeric<=9299, 'Sequelae of external causes',#'Late Effects Of Accidental Injury',
							ifelse(dat.merged$cause.numeric>=9300&dat.merged$cause.numeric<=9499, 'Complications of medical and surgical care',#'Complications of medical and surgical care',
							ifelse(dat.merged$cause.numeric>=9500&dat.merged$cause.numeric<=9599, 'Suicide',#'Suicide And Self-Inflicted Injury',
							ifelse(dat.merged$cause.numeric>=9600&dat.merged$cause.numeric<=9699, 'Assault',#'Homicide And Injury Purposely Inflicted By Other Persons',
							ifelse(dat.merged$cause.numeric>=9700&dat.merged$cause.numeric<=9799, 'Legal intervention and operations of war',#'Legal Intervention',
							ifelse(dat.merged$cause.numeric>=9800&dat.merged$cause.numeric<=9899, 'Intention undetermined',#'Injury Undetemined Whether Accidentlally Or Purposely Inflicted',
							ifelse(dat.merged$cause.numeric>=9900&dat.merged$cause.numeric<=9999, 'Legal intervention and operations of war',#'Injury Resulting From Operations Of War',
							'NA'))))))))))))))))))))))))


        dat.merged$letter = ' '

	}

	if(x>=start_year){
        # merge cod in ICD 10 coding for broad letter coding
		dat$cause[nchar(dat$cause)==3] <- paste0(dat$cause[nchar(dat$cause)==3],'0')
		dat$letter = substr(dat$cause,1,1)
		dat.merged = merge(dat,cod.lookup.10,by.x='letter',by.y='letter',all.x=1)

        dat.merged$cause.group = as.character(dat.merged$cause.group)

        # only filter for external
        dat.merged = subset(dat.merged,cause.group=='Injuries')

        # numerical cause
        dat.merged$cause.numeric = as.numeric(as.character(substr(dat.merged$cause,2,4)))

        # cause subgroups
        dat.merged$cause.sub =
                            ifelse(dat.merged$letter=='V'&dat.merged$cause.numeric>=0&dat.merged$cause.numeric<=999,	'Unintentional', # 'Transport'
                            ifelse(dat.merged$letter=='W'&dat.merged$cause.numeric>=0&dat.merged$cause.numeric<=199,	'Unintentional', # 'Falls'
							ifelse(dat.merged$letter=='W'&dat.merged$cause.numeric>=200&dat.merged$cause.numeric<=649,	'Unintentional', # 'Other unintentional'
							ifelse(dat.merged$letter=='W'&dat.merged$cause.numeric>=650&dat.merged$cause.numeric<=749,	'Unintentional', # 'Drownings'
							ifelse(dat.merged$letter=='W'&dat.merged$cause.numeric>=750&dat.merged$cause.numeric<=999,	'Unintentional', # 'Other unintentional'
                            ifelse(dat.merged$letter=='X'&dat.merged$cause.numeric>=0&dat.merged$cause.numeric<=599,	'Unintentional', # 'Other unintentional'
                            ifelse(dat.merged$letter=='X'&dat.merged$cause.numeric>=600&dat.merged$cause.numeric<=849,	'Intentional', # 'Suicide'
                            ifelse(dat.merged$letter=='X'&dat.merged$cause.numeric>=850&dat.merged$cause.numeric<=999,	'Intentional', # 'Assault'
                            ifelse(dat.merged$letter=='Y'&dat.merged$cause.numeric>=0&dat.merged$cause.numeric<=99,		'Intentional', # 'Assault'
                            ifelse(dat.merged$letter=='Y'&dat.merged$cause.numeric>=100&dat.merged$cause.numeric<=349,	'Other', # Intention undetermined
                            ifelse(dat.merged$letter=='Y'&dat.merged$cause.numeric>=350&dat.merged$cause.numeric<=389,	'Other', # Legal intervention, operations of war, military operations, and terrorism
                            ifelse(dat.merged$letter=='Y'&dat.merged$cause.numeric>=400&dat.merged$cause.numeric<=849,	'Other', # medical complications etc.
                            ifelse(dat.merged$letter=='Y'&dat.merged$cause.numeric>=850&dat.merged$cause.numeric<=899,	'Other', # Sequelae of external causes
                            'NA')))))))))))))

        # cause subsubgroups
        dat.merged$cause.sub.sub =
                            ifelse(dat.merged$letter=='V'&dat.merged$cause.numeric>=0&dat.merged$cause.numeric<=999,	'Transport',
                            ifelse(dat.merged$letter=='W'&dat.merged$cause.numeric>=0&dat.merged$cause.numeric<=199,	'Falls',
							ifelse(dat.merged$letter=='W'&dat.merged$cause.numeric>=200&dat.merged$cause.numeric<=649,	'Other unintentional', # 'exposure to mechnical forces'
							ifelse(dat.merged$letter=='W'&dat.merged$cause.numeric>=650&dat.merged$cause.numeric<=749,	'Drownings',
							ifelse(dat.merged$letter=='W'&dat.merged$cause.numeric>=750&dat.merged$cause.numeric<=999,	'Other unintentional', # 'exposure to electric current, radiation and extreme ambient air temperature and pressure'
                            ifelse(dat.merged$letter=='X'&dat.merged$cause.numeric>=0&dat.merged$cause.numeric<=599,	'Other unintentional', # encounters with forces of nature/overexertion
                            ifelse(dat.merged$letter=='X'&dat.merged$cause.numeric>=600&dat.merged$cause.numeric<=849,	'Suicide',
                            ifelse(dat.merged$letter=='X'&dat.merged$cause.numeric>=850&dat.merged$cause.numeric<=999,	'Assault',
                            ifelse(dat.merged$letter=='Y'&dat.merged$cause.numeric>=0&dat.merged$cause.numeric<=99,		'Assault',
                            ifelse(dat.merged$letter=='Y'&dat.merged$cause.numeric>=100&dat.merged$cause.numeric<=349,	'Intention undetermined', # 'event of undeterminded intent'
                            ifelse(dat.merged$letter=='Y'&dat.merged$cause.numeric>=350&dat.merged$cause.numeric<=389,	'Legal intervention and operations of war', # 'Legal intervention, operations of war, military operations, and terrorism'
                            ifelse(dat.merged$letter=='Y'&dat.merged$cause.numeric>=400&dat.merged$cause.numeric<=849,	'Complications of medical and surgical care', # medical complications etc.
                            ifelse(dat.merged$letter=='Y'&dat.merged$cause.numeric>=850&dat.merged$cause.numeric<=899,	'Sequelae of external causes', #
                            'NA')))))))))))))

	}

  	# find unique values of causes of death and sub-groupings and save
  	dat.unique = unique(dat.merged[c('cause.group','cause.sub','cause.sub.sub')])
  	saveRDS(dat.unique,paste0(cods.folder,'cods_injuries_sub_',x))

	  # add agegroup groupings
	  dat.merged$agegroup <-
		ifelse (dat.merged$age<5,   0,
				ifelse (dat.merged$age<15,  5,
						ifelse (dat.merged$age<25,  15,
								ifelse (dat.merged$age<35,  25,
										ifelse (dat.merged$age<45,  35,
												ifelse (dat.merged$age<55,  45,
														ifelse (dat.merged$age<65,  55,
																ifelse (dat.merged$age<75,  65,
																		ifelse (dat.merged$age<85,  75,
																				85)))))))))

	  # # summarise by county, year, month, sex, agegroup
	  dat.summarised <- dplyr::summarise(group_by(dat.merged,cause.group,cause.sub,cause.sub.sub,fips,year,monthdth,sex,agegroup),deaths=sum(deaths))
	  names(dat.summarised)[1:9] <- c('cause.group','cause.sub','cause.sub.sub','fips','year','month','sex','age','deaths')
	  dat.summarised <- na.omit(dat.summarised)

	  # add year
	  dat.summarised$year <- x

	  # add state fips
	  dat.summarised$state.fips <- substr(dat.summarised$fips,1,2)

	  print(paste0('total deaths in year ',sum(dat$deaths),', total deaths for injuries ',sum(dat.merged$deaths),' and with na rows removed with ',sum(dat.summarised$deaths)))

	  print(head(dat.summarised),20)

	  return(dat.summarised)
}

# Function to summarise a year's data for cancers
yearsummary_cancers  <- function(x=2000) {
  
  print(paste0('year ',x,' now being processed'))
  
  # load year of deaths
  raw_mortalty_data <- '~/data/mortality/US/state/processed/cod/'
  dat.name <- paste0(raw_mortalty_data,"deathscod",x,".dta")
  dat <- read.dta(dat.name)
  
  # fix sex classification if in certain years
  if(x %in% c(2003:2010,2012)){
    dat$sex = as.integer(plyr::mapvalues(dat$sex,from=sort(unique(dat$sex)),to=c(2,1)))
  }
  
  # add extra label for CODs based on relevant ICD year
  start_year = 1999
  if(x<start_year) {
    # ICD 9 coding for broad cod coding
    dat$cause[nchar(dat$cause)==3] <- paste0(dat$cause[nchar(dat$cause)==3],'0')
    dat$cause.numeric = as.numeric(dat$cause)
    dat$cause.group =
      ifelse(dat$cause.numeric>=1400&dat$cause.numeric<=2399,'Cancers',
             ifelse(dat$cause.numeric>=3810&dat$cause.numeric<=3829,'Respiratory diseases', # Otitis Media addition
                    ifelse(dat$cause.numeric>=3900&dat$cause.numeric<=4599,'Cardiovascular diseases',
                           ifelse(dat$cause.numeric>=4600&dat$cause.numeric<=5199,'Respiratory diseases',
                                  ifelse(dat$cause.numeric>=8000&dat$cause.numeric<=9999,'Injuries',
                                         'Other')))))
    
    dat$cause.group = as.character(dat$cause.group)
    
    # only filter for cancers
    dat.merged = subset(dat,cause.group %in% 'Cancers')
    
    # cause subgroups
    dat.merged$cause.sub =
      ifelse(dat.merged$cause.numeric>=1400&dat.merged$cause.numeric<=2089, 'Malignant',# 'Malignant,
             ifelse(dat.merged$cause.numeric>=2090&dat.merged$cause.numeric<=2399, 'Other',# 'Other',
            'NA'))
    
    # cause subsubgroups
    dat.merged$cause.sub.sub =
      ifelse(dat.merged$cause.numeric>=1400&dat.merged$cause.numeric<=1499, 'Mouth',
      ifelse(dat.merged$cause.numeric>=1500&dat.merged$cause.numeric<=1509, 'Oesophagus',
      ifelse(dat.merged$cause.numeric>=1510&dat.merged$cause.numeric<=1519, 'Stomach',
      ifelse(dat.merged$cause.numeric>=1520&dat.merged$cause.numeric<=1529, 'Other malignant', 
      ifelse(dat.merged$cause.numeric>=1530&dat.merged$cause.numeric<=1549, 'Colon and rectum', 
      ifelse(dat.merged$cause.numeric>=1550&dat.merged$cause.numeric<=1559, 'Liver',  
      ifelse(dat.merged$cause.numeric>=1560&dat.merged$cause.numeric<=1569, 'Gall bladder',  
      ifelse(dat.merged$cause.numeric>=1570&dat.merged$cause.numeric<=1579, 'Pancreas',
      ifelse(dat.merged$cause.numeric>=1580&dat.merged$cause.numeric<=1609, 'Other malignant',
      ifelse(dat.merged$cause.numeric>=1610&dat.merged$cause.numeric<=1619, 'Larynx',  
      ifelse(dat.merged$cause.numeric>=1620&dat.merged$cause.numeric<=1629, 'Trachea, bronchus and lung',  
      ifelse(dat.merged$cause.numeric>=1630&dat.merged$cause.numeric<=1719, 'Other malignant', 
      ifelse(dat.merged$cause.numeric>=1720&dat.merged$cause.numeric<=1739, 'Skin',
      ifelse(dat.merged$cause.numeric>=1740&dat.merged$cause.numeric<=1759, 'Breast',
      ifelse(dat.merged$cause.numeric>=1760&dat.merged$cause.numeric<=1789, 'Kaposis sarcoma',
      ifelse(dat.merged$cause.numeric>=1790&dat.merged$cause.numeric<=1799, 'Corpus uteri',
      ifelse(dat.merged$cause.numeric>=1800&dat.merged$cause.numeric<=1809, 'Cervix uteri',
      ifelse(dat.merged$cause.numeric>=1810&dat.merged$cause.numeric<=1819, 'Other malignant', 
      ifelse(dat.merged$cause.numeric>=1820&dat.merged$cause.numeric<=1829, 'Corpus uteri', 
      ifelse(dat.merged$cause.numeric>=1830&dat.merged$cause.numeric<=1839, 'Ovary', 
      ifelse(dat.merged$cause.numeric>=1840&dat.merged$cause.numeric<=1849, 'Other malignant',
      ifelse(dat.merged$cause.numeric>=1850&dat.merged$cause.numeric<=1859, 'Prostate', 
      ifelse(dat.merged$cause.numeric>=1860&dat.merged$cause.numeric<=1869, 'Testicular',
      ifelse(dat.merged$cause.numeric>=1870&dat.merged$cause.numeric<=1879, 'Other malignant',
      ifelse(dat.merged$cause.numeric>=1880&dat.merged$cause.numeric<=1889, 'Bladder',
      ifelse(dat.merged$cause.numeric>=1890&dat.merged$cause.numeric<=1899, 'Kidney',
      ifelse(dat.merged$cause.numeric>=1900&dat.merged$cause.numeric<=1909, 'Other malignant',
      ifelse(dat.merged$cause.numeric>=1910&dat.merged$cause.numeric<=1929, 'Brain and nervous system',
      ifelse(dat.merged$cause.numeric>=1930&dat.merged$cause.numeric<=1939, 'Thyroid',
      ifelse(dat.merged$cause.numeric>=1940&dat.merged$cause.numeric<=1999, 'Other malignant',
      ifelse(dat.merged$cause.numeric>=2000&dat.merged$cause.numeric<=2039, 'Lymphomas and myeloma',
      ifelse(dat.merged$cause.numeric>=2040&dat.merged$cause.numeric<=2089, 'Leukaemia',
      ifelse(dat.merged$cause.numeric>=2090&dat.merged$cause.numeric<=2099, 'Neuroendocrine',
      ifelse(dat.merged$cause.numeric>=2100&dat.merged$cause.numeric<=2399, 'Other',
             'NA'))))))))))))))))))))))))))))))))))
    
    
    dat.merged$letter = ' '
    
  }
  
  if(x>=start_year){
    # merge cod in ICD 10 coding for broad letter coding
    dat$cause[nchar(dat$cause)==3] <- paste0(dat$cause[nchar(dat$cause)==3],'0')
    dat$letter = substr(dat$cause,1,1)
    dat.merged = merge(dat,cod.lookup.10,by.x='letter',by.y='letter',all.x=1)
    
    dat.merged$cause.group = as.character(dat.merged$cause.group)
    
    # numerical cause
    dat.merged$cause.numeric = as.numeric(as.character(substr(dat.merged$cause,2,4)))
    
    dat.merged$cause.group = 	ifelse(dat.merged$letter=='C','Cancers',
                                     ifelse(dat.merged$letter=='D'&dat.merged$cause.numeric<=489,'Cancers',
                                            ifelse(dat.merged$letter=='D'&dat.merged$cause.numeric>489,'Other',
                                                   'NA')))
    
    # only filter for cancers
    dat.merged = subset(dat.merged,cause.group=='Cancers')
    
    # cause subgroups
    dat.merged$cause.sub = 	ifelse(dat.merged$letter=='C','Malignant',
                                     ifelse(dat.merged$letter=='D','Other',
                                                   'NA'))
    
    # cause subsubgroups
    dat.merged$cause.sub.sub =
      ifelse(dat.merged$letter=='C'&dat.merged$cause.numeric>=0  &dat.merged$cause.numeric<=149, 'Mouth',
      ifelse(dat.merged$letter=='C'&dat.merged$cause.numeric>=150&dat.merged$cause.numeric<=159, 'Oesophagus',
      ifelse(dat.merged$letter=='C'&dat.merged$cause.numeric>=160&dat.merged$cause.numeric<=169, 'Stomach',
      ifelse(dat.merged$letter=='C'&dat.merged$cause.numeric>=170&dat.merged$cause.numeric<=179, 'Other malignant',
      ifelse(dat.merged$letter=='C'&dat.merged$cause.numeric>=180&dat.merged$cause.numeric<=219, 'Colon and rectum',
      ifelse(dat.merged$letter=='C'&dat.merged$cause.numeric>=220&dat.merged$cause.numeric<=229, 'Liver',
      ifelse(dat.merged$letter=='C'&dat.merged$cause.numeric>=230&dat.merged$cause.numeric<=249, 'Gall bladder',
      ifelse(dat.merged$letter=='C'&dat.merged$cause.numeric>=250&dat.merged$cause.numeric<=259, 'Pancreas',
      ifelse(dat.merged$letter=='C'&dat.merged$cause.numeric>=260&dat.merged$cause.numeric<=319, 'Other malignant',
      ifelse(dat.merged$letter=='C'&dat.merged$cause.numeric>=320&dat.merged$cause.numeric<=329, 'Larynx',
      ifelse(dat.merged$letter=='C'&dat.merged$cause.numeric>=330&dat.merged$cause.numeric<=349, 'Trachea, bronchus and lung',
      ifelse(dat.merged$letter=='C'&dat.merged$cause.numeric>=350&dat.merged$cause.numeric<=369, 'Other malignant', # need to double check
      ifelse(dat.merged$letter=='C'&dat.merged$cause.numeric>=370&dat.merged$cause.numeric<=419, 'Other malignant',
      ifelse(dat.merged$letter=='C'&dat.merged$cause.numeric>=420&dat.merged$cause.numeric<=429, 'Other malignant', # need to double check
      ifelse(dat.merged$letter=='C'&dat.merged$cause.numeric>=430&dat.merged$cause.numeric<=449, 'Skin',
      ifelse(dat.merged$letter=='C'&dat.merged$cause.numeric>=450&dat.merged$cause.numeric<=459, 'Mesothelioma', # not in ICD9
      ifelse(dat.merged$letter=='C'&dat.merged$cause.numeric>=460&dat.merged$cause.numeric<=499, 'Other malignant',
      ifelse(dat.merged$letter=='C'&dat.merged$cause.numeric>=500&dat.merged$cause.numeric<=509, 'Breast',
      ifelse(dat.merged$letter=='C'&dat.merged$cause.numeric>=510&dat.merged$cause.numeric<=529, 'Other malignant',
      ifelse(dat.merged$letter=='C'&dat.merged$cause.numeric>=530&dat.merged$cause.numeric<=539, 'Cervix uteri',
      ifelse(dat.merged$letter=='C'&dat.merged$cause.numeric>=540&dat.merged$cause.numeric<=559, 'Corpus uteri',
      ifelse(dat.merged$letter=='C'&dat.merged$cause.numeric>=560&dat.merged$cause.numeric<=569, 'Ovary',
      ifelse(dat.merged$letter=='C'&dat.merged$cause.numeric>=570&dat.merged$cause.numeric<=609, 'Other malignant',
      ifelse(dat.merged$letter=='C'&dat.merged$cause.numeric>=610&dat.merged$cause.numeric<=619, 'Prostate',
      ifelse(dat.merged$letter=='C'&dat.merged$cause.numeric>=620&dat.merged$cause.numeric<=629, 'Testicular',
      ifelse(dat.merged$letter=='C'&dat.merged$cause.numeric>=630&dat.merged$cause.numeric<=639, 'Other malignant',
      ifelse(dat.merged$letter=='C'&dat.merged$cause.numeric>=640&dat.merged$cause.numeric<=669, 'Kidney',
      ifelse(dat.merged$letter=='C'&dat.merged$cause.numeric>=670&dat.merged$cause.numeric<=679, 'Bladder',
      ifelse(dat.merged$letter=='C'&dat.merged$cause.numeric>=680&dat.merged$cause.numeric<=699, 'Other malignant',
      ifelse(dat.merged$letter=='C'&dat.merged$cause.numeric>=700&dat.merged$cause.numeric<=729, 'Brain and nervous system',
      ifelse(dat.merged$letter=='C'&dat.merged$cause.numeric>=730&dat.merged$cause.numeric<=739, 'Thyroid',
      ifelse(dat.merged$letter=='C'&dat.merged$cause.numeric>=740&dat.merged$cause.numeric<=809, 'Other malignant',
      ifelse(dat.merged$letter=='C'&dat.merged$cause.numeric>=810&dat.merged$cause.numeric<=909, 'Lymphomas and myeloma',
      ifelse(dat.merged$letter=='C'&dat.merged$cause.numeric>=910&dat.merged$cause.numeric<=959, 'Leukaemia',
      ifelse(dat.merged$letter=='C'&dat.merged$cause.numeric>=960&dat.merged$cause.numeric<=969, 'Lymphomas and myeloma',
      ifelse(dat.merged$letter=='C'&dat.merged$cause.numeric>=970&dat.merged$cause.numeric<=999, 'Other malignant',
      ifelse(dat.merged$letter=='D'&dat.merged$cause.numeric>=0  &dat.merged$cause.numeric<=489, 'Other',
                                                                                                 'NA')))))))))))))))))))))))))))))))))))))
    
  }
  
  # find unique values of causes of death and sub-groupings and save
  dat.unique = unique(dat.merged[c('cause.group','cause.sub','cause.sub.sub')])
  saveRDS(dat.unique,paste0(cods.folder,'cods_injuries_sub_',x))
  
  # add agegroup groupings
  dat.merged$agegroup <-
    ifelse (dat.merged$age<5,   0,
            ifelse (dat.merged$age<15,  5,
                    ifelse (dat.merged$age<25,  15,
                            ifelse (dat.merged$age<35,  25,
                                    ifelse (dat.merged$age<45,  35,
                                            ifelse (dat.merged$age<55,  45,
                                                    ifelse (dat.merged$age<65,  55,
                                                            ifelse (dat.merged$age<75,  65,
                                                                    ifelse (dat.merged$age<85,  75,
                                                                            85)))))))))
  
  # # summarise by county, year, month, sex, agegroup
  dat.summarised <- dplyr::summarise(group_by(dat.merged,cause.group,cause.sub,cause.sub.sub,fips,year,monthdth,sex,agegroup),deaths=sum(deaths))
  names(dat.summarised)[1:9] <- c('cause.group','cause.sub','cause.sub.sub','fips','year','month','sex','age','deaths')
  dat.summarised <- na.omit(dat.summarised)
  
  # add year
  dat.summarised$year <- x
  
  # add state fips
  dat.summarised$state.fips <- substr(dat.summarised$fips,1,2)
  
  print(paste0('total deaths in year ',sum(dat$deaths),', total deaths for cancers ',sum(dat.merged$deaths),' and with na rows removed with ',sum(dat.summarised$deaths)))
  
  print(head(dat.summarised),20)
  
  return(dat.summarised)
}

# Function to summarise a year's data for ghe causes
prepare_ghe  <- function(x=2000) {
  
  print(paste0('year ',x,' now being processed'))
  
  # load year of deaths
  raw_mortalty_data <- '~/data/mortality/US/state/processed/cod/'
  dat.name <- paste0(raw_mortalty_data,"deathscod",x,".dta")
  dat <- read.dta(dat.name)
  
  # fix sex classification if in certain years
  if(x %in% c(2003:2010,2012)){
    dat$sex = as.integer(plyr::mapvalues(dat$sex,from=sort(unique(dat$sex)),to=c(2,1)))
  }
  
  # add extra label for CODs based on relevant ICD year
  start_year = 1999
  if(x<start_year) {
    # TO DO (not done to date as focusing on ICD-10 as of end of 2021)
  }
  
  if(x>=start_year){
    # merge cod in ICD 10 coding for broad letter coding
    dat$cause[nchar(dat$cause)==3] <- paste0(dat$cause[nchar(dat$cause)==3],'0')
    dat$letter = substr(dat$cause,1,1)
    
    causes = dat
    
    # numerical cause
    causes$number = as.numeric(as.character(substr(causes$cause,2,4)))
    
    # cause groups (code generated below originally by Theo Rashid 2021)
    causes <- causes %>%
      mutate(
        ghe_level2 = case_when(
          # Ill-defined
          letter == "R" & number >= 0   & number <= 949 ~ "Ill-defined diseases",
          letter == "R" & number >= 960 & number <= 999 ~ "Ill-defined diseases",
          letter == "Y" & number >= 100 & number <= 349 ~ "Ill-defined injuries/accidents",
          letter == "Y" & number == 872                 ~ "Ill-defined injuries/accidents",
          
          letter == "A" & number >= 0   & number <= 999 ~ "Infectious and parasitic diseases",
          letter == "B" & number >= 0   & number <= 999 ~ "Infectious and parasitic diseases",
          letter == "G" & number >= 0   & number <= 9   ~ "Infectious and parasitic diseases",
          letter == "G" & number >= 30  & number <= 49  ~ "Infectious and parasitic diseases",
          letter == "G" & number >= 140 & number <= 149 ~ "Infectious and parasitic diseases",
          letter == "N" & number >= 700 & number <= 739 ~ "Infectious and parasitic diseases",
          
          letter == "J" & number >= 0   & number <= 69  ~ "Respiratory infections",
          letter == "J" & number >= 90  & number <= 189 ~ "Respiratory infections",
          letter == "J" & number >= 200 & number <= 229 ~ "Respiratory infections",
          letter == "H" & number >= 650 & number <= 669 ~ "Respiratory infections",
          
          letter == "O" & number >= 0   & number <= 999 ~ "Maternal conditions",
          
          letter == "P" & number >= 0   & number <= 969 ~ "Perinatal conditions",
          
          letter == "E" & number >= 0   & number <= 29  ~ "Nutritional deficiencies",
          letter == "E" & number >= 400 & number <= 469 ~ "Nutritional deficiencies",
          letter == "E" & number >= 500 & number <= 509 ~ "Nutritional deficiencies",
          letter == "D" & number >= 500 & number <= 539 ~ "Nutritional deficiencies",
          letter == "D" & number == 649                 ~ "Nutritional deficiencies",
          letter == "E" & number >= 510 & number <= 649 ~ "Nutritional deficiencies",
          
          
          letter == "C" & number >= 0   & number <= 979 ~ "Malignant neoplasms",
          
          letter == "D" & number >= 0   & number <= 489 ~ "Other neoplasms",
          
          letter == "E" & number >= 100 & number <= 149 ~ "Diabetes mellitus",
          
          letter == "D" & number >= 550 & number <= 648 ~ "Endocrine disorders",
          letter == "D" & number >= 650 & number <= 899 ~ "Endocrine disorders",
          letter == "E" & number >= 30  & number <= 79  ~ "Endocrine disorders",
          letter == "E" & number >= 150 & number <= 169 ~ "Endocrine disorders",
          letter == "E" & number >= 200 & number <= 349 ~ "Endocrine disorders",
          letter == "E" & number >= 650 & number <= 889 ~ "Endocrine disorders",
          
          letter == "F" & number >= 10  & number <= 999 ~ "Neuropsychiatric conditions",
          letter == "G" & number >= 60  & number <= 139 ~ "Neuropsychiatric conditions",
          letter == "G" & number >= 150 & number <= 989 ~ "Neuropsychiatric conditions",
          letter == "X" & number >= 410 & number <= 429 ~ "Neuropsychiatric conditions",
          letter == "X" & number >= 450 & number <= 459 ~ "Neuropsychiatric conditions",
          
          letter == "H" & number >= 0   & number <= 619 ~ "Sense organ diseases",
          letter == "H" & number >= 680 & number <= 939 ~ "Sense organ diseases",
          
          letter == "I" & number >= 0   & number <= 999 ~ "Cardiovascular diseases",
          
          letter == "J" & number >= 300 & number <= 989 ~ "Respiratory diseases",
          
          letter == "K" & number >= 200 & number <= 929 ~ "Digestive diseases",
          
          letter == "N" & number >= 0   & number <= 649 ~ "Genitourinary diseases",
          letter == "N" & number >= 750 & number <= 989 ~ "Genitourinary diseases",
          
          letter == "L" & number >= 0   & number <= 989 ~ "Skin diseases",
          
          letter == "M" & number >= 0   & number <= 999 ~ "Musculoskeletal diseases",
          
          letter == "Q" & number >= 0   & number <= 999 ~ "Congenital anomalies",
          
          letter == "K" & number >= 0   & number <= 149 ~ "Oral conditions",
          
          letter == "R" & number >= 950 & number <= 959 ~ "Sudden infant death syndrome",
          
          
          letter == "V" & number >= 0   & number <= 999 ~ "Unintentional injuries",
          letter == "W" & number >= 0   & number <= 999 ~ "Unintentional injuries",
          letter == "X" & number >= 0   & number <= 409 ~ "Unintentional injuries",
          letter == "X" & number >= 430 & number <= 449 ~ "Unintentional injuries",
          letter == "X" & number >= 460 & number <= 599 ~ "Unintentional injuries",
          letter == "Y" & number >= 400 & number <= 869 ~ "Unintentional injuries",
          letter == "Y" & number >= 880 & number <= 899 ~ "Unintentional injuries",
          
          letter == "X" & number >= 600 & number <= 999 ~ "Intentional injuries",
          letter == "Y" & number >= 0   & number <= 99  ~ "Intentional injuries",
          letter == "Y" & number >= 350 & number <= 369 ~ "Intentional injuries",
          letter == "Y" & number == 870                 ~ "Intentional injuries",
          letter == "Y" & number == 871                 ~ "Intentional injuries",
          letter == "U" & number >= 0   & number <= 19  ~ "Intentional injuries",
          
          
          TRUE ~ NA_character_
        )
      )
    
    causes <- causes %>%
      mutate(
        ghe_level1 = case_when(
          ghe_level2 == "Infectious and parasitic diseases" ~ "Communicable, maternal, perinatal and nutritional conditions",
          ghe_level2 == "Respiratory infections" ~ "Communicable, maternal, perinatal and nutritional conditions",
          ghe_level2 == "Maternal conditions" ~ "Communicable, maternal, perinatal and nutritional conditions",
          ghe_level2 == "Perinatal conditions" ~ "Communicable, maternal, perinatal and nutritional conditions",
          ghe_level2 == "Nutritional deficiencies" ~ "Communicable, maternal, perinatal and nutritional conditions",
          
          ghe_level2 == "Malignant neoplasms" ~ "Noncommunicable diseases",
          ghe_level2 == "Other neoplasms" ~ "Noncommunicable diseases",
          ghe_level2 == "Diabetes mellitus" ~ "Noncommunicable diseases",
          ghe_level2 == "Endocrine disorders" ~ "Noncommunicable diseases",
          ghe_level2 == "Neuropsychiatric conditions" ~ "Noncommunicable diseases",
          ghe_level2 == "Sense organ diseases" ~ "Noncommunicable diseases",
          ghe_level2 == "Cardiovascular diseases" ~ "Noncommunicable diseases",
          ghe_level2 == "Respiratory diseases" ~ "Noncommunicable diseases",
          ghe_level2 == "Digestive diseases" ~ "Noncommunicable diseases",
          ghe_level2 == "Genitourinary diseases" ~ "Noncommunicable diseases",
          ghe_level2 == "Skin diseases" ~ "Noncommunicable diseases",
          ghe_level2 == "Musculoskeletal diseases" ~ "Noncommunicable diseases",
          ghe_level2 == "Congenital anomalies" ~ "Noncommunicable diseases",
          ghe_level2 == "Oral conditions" ~ "Noncommunicable diseases",
          ghe_level2 == "Sudden infant death syndrome" ~ "Noncommunicable diseases",
          ghe_level2 == "Ill-defined diseases" ~ "Noncommunicable diseases",
          
          ghe_level2 == "Unintentional injuries" ~ "Injuries",
          ghe_level2 == "Intentional injuries" ~ "Injuries",
          ghe_level2 == "Ill-defined injuries/accidents" ~ "Injuries",
          
          TRUE ~ NA_character_
        )
      )
    
    causes <- causes %>%
      mutate(
        ghe_level3 = case_when(
          # Ill-defined and garbage
          letter == "R" & number >= 0   & number <= 949 ~ "Ill-defined diseases",
          letter == "R" & number >= 960 & number <= 999 ~ "Ill-defined diseases",
          letter == "Y" & number >= 100 & number <= 349 ~ "Ill-defined injuries/accidents",
          letter == "Y" & number == 872                 ~ "Ill-defined injuries/accidents",
          letter == "C" & number >= 760 & number <= 769 ~ "Garbage cancer",
          letter == "C" & number >= 800 & number <= 809 ~ "Garbage cancer",
          letter == "C" & number >= 970 & number <= 979 ~ "Garbage cancer",
          letter == "I" & number == 472                 ~ "Garbage CVD",
          letter == "I" & number == 490                 ~ "Garbage CVD",
          letter == "I" & number >= 460 & number <= 469 ~ "Cardiac arrest",
          letter == "I" & number >= 500 & number <= 509 ~ "Heart failure",
          letter == "I" & number >= 514 & number <= 516 ~ "Garbage CVD",
          letter == "I" & number == 519                 ~ "Garbage CVD",
          letter == "I" & number == 709                 ~ "Garbage CVD",
          
          
          letter == "A" & number >= 150 & number <= 199 ~ "Tuberculosis",
          letter == "B" & number >= 900 & number <= 909 ~ "Tuberculosis",
          letter == "A" & number >= 500 & number <= 649 ~ "STDs excluding HIV",
          letter == "N" & number >= 700 & number <= 739 ~ "STDs excluding HIV",
          letter == "B" & number >= 200 & number <= 249 ~ "HIV/AIDS",
          letter == "A" & number >= 0   & number <= 19  ~ "Diarrhoeal diseases",
          letter == "A" & number >= 30  & number <= 49  ~ "Diarrhoeal diseases",
          letter == "A" & number >= 60  & number <= 99  ~ "Diarrhoeal diseases",
          letter == "A" & number >= 330 & number <= 379 ~ "Childhood-cluster diseases",
          letter == "A" & number >= 800 & number <= 809 ~ "Childhood-cluster diseases",
          letter == "B" & number >= 50  & number <= 59  ~ "Childhood-cluster diseases",
          letter == "B" & number >= 910 & number <= 919 ~ "Childhood-cluster diseases",
          letter == "G" & number >= 140 & number <= 149 ~ "Childhood-cluster diseases",
          letter == "A" & number >= 390 & number <= 399 ~ "Meningitis",
          letter == "G" & number >= 0   & number <= 9   ~ "Meningitis",
          letter == "G" & number >= 30  & number <= 39  ~ "Meningitis",
          letter == "B" & number >= 160 & number <= 170 ~ "Hepatitis B",
          letter == "B" & number >= 172 & number <= 181 ~ "Hepatitis B",
          letter == "B" & number >= 183 & number <= 199 ~ "Hepatitis B",
          letter == "B" & number == 171                 ~ "Hepatitis C",
          letter == "B" & number == 182                 ~ "Hepatitis C",
          letter == "B" & number >= 500 & number <= 549 ~ "Malaria",
          letter == "B" & number >= 550 & number <= 579 ~ "Tropical-cluster diseases",
          letter == "B" & number >= 650 & number <= 659 ~ "Tropical-cluster diseases",
          letter == "B" & number >= 730 & number <= 742 ~ "Tropical-cluster diseases",
          letter == "A" & number >= 300 & number <= 309 ~ "Leprosy",
          letter == "A" & number >= 900 & number <= 919 ~ "Dengue",
          letter == "A" & number == 380                 ~ "Japanese encephalitis",
          letter == "A" & number >= 710 & number <= 719 ~ "Trachoma",
          letter == "B" & number >= 760 & number <= 819 ~ "Intestinal nematode infections",
          ghe_level2 == "Infectious and parasitic diseases" ~ "Other infectious diseases",
          
          letter == "J" & number >= 90  & number <= 189 ~ "Lower respiratory infections",
          letter == "J" & number >= 200 & number <= 229 ~ "Lower respiratory infections",
          letter == "J" & number >= 0   & number <= 69  ~ "Upper respiratory infections",
          letter == "H" & number >= 650 & number <= 669 ~ "Otitis media",
          
          letter == "O" & number >= 440 & number <= 469 ~ "Maternal haemorrhage",
          letter == "O" & number >= 670 & number <= 679 ~ "Maternal haemorrhage",
          letter == "O" & number >= 720 & number <= 729 ~ "Maternal haemorrhage",
          letter == "O" & number >= 850 & number <= 869 ~ "Maternal sepsis",
          letter == "O" & number >= 100 & number <= 169 ~ "Hypertensive disorders",
          letter == "O" & number >= 640 & number <= 669 ~ "Obstructed labour",
          letter == "O" & number >= 0   & number <= 79  ~ "Abortion",
          ghe_level2 == "Maternal conditions" ~ "Other maternal conditions",
          
          letter == "P" & number >= 50  & number <= 59  ~ "Low birth weight",
          letter == "P" & number >= 70  & number <= 79  ~ "Low birth weight",
          letter == "P" & number >= 220 & number <= 229 ~ "Low birth weight",
          letter == "P" & number >= 270 & number <= 289 ~ "Low birth weight",
          letter == "P" & number >= 30  & number <= 39  ~ "Birth asphyxia and birth trauma",
          letter == "P" & number >= 100 & number <= 159 ~ "Birth asphyxia and birth trauma",
          letter == "P" & number >= 200 & number <= 219 ~ "Birth asphyxia and birth trauma",
          letter == "P" & number >= 240 & number <= 269 ~ "Birth asphyxia and birth trauma",
          letter == "P" & number >= 290 & number <= 299 ~ "Birth asphyxia and birth trauma",
          ghe_level2 == "Perinatal conditions" ~ "Other perinatal conditions",
          
          letter == "E" & number >= 400 & number <= 469 ~ "Protein-energy malnutrition",
          letter == "E" & number >= 0   & number <= 29  ~ "Iodine deficiency",
          letter == "E" & number >= 500 & number <= 509 ~ "Vitamin A deficiency",
          letter == "D" & number >= 500 & number <= 509 ~ "Iron-deficiency anaemia",
          letter == "D" & number == 649                 ~ "Iron-deficiency anaemia",
          ghe_level2 == "Nutritional deficiencies" ~ "Other nutritional conditions",
          
          
          letter == "C" & number >= 0   & number <= 149 ~ "Mouth and oropharynx cancers",
          letter == "C" & number >= 150 & number <= 159 ~ "Oesophagus cancer",
          letter == "C" & number >= 160 & number <= 169 ~ "Stomach cancer",
          letter == "C" & number >= 180 & number <= 219 ~ "Colon and rectum cancers",
          letter == "C" & number >= 220 & number <= 229 ~ "Liver cancer",
          letter == "C" & number >= 250 & number <= 259 ~ "Pancreas cancer",
          letter == "C" & number >= 330 & number <= 349 ~ "Trachea, bronchus, lung cancers",
          letter == "C" & number >= 430 & number <= 449 ~ "Melanoma and other skin cancers",
          letter == "C" & number >= 500 & number <= 509 ~ "Breast cancer",
          letter == "C" & number >= 530 & number <= 539 ~ "Cervix uteri cancer",
          letter == "C" & number >= 540 & number <= 559 ~ "Corpus uteri cancer",
          letter == "C" & number >= 560 & number <= 569 ~ "Ovary cancer",
          letter == "C" & number >= 610 & number <= 619 ~ "Prostate cancer",
          letter == "C" & number >= 620 & number <= 629 ~ "Testicular cancer",
          letter == "C" & number >= 640 & number <= 669 ~ "Kidney and ureter cancer",
          letter == "C" & number >= 670 & number <= 679 ~ "Bladder cancer",
          letter == "C" & number >= 700 & number <= 729 ~ "Brain and nervous system cancers",
          letter == "C" & number >= 230 & number <= 249 ~ "Gallbladder and biliary tract cancer",
          letter == "C" & number >= 320 & number <= 329 ~ "Larynx cancer",
          letter == "C" & number >= 730 & number <= 739 ~ "Thyroid cancer",
          letter == "C" & number >= 450 & number <= 459 ~ "Mesothelioma",
          letter == "C" & number >= 810 & number <= 909 ~ "Lymphomas, multiple myeloma",
          letter == "C" & number >= 960 & number <= 969 ~ "Lymphomas, multiple myeloma",
          letter == "C" & number >= 910 & number <= 959 ~ "Leukaemia",
          ghe_level2 == "Malignant neoplasms" ~ "Other malignant neoplasms",
          
          ghe_level2 == "Other neoplasms" ~ "Other neoplasms",
          
          ghe_level2 == "Diabetes mellitus" ~ "Diabetes mellitus",
          
          ghe_level2 == "Endocrine disorders" ~ "Endocrine disorders",
          
          letter == "F" & number >= 320 & number <= 339 ~ "Unipolar depressive disorders",
          letter == "F" & number >= 300 & number <= 319 ~ "Bipolar disorder",
          letter == "F" & number >= 200 & number <= 299 ~ "Schizophrenia",
          letter == "G" & number >= 400 & number <= 419 ~ "Epilepsy",
          letter == "F" & number >= 100 & number <= 109 ~ "Alcohol use disorders",
          letter == "X" & number >= 450 & number <= 459 ~ "Alcohol use disorders",
          letter == "F" & number >= 10  & number <= 19  ~ "Alzheimer and other dementias",
          letter == "F" & number >= 30  & number <= 39  ~ "Alzheimer and other dementias",
          letter == "G" & number >= 300 & number <= 319 ~ "Alzheimer and other dementias",
          letter == "G" & number >= 200 & number <= 219 ~ "Parkinson disease",
          letter == "G" & number >= 350 & number <= 359 ~ "Multiple sclerosis",
          letter == "F" & number >= 110 & number <= 169 ~ "Drug use disorders",
          letter == "F" & number >= 180 & number <= 199 ~ "Drug use disorders",
          letter == "X" & number >= 410 & number <= 429 ~ "Drug use disorders",
          letter == "F" & number == 431                 ~ "Post-traumatic stress disorder",
          letter == "F" & number >= 420 & number <= 429 ~ "Obsessive-compulsive disorder",
          letter == "F" & number == 400 | number == 410 ~ "Panic disorder",
          letter == "F" & number >= 510 & number <= 519 ~ "Insomnia (primary)",
          letter == "G" & number >= 430 & number <= 439 ~ "Migraine",
          letter == "F" & number >= 700 & number <= 799 ~ "Mental Retardation",
          ghe_level2 == "Neuropsychiatric conditions" ~ "Other neuropsychiatric disorders",
          
          letter == "H" & number >= 400 & number <= 409 ~ "Glaucoma",
          letter == "H" & number >= 250 & number <= 269 ~ "Cataracts",
          letter == "H" & number == 524                 ~ "Vision disorders, age-related",
          letter == "H" & number >= 900 & number <= 919 ~ "Hearing loss, adult onset",
          ghe_level2 == "Sense organ diseases" ~ "Other sense organ disorders",
          
          letter == "I" & number >= 10  & number <= 99  ~ "Rheumatic heart disease",
          letter == "I" & number >= 100 & number <= 139 ~ "Hypertensive heart disease",
          letter == "I" & number >= 200 & number <= 259 ~ "Ischaemic heart disease",
          letter == "I" & number >= 600 & number <= 699 ~ "Cerebrovascular disease",
          letter == "I" & number >= 300 & number <= 339 ~ "Inflammatory heart diseases",
          letter == "I" & number >= 380 & number <= 389 ~ "Inflammatory heart diseases",
          letter == "I" & number >= 400 & number <= 409 ~ "Inflammatory heart diseases",
          letter == "I" & number >= 420 & number <= 429 ~ "Inflammatory heart diseases",
          ghe_level2 == "Cardiovascular diseases" ~ "Other cardiovascular diseases",
          
          letter == "J" & number >= 400 & number <= 449 ~ "Chronic obstructive pulmonary disease",
          letter == "J" & number >= 450 & number <= 469 ~ "Asthma",
          ghe_level2 == "Respiratory diseases" ~ "Other respiratory diseases",
          
          letter == "K" & number >= 250 & number <= 279 ~ "Peptic ulcer disease",
          letter == "K" & number >= 700 & number <= 709 ~ "Cirrhosis of the liver",
          letter == "K" & number >= 740 & number <= 749 ~ "Cirrhosis of the liver",
          letter == "K" & number >= 350 & number <= 379 ~ "Appendicitis",
          letter == "K" & number >= 290 & number <= 299 ~ "Gastritis and duodenitis",
          letter == "K" & number >= 560 & number <= 569 ~ "Paralytic ileus and intestinal obstruction",
          letter == "K" & number >= 500 & number <= 529 ~ "Inflammatory bowel disease",
          letter == "K" & number == 580                 ~ "Inflammatory bowel disease",
          letter == "K" & number >= 800 & number <= 839 ~ "Gallbladder and biliary diseases",
          letter == "K" & number >= 850 & number <= 869 ~ "Pancreatitis",
          ghe_level2 == "Digestive diseases" ~ "Other digestive diseases",
          
          letter == "N" & number >= 0   & number <= 199 ~ "Nephritis and nephrosis",
          letter == "N" & number >= 400 & number <= 409 ~ "Benign prostatic hypertrophy",
          ghe_level2 == "Genitourinary diseases" ~ "Other genitourinary system diseases",
          
          ghe_level2 == "Skin diseases" ~ "Skin diseases",
          
          letter == "M" & number >= 50  & number <= 69  ~ "Rheumatoid arthritis",
          letter == "M" & number >= 150 & number <= 199 ~ "Osteoarthritis",
          letter == "M" & number >= 100 & number <= 109 ~ "Gout",
          letter == "M" & number >= 450 & number <= 489 ~ "Back pain",
          letter == "M" & number >= 540 & number <= 541 ~ "Back pain",
          letter == "M" & number >= 543 & number <= 549 ~ "Back pain",
          ghe_level2 == "Musculoskeletal diseases" ~ "Other musculoskeletal diseases",
          
          letter == "Q" & number >= 792 & number <= 795 ~ "Abdominal wall defect",
          letter == "Q" & number >= 0   & number <= 9   ~ "Anencephaly",
          letter == "Q" & number >= 420 & number <= 429 ~ "Anorectal atresia",
          letter == "Q" & number >= 360 & number <= 369 ~ "Cleft lip",
          letter == "Q" & number >= 350 & number <= 359 ~ "Cleft palate",
          letter == "Q" & number >= 370 & number <= 379 ~ "Cleft palate",
          letter == "Q" & number >= 390 & number <= 391 ~ "Oesophageal atresia",
          letter == "Q" & number >= 600 & number <= 609 ~ "Renal agenesis",
          letter == "Q" & number >= 900 & number <= 909 ~ "Down syndrome",
          letter == "Q" & number >= 200 & number <= 289 ~ "Congenital heart anomalies",
          letter == "Q" & number >= 50  & number <= 59  ~ "Spina bifida",
          ghe_level2 == "Congenital anomalies" ~ "Other congenital anomalies",
          
          letter == "K" & number >= 20  & number <= 29  ~ "Dental caries",
          letter == "K" & number >= 50  & number <= 59  ~ "Periodontal disease",
          ghe_level2 == "Oral conditions" ~ "Other oral diseases",
          
          ghe_level2 == "Sudden infant death syndrome" ~ "Sudden infant death syndrome",
          
          
          letter == "V" & number >= 0   & number <= 999 ~ "Road traffic accidents",
          letter == "Y" & number == 850                 ~ "Road traffic accidents",
          letter == "X" & number >= 400 & number <= 409 ~ "Poisonings",
          letter == "X" & number >= 430 & number <= 449 ~ "Poisonings",
          letter == "X" & number >= 460 & number <= 499 ~ "Poisonings",
          letter == "W" & number >= 0   & number <= 199 ~ "Falls",
          letter == "X" & number >= 0   & number <= 99  ~ "Fires",
          letter == "W" & number >= 650 & number <= 749 ~ "Drownings",
          letter == "W" & number >= 200 & number <= 389 ~ "Exposure to mechanical forces",
          letter == "W" & number >= 400 & number <= 439 ~ "Exposure to mechanical forces",
          letter == "W" & number >= 450 & number <= 469 ~ "Exposure to mechanical forces",
          letter == "W" & number >= 490 & number <= 529 ~ "Exposure to mechanical forces",
          letter == "W" & number >= 750 & number <= 769 ~ "Exposure to mechanical forces",
          letter == "X" & number >= 300 & number <= 399 ~ "Exposure to forces of nature",
          ghe_level2 == "Unintentional injuries" ~ "Other unintentional injuries",
          
          letter == "X" & number >= 600 & number <= 849 ~ "Self-inflicted injuries",
          letter == "Y" & number == 870                 ~ "Self-inflicted injuries",
          letter == "X" & number >= 850 & number <= 999 ~ "Violence",
          letter == "Y" & number >= 0   & number <= 99  ~ "Violence",
          letter == "Y" & number == 871                 ~ "Violence",
          letter == "U" & number >= 0   & number <= 19  ~ "Violence",
          letter == "Y" & number >= 360 & number <= 369 ~ "War",
          ghe_level2 == "Intentional injuries" ~ "Other intentional injuries",
          
          
          TRUE ~ NA_character_
        )
      ) 
    
  }
  
  # summarise by ghe levels and take top 10 values
  # causes_summarised = causes %>%
  #   dplyr::rename(agegroup=age) %>%
  #   group_by(year,sex,agegroup,ghe_level2) %>%
  #   dplyr::summarise(deaths=sum(deaths)) %>%
  #   group_by(year,sex, agegroup) %>%
  #   dplyr::mutate(perc = round(100*deaths / sum(deaths), 2)) %>%
  #   arrange(year, sex, agegroup,desc(deaths)) %>%
  #   slice(1:10)
  
  causes_summarised = causes %>%
    dplyr::rename(agegroup=age) %>%
    group_by(year,sex,ghe_level3) %>%
    dplyr::summarise(deaths=sum(deaths)) %>%
    group_by(year,sex) %>%
    dplyr::mutate(perc = round(100*deaths / sum(deaths), 1)) %>%
    arrange(year, sex, desc(deaths)) # %>%
    # slice(1:10)
  
  print(paste0(x,': total deaths in year = ',sum(dat$deaths),
               ', total deaths after processing causes = ',sum(causes$deaths),
               ', total deaths at end ',sum(causes_summarised$deaths),
               ' (',round(100*((sum(causes_summarised$deaths)/sum(causes$deaths))),2),'% of total)'))
  
  return(causes_summarised)
}

# Function to examine a year's data for ghe causes
examine_ghe <- function(x=2000) {
  
  print(paste0('year ',x,' now being processed'))
  
  # load year of deaths
  raw_mortalty_data <- '~/data/mortality/US/state/processed/cod/'
  dat.name <- paste0(raw_mortalty_data,"deathscod",x,".dta")
  dat <- read.dta(dat.name)
  
  # fix sex classification if in certain years
  if(x %in% c(2003:2010,2012)){
    dat$sex = as.integer(plyr::mapvalues(dat$sex,from=sort(unique(dat$sex)),to=c(2,1)))
  }
  
  # add extra label for CODs based on relevant ICD year
  start_year = 1999
  if(x<start_year) {
    # TO DO (not done to date as focusing on ICD-10 as of end of 2021)
  }
  
  if(x>=start_year){
    # merge cod in ICD 10 coding for broad letter coding
    dat$cause[nchar(dat$cause)==3] <- paste0(dat$cause[nchar(dat$cause)==3],'0')
    dat$letter = substr(dat$cause,1,1)
    
    causes = dat
    
    # numerical cause
    causes$number = as.numeric(as.character(substr(causes$cause,2,4)))
    
    # cause groups (code generated below originally by Theo Rashid 2021)
    causes <- causes %>%
      mutate(
        ghe_level2 = case_when(
          # Ill-defined
          letter == "R" & number >= 0   & number <= 949 ~ "Ill-defined diseases",
          letter == "R" & number >= 960 & number <= 999 ~ "Ill-defined diseases",
          letter == "Y" & number >= 100 & number <= 349 ~ "Ill-defined injuries/accidents",
          letter == "Y" & number == 872                 ~ "Ill-defined injuries/accidents",
          
          letter == "A" & number >= 0   & number <= 999 ~ "Infectious and parasitic diseases",
          letter == "B" & number >= 0   & number <= 999 ~ "Infectious and parasitic diseases",
          letter == "G" & number >= 0   & number <= 9   ~ "Infectious and parasitic diseases",
          letter == "G" & number >= 30  & number <= 49  ~ "Infectious and parasitic diseases",
          letter == "G" & number >= 140 & number <= 149 ~ "Infectious and parasitic diseases",
          letter == "N" & number >= 700 & number <= 739 ~ "Infectious and parasitic diseases",
          
          letter == "J" & number >= 0   & number <= 69  ~ "Respiratory infections",
          letter == "J" & number >= 90  & number <= 189 ~ "Respiratory infections",
          letter == "J" & number >= 200 & number <= 229 ~ "Respiratory infections",
          letter == "H" & number >= 650 & number <= 669 ~ "Respiratory infections",
          
          letter == "O" & number >= 0   & number <= 999 ~ "Maternal conditions",
          
          letter == "P" & number >= 0   & number <= 969 ~ "Perinatal conditions",
          
          letter == "E" & number >= 0   & number <= 29  ~ "Nutritional deficiencies",
          letter == "E" & number >= 400 & number <= 469 ~ "Nutritional deficiencies",
          letter == "E" & number >= 500 & number <= 509 ~ "Nutritional deficiencies",
          letter == "D" & number >= 500 & number <= 539 ~ "Nutritional deficiencies",
          letter == "D" & number == 649                 ~ "Nutritional deficiencies",
          letter == "E" & number >= 510 & number <= 649 ~ "Nutritional deficiencies",
          
          
          letter == "C" & number >= 0   & number <= 979 ~ "Malignant neoplasms",
          
          letter == "D" & number >= 0   & number <= 489 ~ "Other neoplasms",
          
          letter == "E" & number >= 100 & number <= 149 ~ "Diabetes mellitus",
          
          letter == "D" & number >= 550 & number <= 648 ~ "Endocrine disorders",
          letter == "D" & number >= 650 & number <= 899 ~ "Endocrine disorders",
          letter == "E" & number >= 30  & number <= 79  ~ "Endocrine disorders",
          letter == "E" & number >= 150 & number <= 169 ~ "Endocrine disorders",
          letter == "E" & number >= 200 & number <= 349 ~ "Endocrine disorders",
          letter == "E" & number >= 650 & number <= 889 ~ "Endocrine disorders",
          
          letter == "F" & number >= 10  & number <= 999 ~ "Neuropsychiatric conditions",
          letter == "G" & number >= 60  & number <= 139 ~ "Neuropsychiatric conditions",
          letter == "G" & number >= 150 & number <= 989 ~ "Neuropsychiatric conditions",
          letter == "X" & number >= 410 & number <= 429 ~ "Neuropsychiatric conditions",
          letter == "X" & number >= 450 & number <= 459 ~ "Neuropsychiatric conditions",
          
          letter == "H" & number >= 0   & number <= 619 ~ "Sense organ diseases",
          letter == "H" & number >= 680 & number <= 939 ~ "Sense organ diseases",
          
          letter == "I" & number >= 0   & number <= 999 ~ "Cardiovascular diseases",
          
          letter == "J" & number >= 300 & number <= 989 ~ "Respiratory diseases",
          
          letter == "K" & number >= 200 & number <= 929 ~ "Digestive diseases",
          
          letter == "N" & number >= 0   & number <= 649 ~ "Genitourinary diseases",
          letter == "N" & number >= 750 & number <= 989 ~ "Genitourinary diseases",
          
          letter == "L" & number >= 0   & number <= 989 ~ "Skin diseases",
          
          letter == "M" & number >= 0   & number <= 999 ~ "Musculoskeletal diseases",
          
          letter == "Q" & number >= 0   & number <= 999 ~ "Congenital anomalies",
          
          letter == "K" & number >= 0   & number <= 149 ~ "Oral conditions",
          
          letter == "R" & number >= 950 & number <= 959 ~ "Sudden infant death syndrome",
          
          
          letter == "V" & number >= 0   & number <= 999 ~ "Unintentional injuries",
          letter == "W" & number >= 0   & number <= 999 ~ "Unintentional injuries",
          letter == "X" & number >= 0   & number <= 409 ~ "Unintentional injuries",
          letter == "X" & number >= 430 & number <= 449 ~ "Unintentional injuries",
          letter == "X" & number >= 460 & number <= 599 ~ "Unintentional injuries",
          letter == "Y" & number >= 400 & number <= 869 ~ "Unintentional injuries",
          letter == "Y" & number >= 880 & number <= 899 ~ "Unintentional injuries",
          
          letter == "X" & number >= 600 & number <= 999 ~ "Intentional injuries",
          letter == "Y" & number >= 0   & number <= 99  ~ "Intentional injuries",
          letter == "Y" & number >= 350 & number <= 369 ~ "Intentional injuries",
          letter == "Y" & number == 870                 ~ "Intentional injuries",
          letter == "Y" & number == 871                 ~ "Intentional injuries",
          letter == "U" & number >= 0   & number <= 19  ~ "Intentional injuries",
          
          
          TRUE ~ NA_character_
        )
      )
    
    causes <- causes %>%
      mutate(
        ghe_level1 = case_when(
          ghe_level2 == "Infectious and parasitic diseases" ~ "Communicable, maternal, perinatal and nutritional conditions",
          ghe_level2 == "Respiratory infections" ~ "Communicable, maternal, perinatal and nutritional conditions",
          ghe_level2 == "Maternal conditions" ~ "Communicable, maternal, perinatal and nutritional conditions",
          ghe_level2 == "Perinatal conditions" ~ "Communicable, maternal, perinatal and nutritional conditions",
          ghe_level2 == "Nutritional deficiencies" ~ "Communicable, maternal, perinatal and nutritional conditions",
          
          ghe_level2 == "Malignant neoplasms" ~ "Noncommunicable diseases",
          ghe_level2 == "Other neoplasms" ~ "Noncommunicable diseases",
          ghe_level2 == "Diabetes mellitus" ~ "Noncommunicable diseases",
          ghe_level2 == "Endocrine disorders" ~ "Noncommunicable diseases",
          ghe_level2 == "Neuropsychiatric conditions" ~ "Noncommunicable diseases",
          ghe_level2 == "Sense organ diseases" ~ "Noncommunicable diseases",
          ghe_level2 == "Cardiovascular diseases" ~ "Noncommunicable diseases",
          ghe_level2 == "Respiratory diseases" ~ "Noncommunicable diseases",
          ghe_level2 == "Digestive diseases" ~ "Noncommunicable diseases",
          ghe_level2 == "Genitourinary diseases" ~ "Noncommunicable diseases",
          ghe_level2 == "Skin diseases" ~ "Noncommunicable diseases",
          ghe_level2 == "Musculoskeletal diseases" ~ "Noncommunicable diseases",
          ghe_level2 == "Congenital anomalies" ~ "Noncommunicable diseases",
          ghe_level2 == "Oral conditions" ~ "Noncommunicable diseases",
          ghe_level2 == "Sudden infant death syndrome" ~ "Noncommunicable diseases",
          ghe_level2 == "Ill-defined diseases" ~ "Noncommunicable diseases",
          
          ghe_level2 == "Unintentional injuries" ~ "Injuries",
          ghe_level2 == "Intentional injuries" ~ "Injuries",
          ghe_level2 == "Ill-defined injuries/accidents" ~ "Injuries",
          
          TRUE ~ NA_character_
        )
      )
    
    causes <- causes %>%
      mutate(
        ghe_level3 = case_when(
          # Ill-defined and garbage
          letter == "R" & number >= 0   & number <= 949 ~ "Ill-defined diseases",
          letter == "R" & number >= 960 & number <= 999 ~ "Ill-defined diseases",
          letter == "Y" & number >= 100 & number <= 349 ~ "Ill-defined injuries/accidents",
          letter == "Y" & number == 872                 ~ "Ill-defined injuries/accidents",
          letter == "C" & number >= 760 & number <= 769 ~ "Garbage cancer",
          letter == "C" & number >= 800 & number <= 809 ~ "Garbage cancer",
          letter == "C" & number >= 970 & number <= 979 ~ "Garbage cancer",
          letter == "I" & number == 472                 ~ "Garbage CVD",
          letter == "I" & number == 490                 ~ "Garbage CVD",
          letter == "I" & number >= 460 & number <= 469 ~ "Cardiac arrest",
          letter == "I" & number >= 500 & number <= 509 ~ "Heart failure",
          letter == "I" & number >= 514 & number <= 516 ~ "Garbage CVD",
          letter == "I" & number == 519                 ~ "Garbage CVD",
          letter == "I" & number == 709                 ~ "Garbage CVD",
          
          
          letter == "A" & number >= 150 & number <= 199 ~ "Tuberculosis",
          letter == "B" & number >= 900 & number <= 909 ~ "Tuberculosis",
          letter == "A" & number >= 500 & number <= 649 ~ "STDs excluding HIV",
          letter == "N" & number >= 700 & number <= 739 ~ "STDs excluding HIV",
          letter == "B" & number >= 200 & number <= 249 ~ "HIV/AIDS",
          letter == "A" & number >= 0   & number <= 19  ~ "Diarrhoeal diseases",
          letter == "A" & number >= 30  & number <= 49  ~ "Diarrhoeal diseases",
          letter == "A" & number >= 60  & number <= 99  ~ "Diarrhoeal diseases",
          letter == "A" & number >= 330 & number <= 379 ~ "Childhood-cluster diseases",
          letter == "A" & number >= 800 & number <= 809 ~ "Childhood-cluster diseases",
          letter == "B" & number >= 50  & number <= 59  ~ "Childhood-cluster diseases",
          letter == "B" & number >= 910 & number <= 919 ~ "Childhood-cluster diseases",
          letter == "G" & number >= 140 & number <= 149 ~ "Childhood-cluster diseases",
          letter == "A" & number >= 390 & number <= 399 ~ "Meningitis",
          letter == "G" & number >= 0   & number <= 9   ~ "Meningitis",
          letter == "G" & number >= 30  & number <= 39  ~ "Meningitis",
          letter == "B" & number >= 160 & number <= 170 ~ "Hepatitis B",
          letter == "B" & number >= 172 & number <= 181 ~ "Hepatitis B",
          letter == "B" & number >= 183 & number <= 199 ~ "Hepatitis B",
          letter == "B" & number == 171                 ~ "Hepatitis C",
          letter == "B" & number == 182                 ~ "Hepatitis C",
          letter == "B" & number >= 500 & number <= 549 ~ "Malaria",
          letter == "B" & number >= 550 & number <= 579 ~ "Tropical-cluster diseases",
          letter == "B" & number >= 650 & number <= 659 ~ "Tropical-cluster diseases",
          letter == "B" & number >= 730 & number <= 742 ~ "Tropical-cluster diseases",
          letter == "A" & number >= 300 & number <= 309 ~ "Leprosy",
          letter == "A" & number >= 900 & number <= 919 ~ "Dengue",
          letter == "A" & number == 380                 ~ "Japanese encephalitis",
          letter == "A" & number >= 710 & number <= 719 ~ "Trachoma",
          letter == "B" & number >= 760 & number <= 819 ~ "Intestinal nematode infections",
          ghe_level2 == "Infectious and parasitic diseases" ~ "Other infectious diseases",
          
          letter == "J" & number >= 90  & number <= 189 ~ "Lower respiratory infections",
          letter == "J" & number >= 200 & number <= 229 ~ "Lower respiratory infections",
          letter == "J" & number >= 0   & number <= 69  ~ "Upper respiratory infections",
          letter == "H" & number >= 650 & number <= 669 ~ "Otitis media",
          
          letter == "O" & number >= 440 & number <= 469 ~ "Maternal haemorrhage",
          letter == "O" & number >= 670 & number <= 679 ~ "Maternal haemorrhage",
          letter == "O" & number >= 720 & number <= 729 ~ "Maternal haemorrhage",
          letter == "O" & number >= 850 & number <= 869 ~ "Maternal sepsis",
          letter == "O" & number >= 100 & number <= 169 ~ "Hypertensive disorders",
          letter == "O" & number >= 640 & number <= 669 ~ "Obstructed labour",
          letter == "O" & number >= 0   & number <= 79  ~ "Abortion",
          ghe_level2 == "Maternal conditions" ~ "Other maternal conditions",
          
          letter == "P" & number >= 50  & number <= 59  ~ "Low birth weight",
          letter == "P" & number >= 70  & number <= 79  ~ "Low birth weight",
          letter == "P" & number >= 220 & number <= 229 ~ "Low birth weight",
          letter == "P" & number >= 270 & number <= 289 ~ "Low birth weight",
          letter == "P" & number >= 30  & number <= 39  ~ "Birth asphyxia and birth trauma",
          letter == "P" & number >= 100 & number <= 159 ~ "Birth asphyxia and birth trauma",
          letter == "P" & number >= 200 & number <= 219 ~ "Birth asphyxia and birth trauma",
          letter == "P" & number >= 240 & number <= 269 ~ "Birth asphyxia and birth trauma",
          letter == "P" & number >= 290 & number <= 299 ~ "Birth asphyxia and birth trauma",
          ghe_level2 == "Perinatal conditions" ~ "Other perinatal conditions",
          
          letter == "E" & number >= 400 & number <= 469 ~ "Protein-energy malnutrition",
          letter == "E" & number >= 0   & number <= 29  ~ "Iodine deficiency",
          letter == "E" & number >= 500 & number <= 509 ~ "Vitamin A deficiency",
          letter == "D" & number >= 500 & number <= 509 ~ "Iron-deficiency anaemia",
          letter == "D" & number == 649                 ~ "Iron-deficiency anaemia",
          ghe_level2 == "Nutritional deficiencies" ~ "Other nutritional conditions",
          
          
          letter == "C" & number >= 0   & number <= 149 ~ "Mouth and oropharynx cancers",
          letter == "C" & number >= 150 & number <= 159 ~ "Oesophagus cancer",
          letter == "C" & number >= 160 & number <= 169 ~ "Stomach cancer",
          letter == "C" & number >= 180 & number <= 219 ~ "Colon and rectum cancers",
          letter == "C" & number >= 220 & number <= 229 ~ "Liver cancer",
          letter == "C" & number >= 250 & number <= 259 ~ "Pancreas cancer",
          letter == "C" & number >= 330 & number <= 349 ~ "Trachea, bronchus, lung cancers",
          letter == "C" & number >= 430 & number <= 449 ~ "Melanoma and other skin cancers",
          letter == "C" & number >= 500 & number <= 509 ~ "Breast cancer",
          letter == "C" & number >= 530 & number <= 539 ~ "Cervix uteri cancer",
          letter == "C" & number >= 540 & number <= 559 ~ "Corpus uteri cancer",
          letter == "C" & number >= 560 & number <= 569 ~ "Ovary cancer",
          letter == "C" & number >= 610 & number <= 619 ~ "Prostate cancer",
          letter == "C" & number >= 620 & number <= 629 ~ "Testicular cancer",
          letter == "C" & number >= 640 & number <= 669 ~ "Kidney and ureter cancer",
          letter == "C" & number >= 670 & number <= 679 ~ "Bladder cancer",
          letter == "C" & number >= 700 & number <= 729 ~ "Brain and nervous system cancers",
          letter == "C" & number >= 230 & number <= 249 ~ "Gallbladder and biliary tract cancer",
          letter == "C" & number >= 320 & number <= 329 ~ "Larynx cancer",
          letter == "C" & number >= 730 & number <= 739 ~ "Thyroid cancer",
          letter == "C" & number >= 450 & number <= 459 ~ "Mesothelioma",
          letter == "C" & number >= 810 & number <= 909 ~ "Lymphomas, multiple myeloma",
          letter == "C" & number >= 960 & number <= 969 ~ "Lymphomas, multiple myeloma",
          letter == "C" & number >= 910 & number <= 959 ~ "Leukaemia",
          ghe_level2 == "Malignant neoplasms" ~ "Other malignant neoplasms",
          
          ghe_level2 == "Other neoplasms" ~ "Other neoplasms",
          
          ghe_level2 == "Diabetes mellitus" ~ "Diabetes mellitus",
          
          ghe_level2 == "Endocrine disorders" ~ "Endocrine disorders",
          
          letter == "F" & number >= 320 & number <= 339 ~ "Unipolar depressive disorders",
          letter == "F" & number >= 300 & number <= 319 ~ "Bipolar disorder",
          letter == "F" & number >= 200 & number <= 299 ~ "Schizophrenia",
          letter == "G" & number >= 400 & number <= 419 ~ "Epilepsy",
          letter == "F" & number >= 100 & number <= 109 ~ "Alcohol use disorders",
          letter == "X" & number >= 450 & number <= 459 ~ "Alcohol use disorders",
          letter == "F" & number >= 10  & number <= 19  ~ "Alzheimer and other dementias",
          letter == "F" & number >= 30  & number <= 39  ~ "Alzheimer and other dementias",
          letter == "G" & number >= 300 & number <= 319 ~ "Alzheimer and other dementias",
          letter == "G" & number >= 200 & number <= 219 ~ "Parkinson disease",
          letter == "G" & number >= 350 & number <= 359 ~ "Multiple sclerosis",
          letter == "F" & number >= 110 & number <= 169 ~ "Drug use disorders",
          letter == "F" & number >= 180 & number <= 199 ~ "Drug use disorders",
          letter == "X" & number >= 410 & number <= 429 ~ "Drug use disorders",
          letter == "F" & number == 431                 ~ "Post-traumatic stress disorder",
          letter == "F" & number >= 420 & number <= 429 ~ "Obsessive-compulsive disorder",
          letter == "F" & number == 400 | number == 410 ~ "Panic disorder",
          letter == "F" & number >= 510 & number <= 519 ~ "Insomnia (primary)",
          letter == "G" & number >= 430 & number <= 439 ~ "Migraine",
          letter == "F" & number >= 700 & number <= 799 ~ "Mental Retardation",
          ghe_level2 == "Neuropsychiatric conditions" ~ "Other neuropsychiatric disorders",
          
          letter == "H" & number >= 400 & number <= 409 ~ "Glaucoma",
          letter == "H" & number >= 250 & number <= 269 ~ "Cataracts",
          letter == "H" & number == 524                 ~ "Vision disorders, age-related",
          letter == "H" & number >= 900 & number <= 919 ~ "Hearing loss, adult onset",
          ghe_level2 == "Sense organ diseases" ~ "Other sense organ disorders",
          
          letter == "I" & number >= 10  & number <= 99  ~ "Rheumatic heart disease",
          letter == "I" & number >= 100 & number <= 139 ~ "Hypertensive heart disease",
          letter == "I" & number >= 200 & number <= 259 ~ "Ischaemic heart disease",
          letter == "I" & number >= 600 & number <= 699 ~ "Cerebrovascular disease",
          letter == "I" & number >= 300 & number <= 339 ~ "Inflammatory heart diseases",
          letter == "I" & number >= 380 & number <= 389 ~ "Inflammatory heart diseases",
          letter == "I" & number >= 400 & number <= 409 ~ "Inflammatory heart diseases",
          letter == "I" & number >= 420 & number <= 429 ~ "Inflammatory heart diseases",
          ghe_level2 == "Cardiovascular diseases" ~ "Other cardiovascular diseases",
          
          letter == "J" & number >= 400 & number <= 449 ~ "Chronic obstructive pulmonary disease",
          letter == "J" & number >= 450 & number <= 469 ~ "Asthma",
          ghe_level2 == "Respiratory diseases" ~ "Other respiratory diseases",
          
          letter == "K" & number >= 250 & number <= 279 ~ "Peptic ulcer disease",
          letter == "K" & number >= 700 & number <= 709 ~ "Cirrhosis of the liver",
          letter == "K" & number >= 740 & number <= 749 ~ "Cirrhosis of the liver",
          letter == "K" & number >= 350 & number <= 379 ~ "Appendicitis",
          letter == "K" & number >= 290 & number <= 299 ~ "Gastritis and duodenitis",
          letter == "K" & number >= 560 & number <= 569 ~ "Paralytic ileus and intestinal obstruction",
          letter == "K" & number >= 500 & number <= 529 ~ "Inflammatory bowel disease",
          letter == "K" & number == 580                 ~ "Inflammatory bowel disease",
          letter == "K" & number >= 800 & number <= 839 ~ "Gallbladder and biliary diseases",
          letter == "K" & number >= 850 & number <= 869 ~ "Pancreatitis",
          ghe_level2 == "Digestive diseases" ~ "Other digestive diseases",
          
          letter == "N" & number >= 0   & number <= 199 ~ "Nephritis and nephrosis",
          letter == "N" & number >= 400 & number <= 409 ~ "Benign prostatic hypertrophy",
          ghe_level2 == "Genitourinary diseases" ~ "Other genitourinary system diseases",
          
          ghe_level2 == "Skin diseases" ~ "Skin diseases",
          
          letter == "M" & number >= 50  & number <= 69  ~ "Rheumatoid arthritis",
          letter == "M" & number >= 150 & number <= 199 ~ "Osteoarthritis",
          letter == "M" & number >= 100 & number <= 109 ~ "Gout",
          letter == "M" & number >= 450 & number <= 489 ~ "Back pain",
          letter == "M" & number >= 540 & number <= 541 ~ "Back pain",
          letter == "M" & number >= 543 & number <= 549 ~ "Back pain",
          ghe_level2 == "Musculoskeletal diseases" ~ "Other musculoskeletal diseases",
          
          letter == "Q" & number >= 792 & number <= 795 ~ "Abdominal wall defect",
          letter == "Q" & number >= 0   & number <= 9   ~ "Anencephaly",
          letter == "Q" & number >= 420 & number <= 429 ~ "Anorectal atresia",
          letter == "Q" & number >= 360 & number <= 369 ~ "Cleft lip",
          letter == "Q" & number >= 350 & number <= 359 ~ "Cleft palate",
          letter == "Q" & number >= 370 & number <= 379 ~ "Cleft palate",
          letter == "Q" & number >= 390 & number <= 391 ~ "Oesophageal atresia",
          letter == "Q" & number >= 600 & number <= 609 ~ "Renal agenesis",
          letter == "Q" & number >= 900 & number <= 909 ~ "Down syndrome",
          letter == "Q" & number >= 200 & number <= 289 ~ "Congenital heart anomalies",
          letter == "Q" & number >= 50  & number <= 59  ~ "Spina bifida",
          ghe_level2 == "Congenital anomalies" ~ "Other congenital anomalies",
          
          letter == "K" & number >= 20  & number <= 29  ~ "Dental caries",
          letter == "K" & number >= 50  & number <= 59  ~ "Periodontal disease",
          ghe_level2 == "Oral conditions" ~ "Other oral diseases",
          
          ghe_level2 == "Sudden infant death syndrome" ~ "Sudden infant death syndrome",
          
          
          letter == "V" & number >= 0   & number <= 999 ~ "Road traffic accidents",
          letter == "Y" & number == 850                 ~ "Road traffic accidents",
          letter == "X" & number >= 400 & number <= 409 ~ "Poisonings",
          letter == "X" & number >= 430 & number <= 449 ~ "Poisonings",
          letter == "X" & number >= 460 & number <= 499 ~ "Poisonings",
          letter == "W" & number >= 0   & number <= 199 ~ "Falls",
          letter == "X" & number >= 0   & number <= 99  ~ "Fires",
          letter == "W" & number >= 650 & number <= 749 ~ "Drownings",
          letter == "W" & number >= 200 & number <= 389 ~ "Exposure to mechanical forces",
          letter == "W" & number >= 400 & number <= 439 ~ "Exposure to mechanical forces",
          letter == "W" & number >= 450 & number <= 469 ~ "Exposure to mechanical forces",
          letter == "W" & number >= 490 & number <= 529 ~ "Exposure to mechanical forces",
          letter == "W" & number >= 750 & number <= 769 ~ "Exposure to mechanical forces",
          letter == "X" & number >= 300 & number <= 399 ~ "Exposure to forces of nature",
          ghe_level2 == "Unintentional injuries" ~ "Other unintentional injuries",
          
          letter == "X" & number >= 600 & number <= 849 ~ "Self-inflicted injuries",
          letter == "Y" & number == 870                 ~ "Self-inflicted injuries",
          letter == "X" & number >= 850 & number <= 999 ~ "Violence",
          letter == "Y" & number >= 0   & number <= 99  ~ "Violence",
          letter == "Y" & number == 871                 ~ "Violence",
          letter == "U" & number >= 0   & number <= 19  ~ "Violence",
          letter == "Y" & number >= 360 & number <= 369 ~ "War",
          ghe_level2 == "Intentional injuries" ~ "Other intentional injuries",
          
          
          TRUE ~ NA_character_
        )
      ) 
    
  }
  
  # summarise by ghe levels
  causes_summarised = causes %>%
    rename(agegroup=age) %>%
    group_by(year,sex,agegroup,ghe_level1,ghe_level2,ghe_level3) %>%
    summarise(deaths=sum(deaths)) %>%
    drop_na()
  
  print(paste0(x,': total deaths in year = ',sum(dat$deaths),
               ', total deaths after processing causes = ',sum(causes$deaths),
               ', total deaths after na rows removed = ',sum(causes_summarised$deaths),
               ' (',round(100*(1-(sum(causes_summarised$deaths)/sum(causes$deaths))),2),'% missing)'))
  
  # return unique list of original cause code, with ghe levels
  causes_list = causes %>% 
    select(cause,ghe_level1,ghe_level2,ghe_level3) %>%
    group_by(cause,ghe_level1,ghe_level2,ghe_level3) %>%
    tally() %>%
    select(-n)
  
  return(causes_list)
}
############ PLOTTING FUNCTIONS ############

# plot total deaths by cause level, month, age
plot_cause_month_age = function(dat,cause.level,font.size=15){
  
  dat$cause.level = dat[,get(cause.level)]
  
  data = dat %>% 
    mutate(cause.level = gsub("\\ ", "\n", cause.level)) %>%
    group_by(cause.level,month,sex,sex.long,age,age.long) %>% 
    summarise(deaths=sum(deaths.adj))

  
  p = ggplot(data=data, aes(x=month,y=deaths,color=as.factor(age.long),fill=as.factor(age.long))) +
    geom_bar(width = 0.9, stat = "identity") +
    xlab('Month') + ylab('Number of deaths (adjusted by length of month)') +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
    scale_fill_manual(values=age.colours, guide = guide_legend(nrow = 1,title = paste0("Age group (years)"))) +
    scale_color_manual(values=age.colours, guide = guide_legend(nrow = 1,title = paste0("Age group (years)"))) +
    scale_y_continuous(label = comma) +
    facet_grid(sex.long~cause.level)   +
    theme_bw() +  theme(panel.grid.major = element_blank(),text = element_text(size = font.size),
                        axis.text.x = element_text(angle=90, vjust=0.5, hjust=1), axis.ticks.x=element_blank(),
                        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                        panel.border = element_rect(colour = "black"),strip.background = element_blank(),
                        legend.position = 'bottom',legend.justification='center',
                        legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
  
  return(p)
}

# plot total deaths by cause level, month, age
plot_cause_month_age_single_sex = function(dat,cause.level,font.size=15,sex_selected=1){
  
  dat$cause.level = dat[,get(cause.level)]
  
  data = dat %>% 
    group_by(cause.level,month,sex,sex.long,age,age.long) %>% 
    summarise(deaths=sum(deaths.adj))
  
  p = ggplot(data=subset(data,sex==sex_selected), aes(x=month,y=deaths,color=as.factor(age.long),fill=as.factor(age.long))) +
    geom_bar(width = 0.9, stat = "identity") +
    xlab('Month') + ylab('Number of deaths (adjusted by length of month)') +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
    scale_fill_manual(values=age.colours, guide = guide_legend(nrow = 1,title = paste0("Age group (years)"))) +
    scale_color_manual(values=age.colours, guide = guide_legend(nrow = 1,title = paste0("Age group (years)"))) +
    scale_y_continuous(label = comma) +
    facet_wrap(~cause.level,ncol=1, scale='free')   +
    theme_bw() +  theme(panel.grid.major = element_blank(),text = element_text(size = font.size),
                        axis.text.x = element_text(angle=90, vjust=0.5, hjust=1), axis.ticks.x=element_blank(),
                        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                        panel.border = element_rect(colour = "black"),strip.background = element_blank(),
                        legend.position = 'bottom',legend.justification='center',
                        legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
  
  return(p)
}

# plot deaths over time by cause level, month, age over time
plot_cause_month_time = function(dat,cause.level,font.size=8){
  
  dat$cause.level = dat[,get(cause.level)]
  
  data = dat %>% 
    mutate(cause.level = gsub("\\ ", "\n", cause.level)) %>%
    group_by(cause.level,Date,sex,sex.long,age,age.long) %>% 
    summarise(deaths=sum(deaths.adj))
  
  p = ggplot(data=data) +
    geom_bar(aes(x=Date, y=deaths,fill=age.long),position="stack", stat="identity") + 
    facet_grid(sex.long~cause.level) +
    xlab('Date') + ylab('Deaths') +
    scale_y_continuous(label = comma) +
    scale_fill_manual(values=age.colours, guide = guide_legend(nrow = 1,title = paste0("Age group (years)"))) +
    scale_color_manual(values=age.colours, guide = guide_legend(nrow = 1,title = paste0("Age group (years)"))) +
    theme_bw() + theme(text = element_text(size = 6),
                       panel.grid.major = element_blank(),axis.text.x = element_text(angle=90), axis.text.y = element_text(size=font.size),
                       plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                       panel.border = element_rect(colour = "black"),strip.background = element_blank(),
                       legend.position = 'bottom',legend.justification='center',
                       legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
  return(p)
}

# plot deaths over time by cause level, month, age over time for single state
plot_cause_month_time_single_state = function(dat,cause.level,state,font.size=8){
  
  dat = subset(dat,state.fips==state)
  
  full_name = unique(dat$full_name)
  
  dat$cause.level = dat[,get(cause.level)]
  
  data = dat %>% 
    mutate(cause.level = gsub("\\ ", "\n", cause.level)) %>%
    group_by(cause.level,Date,sex,sex.long,age,age.long) %>% 
    summarise(deaths=sum(deaths.adj))
  
  p = ggplot(data=data) +
    geom_bar(aes(x=Date, y=deaths,fill=age.long),position="stack", stat="identity") + 
    facet_grid(sex.long~cause.level) +
    xlab('Date') + ylab('Deaths') +
    ggtitle(full_name) + 
    scale_y_continuous(label = comma) +
    scale_fill_manual(values=age.colours, guide = guide_legend(nrow = 1,title = paste0("Age group (years)"))) +
    scale_color_manual(values=age.colours, guide = guide_legend(nrow = 1,title = paste0("Age group (years)"))) +
    theme_bw() + theme(text = element_text(size = 6),
                       panel.grid.major = element_blank(),axis.text.x = element_text(angle=90), axis.text.y = element_text(size=font.size),
                       plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                       panel.border = element_rect(colour = "black"),strip.background = element_blank(),
                       legend.position = 'bottom',legend.justification='center',
                       legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
  return(p)
}

# plot deaths over time by cause level, month, age over time
plot_cause_month_time_grouped = function(dat,cause.level,cause.level.2,font.size=8){
  
  dat$cause.level = dat[,get(cause.level)]
  dat$cause.level.2 = dat[,get(cause.level.2)]
  
  data = dat %>% 
    group_by(cause.level,cause.level.2,Date,sex,sex.long,age,age.long) %>% 
    summarise(deaths=sum(deaths.adj))
  
  p = ggplot(data=data) +
    geom_bar(aes(x=Date, y=deaths,fill=cause.level),position="stack", stat="identity") + 
    ggtitle('Total deaths') +
    facet_wrap(~cause.level.2) +
    scale_y_continuous(label = comma) +
    xlab('Date') + ylab('Deaths') +
    theme_bw() + theme(text = element_text(size = 6),
                       panel.grid.major = element_blank(),axis.text.x = element_text(angle=90), axis.text.y = element_text(size=font.size),
                       plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                       panel.border = element_rect(colour = "black"),strip.background = element_blank(),
                       legend.position = 'bottom',legend.justification='center',
                       legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
  return(p)
}

# plot deaths over time by cause level, month, age over time
plot_cause_state = function(dat,cause.level,font.size=8){
  
  dat$cause.level = dat[,get(cause.level)]
  
  data = dat %>% 
    group_by(cause.level,Date,full_name) %>% 
    summarise(deaths=sum(deaths.adj))
  
  p = ggplot(data=data) +
    geom_bar(aes(x=full_name, y=deaths,fill=cause.level),position="stack", stat="identity") + 
    ggtitle('Total deaths') +
    #facet_wrap(~cause.level.2) +
    scale_y_continuous(label = comma) +
    xlab('Date') + ylab('Deaths') +
    theme_bw() + theme(text = element_text(size = 6),
                       panel.grid.major = element_blank(),axis.text.x = element_text(angle=90), axis.text.y = element_text(size=font.size),
                       plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                       panel.border = element_rect(colour = "black"),strip.background = element_blank(),
                       legend.position = 'bottom',legend.justification='center',
                       legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
  return(p)
}

# quick plot of results
model_quick_plot = function(dat,cause_selected,type){
  print(ggplot(data=dat) + 
          geom_point(aes(x=lag,y=rr-1)) + 
          geom_errorbar(aes(x=lag,ymin=rr.ll-1,ymax=rr.ul-1)) +
          ggtitle(cause_selected) +
          geom_hline(yintercept = 0) +
          scale_y_continuous(labels=scales::percent_format(accuracy=1)) +
          xlab('Lag (months after exposure)') + 
          ylab(paste0('Percentage change in death rates associated with ',type,' exposure')) +
          theme_bw() + 
          facet_wrap(~type) + 
          theme(text = element_text(size = 10),
                panel.grid.major = element_blank(),axis.text.x = element_text(angle=0), axis.text.y = element_text(size=10),
                plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
                panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                panel.border = element_rect(colour = "black"),strip.background = element_blank(),
                legend.position = 'bottom',legend.justification='center',
                legend.background = element_rect(fill="white", size=.5, linetype="dotted")))
}