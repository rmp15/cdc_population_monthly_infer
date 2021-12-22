# list of objects used across different scripts
vintage=2020
start_year_overall = 1982
start_year_cdc=1990
end_year_cdc=2020

# start_year_mortality = 1980
# years_mortality = c(start_year_mortality:end_year)
# years_analysis = c(start_year:end_year)
# years_icd10 = c(1999:2019)
# months = c(1:12)
# month.short <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
# sexes = c(1:2)
# ages = c(0,5,15,25,35,45,55,65,75,85)
# ages.broad = c(0,65)
# ages.finer = c(0,25,45,65,85)
# age.print <- as.vector(levels(factor(levels=c('0-4','5-14','15-24','25-34','35-44','45-54','55-64','65-74','75-84','85+'))))
# age.code <- data.frame(age=c(0,5,15,25,35,45,55,65,75,85),age.print=age.print)
# age.colours=c("blue",brewer.pal(9,"BrBG")[c(9:6,4:1)],"grey")[c(4,8)]
# age.colours.10=c("blue",brewer.pal(9,"BrBG")[c(9:6,4:1)],"grey")
# age.colours.5=c("blue",brewer.pal(9,"BrBG")[c(9:6,4:1)],"grey")[c(1,3,5,7,9)]
# 
# classes = c('hurricanes','tropical_cyclones','gales_to_violent_storms')
# 
# # number of degrees of freedom for modelling temperature
# deg_freedom_temperature = 3
# 
# # number of draws for posterior sampling
# num_draws=1000
# 
# # parameters to sample from posterior 
# selected_parameters = list(event_lag0=1,event_lag1=1,event_lag2=1,event_lag3=1,event_lag4=1,event_lag5=1,event_lag6=1)
# 
# # ASDR weightings
# # from http://apps.who.int/healthinfo/statistics/mortality/whodpms/definitions/pop.htm
# # and https://www.who.int/healthinfo/paper31.pdf
# StdPopMF =      c(1822+7033,        # 0-4
#                   8687+8597,          # 5-14
#                   8474+8222,          # 15-24
#                   7928+7605,          # 25-34
#                   7145+6590,          # 35-44
#                   6038+5371,          # 45-54
#                   4547+3723,          # 55-64
#                   2955+2210,          # 65-74
#                   1515+905,           # 75-84
#                   632)                # 85+
# StdPopMF = data.frame(age=ages,weight=StdPopMF)
# 
# # COD look-up for ICD 10
# cod.lookup.10 <- data.frame(letter=as.character(toupper(letters)),
#                             cause.group=c('Other','Other','Cancers','Cancers','Other', # A-E
#                                           'Other','Other','Other','Cardiovascular diseases','Respiratory diseases', # F-J
#                                           'Other','Other','Other','Other','Other', # K-O
#                                           'Other','Other','Other','Injuries','Injuries', # P-T
#                                           'Other','Injuries','Injuries','Injuries','Injuries', # U-Y
#                                           'Injuries')) # Z
# 
# # states included in analysis from tropical cyclones analysis
# states_included = c("01", "05", "09", "10", "11", 
#                     "12", "13", "17", "18", "19",
#                     "20", "21", "22", "23", "24", 
#                     "25", "26", "28", "29", "33", 
#                     "34", "36", "37", "39", "40", 
#                     "42", "44", "45", "47", "48", 
#                     "50", "51", "54", "55")
# 
# # cause groups in analysis
# broad_causes_included <- c("Cancers", "Cardiovascular diseases", "Digestive diseases", "Endocrine disorders", "Genitourinary diseases", 
#                            "Infectious and parasitic diseases", "Injuries", "Neuropsychiatric conditions", "Respiratory diseases","Other NCDs",
#                            "Maternal, neonatal, perinatal")
# 
# original_broad_causes_included <- c("Cancers", "Cardiovascular diseases", "Digestive diseases", "Endocrine disorders", "Genitourinary diseases", 
#                                     "Infectious and parasitic diseases", "Injuries", "Neuropsychiatric conditions", "Respiratory diseases")
# 
# additional_broad_causes_included <- c("Other NCDs","Maternal, neonatal, perinatal")
# 
# injuries_subcauses_included <- c("Intentional","Unintentional")
# 
# broad_causes_majid <- c("Cancers", "Cardiovascular diseases", "Infectious and parasitic diseases", 
#                         "Injuries", "Neuropsychiatric conditions", "Respiratory diseases",
#                          "Maternal, neonatal, perinatal","Other NCDs")
# 
# broad_causes_main_results <- c("Cancers", "Cardiovascular diseases", "Infectious and parasitic diseases", 
#                         "Injuries", "Neuropsychiatric conditions", "Respiratory diseases")
# 
# causes_included <- c(broad_causes_included,injuries_subcauses_included)
# 
# # useful general color scheme
# f <- function(pal) brewer.pal(brewer.pal.info[pal, "maxcolors"], pal)
# mycols <- c(f("Dark2"), f("Set1")[1:8], f("Set2"),
#             f("Set3"),"#89C5DA", "#DA5724", "#74D944", "#CE50CA", "#3F4921",
#             "#C0717C", "#CBD588", "#5F7FC7", "#673770", "#D3D93E",
#             "#38333E", "#508578", "#D7C1B1", "#689030", "#AD6F3B",
#             "#CD9BCD", "#D14285", "#6DDE88", "#652926", "#7FDCC0",
#             "#C84248", "#8569D5", "#5E738F", "#D1A33D", "#8A7C64",
#             "#599861" )
# 
# # colors for broad causes of death
# colors.broad.causes <- mycols[c(
#                                 57,  # Cancers
#                                 62,  # Cardiovascular diseases
#                                 22,  # Digestive system diseases
#                                 13,  # Endocrine disorders
#                                 36,  # Genitourinary diseases
#                                 60,  # Infectious and parasitic diseases
#                                 40,  # Injuries
#                                 12,  # Neuropsychiatric disorders
#                                 48)]  # Respiratory diseases
# 
#                                 # 30,  # Blood diseases
#                                 # 6,   # Musculoskeletal and connective tissue diseases
#                                 # 46,  # Nervous system diseases
#                                 # 43,  # Skin and subcutaneous tissue diseases
#                                 # 24)] # Other
# 
# # colors for strength of wind
# colors.wind.class <-# mycols[c(10,20,30)] 
#   c("blue","purple","black")
# 
# # colors for sexes
# colors.sex <- c(mycols[c(28,44)],"black")
# 
# # colors for ages
# colors.age <- c(rev(age.colours),"black")
# 
# # colors for SVI map
# colorpalette.svi <-
#   
#   c(mycols[c(
#     37,  # 1
#     46,  # 2
#     57   # 3
#   )],
#   'cornsilk') # not in analysis
# 
# # colors for SVI plot
# colorpalette.svi.plot <-
#   
#   c(mycols[c(
#     37,  # 1
#     46,  # 2
#     57   # 3
#   )],
#   'black') # main results