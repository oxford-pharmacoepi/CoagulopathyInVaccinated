
# Initiate lists to store output ----
Patient.characteristcis<-list() 
Survival.summary<-list() 
Model.estimates<-list()  
Cohort.age.plot.data<-list()

# if only running main analysis -----
if(run.main.analyses.only==TRUE){
study.cohorts<-study.cohorts %>% 
  filter(name %in% 
           c("General population 2017",
             "Any first-dose", 
             "Any full-dose"))

outcome.cohorts<-outcome.cohorts %>% 
  filter(name %in%  
           c("CVST", "MI isc stroke",
             "SVT", "thrombocyt",
             "VTE narrow","TTS_ATE",
             "TTS_VTE"))
}


# run through exposures and outcomes ----
n.sudy.cohorts.to.run<-ifelse(run.as.test==TRUE,1,
                              length(study.cohorts$id) )
n.outcomes.to.run<-ifelse(run.as.test==TRUE,1,
                              length(outcome.cohorts$id) )

for(i in 1:n.sudy.cohorts.to.run){
# for(i in c(1,12)){  
working.study.cohort.id<-  study.cohorts$id[i]
working.study.cohort<-  study.cohorts$name[i]
print(paste0("Running anyalsis for: ", working.study.cohort, " (", i, " of ", 
             length(study.cohorts$id), ")"))
info(logger, paste0("Running anyalsis for: ", working.study.cohort, " (", i, " of ", 
             length(study.cohorts$id), ")"))

  
# Create Pop df ----
if(working.study.cohort %in% c("dose1_AZ", "dose1_moderna", "dose1_pfizer" )){
ids.to.collect<-exposure.cohorts %>% 
  filter(name==working.study.cohort) %>% 
  select(id) %>% 
  pull()
working.exposure.cohorts_db<-exposure.cohorts_db %>% 
               filter(cohort_definition_id %in% !!ids.to.collect ) %>% 
               select(subject_id,cohort_start_date,cohort_definition_id) %>% 
               rename("person_id"="subject_id") 
# first 
working.exposure.cohorts_db<-working.exposure.cohorts_db %>% 
  group_by(person_id) %>% 
  slice_min(cohort_start_date, n = 1) %>% 
  ungroup()%>% 
  distinct()
rm(ids.to.collect)

# get next vax for censoring
ids.to.collect<-exposure.cohorts %>% 
  filter(name %in% c("dose2_AZ", "dose2_moderna_any", "dose2_pfizer_any") ) %>% 
  select(id) %>% 
  pull()
working.vax.censor_db<-exposure.cohorts_db %>% 
               filter(cohort_definition_id %in% !!ids.to.collect ) %>% 
               select(subject_id,cohort_start_date) %>% 
               rename("person_id"="subject_id") 
working.vax.censor_db<-working.vax.censor_db %>% 
  group_by(person_id) %>% 
  slice_min(cohort_start_date, n = 1) %>% 
  distinct()
working.vax.censor_db<-working.vax.censor_db %>%
  rename("next_vax_date"="cohort_start_date")

working.exposure.cohorts_db<-working.exposure.cohorts_db %>% 
  left_join(working.vax.censor_db)

rm(ids.to.collect)
}

if(working.study.cohort %in% c("dose1_janssen") |  str_detect(working.study.cohort,"dose2")){
ids.to.collect<-exposure.cohorts %>% 
  filter(name==working.study.cohort) %>% 
  select(id) %>% 
  pull()
working.exposure.cohorts_db<-exposure.cohorts_db %>% 
               filter(cohort_definition_id %in% !!ids.to.collect ) %>% 
               select(subject_id,cohort_start_date,cohort_definition_id) %>% 
               rename("person_id"="subject_id") 
# first 
working.exposure.cohorts_db<-working.exposure.cohorts_db %>% 
  group_by(person_id) %>% 
  slice_min(cohort_start_date, n = 1) %>% 
  ungroup()%>% 
  distinct()
rm(ids.to.collect)

# get next vax for censoring
# no subsequent vax
working.exposure.cohorts_db<-working.exposure.cohorts_db %>% 
  mutate("next_vax_date"=as.Date(NA))


}

if(working.study.cohort=="Any first-dose"){
ids.to.collect<-exposure.cohorts %>% 
  filter(str_detect(name,"dose1")) %>% 
  select(id) %>% 
  pull()
working.exposure.cohorts_db<-exposure.cohorts_db %>% 
               filter(cohort_definition_id %in% !!ids.to.collect ) %>% 
               select(subject_id,cohort_start_date,cohort_definition_id) %>% 
               rename("person_id"="subject_id") 
# first 
working.exposure.cohorts_db<-working.exposure.cohorts_db %>% 
  group_by(person_id) %>% 
  slice_min(cohort_start_date, n = 1) %>% 
  ungroup()%>% 
  distinct()
rm(ids.to.collect)

# get next vax for censoring
ids.to.collect<-exposure.cohorts %>% 
  filter(str_detect(name,"dose2")) %>% 
  select(id) %>% 
  pull()
working.vax.censor_db<-exposure.cohorts_db %>% 
               filter(cohort_definition_id %in% !!ids.to.collect ) %>% 
               select(subject_id,cohort_start_date) %>% 
               rename("person_id"="subject_id") 
working.vax.censor_db<-working.vax.censor_db %>% 
  group_by(person_id) %>% 
  slice_min(cohort_start_date, n = 1) %>% 
  distinct()
working.vax.censor_db<-working.vax.censor_db %>%
  rename("next_vax_date"="cohort_start_date")

working.exposure.cohorts_db<-working.exposure.cohorts_db %>% 
  left_join(working.vax.censor_db)

rm(ids.to.collect)
}

if(working.study.cohort=="Any full-dose"){
ids.to.collect<-exposure.cohorts %>% 
  filter(name %in% 
       c("dose1_janssen","dose2_AZ",
         "dose2_moderna_any", "dose2_pfizer_any" )) %>% 
  select(id) %>% 
  pull()
working.exposure.cohorts_db<-exposure.cohorts_db %>% 
               filter(cohort_definition_id %in% !!ids.to.collect ) %>% 
               select(subject_id,cohort_start_date,cohort_definition_id) %>% 
               rename("person_id"="subject_id")
# first 
working.exposure.cohorts_db<-working.exposure.cohorts_db %>% 
  group_by(person_id) %>% 
  slice_min(cohort_start_date, n = 1) %>% 
  ungroup()%>% 
  distinct()
rm(ids.to.collect)

# no subsequent vax
working.exposure.cohorts_db<-working.exposure.cohorts_db %>% 
  mutate("next_vax_date"=as.Date(NA))
}

if(working.study.cohort=="Viral vector first-dose"){
ids.to.collect<-exposure.cohorts %>% 
  filter(name %in% 
       c("dose1_janssen","dose1_AZ" )) %>% 
  select(id) %>% 
  pull()
working.exposure.cohorts_db<-exposure.cohorts_db %>% 
               filter(cohort_definition_id %in% !!ids.to.collect ) %>% 
               select(subject_id,cohort_start_date,cohort_definition_id) %>% 
               rename("person_id"="subject_id") 
# first 
working.exposure.cohorts_db<-working.exposure.cohorts_db %>% 
  group_by(person_id) %>% 
  slice_min(cohort_start_date, n = 1) %>% 
  ungroup()%>% 
  distinct()
rm(ids.to.collect)

# get next vax for censoring
ids.to.collect<-exposure.cohorts %>% 
  filter(str_detect(name,"dose2")) %>% 
  select(id) %>% 
  pull()
working.vax.censor_db<-exposure.cohorts_db %>% 
               filter(cohort_definition_id %in% !!ids.to.collect ) %>% 
               select(subject_id,cohort_start_date) %>% 
               rename("person_id"="subject_id") 
working.vax.censor_db<-working.vax.censor_db %>% 
  group_by(person_id) %>% 
  slice_min(cohort_start_date, n = 1) %>% 
  distinct()
working.vax.censor_db<-working.vax.censor_db %>%
  rename("next_vax_date"="cohort_start_date")

working.exposure.cohorts_db<-working.exposure.cohorts_db %>% 
  left_join(working.vax.censor_db)

rm(ids.to.collect)
}

if(working.study.cohort=="mRNA first-dose"){
ids.to.collect<-exposure.cohorts %>% 
  filter(name %in% 
       c("dose1_moderna","dose1_pfizer" )) %>% 
  select(id) %>% 
  pull()
working.exposure.cohorts_db<-exposure.cohorts_db %>% 
               filter(cohort_definition_id %in% !!ids.to.collect ) %>% 
               select(subject_id,cohort_start_date,cohort_definition_id) %>% 
               rename("person_id"="subject_id") 
# first 
working.exposure.cohorts_db<-working.exposure.cohorts_db %>% 
  group_by(person_id) %>% 
  slice_min(cohort_start_date, n = 1) %>% 
  ungroup()%>% 
  distinct()
rm(ids.to.collect)

# get next vax for censoring
ids.to.collect<-exposure.cohorts %>% 
  filter(str_detect(name,"dose2")) %>% 
  select(id) %>% 
  pull()
working.vax.censor_db<-exposure.cohorts_db %>% 
               filter(cohort_definition_id %in% !!ids.to.collect ) %>% 
               select(subject_id,cohort_start_date) %>% 
               rename("person_id"="subject_id") 
working.vax.censor_db<-working.vax.censor_db %>% 
  group_by(person_id) %>% 
  slice_min(cohort_start_date, n = 1) %>% 
  distinct()
working.vax.censor_db<-working.vax.censor_db %>%
  rename("next_vax_date"="cohort_start_date")

working.exposure.cohorts_db<-working.exposure.cohorts_db %>% 
  left_join(working.vax.censor_db)

rm(ids.to.collect)
}

if(working.study.cohort=="Viral vector full-dose"){
ids.to.collect<-exposure.cohorts %>% 
  filter(name %in% 
       c("dose2_AZ","dose1_janssen" )) %>% 
  select(id) %>% 
  pull()  
working.exposure.cohorts_db<-exposure.cohorts_db %>% 
               filter(cohort_definition_id %in% !!ids.to.collect ) %>% 
               select(subject_id,cohort_start_date,cohort_definition_id) %>% 
               rename("person_id"="subject_id")
# first 
working.exposure.cohorts_db<-working.exposure.cohorts_db %>% 
  group_by(person_id) %>% 
  slice_min(cohort_start_date, n = 1) %>% 
  ungroup()%>% 
  distinct()
rm(ids.to.collect)

# no subsequent vax
working.exposure.cohorts_db<-working.exposure.cohorts_db %>% 
  mutate("next_vax_date"=as.Date(NA))
}

if(working.study.cohort=="mRNA full-dose"){
ids.to.collect<-exposure.cohorts %>% 
  filter(name %in% c("dose2_moderna","dose2_pfizer")) %>% 
  select(id) %>% 
  pull()
working.exposure.cohorts_db<-exposure.cohorts_db %>% 
               filter(cohort_definition_id %in% !!ids.to.collect ) %>% 
               select(subject_id,cohort_start_date,cohort_definition_id) %>% 
               rename("person_id"="subject_id")
# first 
working.exposure.cohorts_db<-working.exposure.cohorts_db %>% 
  group_by(person_id) %>% 
  slice_min(cohort_start_date, n = 1) %>% 
  ungroup()%>% 
  distinct()
rm(ids.to.collect)

# no subsequent vax
working.exposure.cohorts_db<-working.exposure.cohorts_db %>% 
  mutate("next_vax_date"=as.Date(NA))
}

if(working.study.cohort=="mRNA second-dose after viral vector first-dose"){
ids.to.collect<-exposure.cohorts %>% 
  filter(name %in% c("dose2_moderna_mix","dose2_pfizer_mix")) %>% 
  select(id) %>% 
  pull()
working.exposure.cohorts_db<-exposure.cohorts_db %>% 
               filter(cohort_definition_id %in% !!ids.to.collect ) %>% 
               select(subject_id,cohort_start_date,cohort_definition_id) %>% 
               rename("person_id"="subject_id")
# first 
working.exposure.cohorts_db<-working.exposure.cohorts_db %>% 
  group_by(person_id) %>% 
  slice_min(cohort_start_date, n = 1) %>% 
  ungroup()%>% 
  distinct()
rm(ids.to.collect)

# no subsequent vax
working.exposure.cohorts_db<-working.exposure.cohorts_db %>% 
  mutate("next_vax_date"=as.Date(NA))
}

if(working.study.cohort=="General population 2017"){
ids.to.collect<-exposure.cohorts %>% 
  filter(name=="General population 2017") %>% 
  select(id) %>% 
  pull()
working.exposure.cohorts_db<-exposure.cohorts_db %>% 
               filter(cohort_definition_id %in% !!ids.to.collect ) %>% 
               select(subject_id,cohort_start_date,cohort_definition_id) %>% 
               rename("person_id"="subject_id")
# first 
working.exposure.cohorts_db<-working.exposure.cohorts_db %>% 
  group_by(person_id) %>% 
  slice_min(cohort_start_date, n = 1) %>% 
  ungroup()%>% 
  distinct()
rm(ids.to.collect)

# no subsequent vax
working.exposure.cohorts_db<-working.exposure.cohorts_db %>% 
  mutate("next_vax_date"=as.Date(NA))
}


# take smaple -----
current.n<-as.numeric(working.exposure.cohorts_db %>% 
        tally() %>% 
        collect() %>% pull()) 
print(paste0("Current sample size of ",  
              working.study.cohort, ": ", current.n))
info(logger, paste0("Current sample size of ",    
                     working.study.cohort, ": ", current.n))

if(working.study.cohort=="General population 2017"){
if(current.n>15000000){
 print(paste0("Taking random sample: current size of ",  
              working.study.cohort, ": ", current.n))
 info(logger, paste0("Taking random sample: current size of ",  
                     working.study.cohort, ": ", current.n))

 working.exposure.cohorts_db<-working.exposure.cohorts_db %>% slice_sample(n=15000000)
 
 current.n<-as.numeric(working.exposure.cohorts_db %>% 
        tally() %>% 
        collect() %>% pull()) 
 
 print(paste0("Sampled size of ",  working.study.cohort, ": ", current.n))
 info(logger, paste0("Sampled size of ",  working.study.cohort, ": ", current.n))
}}
rm(current.n)


Pop<-person_db %>% 
  inner_join(working.exposure.cohorts_db,
             by = "person_id") %>% 
  select(person_id,gender_concept_id, 
         year_of_birth, month_of_birth, day_of_birth,
         cohort_start_date, next_vax_date,
         cohort_definition_id)  %>% 
  left_join(observation_period_db %>% 
              select("person_id",  "observation_period_start_date", "observation_period_end_date") %>% 
              distinct(),
             by = "person_id") %>% 
  collect()

Pop<-Pop %>% 
  left_join(exposure.cohorts %>% select(id, name) %>% 
              rename("vax_type"="name"),
            by=c("cohort_definition_id"="id"))
# table(Pop$vax_type, useNA = "always")

# compute ---
working.exposure.cohorts_db<-working.exposure.cohorts_db %>% 
  compute()

# drop anyone with duplicates ----
# these are people with more than one type of vax recorded on the same day
duplicates<-Pop %>% 
  group_by(person_id) %>% 
  mutate(seq=length(person_id)) %>% 
  filter(seq>1) %>% 
  select(person_id) %>% 
  distinct()
Pop<-Pop %>% 
  anti_join(duplicates)
# nrow(Pop %>% select(person_id) %>% distinct())/nrow(Pop)

# drop if within 7 days prior to db.end.date -----
# sum(Pop$cohort_start_date>=(db.end.date-days(7)))
Pop<-Pop %>% 
  filter(cohort_start_date<(db.end.date-days(7)))
# sum(Pop$cohort_start_date>=(db.end.date-days(7)))

# add age -----
Pop$age<- NA
if(sum(is.na(Pop$day_of_birth))==0 & sum(is.na(Pop$month_of_birth))==0){
  # if we have day and month 
  Pop<-Pop %>%
    mutate(age=floor(as.numeric((ymd(cohort_start_date)-
                                   ymd(paste(year_of_birth,
                                             month_of_birth,
                                             day_of_birth, sep="-"))))/365.25))
} else { 
  Pop<-Pop %>% 
    mutate(age= year(cohort_start_date)-year_of_birth)
}

# age age groups ----
Pop<-Pop %>% 
  mutate(age_gr=ifelse(age<20,  "<20",
                ifelse(age>=20 &  age<=44,  "20-44",
                ifelse(age>=45 & age<=54,  "45-54",
                ifelse(age>=55 & age<=64,  "55-64",
                ifelse(age>=65 & age<=74, "65-74", 
                ifelse(age>=75 & age<=84, "75-84",      
                ifelse(age>=85, ">=85",
                        NA)))))))) %>% 
  mutate(age_gr= factor(age_gr, 
                        levels = c("<20","20-44","45-54", "55-64",
                                   "65-74", "75-84",">=85"))) 
table(Pop$age_gr, useNA = "always")

# wider age groups
Pop<-Pop %>% 
  mutate(age_gr2=ifelse(age<=44,  "<=44",
                        ifelse(age>=45 & age<=64,  "45-64",    
                               ifelse(age>=65, ">=65",
                                      NA)))) %>% 
  mutate(age_gr2= factor(age_gr2, 
                         levels = c("<=44", "45-64",">=65")))
table(Pop$age_gr2, useNA = "always")

# another alternative set of age groups
Pop<-Pop %>% 
  mutate(age_gr3= ifelse(age<20,  "<20",
                         ifelse(age>=20 &  age<=29,  "20-29",
                         ifelse(age>=30 &  age<=39,  "30-39",
                         ifelse(age>=40 &  age<=49,  "40-49",
                                ifelse(age>=50 &  age<=59,  "50-59",
                                       ifelse(age>=60 & age<=69,  "60-69",
                                              ifelse(age>=70 & age<=79,  "70-79",      
                                                     ifelse(age>=80, ">=80",
                                                            NA))))))))) %>% 
  mutate(age_gr3= factor(age_gr3, 
                         levels = c("<20", "20-29","30-39","40-49", "50-59",
                                    "60-69", "70-79",">=80")))
table(Pop$age_gr3, useNA = "always")


# add gender -----
#8507 male
#8532 female
Pop<-Pop %>% 
  mutate(gender= ifelse(gender_concept_id==8507, "Male",
                        ifelse(gender_concept_id==8532, "Female", NA ))) %>% 
  mutate(gender= factor(gender, 
                        levels = c("Male", "Female")))
table(Pop$gender, useNA = "always")

# if missing (or unreasonable) age or gender, drop ----
Pop<-Pop %>% 
  filter(!is.na(age)) %>% 
  filter(age>=18) %>% 
  filter(age<=110)

Pop<-Pop %>% 
  filter(!is.na(gender))

if(db.name=="OpenClaims"){
Pop<-Pop %>% 
  filter(year_of_birth>1936)}

# drop if missing observation period end date ----
Pop<-Pop %>% 
  filter(!is.na(observation_period_end_date))

# add prior observation time -----
Pop<-Pop %>%  
  mutate(prior_obs_days=as.numeric(difftime(cohort_start_date,
                                            observation_period_start_date,
                                            units="days"))) %>% 
  mutate(prior_obs_years=prior_obs_days/365.25)
# make sure all have year of prior history
Pop<-Pop %>%
  filter(prior_obs_years>=1)





# condition history ------
# add each condition to pop

for(n in 1:length(cond.codes)){# add each to Pop
working.code<-cond.codes[n]
working.name<-cond.names[n]
print(paste0("-- Getting history of ",working.name ," for ", working.study.cohort))
info(logger, paste0("-- Getting history of ",working.name ," for ", working.study.cohort))

working.persons.all.history <- cohortTableComorbidities_db %>%
  rename("person_id"="subject_id") %>% 
  filter(cohort_definition_id==working.code) %>% 
  distinct() %>% 
  inner_join(working.exposure.cohorts_db %>% select(person_id) %>% distinct(),
               by = "person_id")  %>% 
  rename("condition_start_date"="cohort_start_date") %>%  
  select(person_id, condition_start_date) %>% 
  collect() 
# nrow(working.persons.all.history %>% select(person_id) %>% distinct())/
#   nrow(working.persons.all.history)

if(nrow(working.persons.all.history)>0){
working.persons.all.history <-  working.persons.all.history %>% 
  inner_join(Pop %>% select(person_id,cohort_start_date),
               by = "person_id") %>% 
  filter(cohort_start_date>condition_start_date) %>% # before index date
  select(person_id) %>% 
  mutate(working.cond.all.hist=1) %>% 
  distinct()
}

if(nrow(working.persons.all.history)>0){
    Pop<-Pop %>%
      left_join(working.persons.all.history,
                by = "person_id") %>% 
      rename(!!paste0(working.name, ".all.history"):="working.cond.all.hist")
  } else {
    Pop$working.cond.all.hist<-0
    Pop<-Pop %>% 
      rename(!!paste0(working.name, ".all.history"):="working.cond.all.hist")
  }
}

#to zero if absent
Pop<-Pop %>%
  mutate(across(all_of(paste0(cond.names, ".all.history")), ~ replace_na(.x, 0))) 

# covid history -----
covid.cohorts<-covid.cohorts_db %>%
  rename("person_id"="subject_id") %>% 
  rename("covid_date"="cohort_start_date") %>% 
  select(person_id, covid_date) %>% 
  inner_join(working.exposure.cohorts_db %>% 
               select(person_id)) %>% 
  collect()

if(nrow(covid.cohorts)>0){
covid.cohorts<-covid.cohorts %>% 
  mutate(y_covid_date=year(covid_date)) %>% 
  filter(y_covid_date>=2020)
# min(covid.cohort$covid_date)
# add index
covid.cohorts<-covid.cohorts %>% 
  left_join(Pop %>% select(person_id, cohort_start_date)) %>% 
  filter(covid_date<=(cohort_start_date))%>% 
  filter(covid_date>=(cohort_start_date-years(1))) %>% 
  select(person_id) %>% 
  distinct() %>% 
  mutate(covid.year_prior=1)
}

if(nrow(covid.cohorts)>0){
#add
Pop<-Pop %>% 
  left_join(covid.cohorts,
             by = "person_id")
} else {
  Pop$covid.year_prior<-NA
}

Pop<-Pop %>% 
  mutate(covid.year_prior=ifelse(is.na(covid.year_prior), 0,1))
table(Pop$covid.year_prior)

# medication history -----
#  183 days prior to four days prior index date
# add each medication to pop
for(n in 1:length(drug.codes)){# add each to Pop
working.code<-drug.codes[n]
working.name<-drug.names[n]

print(paste0("-- Getting history of ",working.name ," for ", working.study.cohort))
info(logger, paste0("-- Getting history of ",working.name ," for ", working.study.cohort))

working.persons <- cohortTableMedications_db %>% 
  filter(cohort_definition_id==working.code) %>% 
  rename("person_id"="subject_id")  %>% 
  rename("drug_era_start_date"="cohort_start_date")  %>% 
  rename("drug_era_end_date"="cohort_end_date") %>% 
  select(person_id, drug_era_start_date, drug_era_end_date) %>% 
  inner_join(working.exposure.cohorts_db %>% select(person_id) %>% distinct(),
               by = "person_id")%>%  
  collect()

if(nrow(working.persons)>0){ 
working.persons<-working.persons  %>% 
    inner_join(Pop %>% select(person_id,cohort_start_date),
               by = "person_id") %>% 
    filter((drug_era_start_date<=(cohort_start_date-days(4))
           & drug_era_start_date>=(cohort_start_date-days(183))) |
             (drug_era_end_date<=(cohort_start_date-days(4))
           & drug_era_end_date>=(cohort_start_date-days(183)))) %>% 
    select(person_id) %>% 
    mutate(working.drug=1) %>% 
    distinct()
nrow(working.persons %>% select(person_id) %>% distinct())/
  nrow(working.persons)
} 
  
if(nrow(working.persons)>0){
    Pop<-Pop %>%
      left_join(working.persons,
                by = "person_id") %>% 
      rename(!!working.name:="working.drug")
  } else {
    Pop$working.drug<-0
    Pop<-Pop %>% 
      rename(!!working.name:="working.drug")
  }
  
}
#to zero if absent
Pop<-Pop %>%
  mutate(across(all_of(drug.names), ~ replace_na(.x, 0))) 

# composite condition and medication measure -----
c.names<-c("autoimmune_disease",
           "antiphospholipid_syndrome",
           "thrombophilia",
           "atrial_fibrillation",
           "malignant_neoplastic_disease",
           "diabetes_mellitus",
           "obesity",
           "renal_impairment")
c.names<-paste0(c.names, ".all.history")
d.names<-c("antiinflamatory_and_antirheumatic", 
           "coxibs",
           "corticosteroids",
           "hormonal_contraceptives",
           "tamoxifen",
           "sex_hormones_modulators")

if(run.as.test==TRUE){
c.names<-c("autoimmune_disease")
c.names<-paste0(c.names, ".all.history")
d.names<-c("antiinflamatory_and_antirheumatic")
}

Pop$cond.comp<-rowSums(Pop %>% select(all_of(c.names))) 
Pop$drug.comp<-rowSums(Pop %>% select(all_of(d.names))) 
Pop$cond.drug.comp<-rowSums(Pop %>% select(all_of(c(c.names,  d.names)))) 

Pop<-Pop %>% 
  mutate(cond.comp=ifelse(cond.comp==0, 0, 1))%>% 
  mutate(drug.comp=ifelse(drug.comp==0, 0, 1))%>% 
  mutate(cond.drug.comp=ifelse(cond.drug.comp==0, 0, 1))


# add months since start -----
if(working.study.cohort=="General population 2017"){
 Pop<-Pop %>% 
  mutate(months_since_start=NA) 
} else {
Pop<-Pop %>% 
  mutate(months_since_start=as.period(interval(earliest.date,cohort_start_date)),
         unit="day") %>% 
  mutate(months_since_start=months_since_start/ as.period(months(1)))   
}


# summarise characteristics -----
get.summary.characteristics<-function(working.data, working.name){

summary.characteristics1<-data.frame(Overall=t(working.data %>% 
                           mutate(index_year=year(cohort_start_date)) %>% 
                           summarise(n=nice.num.count(length(person_id)),
                                     min.index.date=min(cohort_start_date),
                                     max.index.date=max(cohort_start_date),
                                     index.az=paste0(nice.num.count(sum(str_detect(vax_type,"AZ"))),
                                                      " (",  nice.num((sum(str_detect(vax_type,"AZ"))/
                                                                         length(person_id))*100),  "%)"),
                                     index.jnj=paste0(nice.num.count(sum(str_detect(vax_type,"janssen"))),
                                                      " (",  nice.num((sum(str_detect(vax_type,"janssen"))/
                                                                         length(person_id))*100),  "%)"),
                                     index.pfizer=paste0(nice.num.count(sum(str_detect(vax_type,"pfizer"))),
                                                      " (",  nice.num((sum(str_detect(vax_type,"pfizer"))/
                                                                         length(person_id))*100),  "%)"),
                                     index.mrna=paste0(nice.num.count(sum(str_detect(vax_type,"moderna"))),
                                                      " (",  nice.num((sum(str_detect(vax_type,"moderna"))/
                                                                         length(person_id))*100),  "%)"),
                                     age=paste0(nice.num.count(median(age)),  " [",
                                                nice.num.count(quantile(age,probs=0.25)),  " to ",
                                                nice.num.count(quantile(age,probs=0.75)),   "]" ), 
                                     age.under.20=paste0(nice.num.count(sum(age_gr3=="<20")),
                                                      " (",  nice.num((sum(age_gr3=="<20")/length(person_id))*100),  "%)"),
                                     age.20_29=paste0(nice.num.count(sum(age_gr3=="20-29")),
                                                      " (",  nice.num((sum(age_gr3=="20-29")/length(person_id))*100),  "%)"),
                                     age.30_39=paste0(nice.num.count(sum(age_gr3=="30-39")),
                                                      " (",  nice.num((sum(age_gr3=="30-39")/length(person_id))*100),  "%)"), 
                                     age.40_49=paste0(nice.num.count(sum(age_gr3=="40-49")),
                                                      " (",  nice.num((sum(age_gr3=="40-49")/length(person_id))*100),  "%)"), 
                                     age.50_59=paste0(nice.num.count(sum(age_gr3=="50-59")),
                                                      " (",  nice.num((sum(age_gr3=="50-59")/length(person_id))*100),  "%)"), 
                                     age.60_69=paste0(nice.num.count(sum(age_gr3=="60-69")),
                                                      " (",  nice.num((sum(age_gr3=="60-69")/length(person_id))*100),  "%)"), 
                                     age.70_79=paste0(nice.num.count(sum(age_gr3=="70-79")),
                                                      " (",  nice.num((sum(age_gr3=="70-79")/length(person_id))*100),  "%)"), 
                                     age.80u=paste0(nice.num.count(sum(age_gr3==">=80")),
                                                    " (",  nice.num((sum(age_gr3==">=80")/length(person_id))*100),  "%)"), 
                                     sex.male=paste0(nice.num.count(sum(gender=="Male")),
                                                     " (",  nice.num((sum(gender=="Male")/length(person_id))*100),
                                                     "%)"),
                                     prior_obs_years=paste0(nice.num(median(prior_obs_years)),
                                                            " [", nice.num(quantile(prior_obs_years,probs=0.25)), 
                                                            " to ", nice.num(quantile(prior_obs_years,probs=0.75)),   "]" ))
    ))

    # and all the conds and medications
summary.characteristics2<-data.frame(Overall=t(working.data %>% 
                           summarise_at(.vars = all_of(c("covid.year_prior",
                                                         paste0(cond.names, ".all.history"),
                                                         drug.names)), 
                                        .funs = function(x, tot){
                                          paste0(nice.num.count(sum(x)), " (", nice.num((sum(x)/tot)*100), "%)")
                                        } , tot=nrow(working.data))))

summary.characteristics3<-data.frame(Overall=t(working.data %>% 
                           summarise_at(.vars = all_of(c("cond.comp", "drug.comp", 
                                                         "cond.drug.comp")), 
                                        .funs = function(x, tot){
                                          paste0(nice.num.count(sum(x)), " (", nice.num((sum(x)/tot)*100), "%)")
                                        } , tot=nrow(working.data))) )
  


summary.characteristics<-bind_rows(summary.characteristics1,
                                   summary.characteristics2,
                                   summary.characteristics3)
summary.characteristics$Overall<-as.character(summary.characteristics$Overall)
  
rownames(summary.characteristics)<-str_to_sentence(rownames(summary.characteristics))
rownames(summary.characteristics)<-str_replace(rownames(summary.characteristics),
                                                 "Sex.male", "Sex: Male")
rownames(summary.characteristics)<-str_replace(rownames(summary.characteristics),
                                                 "Prior_obs_years", "Years of prior observation time")
rownames(summary.characteristics)<-str_replace_all(rownames(summary.characteristics) , "_", " ")
  
  #obscure any counts less than 5
summary.characteristics$Overall<-
    ifelse(str_sub(summary.characteristics$Overall, 1, 2) %in%  c("1 ","2 ", "3 ","4 "),
           "<5",summary.characteristics$Overall)
summary.characteristics$var<-row.names(summary.characteristics)
row.names(summary.characteristics)<-1:nrow(summary.characteristics)
  
summary.characteristics <-summary.characteristics %>% 
    mutate(var=ifelse(var=="Cond.comp",
                      "One or more condition of interest", var )) %>% 
    mutate(var=ifelse(var=="Drug.comp",
                      "One or more medication of interest", var )) %>% 
    mutate(var=ifelse(var=="Cond.drug.comp",
                      "One or more condition/ medication of interest", var ))
  
summary.characteristics %>% 
    select(var, Overall) %>% 
    rename(!!working.name:=Overall)
  
}

# overall
# Pop.summary.characteristics<-get.summary.characteristics(Pop, "Overall")
# those with a year of prior history
Pop.summary.characteristics.with.history<-get.summary.characteristics(
  Pop %>% filter(prior_obs_years>=1),
  "Overall")

# age plot data -----
get.age.plot.data<-function(working.data){
ggplot_build(working.data %>%
  ggplot(aes(x = age),
              environment = environment())+
  geom_histogram(binwidth = 3))$data[[1]]
}


Cohort.age.plot.data[[paste0(working.study.cohort,";","No.prior.obs", ";", "pop.all")]]<-
  get.age.plot.data(working.data=Pop) %>% 
    mutate(pop.type="All") %>% 
    mutate(prior.obs.required="No") %>% 
    mutate(pop=working.study.cohort) 
Cohort.age.plot.data[[paste0(working.study.cohort,";","prior.obs", ";", "pop.all")]]<-
  get.age.plot.data(working.data=Pop %>% filter(prior_obs_years>=1)) %>% 
    mutate(pop.type="All") %>% 
    mutate(prior.obs.required="Yes") %>% 
    mutate(pop=working.study.cohort) 


# Cohort.age.plot.data[[paste0(working.study.cohort,";","No.prior.obs", ";", "pop.all")]] %>%
#   ggplot()+
#   geom_col(aes(xmax, density), width=3, colour="grey")


# results for each outcome of interest -----

for(j in 1:n.outcomes.to.run){ # for each outcome of interest
# for(j in 1:2){
working.outcome<-outcome.cohorts$id[j]
working.outcome.name<-outcome.cohorts$name[j]
  
print(paste0("- Getting ", working.outcome.name,
               " (", j, " of ", length(outcome.cohorts$id), ")"))
info(logger, paste0("- Getting ", working.outcome.name,
               " (", j, " of ", length(outcome.cohorts$id), ")"))

working.Pop<-Pop 

# event of interest
working.outcomes<-outcome.cohorts_db %>%
      filter(cohort_definition_id %in% working.outcome) %>%
        inner_join(working.exposure.cohorts_db  %>%
                     rename("subject_id" = "person_id")%>%
                     select(subject_id) %>%
                     distinct()) %>%
      select(subject_id, cohort_start_date) %>% 
      collect()

# any occurrences
if(working.outcomes %>%  
     inner_join(working.Pop %>% 
                select(person_id) %>% 
                rename("subject_id"="person_id")) %>% 
     tally() %>% 
     pull()<5){
  print(paste0("Less than five occurrences of ", working.outcome.name, " among study population"))
  info(logger, paste0("Less than five occurrences of ", working.outcome.name, " among study population"))

  } else {
    # continue only if occurrences of outcome
    
# drop anyone with the outcome in the year prior to the index ------
washout.outcomes<-working.outcomes %>% 
      inner_join(working.Pop %>% 
                   select(person_id,cohort_start_date) %>% 
                   rename("subject_id"="person_id") %>% 
                   rename("Pop_cohort_start_date"="cohort_start_date")) %>% 
      filter(cohort_start_date>= (Pop_cohort_start_date-years(1))) %>% 
      filter(cohort_start_date< Pop_cohort_start_date) # nb excluding index date itself
    
working.Pop<-working.Pop %>% 
      anti_join(washout.outcomes %>% 
                  select(subject_id) %>% 
                  distinct(),
                by=c("person_id"="subject_id"))  
    
# history of outcome event -----
history.outcomes<-working.outcomes %>%  
      inner_join(working.Pop %>% 
                   select(person_id,cohort_start_date) %>% 
                   rename("subject_id"="person_id") %>% 
                   rename("Pop_cohort_start_date"="cohort_start_date")) %>% 
      filter(cohort_start_date< (Pop_cohort_start_date-years(1))) 
    
working.Pop<-working.Pop %>% 
      left_join(history.outcomes  %>% 
                  select(subject_id) %>% 
                  distinct() %>% 
                  mutate(history_outcome=1),
                by=c("person_id"="subject_id"))
working.Pop<-working.Pop %>% 
      mutate(history_outcome=ifelse(is.na(history_outcome),0,1))
    
# first event after index date up to  -----
if(working.study.cohort=="General population 2017"){
f_u.outcome<-working.outcomes %>%  
        inner_join(working.Pop %>% 
                     select(person_id,cohort_start_date, 
                            next_vax_date) %>% 
                     rename("subject_id"="person_id") %>% 
                     rename("Pop_cohort_start_date"="cohort_start_date"))  %>% 
    #within 2 years
        filter(cohort_start_date>= Pop_cohort_start_date) %>% 
        filter(cohort_start_date<= (Pop_cohort_start_date+years(2)))

} else {
f_u.outcome<-working.outcomes %>%  
        inner_join(working.Pop %>% 
                     select(person_id,cohort_start_date, 
                            next_vax_date) %>% 
                     rename("subject_id"="person_id") %>% 
                     rename("Pop_cohort_start_date"="cohort_start_date"))  %>% 
    #within 28 days
        filter(cohort_start_date>= Pop_cohort_start_date) %>% 
        filter(cohort_start_date<= (Pop_cohort_start_date+days(28))) %>% 
   #  on or after next vax
        filter(is.na(next_vax_date) |
          cohort_start_date< next_vax_date) 
}

if(nrow(f_u.outcome)>=5){ 
f_u.outcome<-f_u.outcome %>% 
        group_by(subject_id) %>%
        arrange(cohort_start_date) %>% 
        mutate(seq=1:length(subject_id)) %>% 
        filter(seq==1) %>% 
        select(subject_id,cohort_start_date)  %>% 
        rename("f_u.outcome_date"="cohort_start_date") %>% 
        mutate(f_u.outcome=1)
working.Pop<-working.Pop %>% 
        left_join(f_u.outcome,
                  by=c("person_id"="subject_id"))
working.Pop<-working.Pop %>% 
        mutate(f_u.outcome=ifelse(is.na(f_u.outcome),0,1))
      # sum(working.Pop$f_u.outcome)
      
# TAR -----
# will be from the 1st vaccination date to 28-days (2 years for general pop) or 
# the earliest of experiencing outcome, 
# the last data availability date or the date of 2nd vaccination. 

if(working.study.cohort=="General population 2017"){
working.Pop<-working.Pop %>%
          mutate(f_u.outcome_date=
                   if_else(f_u.outcome==1,f_u.outcome_date, 
                  if_else(observation_period_end_date < (cohort_start_date+years(2)),
                            observation_period_end_date, 
                            (cohort_start_date+years(2)) )))
} else {
 working.Pop<-working.Pop %>%
          mutate(f_u.outcome_date=
                   if_else(f_u.outcome==1,f_u.outcome_date, 
                   if_else(!is.na(next_vax_date) &
                     next_vax_date< (cohort_start_date+days(28)),
                            next_vax_date,
                  if_else(observation_period_end_date < (cohort_start_date+days(28)),
                            observation_period_end_date, 
                            (cohort_start_date+days(28)) )))) 
  
}


working.Pop<-working.Pop %>% 
        mutate(f_u.outcome.days=as.numeric(difftime(f_u.outcome_date,
                                                    cohort_start_date, 
                                                    units="days")))
# working.Pop %>%
#         group_by(f_u.outcome) %>%
#         summarise(min(f_u.outcome.days),
#                   median(f_u.outcome.days),
#                   max(f_u.outcome.days))
      
# working.Pop.w.outcome -----
# working.Pop.w_o.outcome<-working.Pop %>% filter(f_u.outcome==0) 
working.Pop.w.outcome<-working.Pop %>% filter(f_u.outcome==1) 
      
# summarise characteristics -----
# overall
# Pop.summary.characteristics<-left_join(Pop.summary.characteristics,
#                                 get.summary.characteristics(working.Pop.w.outcome, working.outcome.name),
#                                              by="var")
# #with.history
# # overall
Pop.summary.characteristics.with.history<-left_join(Pop.summary.characteristics.with.history,
                                                          get.summary.characteristics(working.Pop.w.outcome %>%
                                                                                        filter(prior_obs_years>=1), working.outcome.name),
                                                          by="var")


# age plot data -----
if(nrow(working.Pop.w.outcome) >30 ) {
Cohort.age.plot.data[[paste0(working.study.cohort,";","No.prior.obs", ";", "pop.all", ";",working.outcome.name)]]<-
  get.age.plot.data(working.data=working.Pop.w.outcome ) %>% 
    mutate(pop.type="All") %>% 
    mutate(prior.obs.required="No")  %>% 
    mutate(pop=paste0(working.study.cohort,";",working.outcome.name))
}

if(nrow(working.Pop.w.outcome %>% filter(prior_obs_years>=1)) >30 ) {
Cohort.age.plot.data[[paste0(working.study.cohort,";","prior.obs", ";", "pop.all", ";",working.outcome.name)]]<-
  get.age.plot.data(working.data=working.Pop.w.outcome  %>%
                      filter(prior_obs_years>=1)) %>% 
    mutate(pop.type="All") %>% 
    mutate(prior.obs.required="Yes") %>% 
    mutate(pop=paste0(working.study.cohort,";",working.outcome.name))
}
# Cohort.age.plot.data[[paste0(working.study.cohort,";","No.prior.obs", ";", "pop.all", ";",working.outcome.name)]] %>%
#   ggplot()+
#   geom_col(aes(xmax, density), width=3, colour="grey")

# KM Cumulative incidence ------
get.Surv.summaries<-function(working.data,
                             value.prior.obs.required,
                             value.pop.type,
                             value.working.outcome,
                             value.working.outcome.name,
                             value.working.study.cohort
                             ){
  
  if(nrow(working.data)>=5){
  # add to working.Survival.summary
  working.Survival.summary<-list()
  working.times<-seq(0,max(working.data$f_u.outcome.days))
  
  #overall
  #km method
  s<-summary(survfit(Surv(f_u.outcome.days, f_u.outcome) ~ 1, 
                     data = working.data), times = working.times)
  working.Survival.summary[[paste0(value.working.study.cohort,";",value.working.outcome.name,";",
                                   "overall")]]<-data.frame(
                                     time=s$time,
                                     n.risk=s$n.risk,
                                     n.event= s$n.event,
                                     surv=s$surv,
                                     lower=s$lower,
                                     upper=s$upper,
                                     group="overall")%>% 
    mutate(strata="overall") %>% 
    mutate(outcome=value.working.outcome) %>% 
    mutate(outcome.name=value.working.outcome.name) %>% 
    mutate(pop.type=value.pop.type) %>% 
    mutate(prior.obs.required=value.prior.obs.required)  %>% 
    mutate(pop=value.working.study.cohort) %>% 
    mutate(surv.type="KM")

  #gender
  #km method  
  s<-summary(survfit(Surv(f_u.outcome.days, f_u.outcome) ~ gender, 
                     data = working.data), times = working.times)
  working.Survival.summary[[paste0(value.working.study.cohort,";",value.working.outcome.name,
                                   ";","gender")]]<-data.frame(
                                     time=s$time,
                                     n.risk=s$n.risk,
                                     n.event= s$n.event,
                                     surv=s$surv,
                                     lower=s$lower,
                                     upper=s$upper,
                                     group=s$strata)%>% 
    mutate(strata="gender") %>% 
    mutate(outcome=value.working.outcome) %>% 
    mutate(outcome.name=value.working.outcome.name) %>% 
    mutate(pop.type=value.pop.type) %>% 
    mutate(prior.obs.required=value.prior.obs.required)  %>% 
    mutate(pop=value.working.study.cohort)%>% 
    mutate(surv.type="KM")

  
# cond.comp  
if( nrow(working.data %>% filter(cond.comp==1))>5 ){
  s<-summary(survfit(Surv(f_u.outcome.days, f_u.outcome) ~ cond.comp, 
                     data = working.data), times = working.times)
  working.Survival.summary[[paste0(value.working.study.cohort,";",value.working.outcome.name,
                                   ";","cond.comp")]]<-data.frame(
                                     time=s$time,
                                     n.risk=s$n.risk,
                                     n.event= s$n.event,
                                     surv=s$surv,
                                     lower=s$lower,
                                     upper=s$upper,
                                     group=s$strata)%>% 
    mutate(strata="cond.comp") %>% 
    mutate(outcome=value.working.outcome) %>% 
    mutate(outcome.name=value.working.outcome.name) %>% 
    mutate(pop.type=value.pop.type) %>% 
    mutate(prior.obs.required=value.prior.obs.required)  %>% 
    mutate(pop=value.working.study.cohort)%>% 
    mutate(surv.type="KM")
}
  
if( nrow(working.data %>% filter(drug.comp==1))>5 ){
  s<-summary(survfit(Surv(f_u.outcome.days, f_u.outcome) ~ drug.comp, 
                     data = working.data), times = working.times)
  working.Survival.summary[[paste0(value.working.study.cohort,";",value.working.outcome.name,
                                   ";","drug.comp")]]<-data.frame(
                                     time=s$time,
                                     n.risk=s$n.risk,
                                     n.event= s$n.event,
                                     surv=s$surv,
                                     lower=s$lower,
                                     upper=s$upper,
                                     group=s$strata)%>% 
    mutate(strata="drug.comp") %>% 
    mutate(outcome=value.working.outcome) %>% 
    mutate(outcome.name=value.working.outcome.name) %>% 
    mutate(pop.type=value.pop.type) %>% 
    mutate(prior.obs.required=value.prior.obs.required)  %>% 
    mutate(pop=value.working.study.cohort)%>% 
    mutate(surv.type="KM")
   
  
  
}
  
if( nrow(working.data %>% filter(cond.drug.comp==1))>5 ){
  s<-summary(survfit(Surv(f_u.outcome.days, f_u.outcome) ~ cond.drug.comp, 
                     data = working.data), times = working.times)
  working.Survival.summary[[paste0(value.working.study.cohort,";",value.working.outcome.name,
                                   ";","cond.drug.comp")]]<-data.frame(
                                     time=s$time,
                                     n.risk=s$n.risk,
                                     n.event= s$n.event,
                                     surv=s$surv,
                                     lower=s$lower,
                                     upper=s$upper,
                                     group=s$strata)%>% 
    mutate(strata="cond.drug.comp") %>% 
    mutate(outcome=value.working.outcome) %>% 
    mutate(outcome.name=value.working.outcome.name) %>% 
    mutate(pop.type=value.pop.type) %>% 
    mutate(prior.obs.required=value.prior.obs.required)  %>% 
    mutate(pop=value.working.study.cohort)%>% 
    mutate(surv.type="KM")

}
  
  s<-summary(survfit(Surv(f_u.outcome.days, f_u.outcome) ~ age_gr+gender, 
                     data = working.data), times = working.times)
  working.Survival.summary[[paste0(value.working.study.cohort,";",value.working.outcome.name,
                                   ";","age_gr_gender")]]<-data.frame(
                                     time=s$time,
                                     n.risk=s$n.risk,
                                     n.event= s$n.event,
                                     surv=s$surv,
                                     lower=s$lower,
                                     upper=s$upper,
                                     group=s$strata)%>% 
    mutate(strata="age_gr_gender") %>% 
    mutate(outcome=value.working.outcome) %>% 
    mutate(outcome.name=value.working.outcome.name) %>% 
    mutate(pop.type=value.pop.type) %>% 
    mutate(prior.obs.required=value.prior.obs.required) %>% 
    mutate(pop=value.working.study.cohort)%>% 
    mutate(surv.type="KM")

  
  
  s<-summary(survfit(Surv(f_u.outcome.days, f_u.outcome) ~ age_gr2+gender, 
                     data = working.data), times = working.times)
  working.Survival.summary[[paste0(value.working.study.cohort,";",value.working.outcome.name,
                                   ";","age_gr2_gender")]]<-data.frame(
                                     time=s$time,
                                     n.risk=s$n.risk,
                                     n.event= s$n.event,
                                     surv=s$surv,
                                     lower=s$lower,
                                     upper=s$upper,
                                     group=s$strata)%>% 
    mutate(strata="age_gr2_gender") %>% 
    mutate(outcome=value.working.outcome) %>% 
    mutate(outcome.name=value.working.outcome.name) %>% 
    mutate(pop.type=value.pop.type) %>% 
    mutate(prior.obs.required=value.prior.obs.required)   %>% 
    mutate(pop=value.working.study.cohort)%>% 
    mutate(surv.type="KM")

  
s<-summary(survfit(Surv(f_u.outcome.days, f_u.outcome) ~ age_gr3+gender, 
                     data = working.data), times = working.times)
working.Survival.summary[[paste0(value.working.study.cohort,";",value.working.outcome.name,
                                   ";","age_gr3_gender")]]<-data.frame(
                                     time=s$time,
                                     n.risk=s$n.risk,
                                     n.event= s$n.event,
                                     surv=s$surv,
                                     lower=s$lower,
                                     upper=s$upper,
                                     group=s$strata)%>% 
    mutate(strata="age_gr3_gender") %>% 
    mutate(outcome=value.working.outcome) %>% 
    mutate(outcome.name=value.working.outcome.name) %>% 
    mutate(pop.type=value.pop.type) %>% 
    mutate(prior.obs.required=value.prior.obs.required)   %>% 
    mutate(pop=value.working.study.cohort)%>% 
    mutate(surv.type="KM")

s<-summary(survfit(Surv(f_u.outcome.days, f_u.outcome) ~ age_gr, 
                     data = working.data), times = working.times)
working.Survival.summary[[paste0(value.working.study.cohort,";",value.working.outcome.name,
                                   ";","age_gr")]]<-data.frame(
                                     time=s$time,
                                     n.risk=s$n.risk,
                                     n.event= s$n.event,
                                     surv=s$surv,
                                     lower=s$lower,
                                     upper=s$upper,
                                     group=s$strata)%>% 
    mutate(strata="age_gr") %>% 
    mutate(outcome=value.working.outcome) %>% 
    mutate(outcome.name=value.working.outcome.name) %>% 
    mutate(pop.type=value.pop.type) %>% 
    mutate(prior.obs.required=value.prior.obs.required)  %>% 
    mutate(pop=value.working.study.cohort)%>% 
    mutate(surv.type="KM")

  s<-summary(survfit(Surv(f_u.outcome.days, f_u.outcome) ~ age_gr2, 
                     data = working.data), times = working.times)
  working.Survival.summary[[paste0(value.working.study.cohort,";",value.working.outcome.name,
                                   ";","age_gr2")]]<-data.frame(
                                     time=s$time,
                                     n.risk=s$n.risk,
                                     n.event= s$n.event,
                                     surv=s$surv,
                                     lower=s$lower,
                                     upper=s$upper,
                                     group=s$strata)%>% 
    mutate(strata="age_gr2") %>% 
    mutate(outcome=value.working.outcome) %>% 
    mutate(outcome.name=value.working.outcome.name) %>% 
    mutate(pop.type=value.pop.type) %>% 
    mutate(prior.obs.required=value.prior.obs.required)   %>% 
    mutate(pop=value.working.study.cohort)%>% 
    mutate(surv.type="KM")

  s<-summary(survfit(Surv(f_u.outcome.days, f_u.outcome) ~ age_gr3, 
                     data = working.data), times = working.times)
  working.Survival.summary[[paste0(value.working.study.cohort,";",value.working.outcome.name,
                                   ";","age_gr3")]]<-data.frame(
                                     time=s$time,
                                     n.risk=s$n.risk,
                                     n.event= s$n.event,
                                     surv=s$surv,
                                     lower=s$lower,
                                     upper=s$upper,
                                     group=s$strata)%>% 
    mutate(strata="age_gr3") %>% 
    mutate(outcome=value.working.outcome) %>% 
    mutate(outcome.name=value.working.outcome.name) %>% 
    mutate(pop.type=value.pop.type) %>% 
    mutate(prior.obs.required=value.prior.obs.required)   %>% 
    mutate(pop=value.working.study.cohort)%>% 
    mutate(surv.type="KM")

  bind_rows(working.Survival.summary,.id = NULL)
  } else {
    tibble()
  }
  
}

# Survival.summary[[paste0(working.study.cohort,";",working.outcome.name,";","No.prior.obs", ";", "pop.all")]]<-
#     get.Surv.summaries(working.data=working.Pop,
#                        value.prior.obs.required="No",
#                        value.pop.type="All",
#                        value.working.outcome=working.outcome,
#                        value.working.outcome.name=working.outcome.name,
#                        value.working.study.cohort=working.study.cohort)
# 
# if(nrow(working.Pop %>% filter(prior_obs_years>=1)) >5 ) {
# Survival.summary[[paste0(working.study.cohort,";",working.outcome.name,";","No.prior.obs", ";", "pop.no.covid")]]<-
#     get.Surv.summaries(working.data=working.Pop %>% 
#                          filter(covid.year_prior==0),
#                        value.prior.obs.required="No",
#                        value.pop.type="No covid",
#                        value.working.outcome=working.outcome,
#                        value.working.outcome.name=working.outcome.name,
#                        value.working.study.cohort=working.study.cohort)
# }
# 
# if(nrow(working.Pop %>% filter(prior_obs_years>=1)) >5 ) {
# Survival.summary[[paste0(working.study.cohort,";",working.outcome.name,";","No.prior.obs", ";", "pop.covid")]]<-
#     get.Surv.summaries(working.data=working.Pop %>% 
#                          filter(covid.year_prior==1),
#                        value.prior.obs.required="No",
#                        value.pop.type="Covid",
#                        value.working.outcome=working.outcome,
#                        value.working.outcome.name=working.outcome.name,
#                        value.working.study.cohort=working.study.cohort)
# }
#

if(nrow(working.Pop %>% filter(prior_obs_years>=1)) >5 ) {
Survival.summary[[paste0(working.study.cohort,";",working.outcome.name,";","Prior.obs", ";", "pop.all")]]<-
  get.Surv.summaries(working.data=working.Pop %>%
                       filter(prior_obs_years>=1),
                     value.prior.obs.required="Yes",
                     value.pop.type="All",
                     value.working.outcome=working.outcome,
                     value.working.outcome.name=working.outcome.name,
                     value.working.study.cohort=working.study.cohort)
}

if(nrow(working.Pop %>% filter(prior_obs_years>=1)) >5 ) {
Survival.summary[[paste0(working.study.cohort,";",working.outcome.name,";","Prior.obs", ";", "pop.no.covid")]]<-
  get.Surv.summaries(working.data=working.Pop %>%
                       filter(prior_obs_years>=1)%>%
                         filter(covid.year_prior==0),
                     value.prior.obs.required="Yes",
                     value.pop.type="No covid",
                     value.working.outcome=working.outcome,
                     value.working.outcome.name=working.outcome.name,
                     value.working.study.cohort=working.study.cohort)
}

if(nrow(working.Pop %>% filter(prior_obs_years>=1)) >5 ) {
Survival.summary[[paste0(working.study.cohort,";",working.outcome.name,";","Prior.obs", ";", "pop.covid")]]<-
  get.Surv.summaries(working.data=working.Pop %>%
                       filter(prior_obs_years>=1)%>%
                         filter(covid.year_prior==1),
                     value.prior.obs.required="Yes",
                     value.pop.type="Covid",
                     value.working.outcome=working.outcome,
                     value.working.outcome.name=working.outcome.name,
                     value.working.study.cohort=working.study.cohort)
}
# modelling  ------
print(" -- Fitting models")
info(logger," -- Fitting models")

if(working.study.cohort=="General population 2017"){
dd<<-suppressWarnings( datadist(working.Pop %>% 
                select(-c("day_of_birth", "month_of_birth", "next_vax_date",
                         "cohort_definition_id",
                         "months_since_start")))); options(datadist = "dd" )  
} else { 
dd<<-suppressWarnings( datadist(working.Pop %>% 
                select(-c("day_of_birth", "month_of_birth", "next_vax_date",
                         "cohort_definition_id" )))); options(datadist = "dd" )
}

get.models<-function(working.data){

if(nrow(working.data)>15000000 &
   str_detect(working.outcome.name,"TTS_")){
working.data<-working.data %>% 
   slice_sample(n=15000000)
} 
  
  if(nrow(working.data)>15000000 &
   str_detect(working.outcome.name,"CVST")){
working.data<-working.data %>% 
   slice_sample(n=15000000)
  } 
  
  if(nrow(working.data)>15000000 &
   str_detect(working.outcome.name,"SVT")){
working.data<-working.data %>% 
   slice_sample(n=15000000)
} 
  
if(nrow(working.data)>6000000 &
   str_detect(working.outcome.name,"CVST", negate=TRUE) &
   str_detect(working.outcome.name,"SVT", negate=TRUE) &
   str_detect(working.outcome.name,"TTS_", negate=TRUE)){
working.data<-working.data %>% 
   slice_sample(n=6000000)
}   
  

n.tot<-working.data  %>% tally() %>% pull()
n.w.outcome <-working.data %>% filter(f_u.outcome==1) %>% tally()  %>% pull() 
  
# choose whether to fit age as linear or with rcs (3 knots)
m.age.linear<-lrm(f_u.outcome ~ age,
       x=TRUE,y=TRUE,  maxit=100000,
       data = working.data)
m.age.rcs.3<-lrm(f_u.outcome ~ rcs(age,3),
       x=TRUE,y=TRUE,  maxit=100000,
       data = working.data)
age.fit<-ifelse(BIC(m.age.linear)<=BIC(m.age.rcs.3),
       "age", "rcs(age,3)")
# interaction between age and sex
a<-anova(lrm(as.formula(paste("f_u.outcome~age_gr2*gender")),
       x=TRUE,y=TRUE,  maxit=100000,
       data = working.data))
interaction<-data.frame(a)[6,3]<0.05

age.gender.f<-paste0(age.fit, "+ gender")

# 1.1 age, stratified by gender 
if(interaction==TRUE){
m.age.male<-lrm(as.formula(paste("f_u.outcome~", age.fit)),
       x=TRUE,y=TRUE,  maxit=100000,
       data = working.data %>% filter(gender=="Male"))
m.age.female<-lrm(as.formula(paste("f_u.outcome~", age.fit)),
       x=TRUE,y=TRUE,  maxit=100000,
       data = working.data %>% filter(gender=="Female"))  
} else {
 m.age<-lrm(as.formula(paste("f_u.outcome~", age.fit)),
       x=TRUE,y=TRUE,  maxit=100000,
       data = working.data)
}



# summarise relative hazard ratios for age
ages<-seq(20,90, 5)
ref.age<-65

working.summary.age.male<-list()
for(age.i in 1:length(ages)){
if(interaction==TRUE){
working.summary.age.male[[paste0(age.i, ".overall")]]<-head(as.data.frame(summary(m.age.male, 
        age=c(65,ages[age.i]),
        antilog=FALSE)),1)   %>% 
  mutate(model.type="overall")
} else {
working.summary.age.male[[paste0(age.i, ".overall")]]<-head(as.data.frame(summary(m.age, 
        age=c(65,ages[age.i]),
        antilog=FALSE)),1)   %>% 
  mutate(model.type="overall")
  } 
}

working.summary.age.male<-bind_rows(working.summary.age.male) %>% 
  mutate(n.tot=n.tot,
         n.w.outcome=n.w.outcome,
         or=exp(Effect),
         or.low=exp(`Lower 0.95`),
         or.high=exp(`Upper 0.95`)) %>% 
  mutate(ref.age=Low,
         rel.age=High) %>% 
  select(n.tot, n.w.outcome, or, or.low, or.high, ref.age, rel.age, model.type) %>%  
  mutate(gender="Male")


working.summary.age.female<-list()
for(age.i in 1:length(ages)){
if(interaction==TRUE){
working.summary.age.female[[paste0(age.i, ".overall")]]<-head(as.data.frame(summary(m.age.female, 
        age=c(65,ages[age.i]),
        antilog=FALSE)),1)   %>% 
  mutate(model.type="overall")
} else {
working.summary.age.female[[paste0(age.i, ".overall")]]<-head(as.data.frame(summary(m.age, 
        age=c(65,ages[age.i]),
        antilog=FALSE)),1)   %>% 
  mutate(model.type="overall")
  } 
}
working.summary.age.female<-bind_rows(working.summary.age.female) %>% 
  mutate(n.tot=n.tot,
         n.w.outcome=n.w.outcome,
         or=exp(Effect),
         or.low=exp(`Lower 0.95`),
         or.high=exp(`Upper 0.95`)) %>% 
  mutate(ref.age=Low,
         rel.age=High) %>% 
  select(n.tot, n.w.outcome, or, or.low, or.high, ref.age, rel.age, model.type) %>%  
  mutate(gender="Female")

working.summary.age<-bind_rows(working.summary.age.male, working.summary.age.female)
# working.summary.age %>%
#   ggplot()+
#   facet_grid(. ~ model.type)+
#   geom_line(aes(rel.age, or, colour=gender))+
#   geom_line(aes(rel.age, or.low, colour=gender), linetype="dashed")+
#   geom_line(aes(rel.age, or.high, colour=gender), linetype="dashed")


# # 1.2 age categories
# m.age.cat<-lrm(as.formula(paste("f_u.outcome~", "age_gr2")),
#        x=TRUE,y=TRUE,  maxit=100000,
#        data = working.data)
# 
# 
# working.summary.age_gr<-head(as.data.frame(summary(m.age.cat, age_gr2='<=44', antilog=FALSE)),2) %>% 
#   mutate(n.tot=n.tot,
#          n.w.outcome=n.w.outcome,
#          or=exp(Effect),
#          or.low=exp(`Lower 0.95`),
#          or.high=exp(`Upper 0.95`)) %>% 
#   select(n.tot, n.w.outcome,or, or.low, or.high)%>%  
#   mutate(model=c("Unadjusted;45-64","Unadjusted;>=65"))  %>% 
#   mutate(model.type="overall")



# 2 gender
# unadjusted 
m.unadj<-lrm(as.formula(paste("f_u.outcome~","gender")),
       x=TRUE,y=TRUE,  maxit=100000,
       data = working.data)
# adjusted for age
m.adj<-lrm(as.formula(paste("f_u.outcome~","gender", "+",  age.fit)),
       x=TRUE,y=TRUE,  maxit=100000,
       data = working.data)

working.summary.gender<- bind_rows(
head(as.data.frame(summary(m.unadj, gender='Female', antilog=FALSE)),1) %>% 
  mutate(n.tot=n.tot,
         n.w.outcome=n.w.outcome,
         or=exp(Effect),
         or.low=exp(`Lower 0.95`),
         or.high=exp(`Upper 0.95`)) %>% 
  select(n.tot, n.w.outcome,or, or.low, or.high)%>%  
  mutate(model="Unadjusted") %>% 
  mutate(model.type="overall"),

tail(as.data.frame(summary(m.adj, gender='Female',antilog=FALSE)),1) %>% 
  mutate(n.tot=n.tot,
         n.w.outcome=n.w.outcome,
         or=exp(Effect),
         or.low=exp(`Lower 0.95`),
         or.high=exp(`Upper 0.95`)) %>% 
  select(n.tot, n.w.outcome, or, or.low, or.high)%>%  
  mutate(model="Adjusted for age") %>% 
  mutate(model.type="overall")) %>% 
 mutate(var="Sex (Male:Female)")


# 3 other exposures of interest
# unadjusted and adjusted for age and gender (or in the case of gender, just age)
get.models.exposures<-function(var){
  if(nrow(working.data %>% 
    filter(!!as.name(var)==1))>5){

m.unadj<-lrm(as.formula(paste("f_u.outcome~",{{var}})),
       x=TRUE,y=TRUE,  maxit=100000,
       data = working.data)
m.adj<-lrm(as.formula(paste("f_u.outcome~",{{var}}, "+",  age.gender.f)),
       x=TRUE,y=TRUE,  maxit=100000,
       data = working.data)
if(nrow(working.data %>% 
    filter(!is.na(months_since_start)))>0){
m.adj2<-lrm(as.formula(paste("f_u.outcome~",{{var}}, "+",  age.gender.f, 
                             "+ months_since_start")),
       x=TRUE,y=TRUE,  maxit=100000,
       data = working.data) }

if(nrow(working.data %>% 
    filter(!is.na(months_since_start)))>0){
bind_rows(
head(as.data.frame(summary(m.unadj, antilog=FALSE)),1) %>% 
  mutate(n.tot=n.tot,
         n.w.outcome=n.w.outcome,
         or=exp(Effect),
         or.low=exp(`Lower 0.95`),
         or.high=exp(`Upper 0.95`)) %>% 
  select(or, or.low, or.high)%>%  
  mutate(model="Unadjusted") %>% 
  mutate(model.type="overall"),

head(as.data.frame(summary(m.adj, antilog=FALSE)),1) %>% 
  mutate(n.tot=n.tot,
         n.w.outcome=n.w.outcome,
         or=exp(Effect),
         or.low=exp(`Lower 0.95`),
         or.high=exp(`Upper 0.95`)) %>% 
  select(or, or.low, or.high)%>%  
  mutate(model="Adjusted for age and sex") %>% 
  mutate(model.type="overall"), 

head(as.data.frame(summary(m.adj2, antilog=FALSE)),1) %>% 
  mutate(n.tot=n.tot,
         n.w.outcome=n.w.outcome,
         or=exp(Effect),
         or.low=exp(`Lower 0.95`),
         or.high=exp(`Upper 0.95`)) %>% 
  select(or, or.low, or.high)%>%  
  mutate(model="Adjusted for age, sex, and calendar time") %>% 
  mutate(model.type="overall")
) %>% 
  mutate(var={{var}}) 
} else {
  
bind_rows(
head(as.data.frame(summary(m.unadj, antilog=FALSE)),1) %>% 
  mutate(n.tot=n.tot,
         n.w.outcome=n.w.outcome,
         or=exp(Effect),
         or.low=exp(`Lower 0.95`),
         or.high=exp(`Upper 0.95`)) %>% 
  select(or, or.low, or.high)%>%  
  mutate(model="Unadjusted") %>% 
  mutate(model.type="overall"),

head(as.data.frame(summary(m.adj, antilog=FALSE)),1) %>% 
  mutate(n.tot=n.tot,
         n.w.outcome=n.w.outcome,
         or=exp(Effect),
         or.low=exp(`Lower 0.95`),
         or.high=exp(`Upper 0.95`)) %>% 
  select(or, or.low, or.high)%>%  
  mutate(model="Adjusted for age and sex") %>% 
  mutate(model.type="overall")
) %>% 
  mutate(var={{var}})   
  
  
}


} else {
   tibble()
 }  
  
  
  
  

}

working.summary.exposures<-list()

if(run.as.test!=TRUE){
working.summary.exposures[[1]]<-get.models.exposures("asthma.all.history")
working.summary.exposures[[2]]<-get.models.exposures("diabetes_mellitus.all.history")
working.summary.exposures[[3]]<-get.models.exposures("malignant_neoplastic_disease.all.history")
working.summary.exposures[[4]]<-get.models.exposures("heart_disease.all.history")
working.summary.exposures[[5]]<-get.models.exposures("hypertensive_disorder.all.history")
working.summary.exposures[[6]]<-get.models.exposures("obesity.all.history")
working.summary.exposures[[7]]<-get.models.exposures("renal_impairment.all.history")
working.summary.exposures[[8]]<-get.models.exposures("cond.comp")

working.summary.exposures[[9]]<-get.models.exposures("antiinflamatory_and_antirheumatic")
working.summary.exposures[[10]]<-get.models.exposures("antithrombotic")
working.summary.exposures[[11]]<-get.models.exposures("corticosteroids")
working.summary.exposures[[12]]<-get.models.exposures("drug.comp") 
} else {
working.summary.exposures[[1]]<-get.models.exposures("cond.comp")
}

working.summary.exposures<- bind_rows(working.summary.exposures)


row.names(working.summary.age)<-1:nrow(working.summary.age)
# row.names(working.summary.age_gr)<-1:nrow(working.summary.age_gr)
row.names(working.summary.gender)<-1:nrow(working.summary.gender)
if(nrow(working.summary.exposures)>0){ 
row.names(working.summary.exposures)<-1:nrow(working.summary.exposures)}

bind_rows(working.summary.age,
          # working.summary.age_gr,
          working.summary.gender,
          working.summary.exposures)

}

# if(nrow(working.Pop %>% 
#         filter(f_u.outcome==1))>=30){  
# Model.estimates[[paste0(working.study.cohort,
#                                    ";",working.outcome.name,";",
#                                    "No.prior.obs", ";",
#                                    "pop.all")]]<-get.models(working.Pop) %>% 
#   mutate(prior.obs.required="No",
#          pop.type="All",
#          working.outcome=working.outcome,
#          working.outcome.name=working.outcome.name,
#          working.study.cohort=working.study.cohort)
# }
# 
# if(nrow(working.Pop %>% 
#         filter(covid.year_prior==0) %>% 
#         filter(f_u.outcome==1))>=30){  
# Model.estimates[[paste0(working.study.cohort,
#                                    ";",working.outcome.name,";",
#                                    "No.prior.obs", ";",
#                                    "pop.no.covid")]]<-get.models(working.Pop %>% 
#         filter(covid.year_prior==0) ) %>% 
#   mutate(prior.obs.required="No",
#          pop.type="No covid",
#          working.outcome=working.outcome,
#          working.outcome.name=working.outcome.name,
#          working.study.cohort=working.study.cohort)
# }
# 
# if(nrow(working.Pop %>% 
#         filter(covid.year_prior==1) %>% 
#         filter(f_u.outcome==1))>=30){  
# Model.estimates[[paste0(working.study.cohort,
#                                    ";",working.outcome.name,";",
#                                    "No.prior.obs", ";",
#                                    "pop.covid")]]<-get.models(working.Pop %>% 
#         filter(covid.year_prior==1) ) %>% 
#   mutate(prior.obs.required="No",
#          pop.type="Covid",
#          working.outcome=working.outcome,
#          working.outcome.name=working.outcome.name,
#          working.study.cohort=working.study.cohort)
# }


if(nrow(working.Pop %>%
        filter(prior_obs_years>=1) %>%
        filter(f_u.outcome==1))>=30){
Model.estimates[[paste0(working.study.cohort,
                                   ";",working.outcome.name,";",
                                   "prior.obs", ";",
                                   "pop.all")]]<-get.models(working.Pop %>%   filter(prior_obs_years>=1)) %>%
  mutate(prior.obs.required="Yes",
         pop.type="All",
         working.outcome=working.outcome,
         working.outcome.name=working.outcome.name,
         working.study.cohort=working.study.cohort)}

if(nrow(working.Pop %>%
        filter(covid.year_prior==0) %>%
        filter(prior_obs_years>=1) %>%
        filter(f_u.outcome==1))>=30){
Model.estimates[[paste0(working.study.cohort,
                                   ";",working.outcome.name,";",
                                   "prior.obs", ";",
                                   "no.covid")]]<-get.models(working.Pop %>%
                                                               filter(covid.year_prior==0) %>%
                                                               filter(prior_obs_years>=1)) %>%
  mutate(prior.obs.required="Yes",
         pop.type="No covid",
         working.outcome=working.outcome,
         working.outcome.name=working.outcome.name,
         working.study.cohort=working.study.cohort)}

if(nrow(working.Pop %>%
        filter(covid.year_prior==1) %>%
        filter(prior_obs_years>=1) %>%
        filter(f_u.outcome==1))>=30){
Model.estimates[[paste0(working.study.cohort,
                                   ";",working.outcome.name,";",
                                   "prior.obs", ";",
                                   "Covid")]]<-get.models(working.Pop %>%
                                                               filter(covid.year_prior==1) %>%
                                                               filter(prior_obs_years>=1)) %>%
  mutate(prior.obs.required="Yes",
         pop.type="Covid",
         working.outcome=working.outcome,
         working.outcome.name=working.outcome.name,
         working.study.cohort=working.study.cohort)}



}
    }

gc()     }
  


pat.ch<-list()
# pat.ch[[1]]<-Pop.summary.characteristics %>%
#     mutate(pop.type="All") %>% 
#     mutate(prior.obs.required="No") %>% 
#     mutate(pop=working.study.cohort) %>% 
#     mutate(age_gr2="All")
pat.ch[[2]]<-Pop.summary.characteristics.with.history %>%
    mutate(pop.type="All") %>%
    mutate(prior.obs.required="Yes") %>%
    mutate(pop=working.study.cohort) %>%
    mutate(age_gr2="All")
Patient.characteristcis[[paste0(i)]]<-bind_rows(pat.ch)

gc() }







