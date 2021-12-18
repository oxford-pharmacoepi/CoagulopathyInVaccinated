
#install.packages("renv") # if not already installed, install renv from CRAN
renv::restore() # this should prompt you to install the various packages required for the study
renv::activate()

# packages ------
library(SqlRender)
library(DatabaseConnector)
library(FeatureExtraction)
library(here)
library(lubridate)
library(stringr)
library(ggplot2)
library(DBI)
library(dbplyr)
library(dplyr)
library(tidyr)
library(kableExtra)
library(RSQLite)
library(rmarkdown)
library(tableone)
library(scales)
library(forcats)
library(epiR)
library(RPostgreSQL)
library(cmprsk)
library(mstate)
library(broom)
library(rms)
library(glue)
library(readr)



# set up ------
# the path to a folder where the results from this analysis will be saved
output.folder<-here::here("output")

# The OHDSI DatabaseConnector connection details
server<-"...." #eg "10.80...../mypostgres"
user<-"...."
password<- "...."
port<-"...."
host<-"...."
oracleTempSchema<-NULL

connectionDetails <-DatabaseConnector::downloadJdbcDrivers("postgresql", here::here())
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "postgresql",
                                                                server =server,
                                                                user = user,
                                                                password = password,
                                                                port = port ,
                                                                pathToDriver = here::here())
# The DBI connection details
server_dbi<-"...."   #eg "mypostgres"
db <- dbConnect("...")
# eg for postgres 
# db <- dbConnect(RPostgreSQL::PostgreSQL(),
#                 dbname = server_dbi,
#                 port = port,
#                 host = host, 
#                 user = user, 
#                 password = password)

# your sql dialect used with the OHDSI SqlRender package
targetDialect <-"....."  
# The name of the schema that contains the OMOP CDM with patient-level data
cdm_database_schema<-"....."
# The name of the schema that contains the vocabularies
vocabulary_database_schema<-"....."
# The name of the schema where a results table will be created 
results_database_schema<-"....."

# Tables to be created in your results schema for this analysis will be named using this as the stem 
# Note, any existing tables in your results schema with the same names will be overwritten
cohortTableStem<-"CoagulopathyInVaccinated"

# The name/ acronym for your database (to be used in the titles of reports, etc)
db.name<-"....."

# if you have already created the cohorts, you can set this to FALSE to skip instantiating these cohorts again
create.exposure.cohorts<-TRUE
create.outcome.cohorts<-TRUE
create.profile.cohorts<-TRUE


# run the analysis ------
# to run for just one exposure/ outcome pair
run.as.test<-TRUE

source(here("RunStudy.R"))





