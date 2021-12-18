
working.id<-exposure.cohorts$id[1]

settings <- createCovariateSettings(useDemographicsGender = TRUE,
                                    useDemographicsAgeGroup = TRUE)

covariateData <- getDbCovariateData(connectionDetails = connectionDetails,
                                    cdmDatabaseSchema = cdm_database_schema,
                                    cohortDatabaseSchema = results_database_schema,
                                    cohortTable = cohortTableExposures,
                                    cohortId = working.id,
                                    rowIdField = "subject_id",
                                    covariateSettings = settings)


summary(covariateData)