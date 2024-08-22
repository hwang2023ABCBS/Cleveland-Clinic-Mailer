
# source other file -------------------------------------------------------
source("CorhortSelection_Criteria.R")



# library -----------------------------------------------------------------


library(tidyverse)
library(data.table)
library(lubridate)
library(dbplyr)
# library(shiny)
library(DBI)
library(DT)
# library(shinyWidgets)


# Con ---------------------------------------------------------------------
con <- DBI::dbConnect(odbc::odbc(), "snowflakeodbc", UID = "SFK_SVC_PRD_POSIT")
dbGetQuery(con, 'use warehouse PRD_ML_STD_WH')
dbGetQuery(con, 'USE DATABASE PRD_DATASCIENCE_DB')
dbGetQuery(con,'USE SCHEMA HEALTHECON')


# Tables ------------------------------------------------------------------

# mbr claims
MEMBER_MONTHLY_COMPLETE <- tbl(con, in_catalog('PRD_DATASCIENCE_DB','HEALTHECON','MEMBER_MONTHLY_COMPLETE'))

# mbr characteristics
MBR_MASTER_STATIC <- tbl(con, in_catalog('PRD_DATASCIENCE_DB','HEALTHECON','MBR_MASTER_STATIC'))

# mbr CPT codes
CPT_Code_tbl <- tbl(con, in_catalog('PRD_RAW_DB', 'VDT_CM', 'FINALCLAIMSDETAIL')) 

# mbr Diagnosis codes 
Diag_Code_tbl <- tbl(con, in_catalog('PRD_RAW_DB', 'VDT_CM', 'FINALCLAIMSDETAILCODE'))



# merge tables
date_24Months_ago <- Sys.Date() %m-% months(24)  
# "2022-07-30"

WM_Mbrs_CPTs_Diags <- CPT_Code_tbl %>% 
  select(MVDID:SERVICEFROMDATE,PROCEDURECODE,PARTYKEY:COMPANYKEY) %>% 
  inner_join(Diag_Code_tbl %>% select(CLAIMNUMBER,CODEVALUE), by=join_by(CLAIMNUMBER)) %>% 
  filter(COMPANYKEY==11753
         ,SERVICEFROMDATE>= date_24Months_ago # base on current date trace back 2 years in the past. 
  ) %>% 
  distinct(MEMBERID, CLAIMNUMBER, SERVICEFROMDATE, PROCEDURECODE, CODEVALUE) %>% 
  rename('DXCODE'='CODEVALUE')

# 108,984,656 that is 0.1 Billion rows


# merge with both CLAIMNUMBER & CLAIMLINENUMBER
# WM_Mbrs_CPTs_Diags_2 <- CPT_Code_tbl %>% 
#   select(MVDID:SERVICEFROMDATE,PROCEDURECODE,PARTYKEY:COMPANYKEY) %>% 
#   inner_join(Diag_Code_tbl %>% select(CLAIMNUMBER,CLAIMLINENUMBER,CODEVALUE), by=join_by(CLAIMNUMBER,CLAIMLINENUMBER)) %>% 
#   filter(COMPANYKEY==11753
#          ,SERVICEFROMDATE>= date_24Months_ago # base on current date trace back 2 years in the past. 
#          ) %>% 
#   distinct(MEMBERID, CLAIMNUMBER, SERVICEFROMDATE, PROCEDURECODE, CODEVALUE)
# 88,277,926 rows instead


# Cohorts identifying -----------------------------------------------------

# cohort1-Aortic
Aortic_Cohort <- WM_Mbrs_CPTs_Diags %>% 
  filter(DXCODE %in% Target_Diag_list_Aortic & 
           PROCEDURECODE %in% Target_TTE_CPT_list &
           !PROCEDURECODE %in% Aortic_SurgicalProcedure_Exclusion) %>% 
  collect()
# there are 3604 Aortic members 
# saved
# saveRDS(Aortic_Cohort, file = 'output/Aortic_Cohort.rds')


# summary 
# how many member are there?
# Aortic_Cohort %>% 
#   group_by(MEMBERID) %>% 
#   summarise(
#     RecordCounts= n(),
#     TimeRange= paste(min(SERVICEFROMDATE), "to", max(SERVICEFROMDATE)),
#     ProcedureCounts= n_distinct(PROCEDURECODE),
#     DiagCounts= n_distinct(DXCODE)
#   )

# cohort2-Mitral
Mitral_Cohort <- WM_Mbrs_CPTs_Diags %>% 
  filter(DXCODE %in% Target_Diag_list_Mitral & 
           PROCEDURECODE %in% Target_TTE_CPT_list &
           !PROCEDURECODE %in% Mitral_SurgicalProcedure_Exclusion) %>% 
  collect()
# n_distinct(Mitral_Cohort$MEMBERID)
# 6320 distinct Mitral members


# summary 
# Mitral_Cohort %>% 
#   group_by(MEMBERID) %>% 
#   summarise(
#     RecordCounts= n(),
#     TimeRange= paste(min(SERVICEFROMDATE), "to", max(SERVICEFROMDATE)),
#     ProcedureCounts= n_distinct(PROCEDURECODE),
#     DiagCounts= n_distinct(DXCODE)
#   )
# 
# # save
# saveRDS(Mitral_Cohort, file = 'output/Mitral_Cohort.rds')

# cohort3-MV and AV
MVandAV_Cohort <- WM_Mbrs_CPTs_Diags %>% 
  filter(DXCODE %in% Target_Diag_list_MVandAV & 
           PROCEDURECODE %in% Target_TTE_CPT_list &
           !PROCEDURECODE %in% MVandAV_SurgicalProcedure_Exclusion) %>% 
  collect()
# n_distinct(MVandAV_Cohort$MEMBERID)
# 3706 distinct MVandAV members

# summary 
# MVandAV_Cohort %>% 
#   group_by(MEMBERID) %>% 
#   summarise(
#     RecordCounts= n(),
#     TimeRange= paste(min(SERVICEFROMDATE), "to", max(SERVICEFROMDATE)),
#     ProcedureCounts= n_distinct(PROCEDURECODE),
#     DiagCounts= n_distinct(DXCODE)
#   )
# 
# # save
# saveRDS(MVandAV_Cohort, file = 'output/MVandAV_Cohort.rds')

# cohort4-HCM
HCM_Cohort <- WM_Mbrs_CPTs_Diags %>% 
  filter(DXCODE %in% Target_Diag_list_HCM & 
           PROCEDURECODE %in% Target_TTE_CPT_list &
           !PROCEDURECODE %in% HCM_SurgicalProcedure_Exclusion) %>% 
  collect()
# n_distinct(HCM_Cohort$MEMBERID)
# 332 distinct HCM members

# summary 
# HCM_Cohort %>% 
#   group_by(MEMBERID) %>% 
#   summarise(
#     RecordCounts= n(),
#     TimeRange= paste(min(SERVICEFROMDATE), "to", max(SERVICEFROMDATE)),
#     ProcedureCounts= n_distinct(PROCEDURECODE),
#     DiagCounts= n_distinct(DXCODE)
#   )
# # save 
# saveRDS(HCM_Cohort, file = 'output/HCM_Cohort.rds')

# cohort5-CAD
CAD_Cohort <- WM_Mbrs_CPTs_Diags %>% 
  filter(DXCODE %in% Target_Diag_list_CAD | 
           PROCEDURECODE %in% Target_DiagnosticProc_CPT4CAD_list &
           !PROCEDURECODE %in% CAD_SurgicalProcedure_Exclusion) %>% 
  collect()
# n_distinct(CAD_Cohort$MEMBERID)
# 162444 distinct CAD members


# summary 
# CAD_Cohort %>% 
#   group_by(MEMBERID) %>% 
#   summarise(
#     RecordCounts= n(),
#     TimeRange= paste(min(SERVICEFROMDATE), "to", max(SERVICEFROMDATE)),
#     ProcedureCounts= n_distinct(PROCEDURECODE),
#     DiagCounts= n_distinct(DXCODE)
#   )
# 
# 
# # save
# saveRDS(CAD_Cohort, file = 'output/CAD_Cohort.rds')


# Combine cohorts ---------------------------------------------------------
All_Cohorts <- bind_rows(
  Aortic_Cohort %>% mutate(Cohort= 'Aortic'),
  Mitral_Cohort %>% mutate(Cohort= 'Mitral'),
  MVandAV_Cohort %>% mutate(Cohort= 'MVandAV'),
  HCM_Cohort %>% mutate(Cohort= 'HCM'),
  CAD_Cohort %>% mutate(Cohort= 'CAD')
)
# saveRDS(All_Cohorts, file = 'output/All_Cohorts.rds')


# summary_tbl_cohorts <- All_Cohorts %>% 
#   group_by(Cohort) %>% 
#   summarise(
#     MbrCounts= n_distinct(MEMBERID),
#     UniqueProcedureCnts= n_distinct(PROCEDURECODE),
#     UniqueDxCnts= n_distinct(DXCODE)
#   )
# summary_tbl_cohorts


# bring in characteristics & utilizations ---------------------------------
MbrCohortLevelSummary_tbl_combined <- All_Cohorts %>% 
  group_by(MEMBERID,Cohort) %>% 
  summarise(
    RecordCounts= n(),
    TimeRange= paste(min(SERVICEFROMDATE), "to", max(SERVICEFROMDATE)),
    ProcedureCounts= n_distinct(PROCEDURECODE),
    DiagCounts= n_distinct(DXCODE)
  ) %>% ungroup() %>% collect()
# 'MbrCohortLevelSummary_tbl_combined' this table is the one only has members'procedures & diagnoses info




# cost summary - member level 
Mbrs <- MbrCohortLevelSummary_tbl_combined %>% pull(MEMBERID) %>% unique()

# conditions- member level
temp_conditions <- MEMBER_MONTHLY_COMPLETE %>% 
  filter(MEMBERID %in% Mbrs & MONTH>=date_24Months_ago) %>% 
  group_by(MEMBERID) %>% 
  summarise(across(c(AIDS:WEIGHT_LOSS,BLOOD_IMMUNE:SYMPTOMS_SIGNS_ABNORMALITES), ~ max(.))
  )  %>% as.data.table() 

temp <- MbrCohortLevelSummary_tbl_combined %>% 
  left_join(MBR_MASTER_STATIC %>% select(MBR_ID,GENDER,AGE_GROUP,RACE,RACE_MINORITY_CAT,SOCIOECONOMIC_SVI_CAT:ALL_SVI_CAT,NEAREST_ED_DIST_TRACT,PCP_PER_100K_FCC), by=join_by('MEMBERID'=='MBR_ID'), copy = TRUE)%>% 
  inner_join(temp_conditions, by=join_by(MEMBERID),copy = TRUE) %>% 
  mutate(across(where(is.character),as.factor))%>% 
  mutate(AGE_GROUP= factor(AGE_GROUP, levels= c("Age [0-18]","Age [19-30]","Age [31-40]","Age [41-50]","Age [51-60]","Age [>60]")),
         across(c(SOCIOECONOMIC_SVI_CAT:ALL_SVI_CAT), ~factor(.x, levels=c("Low","Moderate","High","Very High"))),
         across(c(AIDS:SYMPTOMS_SIGNS_ABNORMALITES), ~ factor(.x))
         ) %>% collect() %>% 
  as.data.table()
# based on 'MbrCohortLevelSummary_tbl_combined' tie to static charateristics.


temp2 <- MEMBER_MONTHLY_COMPLETE %>% 
  filter(MEMBERID %in% Mbrs & MONTH>=date_24Months_ago) %>% 
  group_by(MEMBERID) %>% 
  summarise(across(ALLOWED:PAID_PHARM, ~ round(mean(.,na.rm=TRUE),2)),
            across(ER:TH, sum,na.rm=TRUE),
            TERMED= max(TERMED,na.rm = TRUE),
            MonthCnts= n()
  ) %>% left_join(MbrCohortLevelSummary_tbl_combined %>% select(MEMBERID,Cohort), by=join_by(MEMBERID), copy = TRUE) %>% 
  collect() %>% as.data.table()

# 'temp2': further to tie to costs and utilizations

# summary stats 
## try publish ready formated summary table with member characteristics 
# library(gtsummary)
# tbl_summary(
#   temp, 
#   include= c(GENDER,
#               AGE_GROUP,
#                    RACE,
#       RACE_MINORITY_CAT,
#   SOCIOECONOMIC_SVI_CAT,
#       HOUSEHOLD_SVI_CAT,
#        MINORITY_SVI_CAT,
#  HOUSING_TRANSP_SVI_CAT,
#             ALL_SVI_CAT,
#   NEAREST_ED_DIST_TRACT,
#        PCP_PER_100K_FCC),
#   by= Cohort,
#   missing = "ifany"
# ) %>% 
#   add_n() %>% 
#   add_p() %>% 
#   modify_header(label="**Variable**") %>% 
#   bold_labels()


# explore eligibility status of cohort members: updated8/6 , later add-on

eligibility_tbl <- MEMBER_MONTHLY_COMPLETE %>% 
  filter(MEMBERID %in% Mbrs & MONTH>=date_24Months_ago) %>% 
  select(MEMBERID,TERMED) %>% 
  group_by(MEMBERID) %>% 
  summarise(TERMED= max(TERMED,na.rm = TRUE)) %>% 
  distinct() %>% 
  collect()

# count how many are termed?  
# eligibility_tbl %>% 
#   group_by(MEMBERID) %>% 
#   summarise(TERMED=max(TERMED, na.rm = TRUE)) %>% 
#   distinct() %>% 
#   count(TERMED)
# TERMED  count
# <dbl>  <dbl>
# 1      1  40896
# 2      0 122334


# update tables that used in shiny to add TERMED column
MbrCohortLevelSummary_tbl_combined <- MbrCohortLevelSummary_tbl_combined %>% 
  left_join(eligibility_tbl,join_by(MEMBERID),relationship =
              "many-to-many",copy = TRUE) %>% 
  mutate(TERMED= ifelse(TERMED==1,"Yes","No"))
temp <- temp %>% 
  left_join(eligibility_tbl, join_by(MEMBERID),relationship =
              "many-to-many", copy = TRUE) %>% 
  mutate(TERMED= ifelse(TERMED==1,"Yes","No"))
temp2 <- temp2 %>% mutate(TERMED= ifelse(TERMED==1,"Yes","No"))

#update:8/21
# replace all NA values to "Missing" of column age, gender, minority
temp <- temp %>% mutate(AGE_GROUP= ifelse(is.na(AGE_GROUP),'Missing',as.character(AGE_GROUP)),
                        AGE_GROUP= factor(AGE_GROUP, levels=c('Age [0-18]','Age [19-30]','Age [31-40]','Age [41-50]','Age [51-60]','Age [>60]','Missing')),
                        GENDER= ifelse(is.na(GENDER),'Missing',as.character(GENDER)),
                        GENDER= factor(GENDER, levels= c('M','F','Missing')),
                        RACE_MINORITY_CAT=ifelse(is.na(RACE_MINORITY_CAT),'Missing',as.character(RACE_MINORITY_CAT)),
                        RACE_MINORITY_CAT= factor(RACE_MINORITY_CAT, levels= c('Minority','Non-Minority','Missing')))
# add age, gender, minority columns to this table 'MbrCohortLevelSummary_tbl_combined'
MbrCohortLevelSummary_tbl_combined <- MbrCohortLevelSummary_tbl_combined %>% 
  left_join(MBR_MASTER_STATIC %>% select(MBR_ID,AGE_GROUP,GENDER,RACE_MINORITY_CAT), join_by(MEMBERID == MBR_ID), copy = TRUE)

MbrCohortLevelSummary_tbl_combined <- MbrCohortLevelSummary_tbl_combined %>% 
  mutate(Cohort= as.factor(Cohort),
         TERMED= as.factor(TERMED),
         AGE_GROUP= ifelse(is.na(AGE_GROUP),'Missing',as.character(AGE_GROUP)),
         AGE_GROUP= factor(AGE_GROUP, levels=c('Age [0-18]','Age [19-30]','Age [31-40]','Age [41-50]','Age [51-60]','Age [>60]','Missing')),
         GENDER= ifelse(is.na(GENDER),'Missing',as.character(GENDER)),
         GENDER= factor(GENDER, levels= c('M','F','Missing')),
         RACE_MINORITY_CAT=ifelse(is.na(RACE_MINORITY_CAT),'Missing',as.character(RACE_MINORITY_CAT)),
         RACE_MINORITY_CAT= factor(RACE_MINORITY_CAT, levels= c('Minority','Non-Minority','Missing')) )


# again add age, gender, minority columns to this table 'temp2'
temp2 <- temp2 %>% 
  left_join(MBR_MASTER_STATIC %>% select(MBR_ID,AGE_GROUP,GENDER,RACE_MINORITY_CAT), join_by(MEMBERID == MBR_ID), copy = TRUE)

temp2 <- temp2 %>% 
  mutate(Cohort= as.factor(Cohort),
         TERMED= as.factor(TERMED),
         AGE_GROUP= ifelse(is.na(AGE_GROUP),'Missing',as.character(AGE_GROUP)),
         AGE_GROUP= factor(AGE_GROUP, levels=c('Age [0-18]','Age [19-30]','Age [31-40]','Age [41-50]','Age [51-60]','Age [>60]','Missing')),
         GENDER= ifelse(is.na(GENDER),'Missing',as.character(GENDER)),
         GENDER= factor(GENDER, levels= c('M','F','Missing')),
         RACE_MINORITY_CAT=ifelse(is.na(RACE_MINORITY_CAT),'Missing',as.character(RACE_MINORITY_CAT)),
         RACE_MINORITY_CAT= factor(RACE_MINORITY_CAT, levels= c('Minority','Non-Minority','Missing')) )



# save & load -------------------------------------------------------------
save(All_Cohorts,
     # summary_tbl_cohorts,
     MbrCohortLevelSummary_tbl_combined,
     temp, # add characteristics
     temp2, # add costs and utilizations
     criteria_tbl, 
     file = 'EnvObjects.RData')
# load(file = 'EnvObjects.RData')

# close the connection
dbDisconnect(con)





