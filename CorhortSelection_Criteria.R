# target Diag list provided
## Aortic 
Target_Diag_list_Aortic <- c(
  'I1060',
  'I1061',
  'I1062',
  'I1068',
  'I1069',
  'I350',
  'I351',
  'I352',
  'I358',
  'I359',
  'Q230',
  'Q231',
  'Q244'
)

## Mitral
Target_Diag_list_Mitral <- c(
  'I050',
  'I051',
  'I052',
  'I058',
  'I059',
  'I340',
  'I341',
  'I342',
  'I348',
  'I349',
  'Q232',
  'Q233'
)


## MVandAV 
Target_Diag_list_MVandAV <- c(
  'I080',
  'I081',
  'I082',
  'I083',
  'I088',
  'I089',
  'Q238',
  'Q239'
)


## HCM
Target_Diag_list_HCM <- c(
  'I421',
  'I422'
)


## CAD
Target_Diag_list_CAD <- c(
  'Z951',
  'Z955',
  'Z9861',
  'I200',
  'I201',
  'I208',
  'I209',
  'I2101',
  'I2102',
  'I2109',
  'I2111',
  'I2119',
  'I2121',
  'I2129',
  'I213',
  'I214',
  'I220',
  'I221',
  'I222',
  'I228',
  'I229',
  'I2510',
  'I25110',
  'I25111',
  'I25118',
  'I25119',
  'I252',
  'I25700',
  'I25701',
  'I25708',
  'I25709',
  'I25710',
  'I25711',
  'I25718',
  'I25719',
  'I25720',
  'I25721',
  'I25728',
  'I25729',
  'I25730',
  'I25731',
  'I25738',
  'I25739',
  'I25750',
  'I25751',
  'I25758',
  'I25759',
  'I25760',
  'I25761',
  'I25768',
  'I25769',
  'I25790',
  'I25791',
  'I25798',
  'I25799',
  'I25810',
  'I25811',
  'I25812',
  'R9430'
)




# target TTE procedure list provided- for the first 4 diagnoses 
Target_TTE_CPT_list <- c('93303',
                         '93304',
                         '93306',
                         '93307',
                         '93308',
                         '93312',
                         '93313',
                         '93314',
                         '93315',
                         '93316',
                         '93317',
                         '93318',
                         '93350',
                         '93351')


# Diagnostic Proc given for CAD specific only 
Target_DiagnosticProc_CPT4CAD_list <- c(
  '75574',
  '92924',
  '92925',
  '92978',
  '92979',
  '93000',
  '93005',
  '93010',
  '93015',
  '93016',
  '93017',
  '93018',
  '93350',
  '93351',
  '93452',
  '93453',
  '93454',
  '93455',
  '93456',
  '93457',
  '93458',
  '93459',
  '93460',
  '93461',
  '93462',
  '93463',
  '93464',
  '93565',
  '93566',
  '93567',
  '93568',
  '93571',
  '93572',
  '0291T',
  '0292T',
  '1755',
  '3606',
  '3607',
  '3611',
  '3612',
  '3613',
  '3614',
  '3615',
  '3616'
)

# exclusion critera: surgical procedures
Aortic_SurgicalProcedure_Exclusion <- c(
  '33390',
  '33391',
  '33400',
  '33401',
  '33403',
  '33404',
  '33405',
  '33406',
  '33410',
  '33411',
  '33412',
  '33413',
  '33415',
  '33416',
  '33417'
)

Mitral_SurgicalProcedure_Exclusion <- c(
  '33420',
  '33422',
  '33425',
  '33426',
  '33427',
  '33430'
)

MVandAV_SurgicalProcedure_Exclusion <- c(
  '33390',
  '33391',
  '33400',
  '33401',
  '33403',
  '33404',
  '33405',
  '33406',
  '33410',
  '33411',
  '33412',
  '33413',
  '33415',
  '33416',
  '33417',
  '33420',
  '33422',
  '33425',
  '33426',
  '33427',
  '33430'
  
)

HCM_SurgicalProcedure_Exclusion <- c(
  '33416',
  '92928',
  '92929',
  '92933',
  '92934',
  '92937',
  '92938',
  '92941',
  '92943',
  '92944'
  
)

CAD_SurgicalProcedure_Exclusion <- c(
  # '33510',
  # '33511',
  # '33512',
  # '33513',
  # '33514',
  # '33516',
  # '33517',
  # '33518',
  # '33519',
  # '33521',
  # '33522',
  # '33523',
  # '33533',
  # '33534',
  # '33535',
  # '33536',
  '92928',
  '92929',
  '92933',
  '92934',
  '92937',
  '92938',
  '92941',
  '92943',
  '92944',
  'C9600',
  'C9601',
  'C9602',
  'C9603',
  'C9604'
  
)
# update 8/21: remove some exclusion codes as show above per request.

# Manually create a data frame with two columns: Cohort and Criteria
criteria_tbl <- data.frame(
  Cohort = c("Aortic","Mitral","MVandAV", "HCM","CAD"),
  Criteria = c(
    "<p><b>Criteria for Aortic:</b></p><ul><li>Include any following diag codes:I1060
I1061
I1062
I1068
I1069
I350
I351
I352
I358
I359
Q230
Q231
Q244</li><li>AND any of TTE Procedure codes:93303
93304
93306
93307
93308
93312
93313
93314
93315
93316
93317
93318
93350
93351</li><li>But exclude any of the following surgical codes:33390
33391
33400
33401
33403
33404
33405
33406
33410
33411
33412
33413
33415
33416
33417
</li></ul>",
    "<p><b>Criteria for Mitral:</b></p><ul><li>Include the following Diagnosis codes: 
I050
I051
I052
I058
I059
I340
I341
I342
I348
I349
Q232
Q233</li><li>AND any of TTE Procedure codes: 
93303
93304
93306
93307
93308
93312
93313
93314
93315
93316
93317
93318
93350
93351</li><li>But exclude any of the following surgical codes: 
33420
33422
33425
33426
33427
33430</li></ul>",
    "<p><b>Criteria for MVandAV:</b></p><ul><li>Include the following Diagnosis codes: 
I080
I081
I082
I083
I088
I089
Q238
Q239</li><li>AND any of TTE Procedure codes: 
93303
93304
93306
93307
93308
93312
93313
93314
93315
93316
93317
93318
93350
93351</li><li>But exclude any of the following surgical codes: 
33390
33391
33400
33401
33403
33404
33405
33406
33410
33411
33412
33413
33415
33416
33417
33420
33422
33425
33426
33427
33430</li></ul>",
    "<p><b>Criteria for HCM:</b></p><ul><li>Include the following Diagnosis codes: 
I421
I422</li><li>AND any of TTE Procedure codes: 
93303
93304
93306
93307
93308
93312
93313
93314
93315
93316
93317
93318
93350
93351</li><li>But exclude any of the following surgical codes: 
33416
92928
92929
92933
92934
92937
92938
92941
92943
92944</li></ul>",
    "<p><b>Criteria for CAD:</b></p><ul><li>Include the following Diagnosis codes: 
Z951
Z955
Z9861
I200
I201
I208
I209
I2101
I2102
I2109
I2111
I2119
I2121
I2129
I213
I214
I220
I221
I222
I228
I229
I2510
I25110
I25111
I25118
I25119
I252
I25700
I25701
I25708
I25709
I25710
I25711
I25718
I25719
I25720
I25721
I25728
I25729
I25730
I25731
I25738
I25739
I25750
I25751
I25758
I25759
I25760
I25761
I25768
I25769
I25790
I25791
I25798
I25799
I25810
I25811
I25812
R9430</li><li>OR any of Diagnostic Procedure codes: 
75574
92924
92925
92978
92979
93000
93005
93010
93015
93016
93017
93018
93350
93351
93452
93453
93454
93455
93456
93457
93458
93459
93460
93461
93462
93463
93464
93565
93566
93567
93568
93571
93572
0291T
0292T
1755
3606
3607
3611
3612
3613
3614
3615
3616</li><li>But exclude any of the following surgical codes: 
92928
92929
92933
92934
92937
92938
92941
92943
92944
C9600
C9601
C9602
C9603
C9604</li></ul>"
  ),
  stringsAsFactors = FALSE
)




