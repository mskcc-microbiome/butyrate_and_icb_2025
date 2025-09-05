library(dplyr)
library(Hmisc)

data=read.csv('public_data/fiber_data/DATA_dietary_analysis.csv', na.strings = c("", "NA"))




label(data$age)="Age at time of dietary questionnaire (in years)"
label(data$sex)="Sex"
label(data$race)="Race"
label(data$latinx)="Ethnicity"
label(data$cancer_type)="Malignancy type"
label(data$varhistoyn_uc)="Variant histology? (i.e. anything other than urothelial carcinoma, NOS)"
label(data$visceral_mets)="Visceral metastases present? (per Bajorin risk factors: lung liver, bone or other non-lymph node metastasis)"
label(data$liver_mets)="Liver metastases present?"
label(data$non_cc_rcc_yn)="Non-Clear Cell Histology?"
label(data$imdc)="IMDC (International Metastatic Renal Cell Carcinoma Database Consortium) group?  (IMDC risk factors: Karnofsky performance status score < 80 Time from original diagnosis to initiation of targeted therapy < 1 year Hemoglobin less than the lower limit of normal Serum calcium greater than the upper limit of normal Neutrophil count greater than the upper limit of normal Platelet count greater than the upper limit of normal)"
label(data$io_regimen)="Planned immunotherapy regimen"
label(data$bmi)="BMI at time of questionnaire in kg/m2"
label(data$ecogps)="ECOG performance status at time of questionnaire"
label(data$pfs_mo)="Progression-free survival time interval in months"
label(data$pod_status)="Disease progression status"
label(data$os_mo)="Overall survival time interval in months"
label(data$os_status)="Overall survival status"

data$sex.factor = factor(data$sex,levels=c("1","2","3"))
data$race.factor = factor(data$race,levels=c("1","2","3","4","99","5"))
data$latinx.factor = factor(data$latinx,levels=c("1","2","3"))
data$cancer_type.factor = factor(data$cancer_type,levels=c("1","2","3","5","6","4"))
data$varhistoyn_uc.factor = factor(data$varhistoyn_uc,levels=c("1","0"))
data$visceral_mets.factor = factor(data$visceral_mets,levels=c("1","2","3"))
data$liver_mets.factor = factor(data$liver_mets,levels=c("0","1","99"))
data$non_cc_rcc_yn.factor = factor(data$non_cc_rcc_yn,levels=c("1","0"))
data$imdc.factor = factor(data$imdc,levels=c("0","1","2","3"))
data$io_regimen.factor = factor(data$io_regimen,levels=c("1","2","3","4","14","5","6","7","8","18","9","17","10","11","16","12","13","15","99"))
data$ecogps.factor = factor(data$ecogps,levels=c("0","1","2","3","4","5","6"))
data$kps.factor = factor(data$kps,levels=c("0","1","2","3","4","5","6","7","8","9","10","99"))
data$pod_status.factor = factor(data$pod_status,levels=c("0","1"))
data$os_status.factor = factor(data$os_status,levels=c("0","1"))

levels(data$sex.factor)=c("Male","Female","Other")
levels(data$race.factor)=c("White","Black","Asian","Native American","Other","Prefers not to disclose/Unknown")
levels(data$latinx.factor)=c("Latinx","Non-Latinx","Prefers not to disclose/Unknown")
levels(data$cancer_type.factor)=c("urothelial cancer of the bladder","urothelial cancer of the ureter","urothelial cancer of the renal pelvis","urothelial cancer of the ureter and renal pelvis","urothelial cancer of the urethra","renal cell carcinoma")
levels(data$varhistoyn_uc.factor)=c("Yes","No")
levels(data$visceral_mets.factor)=c("Yes","No","Unknown")
levels(data$liver_mets.factor)=c("Yes","No","Unknown")
levels(data$non_cc_rcc_yn.factor)=c("Yes","No")
levels(data$imdc.factor)=c("Favorable risk: None of the above risk factors present.","Intermediate risk: 1 or 2 of the above risk factors present.","Poor risk: 3 or more risk factors present.","Unknown")
levels(data$io_regimen.factor)=c("pembrolizumab 1st line metastatic","atezolizumab 1st line metastatic","pembrolizumab metastatic, 2nd line+","atezolizumab, metastatic, 2nd line+","nivolumab, metastatic, 2nd line+","maintenance avelumab","adjuvant nivolumab","adjuvant pembrolizumab","pembrolizumab + axitinib, metastatic, 1st line","pembrolizumab + axitinib, metastatic, 2nd line+","ipilimumab + nivolumab, metastatic, 1st line","ipilimumab + nivolumab, metastatic, 2nd line+","cabozantinib + nivolumab, metastatic, 1st line","avelumab + axitinib, metastatic, 1st line","pembrolizumab + lenvatinib, metastatic, 1st line","pembrolizumab + lenvatinib, metastatic, 2nd line+","pembrolizumab, NMIBC","investigational protocol regimen","unknown")
levels(data$ecogps.factor)=c("0","1","2","3","4","5","Unknown")
levels(data$kps.factor)=c("100","90","80","70","60","50","40","30","20","10","0","Unknown")
levels(data$pod_status.factor)=c("Censored","Progressed")
levels(data$os_status.factor)=c("Censored","Deceased")

data <- mutate(data,io_line = case_when(io_regimen==1~'PD-(L)1 1st line',
                                        io_regimen==2~'PD-(L)1 1st line',
                                        io_regimen==5~'Maintenance avelumab',
                                        io_regimen==3~'PD-(L)1 2nd line+',
                                        io_regimen==4~'PD-(L)1 2nd line+',
                                        io_regimen==14~'PD-(L)1 2nd line+',
                                        io_regimen==6~'Adjuvant PD-1',
                                        io_regimen==7~'Adjuvant PD-1',
                                        io_regimen==13~'Pembrolizumab for NMIBC',
                                        io_regimen==17~'Ipilimumab plus nivolumab 2nd line+',
                                        io_regimen==9~'1st line ipilimumab plus nivolumab',
                                        io_regimen==8~'1st line IO + TKI',
                                        io_regimen==10~'1st line IO + TKI',
                                        io_regimen==11~'1st line IO + TKI',
                                        io_regimen==16~'1st line IO + TKI',
                                        io_regimen==18~'IO + TKI 2nd line+',
                                        io_regimen==12~'IO + TKI 2nd line+',
                                        io_regimen==15~'Investigational regimen'))

data$io_line.factor <- as.factor(data$io_line)

data <- mutate(data,ecog_kpsConverted = case_when(ecogps==0~"0",
                                                  ecogps==1~"1",
                                                  ecogps==2~"2",
                                                  ecogps==3~"3",
                                                  ecogps==4~"4",
                                                  ecogps==5~"5",
                                                  ecogps==6&kps==0~"0",
                                                  ecogps==6&kps==1~"0",
                                                  ecogps==6&kps==2~"1",
                                                  ecogps==6&kps==3~"1",
                                                  ecogps==6&kps==4~"2",
                                                  ecogps==6&kps==5~"2",
                                                  ecogps==6&kps==6~"3",
                                                  ecogps==6&kps==7~"3",
                                                  ecogps==6&kps==8~"4",
                                                  ecogps==6&kps==9~"4",
                                                  ecogps==6&kps==10~"5",
                                                  ecogps==6&kps==99~"99"))

data$ecog_kpsConverted[data$ecog_kpsConverted==99] <- NA

data<-mutate(data,ecog_1to5 = case_when(ecog_kpsConverted<1~0,
                                        ecog_kpsConverted>0~1))

data<-mutate(data,ecog_1to5_final = case_when(ecog_1to5=="1"~1,
                                              ecog_1to5=="0"~0,
                                              is.na(ecog_1to5)==TRUE~1))

data[is.na(data$liver_mets.factor),"liver_mets.factor"]<-"No"

data$age.numeric <-as.numeric(data$age)
data$calor<-as.numeric(data$calor)
data <- mutate(data,ageby10 = age.numeric/10)

data <- mutate(data,cancer_RCCvsUC = ifelse(cancer_type==4,0,1))
data <- mutate(data,cancer_RCCvsUC_NAME = ifelse(cancer_type==4,"RCC","UC"))

data<-mutate(data,Impact.TMB.Imputed = case_when(is.na(Impact.TMB.Score)==FALSE~Impact.TMB.Score,
                                                 is.na(Impact.TMB.Score)==TRUE&cancer_RCCvsUC==1~5.8,
                                                 is.na(Impact.TMB.Score)==TRUE&cancer_RCCvsUC==0~3)) ##imputed median TMB of 5.8 for missing values for UC based on the 2017 bladder TCGA publication; imputed TMB =3 for RCC



data <- mutate(data,  fiber_dichotomized = case_when(aofib>=15~1,
                                                     aofib<15~0))

data$fiber_dichotomized_factor = factor(data$fiber_dichotomized,levels=c("0","1"))
levels(data$fiber_dichotomized_factor)=c("Less than 15 g/d","15+ g/day")

label(data$ecog_kpsConverted)="ECOG performance status"
label(data$latinx.factor)="Ethnicity"
label(data$race.factor)="Race"
label(data$visceral_mets.factor)="Visceral metastases"
label(data$liver_mets.factor)="Liver metastases"
label(data$io_line)="Line of therapy"
label(data$sex.factor)="Sex"
label(data$age)="Age (in years)"
label(data$varhistoyn_uc)="Variant histology present"
label(data$io_regimen.factor)="Immunotherapy regimen"
label(data$aofib)="Fiber intake (g/day)"
label(data$bmi) = "BMI (kg/m2)"
label(data$imdc.factor) = "IMDC Risk Category"
label(data$non_cc_rcc_yn.factor) = "Non-clear cell histology"



label(data$Impact.TMB.Score)="TMB (mut/Mb)"
label(data$Impact.TMB.Imputed)="TMB (mut/Mb)"





AllMetdata=filter(data,io_regimen!=6 & io_regimen!=13 & io_regimen!=7 & io_regimen!=15)


AllMetdataFiber=filter(AllMetdata,((calor>=600 & calor<=4200 & sex.factor=="Male") 
                                   |(calor>=500 & calor<=3500 & sex.factor=="Female"))&(nblank<=70))

saveRDS(data, "public_data/fiber_data/fiber_data_clean_labels.RDS")
saveRDS(data, "public_data/fiber_data/all_metadata_fiber.RDS")

