library(Hmisc)
library(dplyr)
library(gtsummary)
library(mice)
library(glmnet)
library(flextable)

data1=read.csv('data/fiber_data/full_metadata_no_phi.csv', na.strings = c("", "NA"))
AllMetdata=

label(data1$age)="Age at time of dietary questionnaire (in years)"
label(data1$sex)="Sex"
label(data1$race)="Race"
label(data1$latinx)="Ethnicity"
label(data1$cancer_type)="Malignancy type"
label(data1$stage)="Stage"
label(data1$varhistoyn_uc)="Variant histology? (i.e. anything other than urothelial carcinoma, NOS)"
label(data1$visceral_mets)="Visceral metastases present? (per Bajorin risk factors: lung liver, bone or other non-lymph node metastasis)"
label(data1$liver_mets)="Liver metastases present?"
label(data1$non_cc_rcc_yn)="Non-Clear Cell Histology?"
label(data1$imdc)="IMDC (International Metastatic Renal Cell Carcinoma Database Consortium) group?  (IMDC risk factors: Karnofsky performance status score < 80 Time from original diagnosis to initiation of targeted therapy < 1 year Hemoglobin less than the lower limit of normal Serum calcium greater than the upper limit of normal Neutrophil count greater than the upper limit of normal Platelet count greater than the upper limit of normal)"
label(data1$io_regimen)="Planned immunotherapy regimen"
label(data1$bmi)="BMI at time of questionnaire in kg/m2"
label(data1$ecogps)="ECOG performance status at time of questionnaire"
label(data1$smoke)="Smoking status"
label(data1$pfs_mo)="Progression-free survival time interval in months"
label(data1$pod_status)="Disease progression status"
label(data1$os_mo)="Overall survival time interval in months"
label(data1$os_status)="Overall survival status"
label(data1$pbx_now)="6. Do you currently use probiotics?"
label(data1$abx_3mo)="2. During how many weeks in the past 3 months have you taken antibiotics?"

data1$sex.factor = factor(data1$sex,levels=c("1","2","3"))
data1$race.factor = factor(data1$race,levels=c("1","2","3","4","99","5"))
data1$latinx.factor = factor(data1$latinx,levels=c("1","2","3"))
data1$cancer_type.factor = factor(data1$cancer_type,levels=c("1","2","3","5","6","4"))
data1$stage.factor = factor(data1$stage,levels=c("10","11","1","2","3","4","6","7","8","9","5"))
data1$varhistoyn_uc.factor = factor(data1$varhistoyn_uc,levels=c("1","0"))
data1$visceral_mets.factor = factor(data1$visceral_mets,levels=c("1","2","3"))
data1$liver_mets.factor = factor(data1$liver_mets,levels=c("0","1","99"))
data1$non_cc_rcc_yn.factor = factor(data1$non_cc_rcc_yn,levels=c("1","0"))
data1$imdc.factor = factor(data1$imdc,levels=c("0","1","2","3"))
data1$io_regimen.factor = factor(data1$io_regimen,levels=c("1","2","3","4","14","5","6","7","8","18","9","17","10","11","16","12","13","15","99"))
data1$ecogps.factor = factor(data1$ecogps,levels=c("0","1","2","3","4","5","6"))
data1$kps.factor = factor(data1$kps,levels=c("0","1","2","3","4","5","6","7","8","9","10","99"))
data1$smoke.factor = factor(data1$smoke,levels=c("0","1","2","3"))
data1$pod_status.factor = factor(data1$pod_status,levels=c("0","1"))
data1$os_status.factor = factor(data1$os_status,levels=c("0","1"))
data1$pbx_now.factor = factor(data1$pbx_now,levels=c("0","1","3"))
data1$abx_3mo.factor = factor(data1$abx_3mo,levels=c("0","1","3","5"))

levels(data1$sex.factor)=c("Male","Female","Other")
levels(data1$race.factor)=c("White","Black","Asian","Native American","Other","Prefers not to disclose/Unknown")
levels(data1$latinx.factor)=c("Latinx","Non-Latinx","Prefers not to disclose/Unknown")
levels(data1$cancer_type.factor)=c("urothelial cancer of the bladder","urothelial cancer of the ureter","urothelial cancer of the renal pelvis","urothelial cancer of the ureter and renal pelvis","urothelial cancer of the urethra","renal cell carcinoma")
levels(data1$stage.factor)=c("stage 0a","stage 0is","stage I","stage II","stage III","stage IV","stage I, NED","stage II, NED","stage III, NED","stage IV, NED","unknown")
levels(data1$varhistoyn_uc.factor)=c("Yes","No")
levels(data1$visceral_mets.factor)=c("Yes","No","Unknown")
levels(data1$liver_mets.factor)=c("Yes","No","Unknown")
levels(data1$non_cc_rcc_yn.factor)=c("Yes","No")
levels(data1$imdc.factor)=c("Favorable risk: None of the above risk factors present.","Intermediate risk: 1 or 2 of the above risk factors present.","Poor risk: 3 or more risk factors present.","Unknown")
levels(data1$io_regimen.factor)=c("pembrolizumab 1st line metastatic","atezolizumab 1st line metastatic","pembrolizumab metastatic, 2nd line+","atezolizumab, metastatic, 2nd line+","nivolumab, metastatic, 2nd line+","maintenance avelumab","adjuvant nivolumab","adjuvant pembrolizumab","pembrolizumab + axitinib, metastatic, 1st line","pembrolizumab + axitinib, metastatic, 2nd line+","ipilimumab + nivolumab, metastatic, 1st line","ipilimumab + nivolumab, metastatic, 2nd line+","cabozantinib + nivolumab, metastatic, 1st line","avelumab + axitinib, metastatic, 1st line","pembrolizumab + lenvatinib, metastatic, 1st line","pembrolizumab + lenvatinib, metastatic, 2nd line+","pembrolizumab, NMIBC","investigational protocol regimen","unknown")
levels(data1$ecogps.factor)=c("0","1","2","3","4","5","Unknown")
levels(data1$kps.factor)=c("100","90","80","70","60","50","40","30","20","10","0","Unknown")
levels(data1$smoke.factor)=c("Current smoker","Former smoker","Never smoker","Unknown")
levels(data1$pod_status.factor)=c("Censored","PFS event")
levels(data1$os_status.factor)=c("Censored","Deceased")
levels(data1$pbx_now.factor)=c("No","Yes","I do not know what probiotics are")
levels(data1$abx_3mo.factor)=c("0 weeks","1 to 2 weeks","3 to 5 weeks","More than 5 weeks")

data1 <- mutate(data1,io_line = case_when(io_regimen==1~'PD-(L)1 1st line',
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

data1$io_line.factor <- as.factor(data1$io_line)

data1 <- mutate(data1,ecog_kpsConverted = case_when(ecogps==0~"0",
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

data1$ecog_kpsConverted[data1$ecog_kpsConverted==99] <- NA

data1<-mutate(data1,ecog_1to5 = case_when(ecog_kpsConverted<1~0,
                                          ecog_kpsConverted>0~1))

data1<-mutate(data1,ecog_1to5_final = case_when(ecog_1to5=="1"~1,
                                                ecog_1to5=="0"~0,
                                                is.na(ecog_1to5)==TRUE~1))

data1[is.na(data1$liver_mets.factor),"liver_mets.factor"]<-"No"

data1$age.numeric <-as.numeric(data1$age)
data1 <- mutate(data1,ageby10 = age.numeric/10)

data1 <- mutate(data1,cancer_RCCvsUC = ifelse(cancer_type==4,0,1))
data1 <- mutate(data1,cancer_RCCvsUC_NAME = ifelse(cancer_type==4,"RCC","UC"))

data1 <- mutate(data1,stage.simplified.factor = case_when(stage.factor == "stage IV, NED"~'Stage IV',
                                                          stage.factor == "stage III" ~ 'Stage III',
                                                          stage.factor == "stage IV" ~ 'Stage IV'))

data1$stage.simplified.factor <- factor(data1$stage.simplified.factor,
                                        levels = c("Stage III", "Stage IV"))

data1<-mutate(data1,Impact.TMB.Imputed = case_when(is.na(Impact.TMB.Score)==FALSE~as.numeric(Impact.TMB.Score),
                                                   is.na(Impact.TMB.Score)==TRUE&cancer_RCCvsUC==1~5.8,
                                                   is.na(Impact.TMB.Score)==TRUE&cancer_RCCvsUC==0~3)) ##imputed median TMB of 5.8 for missing values for UC based on the 2017 bladder TCGA publication; imputed TMB =3 for RCC based on 2014 Nature paper PMID 23945592
data1 <- mutate(data1,abx_3mo_yn = case_when(abx_3mo==0~0,
                                             abx_3mo!=0~1,))

data1 <- mutate(data1,  smoking.binary = case_when(smoke.factor=="Never smoker"~0,
                                                   smoke.factor=="Former smoker"~1,
                                                   smoke.factor=="Current smoker"~1))
levels(data1$smoking.binary)=c("Never smoker","Current or former smoker")
label(data1$smoking.binary)="History of smoking (yes vs no)"

data1 <- mutate(data1, WhiteRace = if_else(race.factor=="White",1,0))

# Create age categories
data1 <- data1 %>%
  mutate(age_cat = cut(age, 
                       breaks = c(0, 65, Inf),
                       labels = c("≤65", ">65")))
levels(data1$age_cat)=c("65 years old or younger","Greater than 65 years old")
label(data1$ecog_kpsConverted)="Age above versus below 65 years"


label(data1$ecog_kpsConverted)="ECOG performance status"
label(data1$latinx.factor)="Ethnicity"
label(data1$race.factor)="Race"
label(data1$cancer_RCCvsUC_NAME) = "Cancer diagnosis"
label(data1$stage.simplified.factor)="Stage"
label(data1$visceral_mets.factor)="Visceral metastases"
label(data1$liver_mets.factor)="Liver metastases"
label(data1$io_line)="Line of therapy"
label(data1$sex.factor)="Sex"
label(data1$age)="Age (in years)"
label(data1$varhistoyn_uc)="Variant histology present"
label(data1$io_regimen.factor)="Immunotherapy regimen"
label(data1$aofib)="Fiber intake (g/day)"
label(data1$bmi) = "BMI (kg/m2)"
label(data1$smoke.factor) = "Smoking status"
label(data1$imdc.factor) = "IMDC Risk Category"
label(data1$non_cc_rcc_yn.factor) = "Non-clear cell histology"
label(data1$Impact.TMB.Score)="TMB (mut/Mb)"
label(data1$Impact.TMB.Imputed)="TMB (mut/Mb)"

data1$calor<-as.numeric(data1$calor)

label(data1$calor)="Total daily caloric intake (kilocalories per day)"

AllMetdata=filter(data1,io_regimen!=6 & io_regimen!=13 & io_regimen!=7 & io_regimen!=15 & 
                    stage!=10 & stage!=11 & stage!=1 & stage!=2 & stage!=5)

AllMetdataFiber=filter(AllMetdata,((calor>=600 & calor<=4200 & sex.factor=="Male") 
                                   |(calor>=500 & calor<=3500 & sex.factor=="Female"))&(nblank<=70))

AllMetdataFiber <- AllMetdataFiber %>%
  mutate(
    fiber_tertile = ntile(aofib, 3),
    fiber_tertile_factor = factor(fiber_tertile, levels = 1:3,
                                  labels = c("T1 (lowest)", "T2 (middle)", "T3 (highest)"))
  )

# Calculate median fiber intake within each tertile
AllMetdataFiber <- AllMetdataFiber %>%
  group_by(fiber_tertile) %>%
  mutate(fiber_tertile_median = median(as.numeric(aofib), na.rm = TRUE)) %>%
  ungroup()

fiber_tertile_medians <- AllMetdataFiber %>% 
  group_by(fiber_tertile) %>% 
  summarise(median_value = dplyr::first(fiber_tertile_median), n = n())

label(AllMetdataFiber$fiber_tertile_median)="Median fiber intake per tertile (g/day)"

AllMetdataFiber <- mutate(AllMetdataFiber, kcalor = calor/1000)

AllMetdataFiber <- mutate(AllMetdataFiber, gram_fiber_per_Kcal = aofib/kcalor)

AllMetdataFiber <- mutate(AllMetdataFiber,  gram_fiber_per_Kcal_aboveRecommended = case_when(gram_fiber_per_Kcal>=15~1,
                                                                                             gram_fiber_per_Kcal<15~0))

AllMetdataFiber <- mutate(AllMetdataFiber, WhiteRace = case_when(race.factor=="White"~1,
                                                                 race.factor=="Black"~0,
                                                                 race.factor=="Asian"~0,
                                                                 race.factor=="Prefers not to disclose/Unknown"~1,
                                                                 race.factor=="Other"~0))
AllMetdataFiber$fiber_5g_increment <- (AllMetdataFiber$aofib)/5


AllMetdataFiber$pbx_now_imputed <- AllMetdataFiber$pbx_now.factor
AllMetdataFiber$pbx_now_imputed[is.na(AllMetdataFiber$pbx_now_imputed)] <- 
  names(sort(table(AllMetdataFiber$pbx_now.factor), decreasing = TRUE))[1]

AllMetdataFiber <- mutate(AllMetdataFiber,pbx_now_imputed = case_when(pbx_now_imputed=='I do not know what probiotics are'~'No',
                                                                      pbx_now_imputed=='No'~'No',
                                                                      pbx_now_imputed=='Yes'~'Yes'))

AllMetdataFiber$pbx_now_imputed <- as.factor(AllMetdataFiber$pbx_now_imputed)


AllMetdataFiber$abx_3mo_yn_imputed <- AllMetdataFiber$abx_3mo_yn
AllMetdataFiber$abx_3mo_yn_imputed[is.na(AllMetdataFiber$abx_3mo_yn_imputed)] <- 
  names(sort(table(AllMetdataFiber$abx_3mo_yn), decreasing = TRUE))[1]

# Collapsing ethnicity to binary variable
AllMetdataFiber <- mutate(AllMetdataFiber,latinx.binary = case_when(latinx.factor=='Prefers not to disclose/Unknown'~'Non-Latinx',
                                                                    latinx.factor=='Non-Latinx'~'Non-Latinx',
                                                                    latinx.factor=='Latinx'~'Latinx'))

AllMetdataFiber$latinx.binary <- as.factor(AllMetdataFiber$latinx.binary)



saveRDS(AllMetdataFiber, "data/gu_survey_cohort.RDS")

