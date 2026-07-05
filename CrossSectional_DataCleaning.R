library(tidyverse)
library(datawizard)
library(ggstatsplot)
library(rstatix)
library(psych)


source("analysis_functions.R")

data<-read_csv("scd_spe_merged.csv")

data1<-data%>%
  filter(
    !is.na(startdate_COF1), #no fu1 data
    ED_UDR04_COM!=9, #edu missing
    AGE_NMBR_COF1>=60,
    ADM_SPECIAL_INHOME_COF1!=1, #phone admin
    !SCD_MEMO_COF1%in%c(8,9,-88888), #scd missing
    !SCD_WORY_COF1%in%c(3,8,9,-88888))%>% #scd worry missing or "undediced" (3)
  mutate(
    scd_status=case_when(
      SCD_WORY_COF1%in%c(1, 2)~3,
      SCD_WORY_COF1%in%c(4,5)~2,
      SCD_WORY_COF1==-99999~1,
      TRUE~NA_real_
    ),
    sex=factor(SDC_BTHSEX_COF1,levels=c(1,2),labels=c("Males","Females")),
    scd_status=factor(scd_status,levels=c(1,2,3),labels=c("Controls","SCD-No Worry","SCD+Worry"))
    )


#~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### EXCLUSION CRITERIA ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~#

###### recode TBI variable ####

tbi_vars<-c("TBI_RSLT_DRM_COF1","TBI_RSLT_KO20_COF1","TBI_RSLT_KO20MORE_COF1")

#recode missing value codes into 0
for (var in tbi_vars) { 
  data1[[var]]<-replace(data1[[var]],data1[[var]] %in%c(-99999),0)
}

data1<-data1%>% #flag for TBI with LoC
  mutate(
    tbi_pos_cof1=TBI_RSLT_DRM_COF1+TBI_RSLT_KO20_COF1+TBI_RSLT_KO20MORE_COF1,
    tbi_pos_cof1=if_else(tbi_pos_cof1>0,1,tbi_pos_cof1)
  )

##### apply exclusion criteria ####
exclusion_vars<-c("CCC_ALZH_COF1","CCC_MEMPB_COF1","CCC_CVA_COF1",
                  "CCC_MS_COF1","CCC_TIA_COF1","CCC_EPIL_COF1",
                  "CCC_PARK_COF1","tbi_pos_cof1")

# filter if value of var exclusion_vars is positive (1) 
# or missing (8,9,-88888,-88880,-77771) 
for (var in exclusion_vars){
  data1<-data1%>%
    filter(!(get(var)%in%c(1,8,9,-88888,-88880,-77771)))
}


#~~~~~~~~~~~~~~~~~~~~#
#### MISSING DATA ####
#~~~~~~~~~~~~~~~~~~~~#

##### recode missing data ####
missing_codes <- c(-99999, -88888,-77771,-77772)
missing_dems<-c(8,9,-88888,-99999)
missing_vars<-c("COG_REYI_SCORE_COF1","COG_REYII_SCORE_COF1","DEP_CESD10_COF1")
missing_vars_dems<-c("INC_TOT_COF1","COG_REYI_STARTLANG_COF1","SDC_CULT_WH_COM")

# use replace_missing() function
data2<-replace_missing(data1,missing_vars,missing_codes)
data2<-replace_missing(data2,missing_vars_dems,missing_dems)

# check % missing
model_vars<-data2[,c("scd_status","COG_REYI_SCORE_COF1","COG_REYII_SCORE_COF1","DEP_CESD10_COF1",
                     "INC_TOT_COF1","COG_REYI_STARTLANG_COF1","SDC_CULT_WH_COM")]

missing<-sapply(model_vars,function(x)mean(is.na(x))*100)
print(missing)


##### missing value analysis ####

data2<-data2%>%
  mutate(
    testing_lang=factor(COG_REYI_STARTLANG_COF1),
    ethnicity= factor(SDC_CULT_WH_COM, levels=c(0,1),labels=c("non-white","white")),
    # create variable that returns 1 if ravlt vars have missing data
    ravlt_missing=if_else(is.na(COG_REYI_SCORE_COF1)&is.na(COG_REYII_SCORE_COF1),1,0)
  )

# Fit logistic regression
missing_model<-glm(ravlt_missing ~ scd_status+sex+AGE_NMBR_COF1+ED_UDR04_COM+
             testing_lang+DEP_CESD10_COF1, 
             data = data2, 
             family = binomial)

# View summary
summary(missing_model)
exp(coef(missing_model))
exp(confint(missing_model))

capture.output(summary(missing_model), file = "logistic_regression_output.txt")

##### exclude people with no RAVLT data ####
data3<- data2 %>%
  filter(ravlt_missing!=1)


#~~~~~~~~~~~~~~#
#### RAVLT #####
#~~~~~~~~~~~~~~#

##### recode RAVLT item values ####
data3 <- data3 %>%
  mutate(across(
    ends_with("CAT_COF1"),
    ~case_when(
      .%in% c(1, 2)~ 1,
      .== 9~0,
      TRUE~.
    )))

##### calculate serial position totals ####
data3 <- data3 %>%
  mutate(
    primacy1=rowSums(across(c(COG_REYI_1_CAT_COF1,COG_REYI_2_CAT_COF1,
                              COG_REYI_3_CAT_COF1,COG_REYI_4_CAT_COF1))),
    middle1=rowSums(across(c(COG_REYI_5_CAT_COF1,COG_REYI_6_CAT_COF1,
                               COG_REYI_7_CAT_COF1,COG_REYI_8_CAT_COF1,
                               COG_REYI_9_CAT_COF1,COG_REYI_10_CAT_COF1,
                               COG_REYI_11_CAT_COF1))),
    recency1=rowSums(across(c(COG_REYI_12_CAT_COF1,COG_REYI_13_CAT_COF1,
                              COG_REYI_14_CAT_COF1,COG_REYI_15_CAT_COF1))),
    primacy2=rowSums(across(c(COG_REYII_1_CAT_COF1,COG_REYII_2_CAT_COF1,
                              COG_REYII_3_CAT_COF1,COG_REYII_4_CAT_COF1))),
    middle2=rowSums(across(c(COG_REYII_5_CAT_COF1,COG_REYII_6_CAT_COF1,
                               COG_REYII_7_CAT_COF1,COG_REYII_8_CAT_COF1,
                               COG_REYII_9_CAT_COF1,COG_REYII_10_CAT_COF1,
                               COG_REYII_11_CAT_COF1))),
    recency2=rowSums(across(c(COG_REYII_12_CAT_COF1,COG_REYII_13_CAT_COF1,
                              COG_REYII_14_CAT_COF1,COG_REYII_15_CAT_COF1))),
    # calculate SPE scores with regional scoring
    primacy1_SPE=100*primacy1/ 4,
    middle1_SPE=100*middle1/7,
    recency1_SPE=100*recency1/4,
    primacy2_SPE=100*primacy2/4,
    middle2_SPE=100*middle2/7,
    recency2_SPE=100*recency2/4,
    # calculate ratio scores with correction
    recency_ratio=ifelse(is.na(recency1)|is.na(recency2),NA,(recency1+1)/(recency2+1)),
    middle_ratio=ifelse(is.na(middle1)|is.na(middle2),NA,(middle1+1)/(middle2+1)),
    primacy_ratio=ifelse(is.na(primacy1)|is.na(primacy2),NA,(primacy1+1)/(primacy2+1)))


#~~~~~~~~~~~~~~~~#
#### OUTLIERS ####
#~~~~~~~~~~~~~~~~#

##### data visualization ####
# use the data_visualization_pdf() function
plot_outcomes<-c("COG_REYII_SCORE_COF1", "COG_REYI_SCORE_COF1",
                 "primacy1_SPE","primacy2_SPE","middle1_SPE","middle2_SPE",
                 "recency1_SPE","recency2_SPE","primacy_ratio","middle_ratio",
                 "recency_ratio","DEP_CESD10_COF1")

# run function to get pdf with plots
data_visualization_pdf(data3,plot_outcomes,"scd_status","cross_sectional_plots.pdf")


#skew and kurtosis
describe(data3[, plot_outcomes])


##### univariate outliers ####
outlier_vars<-c("COG_REYII_SCORE_COF1", "COG_REYI_SCORE_COF1")

# exclude when z score is >= abs(3)
#use remove_group_outliers() 
data4<-remove_group_outliers(data3, "scd_status", outlier_vars)


##### multivariate outliers ####

#vector for variables needed to check for multivariate outliers
vars_md<-c("scd_status","ED_UDR04_COM","sex","AGE_NMBR_COF1",
                  "COG_REYI_SCORE_COF1","COG_REYII_SCORE_COF1",
                  "DEP_CESD10_COF1","testing_lang")

# use check_multivariate_outliers() fuction with alpha = .001
md_results<-check_multivariate_outliers(data4, vars_md)
md_results$summary

#new dataset without outliers
data5 <- data4[md_results$mahalanobis_distances < md_results$cutoff, ]


#### MEAN IMPUTATION OF CESD-10 VAR ####
data6<-impute_by_group(data5, "scd_status","DEP_CESD10_COF1")

# write out clean dataset ####
write.csv(data6,"data_clean.csv",row.names=FALSE)

