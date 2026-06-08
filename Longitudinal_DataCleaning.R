library(tidyverse)
library(ggpubr)
library(rstatix)
library(datawizard)
library(modi)
library(psych)
library(table1)


data <- read_csv("scd_spe_merged.csv")

data1<-data%>%
  filter(
    !is.na(startdate_COF1), #no fu1 data
    !is.na(startdate_COF2))%>% #no fu2 data
  # flag for phone admin because of COVID19
  mutate(
    startdate_COF2=as.Date(startdate_COF2),
    ADM_SPECIAL_INHOME_COF2=as.integer(startdate_COF2 >= as.Date("2020-03-16")))%>%
  filter(
    ED_UDR04_COM!=9, #edu missing
    AGE_NMBR_COF1>=60,
    ADM_SPECIAL_INHOME_COF1!=1, #phone admin fu1
    ADM_SPECIAL_INHOME_COF2!=1, #phone admin fu2    
    !SCD_MEMO_COF1%in%c(8,9,-88888), #scd missing fu1
    !SCD_WORY_COF1%in%c(3,8,9,-88888), #scd worry missing or "undediced" (3) fu1
    !GEN_MEMO_COF2%in%c(8,9,-88888) #scd missing fu2
  )%>%
  mutate(
    scd_status_fu1=case_when(
      SCD_WORY_COF1%in%c(1, 2)~3,
      SCD_WORY_COF1%in%c(4,5)~2,
      SCD_WORY_COF1==-99999~1,
      TRUE~NA_real_
    ),
    scd_status_change=case_when(
      scd_status_fu1==GEN_MEMO_COF2 ~ "stable",
      scd_status_fu1!=GEN_MEMO_COF2 ~ "unstable"),
    scd_status=factor(GEN_MEMO_COF2,levels=c(1,2,3),
                      labels=c("Controls","SCD-No Worry","SCD+Worry")),
    sex=factor(SDC_BTHSEX_COF1,levels=c(1,2),labels=c("Males","Females")),
  )%>%
  filter(!scd_status_change=="unstable")
  


#~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### EXCLUSION CRITERIA ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~#

data2<-data1

###### recode TBI variable ####

tbi_vars<-c("TBI_RSLT_DRM_COF1","TBI_RSLT_KO20_COF1","TBI_RSLT_KO20MORE_COF1",
            "TBI_RSLT_NRM_COF2","TBI_RSLT_KO20_COF2","TBI_RSLT_KO2030_COF2","TBI_RSLT_KO2030_COF2")

#recode missing value codes into 0
for (var in tbi_vars) { 
  data2[[var]] <- replace(data2[[var]], data2[[var]] %in% c(-99999,-88888), 0)
}

data2<-data2%>% #flag for TBI with LoC
  mutate(
    TBI_POS_COF1=as.integer(
      TBI_RSLT_DRM_COF1 +TBI_RSLT_KO20_COF1 +TBI_RSLT_KO20MORE_COF1 > 0),
    TBI_POS_COF2=as.integer(TBI_RSLT_NRM_COF2 +TBI_RSLT_KO20_COF2 +
                                TBI_RSLT_KO2030_COF2 +TBI_RSLT_KO30MORE_COF2 > 0))


##### apply exclusion criteria ####
exclusion_vars<-c("CCC_ALZH_COF1","CCC_CVA_COF1","CCC_MEMPB_COF1",
                  "CCC_MS_COF1","CCC_TIA_COF1","CCC_EPIL_COF1",
                  "CCC_PARK_COF1","TBI_POS_COF1",
                  "CCC_ALZH_COF2","CCC_MEMPB_COF2","CCC_CVA_COF2",
                  "CCC_MS_COF2","CCC_TIA_COF2","EPI_EVER_COF2",
                  "CCC_PARK_COF2","TBI_POS_COF2")

# filter if value of var exclusion_vars is positive (1) 
# or missing (8,9,-88888,-88880,-77771,11) 
for (var in exclusion_vars){
  data2<-data2%>%
    filter(!(get(var)%in%c(1,8,9,-88888,-88880,-77771,11)))
}


#~~~~~~~~~~~~~~#
#### SCALES ####
#~~~~~~~~~~~~~~#

#### RAVLT #####

##### recode RAVLT item values ####
data3 <- data2 %>%
  mutate(across(
    ends_with(c("CAT_COF1", "CAT_COF2")),
    ~ case_when(
      . %in% c(1, 2) ~ 1,
      . == 9 ~ 0,
      TRUE ~ .)))


##### calculate serial position totals ####
data3<-data3 %>%
  mutate(
    primacy1_fu1=rowSums(select(.,matches("^COG_REYI_[1-4]_CAT_COF1$"))),
    middle1_fu1 =rowSums(select(.,matches("^COG_REYI_([5-9]|10|11)_CAT_COF1$"))),
    recency1_fu1=rowSums(select(.,matches("^COG_REYI_(12|13|14|15)_CAT_COF1$"))),
    primacy2_fu1=rowSums(select(.,matches("^COG_REYII_[1-4]_CAT_COF1$"))),
    middle2_fu1 =rowSums(select(.,matches("^COG_REYII_([5-9]|10|11)_CAT_COF1$"))),
    recency2_fu1=rowSums(select(.,matches("^COG_REYII_(12|13|14|15)_CAT_COF1$"))),
    primacy1_fu2=rowSums(select(.,matches("^COG_REYI_[1-4]_CAT_COF2$"))),
    middle1_fu2 =rowSums(select(.,matches("^COG_REYI_([5-9]|10|11)_CAT_COF2$"))),
    recency1_fu2=rowSums(select(.,matches("^COG_REYI_(12|13|14|15)_CAT_COF2$"))),
    primacy2_fu2=rowSums(select(.,matches("^COG_REYII_[1-4]_CAT_COF2$"))),
    middle2_fu2= rowSums(select(.,matches("^COG_REYII_([5-9]|10|11)_CAT_COF2$"))),
    recency2_fu2 =rowSums(select(.,matches("^COG_REYII_(12|13|14|15)_CAT_COF2$"))),
    # calculate SPE regional scores 
    primacy1_SPE_fu1=100*primacy1_fu1/ 4,
    middle1_SPE_fu1=100*middle1_fu1/7,
    recency1_SPE_fu1=100*recency1_fu1/4,
    primacy2_SPE_fu1=100*primacy2_fu1/4,
    middle2_SPE_fu1=100*middle2_fu1/7,
    recency2_SPE_fu1=100*recency2_fu1/4,
    primacy1_SPE_fu2=100*primacy1_fu2/ 4,
    middle1_SPE_fu2=100*middle1_fu2/7,
    recency1_SPE_fu2=100*recency1_fu2/4,
    primacy2_SPE_fu2=100*primacy2_fu2/4,
    middle2_SPE_fu2=100*middle2_fu2/7,
    recency2_SPE_fu2=100*recency2_fu2/4
  )



#### CESD-10 ####

#CESD-10 variables
cesd10_vars_fu2<-c("DEP_BOTR_COF2","DEP_MIND_COF2","DEP_FLDP_COF2","DEP_FFRT_COF2",
                   "DEP_FRFL_COF2","DEP_RSTLS_COF2","DEP_LONLY_COF2","DEP_GTGO_COF2")
#reverse-coded items
cesd10_vars_reverse_fu2<-c("DEP_HPFL_COF2","DEP_HAPP_COF2")

#recode items
data3<-data3 %>%
  mutate(
    across(all_of(cesd10_vars_fu2),~{
      x<-replace(., . %in%c(-88888,-88880,8,9),NA) #recode NAs
      dplyr::recode(x, "1"=3, "3"=1,"4"=0)}, #recode item scores
      .names = "{.col}_recoded"
    ),
    across(all_of(cesd10_vars_reverse_fu2),~{
      x<-replace(., . %in%c(-88888,-88880,8,9),NA)
      dplyr::recode(x, "1"=0,"2"=1,"3"=2, "4"=3)},
      .names = "{.col}_recoded"
    )
  )

all_cesd10_items_fu2<-c("DEP_BOTR_COF2_recoded","DEP_MIND_COF2_recoded",
                    "DEP_FLDP_COF2_recoded","DEP_FFRT_COF2_recoded",
                    "DEP_HPFL_COF2_recoded","DEP_FRFL_COF2_recoded","DEP_RSTLS_COF2_recoded",
                    "DEP_HAPP_COF2_recoded","DEP_LONLY_COF2_recoded","DEP_GTGO_COF2_recoded")

#if missing only one item score, replace by mean then calculate mean CESD10 score
data3<-data3 %>%
  rowwise() %>%
  mutate(
    n_missing=sum(is.na(c_across(all_of(all_cesd10_items_fu2)))),
    DEP_CESD10_COF2=ifelse(n_missing <= 1,
                             sum(replace_na(c_across(all_of(all_cesd10_items_fu2)),
                                            mean(c_across(all_of(all_cesd10_items_fu2)), na.rm = TRUE))),
                             NA_real_))%>%
  ungroup()



#~~~~~~~~~~~~~~~~~~~~#
#### MISSING DATA ####
#~~~~~~~~~~~~~~~~~~~~#

##### recode missing data ####
missing_codes <- c(-99999, -88888,-88887,-77771,-77772)
missing_dems<-c(8,9,-88888,-99999)

missing_vars<-c("COG_REYI_SCORE_COF1","COG_REYII_SCORE_COF1",
                "COG_REYI_SCORE_COF2","COG_REYII_SCORE_COF2",
                "DEP_CESD10_COF1","DEP_CESD10_COF2")
missing_vars_dems<-c("INC_TOT_COF1","INC_TOT_COF2","COG_REYI_STARTLANG_COF1","COG_REYI_STARTLANG_COF2",
                     "SDC_CULT_WH_COM")

#' function to replace missing values
#' @param df dataframe 
#' @param vars character vector with variables to recode
#' @param codes numerical vector with missing code values
#' 
#' @return recoded variables

replace_missing <- function(df, vars, codes) {
  df %>%
    mutate(across(all_of(vars),~replace(.x, .x%in%codes, NA_real_)))}

data4<-replace_missing(data3, missing_vars,missing_codes)
data4<-replace_missing(data4, missing_vars_dems, missing_dems)


# check % missing
model_vars<-data4[,c("COG_REYI_SCORE_COF1","COG_REYII_SCORE_COF1",
                     "COG_REYI_SCORE_COF2","COG_REYII_SCORE_COF2",
                     "DEP_CESD10_COF1","DEP_CESD10_COF2",
                     "COG_REYI_STARTLANG_COF1","COG_REYI_STARTLANG_COF2")]

missing<-sapply(model_vars, function(x)mean(is.na(x))*100)
print(missing)


##### missing value analysis ####

data4<-data4%>%
  mutate(
    testing_lang_fu1=factor(COG_REYI_STARTLANG_COF1),
    testing_lang_fu2=factor(COG_REYI_STARTLANG_COF2),
    ethnicity= factor(SDC_CULT_WH_COM, levels=c(0,1),labels=c("non-white","white")),
    # create variables that returns 1 if any ravlt vars have missing data
    ravlt_missing_any=case_when(
      is.na(COG_REYI_SCORE_COF1)| is.na(COG_REYI_SCORE_COF2)|is.na(COG_REYII_SCORE_COF1)|is.na(COG_REYII_SCORE_COF2)~ 1,
      TRUE ~ 0),
    # create testing language variable
    testing_lang=coalesce(testing_lang_fu1, testing_lang_fu2)
    )


# Fit logistic regression
model <- glm(ravlt_missing_any ~ scd_status+sex+AGE_NMBR_COF1+ED_UDR04_COM+testing_lang+DEP_CESD10_COF1+DEP_CESD10_COF2, 
             data = data4, 
             family = binomial)

# View summary
summary(model)
exp(coef(model))
exp(confint(model))

##### exclude people with no RAVLT data ####
data4<-data4%>%
  filter(!ravlt_missing_any==1)



#~~~~~~~~~~~~~~~~#
#### OUTLIERS ####
#~~~~~~~~~~~~~~~~#

##### data visualization ####

#define parameters for visualization function 
plot_outcomes<-c("COG_REYII_SCORE_COF1", "COG_REYI_SCORE_COF1",
                 "COG_REYII_SCORE_COF2", "COG_REYI_SCORE_COF2",
                 "primacy1_SPE_fu1","primacy2_SPE_fu1","middle1_SPE_fu1","middle2_SPE_fu1",
                 "recency1_SPE_fu1","recency2_SPE_fu1",
                 "primacy1_SPE_fu2","primacy2_SPE_fu2","middle1_SPE_fu2","middle2_SPE_fu2",
                 "recency1_SPE_fu2","recency2_SPE_fu2",
                 "DEP_CESD10_COF1","DEP_CESD10_COF2")
group_var<-"scd_status"

pdf_file_name<-"longitudinal_outcomes_visualization.pdf"

# run function to get pdf with plots
data_visualization_pdf(data4,plot_outcomes,group_var,pdf_file_name)


#skew and kurtosis
describe(data4[, plot_outcomes])


##### univariate outliers ####
outliers<-c("COG_REYII_SCORE_COF1", "COG_REYI_SCORE_COF1","COG_REYII_SCORE_COF2", "COG_REYI_SCORE_COF2")

# exclude when z score is >= abs(3)
data5<-data4 %>%
  group_by(scd_status)%>% #search by scd group
  filter(
    !if_any(all_of(outliers),~{
      z<-(.x - mean(.x, na.rm = TRUE))/sd(.x, na.rm = TRUE)
      !is.na(z)&abs(z)>3
    })
  ) %>%
  ungroup()


##### multivariate outliers ####

#create dataframe with variables needed to check for multivariate outliers
data_md<-data5[,c("scd_status","ED_UDR04_COM","sex","AGE_NMBR_COF1",
                  "COG_REYI_SCORE_COF1","COG_REYII_SCORE_COF1","DEP_CESD10_COF1","testing_lang",
                  "COG_REYI_SCORE_COF2","COG_REYII_SCORE_COF2","DEP_CESD10_COF2")]

#recode factor vars to numeric 
data_md<-data_md%>%
  mutate(
    scd_status=as.numeric(scd_status),
    sex=as.numeric(sex),
    testing_lang=as.numeric(testing_lang)
  )

#calculate mahalanobis distance to find multivariate outliers
center_val <- colMeans(data_md, na.rm = TRUE)
cov_mat <- cov(data_md, use = "pairwise.complete.obs")
mahalanobis_distances <- MDmiss(data_md, center = center_val, cov = cov_mat)

# Generate chi-square quantiles
chi_square_quantiles <- qchisq((1:nrow(data_md)) / ((nrow(data_md)) + 1), df = ncol(data_md))

# Sort Mahalanobis distances to match chi-square quantiles
sorted_mahal_distances <- sort(mahalanobis_distances)

# Plot Mahalanobis distances vs. chi-square quantiles
plot(chi_square_quantiles, sorted_mahal_distances, 
     main = "Mahalanobis Distance vs. Chi-Square Quantiles",
     pch = 19, col = "blue")
abline(0, 1, col = "red", lwd = 2)

#cutoff value for distances from chi-square dist with alpha = .001
cutoff<-qchisq(p=0.999, df=ncol(data_md))
summary(mahalanobis_distances<cutoff)

#new dataset without outliers
data6=data5[mahalanobis_distances<cutoff, ]


#### MEAN IMPUTATION OF CESD-10 VAR ####
data6 <- data6 %>%
  group_by(scd_status) %>% #imputation by group
  mutate(across(
    .cols = c(DEP_CESD10_COF1,DEP_CESD10_COF2), 
    .fns = ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)
  )) %>%
  ungroup()

