library(tidyverse)
library(table1)
library(car)
library(effectsize)
library(emmeans)
library(broom)
library(rstatix)
library(lmtest)
library(flextable)
library(officer)
library(broom)


#have working directory set
setwd()
source("analysis_functions.R")

#~~~~~~~~~~~~~~~~~~~~#
#### DEMOGRAPHICS ####
#~~~~~~~~~~~~~~~~~~~~#

data5<-data5%>%
  mutate(
    edu=factor(ED_UDR04_COM),
    inc=factor(INC_TOT_COF1)
    )

table1(~AGE_NMBR_COF1+sex+edu+testing_lang+DEP_CESD10_COF1+ethnicity+inc |scd_status,data=data5)

##### CONTINUOUS VARIABLES #####
# anova + posthocs 
age<-lm(AGE_NMBR_COF1~scd_status, data=data5)
anova(age)
em_age <- emmeans(age, ~ scd_status)
contrast(em_age, method="pairwise",adjust="bonferroni")
eta_squared(age) #effect sizes

dep<-lm(DEP_CESD10_COF1~scd_status, data=data5)
anova(dep)
em_dep <- emmeans(dep, ~ scd_status)
contrast(em_dep, method="pairwise",adjust="bonferroni")
eta_squared(dep)

##### CATEGORICAL VARIABLES #####
kruskal.test(inc~scd_status, data=data5)
pairwise.wilcox.test(data5$INC_TOT_COF1, data5$scd_status, p.adjust="bonferroni")
tapply(data5$INC_TOT_COF1, data5$scd_status, median, na.rm=TRUE)

kruskal.test(edu~scd_status, data=data5)
pairwise.wilcox.test(data5$ED_UDR04_COM, data5$scd_status, p.adjust="bonferroni")
tapply(data5$ED_UDR04_COM, data5$scd_status, median, na.rm=TRUE)

# chi square tests 
sex<-chisq.test(table(data5$sex, data5$scd_status))
sex
sex$stdres

lang<-chisq.test(table(data5$testing_lang, data5$scd_status))
lang
lang$stdres


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### REGRESSION ASSUMPTIONS ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

##### DEFINE PARAMETERS #####
outcomes<-c("COG_REYI_SCORE_COF1","COG_REYII_SCORE_COF1",
                "primacy1_SPE","middle1_SPE","recency1_SPE",
                "primacy2_SPE","middle2_SPE","recency2_SPE",
                "primacy_ratio","middle_ratio","recency_ratio")
predictor_vars<-c("AGE_NMBR_COF1","sex","ED_UDR04_COM","DEP_CESD10_COF1",
                  "testing_lang")
group<-"scd_status"

##### RUN REGRESSION DIAGNOSTICS FUNCTION #####
regression_diagnostics(data5,outcomes,group,predictor_vars)


#~~~~~~~~~~~~~~~~~~~~#
#### SPE ANALYSIS ####
#~~~~~~~~~~~~~~~~~~~~#

##### MEAN-CENTER AND STANDARDIZE VARIABLES #####
# vars to be standardized
z_vars<-c("COG_REYI_SCORE_COF1",
          "COG_REYII_SCORE_COF1",
          "primacy1_SPE","middle1_SPE","recency1_SPE",
          "primacy2_SPE","middle2_SPE","recency2_SPE",
          "primacy_ratio","middle_ratio","recency_ratio")

data6<-data5%>%
  mutate(
    #mean center continuous covariates
    mage=scale(AGE_NMBR_COF1, center=TRUE, scale=FALSE),
    medu=scale(ED_UDR04_COM, center=TRUE, scale=FALSE),
    mceds10=scale(DEP_CESD10_COF1, center=TRUE, scale=FALSE),
    #standardize continuous covariates 
    zmage=scale(AGE_NMBR_COF1),
    zmedu=scale(ED_UDR04_COM),
    zmceds10=scale(DEP_CESD10_COF1)
  )%>%
  #batch standardize outcome vars
  mutate(across(all_of(z_vars),
                ~(scale(.x)),.names="z{.col}"))


##### DEFINE PARAMETERS #####
group_var<-"scd_status"
predictors_unstd<-"sex +mage+ medu + mceds10 + testing_lang"
predictors_std<-"sex +zmage+ zmedu + zmceds10 + testing_lang"

#unstandardized vars
outcomes_unstd<-c("COG_REYI_SCORE_COF1","COG_REYII_SCORE_COF1",
              "primacy1_SPE","middle1_SPE","recency1_SPE",
              "primacy2_SPE","middle2_SPE","recency2_SPE")
#standardized vars 
outcomes_std<-c("zCOG_REYI_SCORE_COF1", "zCOG_REYII_SCORE_COF1",
                "zprimacy1_SPE","zmiddle1_SPE","zrecency1_SPE",
                "zprimacy2_SPE","zmiddle2_SPE","zrecency2_SPE")


##### CALL REGRESSION ESTIMATION FUNCTIONS #####
# fit unstandardized regression models 
unstd_mod<-fit_unstd_model(data6,outcomes_unstd,group_var,predictors_unstd)
#fit standardized regression models 
std_mod<-fit_std_model(data6,outcomes_std,group_var,predictors_std)

##### MERGE UNSTANDARDIZED AND STANDARDIZED MODEL OUTPUTS #####
all_results<-left_join(
  unstd_mod$coef, std_mod, by=c("Outcome","term"))

##### REFORMAT MODEL OUTPUTS #####
results_final<-format_results(all_results)

##### CREATE WORD DOCUMENT WITH MODEL OUTPUTS #####
doc<-create_results_doc(results_final,unstd_mod$r2)
print(doc, target="cross_sectional_results.docx")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### RATIO SCORE ANALYSIS ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

##### DEFINE PARAMETERS #####
ratio_outcomes_unstd <- c("primacy_ratio","middle_ratio","recency_ratio")
ratio_outcomes_std<-c("zprimacy_ratio","zmiddle_ratio","zrecency_ratio")

##### CALL REGRESSION ESTIMATION FUNCTIONS #####
# fit unstandardized regression models 
ratio_unstd_mod<-fit_unstd_model(data6,ratio_outcomes_unstd,group_var,predictors_unstd)
# fit standardized regression models 
ratio_std_mod<-fit_std_model(data6,ratio_outcomes_std,group_var,predictors_std)

##### MERGE UNSTANDARDIZED AND STANDARDIZED MODEL OUTPUTS #####
all_ratio_results<-left_join(
  ratio_unstd_mod$coef, ratio_std_mod, by=c("Outcome","term"))

##### REFORMAT MODEL OUTPUTS #####
ratio_results_final<-format_results(all_ratio_results)

##### CREATE WORD DOCUMENT WITH MODEL OUTPUTS #####
ratio_doc<-create_results_doc(ratio_results_final,ratio_unstd_mod$r2)
print(doc, target="ratio_results.docx")


#~~~~~~~~~~~~~~~~~#
#### SPE PLOTS ####
#~~~~~~~~~~~~~~~~~#

### make new dfs with emms from reg analyses ###
plot_immediate<-c("primacy1_SPE", "middle1_SPE", "recency1_SPE")
emms_plot_immediate<-unstd_mod$emm%>%
  filter(Outcome%in%plot_immediate)%>%
  mutate(Outcome=factor(Outcome, levels=plot_immediate, labels=c("Primacy", "Middle", "Recency")))

plot_delayed<-c("primacy2_SPE","middle2_SPE","recency2_SPE")
emms_plot_delayed<-unstd_mod$emm%>%
  filter(Outcome%in%plot_delayed)%>%
  mutate(Outcome=factor(Outcome, levels=plot_delayed, labels=c("Primacy", "Middle", "Recency")))


# Add a trial column to each dataset
emms_plot_immediate$trial<-"Immediate Trial"
emms_plot_delayed$trial<-"Delayed Trial"

# Combine datasets
emms_panel<-bind_rows(emms_plot_immediate, emms_plot_delayed)
emms_panel$trial<-factor(emms_panel$trial, levels=c("Immediate Trial","Delayed Trial"))

# SPE curve plot
q<-ggplot(emms_panel, aes(x=Outcome, y=emmean, color=scd_status, group=scd_status))+
  geom_line(size=0.8)+
  geom_point(size=1.5)+
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), width=0.05)+
  facet_wrap(~trial)+
  labs(
    x="Serial Position",
    y="Average % Recalled",
    color="Group"
  ) +
  scale_x_discrete(expand=c(0, 0.2))+
  scale_y_continuous(limits=c(20,60),breaks=seq(20,60, by=10))+
  scale_color_brewer(palette="Dark2")+
  theme_bw()+
  theme(
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    panel.border=element_blank(),        
    axis.line=element_line(color = "black", linewidth = 0.5), 
    strip.background=element_blank(),   
    strip.text=element_text(size = 14), 
    panel.spacing =unit(2, "lines"),     
    axis.title.x=element_text(margin=margin(t=10)),
    axis.title.y= element_text(margin = margin(r=10)),
    axis.text=element_text(size= 12,color= "black"),
    axis.title=element_text(size=14),
    legend.title= element_text(size= 14),
    legend.text=element_text(size=12),
    legend.position="bottom",
    panel.background= element_rect(fill="white")
  )

#will save in wd
ggsave("CS_spe.png", plot=q, width=8, height=6, units="in", dpi=600)

