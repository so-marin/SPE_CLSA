library(effectsize)
library(emmeans)
library(lmerTest)
library(table1)
library(effectsize)
library(lme4)
library(officer)
library(flextable)
library(broom)


source("analysis_functions.R")
data<-read.csv("longitudinal_data_clean.csv")

#~~~~~~~~~~~~~~~~~#
# DEMOGRAPHICS ####
#~~~~~~~~~~~~~~~~~#

data6<-data6%>%
  mutate(
    edu=factor(ED_UDR04_COM),
    inc=factor(INC_TOT_COF1))

table1(~AGE_NMBR_COF1+sex+edu+testing_lang+DEP_CESD10_COF1+DEP_CESD10_COF2+ethnicity+inc |scd_status,data=data6)

## CONTINUOUS VARIABLES ####
# anova + posthocs 
age<-lm(AGE_NMBR_COF1~scd_status, data=data6)
anova(age)
em_age <- emmeans(age, ~ scd_status)
contrast(em_age, method="pairwise",adjust="bonferroni")
eta_squared(age) # effect sizes

dep1<-lm(DEP_CESD10_COF1~scd_status, data=data6)
anova(dep1)
em_dep1 <- emmeans(dep1, ~ scd_status)
contrast(em_dep1, method="pairwise",adjust="bonferroni")
eta_squared(dep1)

dep2<-lm(DEP_CESD10_COF2~scd_status, data=data6)
anova(dep2)
em_dep2 <- emmeans(dep2, ~ scd_status)
contrast(em_dep2, method="pairwise",adjust="bonferroni")
eta_squared(dep2)

## CATEGORICAL VARIABLES ####
kruskal.test(edu~scd_status, data=data6)
pairwise.wilcox.test(data6$ED_UDR04_COM, data6$scd_status, p.adjust="bonferroni")
tapply(data6$ED_UDR04_COM, data6$scd_status, median, na.rm=TRUE)

# chi square test
sex<-chisq.test(table(data6$sex, data6$scd_status))
sex
sex$stdres

lang<-chisq.test(table(data6$testing_lang, data6$scd_status))
lang
lang$stdres



#~~~~~~~~~~~~~~~~#
# FORMAT DATA ####
#~~~~~~~~~~~~~~~~#

#subset dataset to variables required for longitudinal analysis
data1<-data[,c("entity_id","scd_status","sex","testing_lang","ED_UDR04_COM","AGE_NMBR_COF1",
                "DEP_CESD10_COF1","DEP_CESD10_COF2",
                "COG_REYI_SCORE_COF1","COG_REYI_SCORE_COF2","COG_REYII_SCORE_COF1",
                "COG_REYII_SCORE_COF2","primacy1_SPE_fu1","primacy1_SPE_fu2",
                "primacy2_SPE_fu1","primacy2_SPE_fu2","recency1_SPE_fu1","recency1_SPE_fu2",
                "recency2_SPE_fu1","recency2_SPE_fu2","middle1_SPE_fu1","middle2_SPE_fu1",
                "middle1_SPE_fu2","middle2_SPE_fu2")]

# variables to be standardized 
z_vars<-c("COG_REYI_SCORE_COF1","COG_REYI_SCORE_COF2","COG_REYII_SCORE_COF1",
          "COG_REYII_SCORE_COF2","primacy1_SPE_fu1","primacy1_SPE_fu2",
          "primacy2_SPE_fu1","primacy2_SPE_fu2","recency1_SPE_fu1","recency1_SPE_fu2",
          "recency2_SPE_fu1","recency2_SPE_fu2","middle1_SPE_fu1","middle2_SPE_fu1",
          "middle1_SPE_fu2","middle2_SPE_fu2")

##### MEAN-CENTER AND STANDARDIZE VARIABLES, REFORMAT VARIABLE NAMES ####
data1<-data1%>%
  mutate(
    #mean centering for unstandardized models 
    mage=scale(AGE_NMBR_COF1, center=TRUE, scale=FALSE), #mean center
    medu=scale(ED_UDR04_COM, center=TRUE, scale=FALSE),#mean center
    mcesd10_fu1=scale(DEP_CESD10_COF1, center=TRUE, scale=FALSE),#mean center
    mcesd10_fu2=scale(DEP_CESD10_COF2, center=TRUE, scale=FALSE),#mean center
    # standardized variables
    zmage=scale(AGE_NMBR_COF1),
    zmedu=scale(ED_UDR04_COM),
    zmcesd10_fu1=scale(DEP_CESD10_COF1),
    zmcesd10_fu2=scale(DEP_CESD10_COF2)
  )%>%
  # batch z-scoring for outcome vars
  mutate(across(all_of(z_vars),
                ~(scale(.x)),.names="z{.col}"))%>%
  # harmonize var naming across time points 
  rename_with(~str_replace(., "COF1$", "fu1"))%>%
  rename_with(~str_replace(., "COF2$", "fu2"))%>%
  rename_with(~str_replace(., "SPE_fu1$", "fu1"))%>%
  rename_with(~str_replace(., "SPE_fu2$", "fu2"))


# final anlaytic dataset
data_final<-data1[,c("entity_id","scd_status","testing_lang","sex","mage","medu","mcesd10_fu1","mcesd10_fu2",
                            "COG_REYI_SCORE_fu1","COG_REYI_SCORE_fu2","COG_REYII_SCORE_fu1","COG_REYII_SCORE_fu2",
                            "primacy1_fu1","primacy1_fu2","primacy2_fu1","primacy2_fu2",
                            "recency1_fu1","recency1_fu2","recency2_fu1","recency2_fu2",
                            "middle1_fu1","middle2_fu1","middle1_fu2","middle2_fu2",
                            "zmage","zmedu","zmcesd10_fu1","zmcesd10_fu2",
                            "zCOG_REYI_SCORE_fu1","zCOG_REYI_SCORE_fu2","zCOG_REYII_SCORE_fu1","zCOG_REYII_SCORE_fu2",
                            "zprimacy1_fu1","zprimacy1_fu2","zprimacy2_fu1","zprimacy2_fu2",
                            "zrecency1_fu1","zrecency1_fu2","zrecency2_fu1","zrecency2_fu2",
                            "zmiddle1_fu1","zmiddle2_fu1","zmiddle1_fu2","zmiddle2_fu2")]


##### CREATE LONG DATAFRAME ####
#separates measures into time factor (fu1 vs fu2)
data_long<-data_final %>%
  pivot_longer(
    cols=ends_with(c("_fu1", "_fu2")),
    names_to=c(".value", "Time"),
    names_pattern="(.*)_(fu1|fu2)"
  )


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# MIXED EFFECTS MODEL DIAGNOSTICS ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

## DEFINE PARAMETERS #####
#model specification inputs for diagnostic pipeline
outcomes<-c("COG_REYI_SCORE","COG_REYII_SCORE","primacy1","middle1","recency1",
            "primacy2","middle2","recency2")
predictor_vars<-"mage+sex+medu+mcesd10+testing_lang"


## RUN MIXED-EFFECTS MODEL DIAGNOSTICS FUNCTION #####
diagnostics<-mem_diagnostics(data_long,outcomes,"scd_status",predictor_vars,"entity_id")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# SPE MIXED EFFECTS MODELS ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

## DEFINE PARAMETERS ####
#model specification inputs for mixed effects functions
outcomes_unstd<-c("COG_REYI_SCORE","COG_REYII_SCORE","primacy1","middle1","recency1",
                  "primacy2","middle2","recency2")
outcomes_std<-c("zCOG_REYI_SCORE","zCOG_REYII_SCORE","zprimacy1","zmiddle1","zrecency1",
                "zprimacy2","zmiddle2","zrecency2")
covariates_unstd<-"medu + sex + mage + mcesd10 + testing_lang"
covariates_std<-"zmedu + sex + zmage + zmcesd10 + testing_lang"

## MODEL ESTIMATION FUNCTIONS ####
#fit unstandardized models
unstd_long_mod<-fit_unstd_long_model(data_long, outcomes_unstd, "scd_status", 
                                     covariates_unstd, "entity_id")
#fit standardized models 
std_long_mod<-fit_std_long_model(data_long, outcomes_std, "scd_status", 
                                 covariates_std, "entity_id")

## MERGE STANDARDIZED AND UNSTANDARDIZED MODELS OUTPUTS #### 
all_results_long<-left_join(
  unstd_long_mod$coef, std_long_mod, by=c("Outcome","term"))

## REFORMAT OUTPUTS #####
results_final_long<-format_results(all_results_long)

## CREATE WORD DOCUMENT WITH OUTPUTS #####
doc<-create_results_long_doc(results_final_long)

#will save in wd
print(doc, target="results_long.docx")



#~~~~~~~~~~#
# PLOTS ####
#~~~~~~~~~~#

## SPE PLOTS ####

spe_plot <- c("primacy1", "primacy2", "middle1", "middle2", "recency1", "recency2")

emms_plot_immediate<-unstd_long_mod$emm%>%
  filter(Outcome %in% spe_plot) %>%
  mutate(
    region=case_when(
      grepl("primacy",Outcome)~ "Primacy",
      grepl("middle",Outcome)~ "Middle",
      grepl("recency",Outcome)~ "Recency",
      TRUE ~NA_character_
    ),
    trial_type=case_when(
      grepl("1$",Outcome)~"Immediate",
      grepl("2$",Outcome)~"Delayed",
      TRUE ~ NA_character_
    ),
    Time=factor(Time,levels=c("fu1", "fu2"), labels=c("T1", "T2")),
    trial_type=factor(trial_type, levels=c("Immediate", "Delayed")),
    region=factor(region, levels=c("Primacy", "Middle", "Recency"))
  )

p<-ggplot(emms_plot_immediate, 
          aes(x=region, y=emmean, color=scd_status, group=scd_status))+
  geom_line(size=0.8)+
  geom_point(size=1)+
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL),width=0.1)+
  facet_grid(trial_type~Time, labeller=label_value) +
  scale_y_continuous(limits=c(20, 55))+
  scale_x_discrete(expand=expansion(mult=c(0.2, 0.2))) +
  labs(
    x="Serial Position",
    y= "Average % Recalled",
    color=NULL,
  )+
  scale_color_brewer(palette="Dark2")+
  theme_bw() +
  theme(
    plot.title=element_text(hjust=0.5, size=14),
    axis.line=element_line(color= "black", size=0.5),
    axis.title.x=element_text(margin=margin(t=10)),
    axis.title.y=element_text(margin=margin(r=10)),
    axis.text=element_text(size=12, color="black"),
    axis.title=element_text(size=14),
    legend.text=element_text(size=12),
    legend.position="bottom",
    strip.text=element_text(size=14),
    strip.background=element_blank(),
    panel.background=element_rect(fill="white"),
    panel.grid=element_blank() 
  )

ggsave("spe_curves1.png", plot=p, width=8, height=6, units="in",dpi=600)



## TOTAL RAVLT RECALL PLOTS ####

ravlt<-c("COG_REYI_SCORE","COG_REYII_SCORE")

# Filter only the Wave 1 EMMs for the outcomes of interest
emms_plot_immediate<-unstd_long_mod$emm %>%
  filter(Outcome %in% ravlt) %>%
  mutate(
    outcome=factor(Outcome, levels=ravlt, labels=c("Immediate", "Delayed"))
  )

# Plot
q<-ggplot(emms_plot_immediate, aes(x=outcome, y=emmean, fill=scd_status)) +
  geom_bar(stat="identity", position=position_dodge(width=.6), width=.5,alpha=.7)+
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL),position=position_dodge(width=.6), width=.2) +
  labs(
    x="Recall Trial",
    y="Average Recall Score",
    fill="Group"
  )+
  scale_x_discrete(expand=c(.4, 0)) +
  scale_y_continuous(limits=c(0, 7),expand=c(0,0),breaks=seq(0,6.5,by =.5))+
  facet_wrap(~Time, labeller= labeller(Time=c("fu1"="T1","fu2"="T2")))+
  scale_fill_brewer(palette="Dark2")+
  theme_bw()+
  theme(
    axis.line=element_line(color = "black", size=.5),
    axis.title.x=element_text(margin=margin(t=10)),
    axis.title.y=element_text(margin=margin(r=15)),
    axis.text=element_text(size=12,color="black"),
    axis.title=element_text(size= 14),
    legend.title=element_text(size = 14),
    legend.text=element_text(size = 12),
    legend.justification=c("right", "top"),
    panel.background=element_rect(fill = "white"),
    panel.spacing=unit(1, "lines"),
    strip.text=element_blank(),
    panel.grid=element_blank(),
    panel.border= element_blank())

ggsave("bar_graphs.png", plot = q, width = 8, height = 6, units = "in", dpi = 600)



## FACETED EMM PLOTS ####

combined_emms<-unstd_long_mod$emm %>%
  mutate(
    Time=factor(Time, levels = c("fu1", "fu2"), labels = c("T1", "T2")),
    group=interaction(Time, scd_status),
    trial_type=case_when(
      Outcome %in% c("primacy1", "middle1", "recency1") ~ "Immediate",
      Outcome %in% c("primacy2", "middle2", "recency2") ~ "Delayed",
      TRUE ~ "Other"
    ),
    region=case_when(
      grepl("primacy", Outcome) ~ "Primacy",
      grepl("middle", Outcome) ~ "Middle",
      grepl("recency", Outcome) ~ "Recency",
      TRUE ~ Outcome
    ),
    region=factor(region, levels=c("Primacy", "Middle", "Recency")),
    trial_type=factor(trial_type, levels=c("Immediate", "Delayed"))
  )%>%
  filter(trial_type!="Other")

r<-ggplot(combined_emms, aes(x = Time, y = emmean, color = scd_status, group = scd_status)) +
  geom_line(size = 1) +
  geom_point(size = 2)+
  geom_errorbar(aes(ymin = emmean - 1.96 * SE, ymax = emmean + 1.96 * SE), width = 0.1)+
  facet_grid(trial_type ~ region, scales = "free_y", labeller = labeller(trial_type = label_value, region = label_value)) +
  scale_x_discrete(expand = expansion(mult = c(0.2, 0.2)))+
  scale_y_continuous(limit=c(20,60),breaks=seq(20,60,by=10))+
  theme_bw()+
  scale_color_brewer(palette="Dark2")+
  labs(
    y="Average % Recalled",
    x="Time",
    color = NULL
  )+
  theme(
    strip.text = element_text(size = 14), 
    strip.background=element_blank(),
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.line = element_line(color = "black", size = 0.5),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.text = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.position="bottom",
    panel.grid = element_blank()
  )

ggsave("emm_plots.png", plot=r, width= 8, height=6, units="in", dpi=600)



