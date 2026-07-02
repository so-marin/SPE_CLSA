#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### DATA CLEANING FUNCTIONS ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


#' Replace missing values
#' @param df dataframe 
#' @param vars character vector with variables to recode
#' @param codes numerical vector with missing code values
#' 
#' @return recoded variables
replace_missing <- function(df,vars,codes) {
  df %>%
    mutate(across(all_of(vars),~replace(.x,.x %in% codes,NA_real_)))}


#' Detect mutlivariate outliers 
#' @param df dataframe 
#' @param vars character vector with variables to check
#' @param alpha specified alpha level for chi square cutoff 
#' 
#' @return mahalanobis distance values plotted against chi square quantiles
#' "summary" returns logical value for outliers (outliers = TRUE)
#'  
check_multivariate_outliers <- function(data, vars, alpha) {
  
  library(dplyr)
  library(modi)
  
  # Subset data
  data_md <- data[, vars]
  
  # Convert factor variables to numeric
  data_md <- data_md %>%
    mutate(across(where(is.factor), as.numeric))
  
  # Calculate Mahalanobis distances
  center_val <- colMeans(data_md, na.rm = TRUE)
  cov_mat <- cov(data_md, use = "pairwise.complete.obs")
  
  mahalanobis_distances<-MDmiss(data_md,center=center_val,cov=cov_mat)
  
  # Generate chi-square quantiles
  chi_square_quantiles <- qchisq((1:nrow(data_md))/(nrow(data_md)+1),
    df=ncol(data_md)
  )
  
  # Sort distances for Q-Q plot
  sorted_mahal_distances <- sort(mahalanobis_distances)
  
  # Plot
  plot(
    chi_square_quantiles,
    sorted_mahal_distances,
    main = "Mahalanobis Distance vs. Chi-Square Quantiles",
    xlab = "Chi-square Quantiles",
    ylab = "Mahalanobis Distance",
    pch = 19,
    col = "blue"
  )
  abline(0, 1, col = "red", lwd = 2)
  
  # Cutoff
  cutoff <- qchisq(1 - alpha, df = ncol(data_md))
  
  # Outlier summary
  outliers <- mahalanobis_distances > cutoff
  
  return(list(
    data = data_md,
    mahalanobis_distances = mahalanobis_distances,
    cutoff = cutoff,
    outliers = outliers,
    outlier_indices = which(outliers),
    summary = table(Outlier = outliers)
  ))
}


#' Impute Missing Values Using Group Means
#'
#' Replaces missing values in one or more numeric variables with the
#' mean of the observed values within each level of a grouping variable.
#'
#' @param data A data frame containing the variables to be imputed.
#' @param new_df New dataframe after imputation.
#' @param group_var A character string specifying the grouping variable, or a
#'   character vector of grouping variables.
#' @param vars A character vector of the numeric variable(s) to impute.
#'
#' @return A data frame with missing values in the specified variables replaced
#'   by the group-specific mean.
#'
#' @details
#' Missing values are imputed separately within each group defined by
#' `group_var`. The mean is calculated using all non-missing values
#' (`na.rm = TRUE`).

impute_by_group <- function(data, new_df,group_var, vars) {
  
  library(dplyr)
  
  data<-data %>%
    group_by(across(all_of(group_var))) %>%
    mutate(across(
      all_of(vars),
      ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)
    )) %>%
    ungroup()
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### DATA VISUALIZATION FUNCTIONS ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#' creates a pdf with histograms, qqplots and boxplots to assess data distributions
#' 
#' @param data dataframe
#' @param plot_outcomes character vector with outcome variables to be plotted
#' @param group_var grouping variable 
#' @param pdf_file_name name of pdf file to be saved
#' 
#' @return writes pdf file containing all plots

data_visualization_pdf<-function(data,plot_outcomes,group_var,pdf_file_name){
  library(ggplot2)
  library(ggpubr)
  
  pdf(pdf_file_name)
  
  for (var in plot_outcomes){
    b<-ggboxplot(data,x=group_var,y=var,
                 color=group_var, palette="jco")+
      ggtitle(paste("boxplot",var))
    
    q<-ggqqplot(data,var,ggtheme=theme_bw())+
      facet_grid(as.formula(paste("~",group_var)))+
      ggtitle(paste("qqplot",var))
    
    h<-ggplot(data,aes(x=.data[[var]],fill=.data[[group_var]]))+
      geom_histogram(aes(y=after_stat(density)), alpha=.6, position = "identity",bins=30) +  
      geom_density(alpha = 0.3, size = 1) +
      labs(fill=group_var)+
      ggtitle(paste("histogram",var))
    
    print(b)
    print(q)
    print(h)
  }
  dev.off()
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### MODEL DIAGNOSTICS FUNCTIONS ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

##### REGRESSION DIAGNOSTICS #####
#' outputs standard diagnostics for linear regression models
#' 
#' @param data dataframe containing all variables
#' @param outcome_vars character vector of outcome variable names
#' @param group_var name of grouping predictor variable (string)
#' @param covariates string of covariate terms (e.g., "age+sex")
#' 
#' @return writes:
#' diagnostic text files per outcome, diagnostic plots (png)

#' @details
#' Uses:
#' lm() for model fitting
#' Durbin-Watson test for autocorrelation
#' Breusch-Pagan test for heteroscedasticity
#' VIF for multicollinearity
#' augment() for residual diagnostics

regression_diagnostics<-function(data,outcome_vars,group_var,covariates){
  
  library(broom)
  library(lmtest)
  library(rstatix)
  library(car)

  for (outcome in outcome_vars){
    model_formula=paste0(outcome,"~",group_var,"+",covariates)
    model<-lm(as.formula(model_formula), data=data)
    
    #write text file with model diagnostics 
    txt<-capture.output({
      cat("OUTCOME:",outcome,"\n\n")
      cat("\n model summary \n") # model summary
      print(summary(model)) 
      
      cat("\n durbin watson\n") 
      print(dwtest(model))
      
      cat("\n bp test\n")
      print(bptest(model))
      
      cat("\n VIF\n")
      print(vif(model))
      
      cat("\n extreme outliers\n")
      print(augment(model)%>%
              filter(abs(.std.resid)>3))
      
      cat("\n levene test\n") 
      print(augment(model)%>%
              levene_test(as.formula(paste(".resid~",group_var))))
    }
    )
    
    writeLines(txt,con=paste0("diagnostics_",outcome,".txt"))
    
    # save diagnostic plots
    for (i in 1:6){
      png(filename=paste0("diag_",outcome,"_",i,".png"))
      plot(model,which=i)
      dev.off()
    }
    # save histogram of residuals
    png(filename=paste0("hist_resid",outcome,".png"))
    hist(residuals(model), breaks = 30)
    dev.off()
  }
  
}

##### MIXED-EFFECTS MODEL DIAGNOSTICS ####
#' creates diagnostic plots for mixed-effects models and saves onto  pdf file
#' 
#' @param data Data frame
#' @param outcome_vars Character vector of outcomes
#' @param group_var Grouping variable (fixed effect)
#' @param covariates String of covariate terms
#' @param id_var Subject ID variable for random intercept
#' 
#' @return PDF file containing diagnostic plots 
#' 
#' @details for each outcome, writes:
#' residual vs fitted plot
#' residual qq plot
#' random effects qq plot
#' resituals histogram 

mem_diagnostics<-function(data, outcome_vars, group_var, covariates, id_var){
  
  pdf("mixed_model_diagnostics.pdf")
  
  for (outcome in outcome_vars){
    model_formula<-paste0(outcome,"~Time*",group_var,"+",covariates,"+(1|",id_var,")")
    model<-lmerTest::lmer(as.formula(model_formula),data=data,REML=TRUE)
    
    #residual vs fitted plot
    plot(fitted(model),residuals(model),
         main=paste(outcome,"\n residuals vs fitted"),
         xlab="fitted",ylab="residuals")
    abline(h=0)
    
    #residual qq
    qqnorm(residuals(model),
           main=paste(outcome,"\n residual qq plot"))
    qqline(residuals(model))
    
    #random intercept qq
    re<-ranef(model)[[1]][,1]
    qqnorm(re,main=paste(outcome,"\n random intercept qq plot"))
    
    #residual histogram
    hist(residuals(model),breaks=30,
         main=paste(outcome,"\n residual histogram"))
  }
  dev.off()
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### ANALYTICAL MODELS FUNCTIONS ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

##### REGRESSION MODELS (CROSS-SECTIONAL ANALYSIS) ####

###### UNSTANDARDIZED REGRESSION MODELS ####
#' fit unstandardized linear regression models 
#' 
#' @param data data frame
#' @param outcome_vars character vector of outcomes
#' @param group_var group predictor variable
#' @param covariates string of covariate formula terms
#' 
#' @return List containing:
#' coef: tidy regression coefficients with CIs
#' model R2 and adjusted R2
#' estimated marginal means by group

fit_unstd_model<-function(data,outcome_vars,group_var,covariates){
  results_unstd_list<-list()
  r2_list<-list()
  emm_list<-list()
  
  for (outcome in outcome_vars){
    model_formula=paste0(outcome,"~",group_var,"+",covariates)
    model<-lm(as.formula(model_formula), data=data) 
    
    results_unstd_list[[outcome]]<-broom::tidy(model,conf.int=TRUE)%>%
      mutate(Outcome=outcome)
    
    r2_list[[outcome]]<-broom::glance(model)%>%
      select(r.squared,adj.r.squared)%>%
      mutate(Outcome=outcome)
    
    emm_list[[outcome]]<-as.data.frame(emmeans(model,specs=group_var))%>%
      mutate(Outcome=outcome)
  }
  
  list(
    coef=bind_rows(results_unstd_list),
    r2=bind_rows(r2_list),
    emm=bind_rows(emm_list)
  )
}

###### STANDARDIZED REGRESSION MODELS ####
#' Fits regression models using standardized variables
#' 
#' @param data data frame
#' @param outcome_vars character vector of outcomes
#' @param group_var group predictor variable
#' @param covariates string of covariate formula terms
#' 
#' @return standardized beta coefficients and confidence intervals only

fit_std_model<-function(data,outcome_vars,group_var,covariates){
  results_std_list<-list()
  
  for (outcome in outcome_vars){
    model_formula=paste0(outcome,"~",group_var,"+",covariates)
    model<-lm(as.formula(model_formula), data=data)
    
    results_std_list[[outcome]]<-broom::tidy(model,conf.int=TRUE)%>%
      mutate(Outcome=sub("^z", "", outcome),
             term=sub("^z", "", term))%>%
      select(Outcome,term,estimate,conf.low,conf.high)
  }
  
  all_std_results<-bind_rows(results_std_list)%>%
    rename(
      std_beta=estimate,
      std_low=conf.low,
      std_high=conf.high
    )
}


##### LONGITUDINAL MIXED EFFECTS MODELS #####

###### UNSTANDARDIZED MIXED EFFECTS MODELS ######
#' Fit unstandardized linear mixed-effects models (longitudinal)
#'
#' Fits separate random-intercept mixed models for each outcome variable.
#' Fixed effects include:
#' - Time
#' - Group variable
#' - Time × Group interaction
#' - Covariates
#'
#' Random effects:
#' - Random intercept for subject ID
#'
#' @param data Data frame containing all variables used in the model
#' @param outcome_vars Character vector of outcome variable names
#' @param group_var Name of grouping variable (string)
#' @param covariates String of covariate terms (e.g., "age + sex + bmi")
#' @param id_var Name of subject identifier variable for random intercept
#'
#' @return A list with:
#' coef: tibble of fixed-effect estimates with confidence intervals
#' emm: estimated marginal means for Time × Group combinations
#'
#' @details
#' Uses lmerTest::lmer for estimation (REML = TRUE).
#' Estimated marginal means are computed via emmeans and returned per outcome

fit_unstd_long_model<-function(data, outcome_vars, group_var, covariates, id_var){
  
  emm_options(pbkrtest.limit=15000)
  results_unstd_list<-list()
  emm_list<-list()
  
  for (outcome in outcome_vars){
    model_formula<-paste0(outcome,"~Time*",group_var,"+",covariates,"+(1|",id_var,")")
    model<-lmerTest::lmer(as.formula(model_formula),data=data,REML=TRUE)
    
    results_unstd_list[[outcome]]<-broom.mixed::tidy(model, conf.int=TRUE, effects="fixed")%>%
      mutate(Outcome=outcome)
    
    emm_list[[outcome]]<-as.data.frame(emmeans(model,specs=as.formula(paste0("~ Time*",group_var))))%>%
      mutate(Outcome=outcome)
  }
  
  list(
    coef=bind_rows(results_unstd_list),
    emm=bind_rows(emm_list)
  )
}


###### STANDARDIZED MIXED EFFECTS MODELS ####
#' Fit standardized longitudinal mixed-effects models
#'
#' @param data Data frame containing standardized outcomes
#' @param outcome_vars Character vector of standardized outcome variable names
#' @param group_var Name of grouping variable (string)
#' @param covariates String of covariate terms
#' @param id_var Subject identifier variable for random intercept
#'
#' @return tibble containing:
#' Outcome: outcome name (cleaned of "z" prefix)
#' term: fixed-effect term (cleaned of "z" prefix)
#' std_beta: standardized coefficient
#' std_low: lower CI bound
#' std_high: upper CI bound
#'
#' @details
#' Standardization is assumed to have been applied prior to modeling

fit_std_long_model<-function(data, outcome_vars, group_var, covariates, id_var){
  
  results_std_list<-list()
  
  for (outcome in outcome_vars){
    model_formula<-paste0(outcome,"~Time*",group_var,"+",covariates,"+(1|",id_var,")")
    model<-lmerTest::lmer(as.formula(model_formula),data=data,REML=TRUE)
    
    results_std_list[[outcome]]<-broom.mixed::tidy(model, conf.int=TRUE, effects="fixed")%>%
      mutate(Outcome=sub("^z", "", outcome),
             term=sub("^z", "", term))%>%
      select(Outcome,term,estimate,conf.low,conf.high)
  }
  
  all_std_results<-bind_rows(results_std_list)%>%
    rename(
      std_beta=estimate,
      std_low=conf.low,
      std_high=conf.high
    )
}

#### GENERAL FUNCTION TO REFORMAT MODEL OUTUTS ####
#' Format model output for reporting tables
#'
#' Converts raw model outputs into formatted tables
#' with effect sizes, confidence intervals, and adjusted p-values
#'
#' @param results_df data frame containing model output. Must include:
#'   estimate, conf.low, conf.high, std_beta, std_low, std_high,
#'   std.error, statistic, p.value, term, Outcome
#'
#' @return tibble with formatted reporting columns:
#'   Outcome, Term, Estimate, Estimate_std, SE, t, p_adj, p
#'
#' @details
#' - applies benjamini-hochberg fdr correction to p-values
#' - formats coefficients as: "b [lower, upper]"
#' - removes leading zero formatting

format_results<-function(results_df){
  results_df%>%
    mutate(
      p_BH=p.adjust(p.value, method="BH"), #benjamini hochberg FDR correction
      Estimate=paste0(sub("^(-?)0\\.","\\1.",sprintf("%.2f",estimate)),
                      " [",sub("^(-?)0\\.", "\\1.", sprintf("%.2f",conf.low)),", ",
                      sub("^(-?)0\\.", "\\1.", sprintf("%.2f", conf.high)),"]"),
      Estimate_std=paste0(sub("^(-?)0\\.", "\\1.", sprintf("%.2f", std_beta)),
                          " [",sub("^(-?)0\\.", "\\1.", sprintf("%.2f", std_low)),", ",
                          sub("^(-?)0\\.", "\\1.", sprintf("%.2f", std_high)),"]"),
      SE=sub("^(-?)0\\.", "\\1.",sprintf("%.2f",std.error)),
      t=sub("^(-?)0\\.", "\\1.",sprintf("%.2f",statistic)),
      p=sprintf("%.3f",p.value),
      p_adj=case_when(
        p_BH< .001~ "< .001",
        TRUE~sub("^(-?)0\\.", "\\1.", sprintf("%.3f", p_BH))
      ),
      Term=term
    )%>%
    select(Outcome,Term,Estimate,Estimate_std,SE, t, p_adj, p)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### WORD DOCUMENT CREATION FOR MODEL OUTPUTS ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

##### REGRESSION MODEL OUTPUTS ####
#' @param results_df dataframe of formatted regression coefficients from format_results() function
#' @param r2_df dataframe containing r.squared and adj.r.squared per outcome
#' 
#' @return word document 

create_results_doc<-function(results_df, r2_df){
  doc<-read_docx()
  unique_outcomes<-unique(results_df$Outcome)
  
  for (outcome in unique_outcomes){
    sub_table<-results_df %>%
      filter(Outcome== outcome)%>%
      select(-Outcome)
    
    r2_vals<-r2_df %>%
      filter(Outcome==outcome)
    r2_text<-paste0("R2 = ", sprintf("%.3f", r2_vals$r.squared),
                    " | Adjusted R2 = ",sprintf("%.3f", r2_vals$adj.r.squared))
    
    ft<-flextable(sub_table)%>%
      bold(part="header")%>%
      autofit()%>%
      theme_booktabs()
    
    doc<-doc %>%
      body_add_par(paste0("Outcome: ",outcome),style="heading 2")%>%
      body_add_par(r2_text,style="Normal")%>%
      body_add_flextable(ft)%>%
      body_add_par("", style="Normal")
  }
  doc
}

##### MIXED EFFECTS MODEL OUTPUTS ####
#' create word document for mixed effects model outputs
#' @param results_df dataframe for formatted mixed effects model outputs from format_results()
#' 
#' @return word document

create_results_long_doc<-function(results_df){
  doc<-read_docx()
  unique_outcomes<-unique(results_df$Outcome)
  
  for (outcome in unique_outcomes){
    sub_table<-results_df %>%
      filter(Outcome== outcome)%>%
      select(-Outcome)
    
    ft<-flextable(sub_table)%>%
      bold(part="header")%>%
      autofit()%>%
      theme_booktabs()
    
    doc<-doc %>%
      body_add_par(paste0("Outcome: ",outcome),style="heading 2")%>%
      body_add_flextable(ft)%>%
      body_add_par("", style="Normal")
  }
  doc
}
