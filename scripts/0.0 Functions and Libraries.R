# Loading Useful Packages

library(readxl)
library(metafor)
library(ggplot2)
library(tidyverse)
library(esc)
library(brms)

# Functions ---------------------------------------------------------------

# This will fit a meta-analytic model combining all disorders, for each disorder, and
# only for anxiety+depression
fit_all_models = function(dat, basic_priors, cond_priors, yi, vi)
{
  temp = dat
  temp$yi = unlist(temp[yi])
  temp$vi = unlist(temp[vi])
  temp = temp %>% filter(!is.na(yi))
  temp$es_id = 1:nrow(temp)
  
  temp = temp %>%
    select(Study, StudyID, es_id, Year, categorization, NControl, NClinical, yi, vi)
  
  # Fit the overall model
  overall_m = brm(yi|se(vi**.5) ~ 1 + (1|StudyID) + (1|es_id),
                    data=temp, cores=4, sample_prior = 'yes', 
                    save_pars=save_pars(group = TRUE, all = TRUE), 
                    control = list(adapt_delta=.9999, max_treedepth = 20),
                    prior = basic_priors,
                    iter = 10000)
  
  # Fit the Anxiety + Depression model
  anxdep_m = brm(yi|se(vi**.5) ~ 1 + (1|StudyID) + (1|es_id),
                  data=temp %>% filter(categorization %in% c('Anxiety', 'Depression')), cores=4, sample_prior = 'yes', 
                  save_pars=save_pars(group = TRUE, all = TRUE), 
                  control = list(adapt_delta=.9999, max_treedepth = 20),
                  prior = basic_priors,
                  iter = 10000)
  
  # Fit the categorization model
  cat_m = brm(yi|se(vi**.5) ~ categorization - 1 + (1|StudyID) + (1|es_id),
                 data=temp, cores=4, sample_prior = 'yes', 
                 save_pars=save_pars(group = TRUE, all = TRUE), 
                 control = list(adapt_delta=.9999, max_treedepth = 20),
                 prior = cond_priors,
                 iter = 10000)
  
  return(list(data=temp, overall=overall_m, control=anxdep_m, category=cat_m))
}

# Checks for publication bias using a specified moderator
check_publication_bias = function(dat, basic_mod_priors_tight, yi, vi, mod, sqrt_mod=FALSE)
{
  temp = dat
  temp$yi = unlist(temp[yi])
  temp$vi = unlist(temp[vi])
  temp$mod = unlist(temp[mod])
  
  if(sqrt_mod)
    temp$mod = temp$mod**.5
  
  temp$mod = scale(temp$mod)[,1]
  
  temp = temp %>% filter(!is.na(yi))
  temp$es_id = 1:nrow(temp)
  
  temp = temp %>%
    select(Study, StudyID, es_id, Year, categorization, NControl, NClinical, yi, vi, mod)
  
  # Fit the overall model
  overall_m = brm(yi|se(vi**.5) ~ mod + (1|StudyID) + (1|es_id),
                  data=temp, cores=4, sample_prior = 'yes', 
                  save_pars=save_pars(group = TRUE, all = TRUE), 
                  control = list(adapt_delta=.9999, max_treedepth = 20),
                  prior = basic_mod_priors_tight,
                  iter = 10000)
  
  # Fit the Anxiety + Depression model
  anxdep_m = brm(yi|se(vi**.5) ~ mod + (1|StudyID) + (1|es_id),
                 data=temp %>% filter(categorization %in% c('Anxiety', 'Depression')), cores=4, sample_prior = 'yes', 
                 save_pars=save_pars(group = TRUE, all = TRUE), 
                 control = list(adapt_delta=.9999, max_treedepth = 20),
                 prior = basic_mod_priors_tight,
                 iter = 10000)
  
  return(list(data=temp, overall=overall_m, control=anxdep_m))
}

# Used for plotting
prepare_plot_data = function(mods, label = 'None', cats=c('Addiction', 'Anxiety', 'Depression', 'Mixed', 'Psychosis'))
{
  # Get the overall estimate and prediction interval
  overall_ci = 100*posterior_summary(posterior_epred(mods$overall, newdata=data.frame(vi=0), re_formula=NA))
  overall_pi = 100*posterior_summary(posterior_epred(mods$overall, newdata=data.frame(vi=0, es_id=9999), re_formula=~(1|es_id), allow_new_levels=TRUE))
  
  # Get the anxiety + depression estimate and prediction interval
  anxdep_ci = 100*posterior_summary(posterior_epred(mods$control, newdata=data.frame(vi=0), re_formula=NA))
  anxdep_pi = 100*posterior_summary(posterior_epred(mods$control, newdata=data.frame(vi=0, es_id=9999), re_formula=~(1|es_id), allow_new_levels=TRUE))
  
  # Get the anxiety + depression estimate and prediction interval
  cat_ci = 100*posterior_summary(posterior_epred(mods$category, newdata=data.frame(vi=0, categorization=cats), re_formula=NA))
  cat_pi = 100*posterior_summary(posterior_epred(mods$category, newdata=data.frame(vi=0, es_id=9999, categorization=cats), re_formula=~(1|es_id), allow_new_levels=TRUE))
  
  # Get the study estimates
  study_post = data.frame(100*posterior_summary(posterior_epred(mods$category,
                                                   newdata=data.frame(vi=0, es_id=mods$data$es_id, StudyID = mods$data$StudyID, categorization=mods$data$categorization), 
                                                   allow_new_levels=TRUE)))
  
  # Combine study estimates with the actual data
  temp = mods$data %>%
    bind_cols(study_post) %>%
    rename(c_m = Estimate, c_se=Est.Error, c_lb = Q2.5, c_ub = Q97.5)
  
  # Set up facets
  temp$facet_labels = temp$categorization
  #temp[is.na(temp[dv_name]),c('c_m', 'c_lb', 'c_ub')] = NA
  temp[is.na(temp['yi']),c('c_m', 'c_lb', 'c_ub')] = NA
  temp$obs = temp[['yi']]
  
  summary_labels = c(cats, 'Anxiety+Depression', 'Overall')
  n_sum_labels = length(summary_labels)
  
  # Create dummy summary rows
  n_rows = nrow(temp)
  temp %>% 
    bind_rows(temp[rep(1, n_sum_labels),]) -> temp
  temp[n_rows + 1:n_sum_labels,]$NControl = mean(temp$NControl) # Ensures point is typical size
  temp[n_rows + 1:n_sum_labels,]$NClinical = mean(temp$NClinical) # Ensures point is typical size
  temp[n_rows + 1:n_sum_labels,]$facet_labels = 'Summary'
  temp[n_rows + 1:n_sum_labels,]$obs = NA
  temp$plot_labels = temp$Study
  temp[n_rows + 1:n_sum_labels, 'plot_labels'] = summary_labels
  
  # Add in summary for each cluster
  temp[n_rows + 1:length(cats), c('c_m', 'c_lb', 'c_ub')] = cat_ci[,-2]
  temp[n_rows + 1:length(cats), c('pi_m', 'pi_lb', 'pi_ub')] = cat_pi[,-2]

  # Add in summary for Anxiety + Depression
  temp[n_rows + (length(cats)+1), c('c_m', 'c_lb', 'c_ub')] = t(data.frame(anxdep_ci[-2]))
  temp[n_rows + (length(cats)+1), c('pi_m', 'pi_lb', 'pi_ub')] = t(data.frame(anxdep_pi[-2]))
  
  # Add in summary for Overall
  temp[n_rows + (length(cats)+2), c('c_m', 'c_lb', 'c_ub')] = t(data.frame(overall_ci[-2]))
  temp[n_rows + (length(cats)+2), c('pi_m', 'pi_lb', 'pi_ub')] = t(data.frame(overall_pi[-2]))
  
  temp$points = temp$facet_labels
  temp[n_rows + 1:n_sum_labels,]$points = summary_labels

  temp$label=label
  
  return(temp)
}

