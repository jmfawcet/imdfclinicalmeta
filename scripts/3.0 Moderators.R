
# Moderator priors --------------------------------------------------------

basic_mod_priors_tight = c(prior(normal(0,.15), class='Intercept'), 
                           prior(normal(0,.10), class='sd'), 
                           prior(normal(0,.3), class='b'))


# Recall First ------------------------------------------------------------

# Too few studies given heterogeneity
temp = dat %>%
  filter(!is.na(neg_comp_yi), categorization %in% c('Anxiety', 'Depression'), Task=='recognition')
temp$es_id = 1:nrow(temp)
table(temp$RecallFirst)

temp = dat %>%
  filter(!is.na(neu_comp_yi), categorization %in% c('Anxiety', 'Depression'), Task=='recognition')
temp$es_id = 1:nrow(temp)
table(temp$RecallFirst)

# Task --------------------------------------------------------------------

# Found Nothing

temp = dat %>%
  filter(!is.na(neg_comp_yi), categorization %in% c('Anxiety', 'Depression'))
temp$es_id = 1:nrow(temp)

# Fit the overall model
task_neg_m1 = brm(neg_comp_yi|se(neg_comp_vi**.5) ~ Task + (1|StudyID) + (1|es_id),
                data=temp, cores=4, sample_prior = 'yes', 
                save_pars=save_pars(group = TRUE, all = TRUE), 
                control = list(adapt_delta=.9999, max_treedepth = 20),
                prior = basic_mod_priors_tight,
                iter = 10000)
hypothesis(task_neg_m1, 'Taskrecognition > 0')

temp = dat %>%
  filter(!is.na(neu_comp_yi), categorization %in% c('Anxiety', 'Depression'))
temp$es_id = 1:nrow(temp)

# Fit the overall model
task_neu_m1 = brm(neu_comp_yi|se(neu_comp_vi**.5) ~ Task + (1|StudyID) + (1|es_id),
                  data=temp, cores=4, sample_prior = 'yes', 
                  save_pars=save_pars(group = TRUE, all = TRUE), 
                  control = list(adapt_delta=.9999, max_treedepth = 20),
                  prior = basic_mod_priors_tight,
                  iter = 10000)
hypothesis(task_neu_m1, 'Taskrecognition > 0')


# Delay -------------------------------------------------------------------

# Nothing

temp = dat %>%
  filter(!is.na(neg_comp_yi), categorization %in% c('Anxiety', 'Depression')) %>%
  mutate(was_delay = as.numeric(StudyTestDelayCat!='none'))
temp$es_id = 1:nrow(temp)

# Fit the overall model
delay_neg_m1 = brm(neg_comp_yi|se(neg_comp_vi**.5) ~ was_delay + (1|StudyID) + (1|es_id),
                  data=temp, cores=4, sample_prior = 'yes', 
                  save_pars=save_pars(group = TRUE, all = TRUE), 
                  control = list(adapt_delta=.9999, max_treedepth = 20),
                  prior = basic_mod_priors_tight,
                  iter = 10000)
hypothesis(delay_neg_m1, 'was_delay > 0')

temp = dat %>%
  filter(!is.na(neu_comp_yi), categorization %in% c('Anxiety', 'Depression')) %>%
  mutate(was_delay = as.numeric(StudyTestDelayCat!='none'))
temp$es_id = 1:nrow(temp)

# Fit the overall model
delay_neu_m1 = brm(neu_comp_yi|se(neu_comp_vi**.5) ~ was_delay + (1|StudyID) + (1|es_id),
                  data=temp, cores=4, sample_prior = 'yes', 
                  save_pars=save_pars(group = TRUE, all = TRUE), 
                  control = list(adapt_delta=.9999, max_treedepth = 20),
                  prior = basic_mod_priors_tight,
                  iter = 10000)
hypothesis(delay_neu_m1, 'was_delay > 0')


# Population --------------------------------------------------------------

# Smaller Clinical - Control difference in either, but only credible for 
# neutral, possibly owing to the fact that DF is already depressed for negative
# items.

temp = dat %>%
  filter(!is.na(neg_comp_yi), categorization %in% c('Anxiety', 'Depression'))
temp$es_id = 1:nrow(temp)

# Fit the overall model
pop_neg_m1 = brm(neg_comp_yi|se(neg_comp_vi**.5) ~ population + (1|StudyID) + (1|es_id),
                   data=temp, cores=4, sample_prior = 'yes', 
                   save_pars=save_pars(group = TRUE, all = TRUE), 
                   control = list(adapt_delta=.9999, max_treedepth = 20),
                   prior = basic_mod_priors_tight,
                   iter = 10000)
hypothesis(pop_neg_m1, '100*populationsubclinical > 0')
hypothesis(pop_neg_m1, 'Intercept*100 = 0')
hypothesis(pop_neg_m1, '100*(Intercept + populationsubclinical) = 0')

temp = dat %>%
  filter(!is.na(neu_comp_yi), categorization %in% c('Anxiety', 'Depression'))
temp$es_id = 1:nrow(temp)

# Fit the overall model
pop_neu_m1 = brm(neu_comp_yi|se(neu_comp_vi**.5) ~ population + (1|StudyID) + (1|es_id),
                 data=temp, cores=4, sample_prior = 'yes', 
                 save_pars=save_pars(group = TRUE, all = TRUE), 
                 control = list(adapt_delta=.9999, max_treedepth = 20),
                 prior = basic_mod_priors_tight,
                 iter = 10000)
hypothesis(pop_neu_m1, '100*populationsubclinical = 0')
hypothesis(pop_neu_m1, 'Intercept*100 = 0')
hypothesis(pop_neu_m1, '100*(Intercept + populationsubclinical) = 0')


# Sample Type -------------------------------------------------------------
temp = dat %>%
  filter(!is.na(neg_comp_yi), categorization %in% c('Anxiety', 'Depression'), SampleType != 'NA') %>%
  mutate(is_patient = as.numeric(!(SampleType %in% c('Community', 'Students'))))
temp$es_id = 1:nrow(temp)

patient_neg_m1 = brm(neg_comp_yi|se(neg_comp_vi**.5) ~ is_patient + (1|StudyID) + (1|es_id),
                     data=temp, cores=4, sample_prior = 'yes', 
                     save_pars=save_pars(group = TRUE, all = TRUE), 
                     control = list(adapt_delta=.9999, max_treedepth = 20),
                     prior = basic_mod_priors_tight,
                     iter = 10000)
hypothesis(patient_neg_m1, '100*Intercept = 0')
hypothesis(patient_neg_m1, '100*(Intercept+is_patient) = 0')
hypothesis(patient_neg_m1, '100*is_patient = 0')


temp = dat %>%
  filter(!is.na(neu_comp_yi), categorization %in% c('Anxiety', 'Depression'), SampleType != 'NA') %>%
  mutate(is_patient = as.numeric(!(SampleType %in% c('Community', 'Students'))))
temp$es_id = 1:nrow(temp)

patient_neu_m1 = brm(neu_comp_yi|se(neu_comp_vi**.5) ~ is_patient + (1|StudyID) + (1|es_id),
                     data=temp, cores=4, sample_prior = 'yes', 
                     save_pars=save_pars(group = TRUE, all = TRUE), 
                     control = list(adapt_delta=.9999, max_treedepth = 20),
                     prior = basic_mod_priors_tight,
                     iter = 10000)
hypothesis(patient_neu_m1, '100*Intercept = 0')
hypothesis(patient_neu_m1, '100*(Intercept+is_patient) = 0')
hypothesis(patient_neu_m1, '100*is_patient < 0')

# Age ---------------------------------------------------------------------

# Trend in all cases for the difference to be larger in older populations (particularly
# when using the clinical mean age rather than average mean age). This could be due
# to differences in the sample (e.g., comparing university populations to general
# samples)

temp = dat %>%
  filter(!is.na(neg_comp_yi), categorization %in% c('Anxiety', 'Depression')) %>%
  mutate(AgeAverage=ifelse(!is.na(AgeAverage), AgeAverage, (AgeClin*NClinical+AgeCon*NControl)/(NClinical+NControl)), 
         scaled_age = scale(AgeAverage)[,1], 
         diff_age = scale(AgeClin - AgeCon)[,1],
         imputed_age_clin = scale(ifelse(!is.na(AgeClin), AgeClin, AgeAverage))[,1])
temp$es_id = 1:nrow(temp)

# Fit the overall model
age_neg_m1 = brm(neg_comp_yi|se(neg_comp_vi**.5) ~ scaled_age + (1|StudyID) + (1|es_id),
                 data=temp, cores=4, sample_prior = 'yes', 
                 save_pars=save_pars(group = TRUE, all = TRUE), 
                 control = list(adapt_delta=.9999, max_treedepth = 20),
                 prior = basic_mod_priors_tight,
                 iter = 10000)
hypothesis(age_neg_m1, 'scaled_age > 0')

age_neg_m2 = brm(neg_comp_yi|se(neg_comp_vi**.5) ~ imputed_age_clin + (1|StudyID) + (1|es_id),
                 data=temp, cores=4, sample_prior = 'yes', 
                 save_pars=save_pars(group = TRUE, all = TRUE), 
                 control = list(adapt_delta=.9999, max_treedepth = 20),
                 prior = basic_mod_priors_tight,
                 iter = 10000)
hypothesis(age_neg_m2, '100*imputed_age_clin = 0')

age_neg_m3 = brm(neg_comp_yi|se(neg_comp_vi**.5) ~ diff_age + (1|StudyID) + (1|es_id),
                 data=temp, cores=4, sample_prior = 'yes', 
                 save_pars=save_pars(group = TRUE, all = TRUE), 
                 control = list(adapt_delta=.9999, max_treedepth = 20),
                 prior = basic_mod_priors_tight,
                 iter = 10000)
hypothesis(age_neg_m3, 'diff_age > 0')

#
temp = dat %>%
  filter(!is.na(neu_comp_yi), categorization %in% c('Anxiety', 'Depression')) %>%
  mutate(AgeAverage=ifelse(!is.na(AgeAverage), AgeAverage, (AgeClin*NClinical+AgeCon*NControl)/(NClinical+NControl)), 
         scaled_age = scale(AgeAverage)[,1], 
         diff_age = scale(AgeClin - AgeCon)[,1],
         imputed_age_clin = scale(ifelse(!is.na(AgeClin), AgeClin, AgeAverage))[,1])
temp$es_id = 1:nrow(temp)

# Fit the overall model
age_neu_m1 = brm(neu_comp_yi|se(neu_comp_vi**.5) ~ scaled_age + (1|StudyID) + (1|es_id),
                 data=temp, cores=4, sample_prior = 'yes', 
                 save_pars=save_pars(group = TRUE, all = TRUE), 
                 control = list(adapt_delta=.9999, max_treedepth = 20),
                 prior = basic_mod_priors_tight,
                 iter = 10000)
hypothesis(age_neu_m1, 'scaled_age > 0')

age_neu_m2 = brm(neu_comp_yi|se(neu_comp_vi**.5) ~ imputed_age_clin + (1|StudyID) + (1|es_id),
                 data=temp, cores=4, sample_prior = 'yes', 
                 save_pars=save_pars(group = TRUE, all = TRUE), 
                 control = list(adapt_delta=.9999, max_treedepth = 20),
                 prior = basic_mod_priors_tight,
                 iter = 10000)
hypothesis(age_neu_m2, '100*imputed_age_clin > 0')

age_neu_m3 = brm(neu_comp_yi|se(neu_comp_vi**.5) ~ diff_age + (1|StudyID) + (1|es_id),
                 data=temp, cores=4, sample_prior = 'yes', 
                 save_pars=save_pars(group = TRUE, all = TRUE), 
                 control = list(adapt_delta=.9999, max_treedepth = 20),
                 prior = basic_mod_priors_tight,
                 iter = 10000)
hypothesis(age_neu_m3, 'diff_age > 0')

# Medicated ---------------------------------------------------------------

# Insufficient variation and too few studies
# temp = dat %>%
#   filter(!is.na(neg_comp_yi), categorization %in% c('Anxiety', 'Depression')) %>%
#   mutate(meds = as.numeric(meds)) %>%
#   filter(!is.na(meds))
# temp$es_id = 1:nrow(temp)

# Insufficient variation and too few studies
# temp = dat %>%
#   filter(!is.na(neu_comp_yi), categorization %in% c('Anxiety', 'Depression')) %>%
#   mutate(meds = as.numeric(meds)) %>%
#   filter(!is.na(meds))
# temp$es_id = 1:nrow(temp)


# Treated -----------------------------------------------------------------


# No effects

temp = dat %>%
  filter(!is.na(neg_comp_yi), categorization %in% c('Anxiety', 'Depression')) %>%
  mutate(therapy = as.numeric(therapy), scale_therapy = scale(therapy)[,1]) %>%
  filter(!is.na(therapy))
temp$es_id = 1:nrow(temp)

# Fit the overall model
therapy_neg_m1 = brm(neg_comp_yi|se(neg_comp_vi**.5) ~ scale_therapy + (1|StudyID) + (1|es_id),
                 data=temp, cores=4, sample_prior = 'yes', 
                 save_pars=save_pars(group = TRUE, all = TRUE), 
                 control = list(adapt_delta=.9999, max_treedepth = 20),
                 prior = basic_mod_priors_tight,
                 iter = 10000)
hypothesis(therapy_neg_m1, '100*scale_therapy = 0')

# Insufficient variation and too few studies
temp = dat %>%
  filter(!is.na(neu_comp_yi), categorization %in% c('Anxiety', 'Depression')) %>%
  mutate(therapy = as.numeric(therapy), scale_therapy = scale(therapy)[,1]) %>%
  filter(!is.na(therapy))
temp$es_id = 1:nrow(temp)

therapy_neu_m1 = brm(neu_comp_yi|se(neu_comp_vi**.5) ~ scale_therapy + (1|StudyID) + (1|es_id),
                     data=temp, cores=4, sample_prior = 'yes', 
                     save_pars=save_pars(group = TRUE, all = TRUE), 
                     control = list(adapt_delta=.9999, max_treedepth = 20),
                     prior = basic_mod_priors_tight,
                     iter = 10000)
hypothesis(therapy_neu_m1, '100*scale_therapy = 0')

# Sample Type*

# temp = dat %>%
#   filter(!is.na(neg_comp_yi), categorization %in% c('Anxiety', 'Depression'))
# temp$es_id = 1:nrow(temp)


# BDI ---------------------------------------------------------------------

# nothing

temp = dat %>%
  filter(!is.na(neg_comp_yi), categorization %in% c('Anxiety', 'Depression')) %>%
  mutate(MeanClinBDI = as.numeric(MeanClinBDI), MeanConBDI = as.numeric(MeanConBDI), 
         scale_clin_bdi = scale(MeanClinBDI)[,1],
         scale_diff_bdi = scale(MeanClinBDI-MeanConBDI)[,1]) %>%
  filter(!is.na(MeanClinBDI))
temp$es_id = 1:nrow(temp)

# Fit the overall model
bdi_neg_m1 = brm(neg_comp_yi|se(neg_comp_vi**.5) ~ scale_clin_bdi + (1|StudyID) + (1|es_id),
                     data=temp, cores=4, sample_prior = 'yes', 
                     save_pars=save_pars(group = TRUE, all = TRUE), 
                     control = list(adapt_delta=.9999, max_treedepth = 20),
                     prior = basic_mod_priors_tight,
                     iter = 10000)
hypothesis(bdi_neg_m1, '100*scale_clin_bdi = 0')

bdi_neg_m2 = brm(neg_comp_yi|se(neg_comp_vi**.5) ~ scale_diff_bdi + (1|StudyID) + (1|es_id),
                 data=temp, cores=4, sample_prior = 'yes', 
                 save_pars=save_pars(group = TRUE, all = TRUE), 
                 control = list(adapt_delta=.9999, max_treedepth = 20),
                 prior = basic_mod_priors_tight,
                 iter = 10000)
hypothesis(bdi_neg_m2, 'scale_diff_bdi > 0')

# Insufficient variation and too few studies
temp = dat %>%
  filter(!is.na(neu_comp_yi), categorization %in% c('Anxiety', 'Depression')) %>%
  mutate(MeanClinBDI = as.numeric(MeanClinBDI), MeanConBDI = as.numeric(MeanConBDI), 
         scale_clin_bdi = scale(MeanClinBDI)[,1],
         scale_diff_bdi = scale(MeanClinBDI-MeanConBDI)[,1]) %>%
  filter(!is.na(MeanClinBDI))
temp$es_id = 1:nrow(temp)

bdi_neu_m1 = brm(neu_comp_yi|se(neu_comp_vi**.5) ~ scale_clin_bdi + (1|StudyID) + (1|es_id),
                 data=temp, cores=4, sample_prior = 'yes', 
                 save_pars=save_pars(group = TRUE, all = TRUE), 
                 control = list(adapt_delta=.9999, max_treedepth = 20),
                 prior = basic_mod_priors_tight,
                 iter = 10000)
hypothesis(bdi_neu_m1, '100*scale_clin_bdi = 0')

bdi_neu_m2 = brm(neu_comp_yi|se(neu_comp_vi**.5) ~ scale_diff_bdi + (1|StudyID) + (1|es_id),
                 data=temp, cores=4, sample_prior = 'yes', 
                 save_pars=save_pars(group = TRUE, all = TRUE), 
                 control = list(adapt_delta=.9999, max_treedepth = 20),
                 prior = basic_mod_priors_tight,
                 iter = 10000)
hypothesis(bdi_neu_m2, 'scale_diff_bdi > 0')


