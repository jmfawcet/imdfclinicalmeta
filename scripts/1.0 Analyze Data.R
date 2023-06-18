# NOTE: These scripts operate only on the data reported in-text; to
# calculate similar models using over DVs (e.g., symptom relevant
# items), swap the DVs listed in each function call to the
# appropriate dv

# Load functions and data
source('scripts/0.0 Functions and Libraries.R')
saveRDS(dat, 'data/coded_data.rds')

# Priors ------------------------------------------------------------------

basic_priors = c(prior(normal(0,.3), class='Intercept'), 
                 prior(normal(0,.3), class='sd'))
basic_mod_priors = c(prior(normal(0,.3), class='Intercept'), 
                     prior(normal(0,.3), class='sd'), 
                     prior(normal(0,.3), class='b'))

basic_priors_tight = c(prior(normal(0,.15), class='Intercept'), 
                       prior(normal(0,.10), class='sd'))

basic_priors_cond = c(prior(normal(0,.15), class='b'), 
                      prior(normal(0,.10), class='sd'))

basic_mod_priors_tight = c(prior(normal(0,.15), class='Intercept'), 
                           prior(normal(0,.10), class='sd'), 
                           prior(normal(0,.3), class='b'))


# Basic models --------------------------------------------------------

# Clinical negative DF
table(dat$categorization)

neg_clin = fit_all_models(dat, basic_priors_tight, basic_priors_cond, 'neg_clin_yi', 'neg_clin_vi')

# Clinical neutral DF
table(dat$categorization)

neu_clin = fit_all_models(dat, basic_priors_tight, basic_priors_cond, 'neu_clin_yi', 'neu_clin_vi')

# Comparing negative and neutral within Clinical
table(dat$categorization)

emodiff_clin = fit_all_models(dat, basic_priors_tight, basic_priors_cond, 'emodiff_clin_yi', 'emodiff_clin_vi')

hypothesis(emodiff_clin$category, 'categorizationAnxiety*100  - categorizationDepression*100 = 0')

# Control negative DF
table(dat$categorization)

neg_con = fit_all_models(dat, basic_priors_tight, basic_priors_cond, 'neg_con_yi', 'neg_con_vi')

# Control neutral DF
table(dat$categorization)

neu_con = fit_all_models(dat, basic_priors_tight, basic_priors_cond, 'neu_con_yi', 'neu_con_vi')

# Comparing negative and neutral within Control
emodiff_con = fit_all_models(dat, basic_priors_tight, basic_priors_cond, 'emodiff_con_yi', 'emodiff_con_vi')
table(emodiff_con[[1]]$categorization)

# Comparing clinical and control for negative
neg_comp = fit_all_models(dat, basic_priors_tight, basic_priors_cond, 'neg_comp_yi', 'neg_comp_vi')
table(neg_comp[[1]]$categorization)

# Prediction interval
neg_comp_pi = 100*posterior_epred(neg_comp$overall, 
                                    newdata=data.frame(vi=0, es_id=9999), 
                                    re_formula=~(1|es_id), allow_new_levels=TRUE)

# Comparing clinical and control for neutral
neu_comp = fit_all_models(dat, basic_priors_tight, basic_priors_cond, 'neu_comp_yi', 'neu_comp_vi')
table(neu_comp[[1]]$categorization)

neu_comp_pi = 100*posterior_epred(neu_comp$overall, 
                                                    newdata=data.frame(vi=0, es_id=9999), 
                                                    re_formula=~(1|es_id), allow_new_levels=TRUE)

# Comparing the difference in emotional items between clinical and control
comp_comp = fit_all_models(dat, basic_priors_tight, basic_priors_cond, 'comp_comp_yi', 'comp_comp_vi')
table(comp_comp[[1]]$categorization)
