######################################
# Meta-analysis of survival models.
#    Meta-analyse hazard ratios
#    using the metafor package
#
#
######################################


####################
# Load library
####################
library(survival)
library(metafor)
library(ggplot2)
library(survminer)

#######################
# Load data
#######################
data("veteran")
head(veteran)

#########################################
# Build Cox-proportional hazards model
#########################################
cat("\n ########################################################### \n ")
cat("\n Performing survival analysis .... \n ")
surv_object <- survival::Surv(time = veteran$time, event = veteran$status)
cxph <- survival::coxph(formula = surv_object ~ trt + diagtime + age, data = veteran)
summary(cxph)
survminer::ggforest(model = cxph, data = veteran)

# calculate HR and se
# TODO: compute a 2x2 table for use in metafor
#     https://www.openepi.com/TwobyTwo/TwobyTwo.htm
summary(cxph)$coef

# get hazard ratio and se
hr_1 = summary(cxph)$coef[1,1]
se_1 = summary(cxph)$coef[1,2]

################################################
# make up synthetic data for similar studies
################################################
cat("\n Constructing synthetic data ..... \n ")
set.seed(10)
i_num_studies = 30 # say there are 30 studies
# create HR for that many studies (synthetic data)
samples_log_HR <- rnorm(n = i_num_studies, mean = hr_1, sd = se_1)# * sqrt(137) )
# use the same SE for all the synthetic studies
# samples_se     <- se_1 * rep(1, times = i_num_studies)
# randomly sample and create synthetic data SE
f_sd_var = se_1/2
samples_se     <- rnorm(n = i_num_studies, mean = se_1, sd = f_sd_var)
# visualise distribution of hazard ratios
hist(samples_log_HR)

#######################
# Plot diagnostics
#######################
cat("\n Conducting diagnostics for survival analysis ..... \n ")
survminer::ggcoxdiagnostics(fit = cxph)

# test assumptions of proportional hazards model
ftest <- survival::cox.zph(fit = cxph)
summary(ftest)
survminer::ggcoxzph(fit = ftest)
survminer::ggcoxdiagnostics(fit = cxph, type = "schoenfeld", ox.scale = "time")
survminer::ggcoxdiagnostics(fit = cxph, type = "deviance",   ox.scale = "linear.predictions")

#######################
# Plot survival curves
#######################
survminer::ggadjustedcurves(fit = cxph, data = veteran, variable = "trt")
#survminer::ggsurvplot(fit = cxph, data = veteran, fun = "cumhaz")

#######################
# Meta-analyse
#######################
cat("\n Conducting meta-analysis .... \n ")
# construct dat frame
#dat <- get(data("dat.bcg"))
#head(dat)
#dat_rr <- metafor::escalc(measure = "RR", ai = tpos, bi = tneg, ci = cpos, di = cneg, data = dat.bcg)
#head(dat_rr)


input_logHR = samples_log_HR
input_se    = samples_se

# metafor::rma(logHR, sei = SE, data=dat)
meta_model <- metafor::rma(input_logHR, sei = input_se, method = "REML")#, data=dat)
summary(meta_model)

#######################
# Show statistics
#######################
cat("\n Showing summary statistics .... \n ")
# get confidence intervals
# meta_model$ci.lb
# meta_model$ci.ub
metafor::confint.rma.uni(object = meta_model, verbose = FALSE, fixed = TRUE)
# metafor::confint.rma.glmm(object = meta_model)

# show Q-statistic, I^2, Tau, z-value
# see video below by Michael Bronstein
# https://www.youtube.com/watch?v=O6qDlov5-Is
cat("\n")
cat("z-value is :", "\n")
cat(meta_model$zval)
cat("\n")
cat("p-value is:", "\n")
cat(meta_model$pval)
cat("\n")
if (meta_model$pval >= 0.05)
{
    cat("Hence we cannot reject the null hypothesis that the true effect size is 0 or hazard ratio is 1. \n")
} else
{
    cat("Hence we reject the null hypothesis that the true effect size is 0 or hazard ratio is 1. \n")
}

# Q-value : the Q-statistic gives a test of the null hypothesis that all studies 
#     share a common effect size
# Video by Sergei Bronstein
#   Meta-analysis | basic lecture 03
#   https://www.youtube.com/watch?v=O6qDlov5-ls
cat("\n")
cat("The Q-statistic gives a test of the null hypothesis that all studies share a common effect size. \n")
cat("Q-value is: ", "\n")
cat(meta_model$QE)

i_degrees_freedom = i_num_studies - 1
if (meta_model$QE <= i_degrees_freedom)
{
    cat("\n The observed dispersion is less than what we would expect by chance. ")
    cat("There is no evidence that the true effect size varies from study to study. \n")
} else
{
    cat("\n The observed dispersion is greater than what we would expect by chance. ")
    cat("There is evidence that the true effect size varies from study to study. \n")
}

# get I^2 and tau^2
# tau^2 is the variance in true effect sizes
# I^2 statistic tells us what proportion of the observed variance in effect size
#   reflects differences in true effect size rather than sampling error
cat("\n I^2 statistic tells us what proportion of the observed variance in effect size reflects differences in true effect size rather than sampling error \n")
cat("The I^2 statistic is: \n")
cat(meta_model$I2, "\n")
cat("\n tau^2 is the variance in true effect sizes \n")
cat("The tau^2 statistic is: \n")
cat(meta_model$tau2)

cat("\n The final meta-analysed effect size/hazard ratio is: \n")
cat(exp(meta_model$beta))



# TODO: other diagnostics see Rachel Kelly talk Wald Chi-squared statistic
# TODO: metafor visualizations
# http://www.metafor-project.org/doku.php/plots:baujat_plot

# Baujat plot
metafor::baujat(x = meta_model)

# funnel plot
rtf <- metafor::trimfill(x = meta_model)
summary(rtf)
metafor::funnel(x = rtf)

# regtest
metafor::regtest(x = meta_model, model = 'rma')

# test residuals
metafor::rstudent.rma.uni(model = meta_model)
stats::rstudent(model = meta_model)

# influence
inf <- metafor::influence.rma.uni(model = meta_model)
#plot(inf, plotdfb = TRUE)

# cross-validation
metafor::leave1out(x = meta_model, transf = exp, digits = 3)

# Q-Q plots
#metafor::qqnorm.rma.glmm(y = meta_model, main = "Random effects model")
metafor::qqnorm.rma.uni(y = meta_model)#, main = "Random effects model")

# GOSH plot
# http://www.metafor-project.org/doku.php/plots:gosh_plot
# Fits all subsets of studies 
#sav <- metafor::gosh(x = meta_model)
#plot(sav)

########################
# Final forest plot
########################
metafor::forest.rma(x = meta_model)

cat("\n")
cat("\n Parameters of run are: \n")
cat("Number of simulated studies that are meta-analysed: ")
cat(i_num_studies, "\n")
cat("Mean hazard ratio: ")
cat(exp(hr_1), "\n")
cat("Standard error: ")
cat(se_1, f_sd_var, "\n")
cat("\n The final meta-analysed effect size/hazard ratio is: \n")
cat(exp(meta_model$beta))

cat("\n \n ########################################################### \n \n ")

################################################################
# Model selection
# anova
# https://wviechtb.github.io/metafor/reference/anova.rma.html
################################################################

cat("\n Performing model selection .... \n ")
# try a simpler model
cxph_simple <- survival::coxph(formula = surv_object ~ trt + age, data = veteran)
summary(cxph_simple)
survminer::ggforest(model = cxph_simple, data = veteran)

# TODO: add factors for meta-analysis model
meta_model_simple <- metafor::rma(input_logHR, sei = input_se, method = "REML")#, mods = ~factor(alloc))#, data=dat)
summary(meta_model)

# anova(meta_model_simple, meta_model)

# get AIC values
metafor::AIC.rma(object = meta_model)
metafor::AIC.rma(object = meta_model_simple)

# do permutation test
metafor::permutest(x = meta_model)
metafor::permutest(x = meta_model_simple)

# TODO: Wald Chi squared test to assess between strata heterogeneity
#   https://www.khanacademy.org/math/statistics-probability/inference-categorical-data-chi-square-tests/chi-square-goodness-of-fit-tests/v/chi-square-distribution-introduction
# Wald chi-squared test
#   http://www.metafor-project.org/doku.php/tips:multiple_factors_interactions
#   https://wviechtb.github.io/metafor/reference/anova.rma.html
#   http://www.metafor-project.org/doku.php/tips:testing_factors_lincoms
metafor::anova.rma(object = meta_model) #, btt=2:3,

#################################################################################
# Other resources for hazard ratio and meta-analysis
# http://www.sthda.com/english/wiki/cox-proportional-hazards-model
# https://www.youtube.com/watch?v=p1wa8W11JLI
# http://www.deeplytrivial.com/2017/08/statistics-sunday-introduction-to.html
# http://www.deeplytrivial.com/search?q=meta-analysis
# https://stats.stackexchange.com/questions/167497/the-confidence-interval-of-tau2-and-i2-produced-by-the-meta-and-metafor-p
# http://www.metafor-project.org/doku.php/faq#how_are_i_2_and_h_2_computed_i
# http://www.metafor-project.org/doku.php/analyses
# http://www.metafor-project.org/doku.php/analyses:berkey1995
# 
#################################################################################