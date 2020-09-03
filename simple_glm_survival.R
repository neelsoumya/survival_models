#############################################################################
# Simple code to perform survival analysus using GLM Poisson model
#
# Adapted from:
#   https://data.princeton.edu/wws509/r/c7s1
#   https://rdrr.io/github/datashield/dsBaseClient/man/ds.glmSLMA.html
#
# Other resources:
#   why intercept is taken from Poisson GLM model:
#   http://sphweb.bumc.bu.edu/otlt/MPH-Modules/BS/R/R7_LogisticRegression-Survival/R7_LogisticRegression-Survival_print.html
#   https://stats.stackexchange.com/questions/8117/does-cox-regression-have-an-underlying-poisson-distribution/8118#8118
#   https://stats.stackexchange.com/questions/115479/calculate-incidence-rates-using-poisson-model-relation-to-hazard-ratio-from-cox
#   https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/1471-2288-12-34
#   https://data.princeton.edu/wws509/r/c7s1
# 
#############################################################################

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




#####################################
# Repeat using glm() Poisson model
#####################################

veteran$logsurv <- log(veteran$time)

glm_hazard <- glm(formula = status ~ trt + diagtime + age,  # +  offset(veteran$logsurv),
                  family = 'poisson', 
                  offset = veteran$logsurv, 
                  data = veteran
                  )

# Poisson model hazard ratio
#	the intercept is the hazard ratio
# http://sphweb.bumc.bu.edu/otlt/MPH-Modules/BS/R/R7_LogisticRegression-Survival/R7_LogisticRegression-Survival_print.html
# https://stats.stackexchange.com/questions/8117/does-cox-regression-have-an-underlying-poisson-distribution/8118#8118
	
summary(glm_hazard)$coef
summary(glm_hazard)$coef[1,1]
summary(glm_hazard)$coef[1,2]

exp(summary(glm_hazard)$coef[1,1])
exp(summary(glm_hazard)$coef[1,2])

# Compare to Cox model hazard ratios
summary(cxph)$coef
summary(cxph)$coef[1,1]
summary(cxph)$coef[1,2]


# https://data.princeton.edu/wws509/r/c7s1
pchisq(deviance(glm_hazard), glm_hazard$df.residual, lower.tail=FALSE)

############################################
# calculate hazard ratios
############################################
# hazard ratios from Poisson model
exp(coef(glm_hazard[1])) # - 1
# hazard ratios form Cox proportional hazards model
exp(coef(cxph))


# Other resources and explanations
# why intercept:
# http://sphweb.bumc.bu.edu/otlt/MPH-Modules/BS/R/R7_LogisticRegression-Survival/R7_LogisticRegression-Survival_print.html
# https://stats.stackexchange.com/questions/8117/does-cox-regression-have-an-underlying-poisson-distribution/8118#8118
# https://stats.stackexchange.com/questions/115479/calculate-incidence-rates-using-poisson-model-relation-to-hazard-ratio-from-cox
# https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/1471-2288-12-34
# https://data.princeton.edu/wws509/r/c7s1

 


