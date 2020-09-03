##################################################################################################
# Piecewise exponential proportional hazards survival model
# 
# Adapted from:
# https://stats.stackexchange.com/questions/348922/piecewise-exponential-model-how-to-fit
#
# Theory:
#  http://data.princeton.edu/wws509/notes/
#  
# Installation:
#     install.packages("pammtools")
#
##################################################################################################

##################################################
# Load library
##################################################
library(pammtools)
library(survival)
#library(ggp)

#########################
# load example data
#########################
data(tumor)
head(tumor)

##################################################
## transform to piece-wise exponential data (PED)
##################################################
ped_tumor <- as_ped(Surv(days, status) ~ ., data = tumor, cut = seq(0, 3000, by = 100))
# the interval column indicates the interval, ped_status indicates the status
# in the respective interval
filter(ped_tumor, id == 1) %>% head() %>% select(1:8)

###########################################################################
# Fit the Piece-wise-exponential Additive Model (PAM) instead of PEM
###########################################################################
pam <- mgcv::gam(ped_status ~ s(tend) + age + sex + complications, 
                 data = ped_tumor, family = poisson(), offset = offset)
summary(pam)
plot(pam)

#########################
# compare to Cox 
#########################
cox <- coxph(Surv(days, status)~ age + sex + complications, data=tumor)
cbind(coef(pam)[2:4], coef(cox))

cox_fit <- survival::survfit(cox)
plot(cox_fit)


