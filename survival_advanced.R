############################
#
# Simple script to load 
# survival data and show 
# survival functionality
############################

library(survival)

##############
# Load data
##############
file <- read.csv(file = "expand_no_missing_study1.csv", header = TRUE, stringsAsFactors = FALSE)

SURVTIME  <- as.numeric(file$survtime)
EVENT     <- as.numeric(file$cens)
STARTTIME <- as.numeric(file$starttime)
ENDTIME   <- as.numeric(file$endtime)

AGE <- as.numeric(file$age.60)

# build survival object
s <- survival::Surv(time=SURVTIME,event=EVENT)
# survival::coxph(formula = "survival::Surv(time=SURVTIME,event=EVENT) ~ file$age.60", data = file)

survival::coxph(formula = s ~ AGE)#, data=file)

survival::coxph(formula = s ~ AGE, data=file)

aa <- survival::coxph(formula = s ~ file$age.60, data=file)

survival::coxph(formula = s ~ age.60, data=file)

aa <- survival::coxph(formula = s ~ AGE + file$female, data=file)

#################################
# getting number of data points
#################################
aa$coefficients

length(aa$coefficients)

aa$n


##################
# using strata
##################
survival::coxph(formula = s ~ AGE + strata(file$female), data=file)


##################
# Plotting
##################
# TODO: call with time and time2 parameter
s <- survival::Surv(time=SURVTIME,event=EVENT)

s <- survival::Surv(time = STARTTIME, time2 = ENDTIME, event = EVENT)
summary(s)

coxph_object <- survival::coxph(formula = s ~ AGE, data=file)
summary(coxph_object)

# Kaplan-Meier plot
survfit_km <- survival::survfit(formula = s ~ 1 )
plot(survfit_km)
plot(survfit_km, fun="cloglog")

# plot cox model
survfit_cox <- survival::survfit(formula = coxph_object)
plot(survfit_cox)
plot(survfit_cox, fun="cloglog")


# statistics on survival object
# stats::quantile(x = s)
mean(x = s)

summary(s[,1])
summary(s[,2])

stats::quantile(x = s[,1])
stats::quantile(x = s[,2])

ret_list <- list("time"=stats::quantile(x = s[,1]), "event"=stats::quantile(x = s[,2]))

# TODO: try the following
sa <- as.data.frame(s)
summary(sa)


# stats::quantile(x = sa$time)
# and similar for event


##################################
# Counting data
##################################
s_int <- survival::Surv(time = STARTTIME, time2 = ENDTIME, event = EVENT, type = "counting") 
cox_int <- survival::coxph(formula = s_int ~ AGE)

##################################################
# test the cox proportional hazards assumption
#    diagnostics for Cox models
##################################################
out_coxzph <- survival::cox.zph(fit = cox_int)
plot(out_coxzph)

##################################################
# more advanced options like subset, control
##################################################
cox_int <- survival::coxph(formula = s ~ AGE,
                           data = file,
                           subset = age.60 > 7,
                           control = survival::coxph.control(eps = 0.00001, iter.max = 1000)
                           )

# NOTE: default is
survival::coxph.control()

cox_int <- survival::coxph(formula = s ~ AGE,
                           data = file,
                           subset = age.60 > 7,
                           control = "survival::coxph.control`(eps = 0.00001, iter.max = 1000)"
                          )


cox_int <- survival::coxph(formula = s ~ AGE,
                           data = file,
                           subset = age.60 > 7,
                           control = eval(parse(text = "survival::coxph.control(eps = 0.00001, iter.max = 1000)"))
                          )


cox_int <- survival::coxph(formula = s ~ AGE,
                           data = file,
                           subset = age.60 > 7,
                           control = eval(parse(text = "survival::coxph.control()"))
                          )

eval(parse(text = "survival::coxph.control(eps = 0.00001, iter.max = 1000)"))


eval(parse(text = "fn_ptr = survival::coxph.control(eps = 0.00001, iter.max = 1000)"))

cox_int <- survival::coxph(formula = s ~ AGE,
                           data = file,
                           subset = age.60 > 7,
                           control = fn_ptr
                          )




#############################################
# Time-dependent covariates
#############################################

# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6015946/

# excellent article
# https://stackoverflow.com/questions/24488495/how-does-cox-zph-deal-with-time-dependent-covariates

fit_tt <- survival::coxph(formula = Surv(time = time, event=status) ~ age + ph.karno + tt(ph.karno) + sex,
                          data = lung,
                          tt = function(x,t,...) x*log(t+20)
                          )
summary(fit_tt)

#cox_zph_tt <- survival::cox.zph(fit = fit_tt,
#                                transform = function(t) x*log(t+20)
#                                )
#plot(cox_zph_tt)

cox_zph_time <- survival::cox.zph(fit = fit_tt)
plot(cox_zph_time)
