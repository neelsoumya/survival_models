#######################################################################################
# Fitting a Kaplan-Meier and a 
#   Cox proportional hazards model.
#
# survival curves and estimate parameters for a Cox proportional hazards model
#
# adapted from 
# https://stat.ethz.ch/R-manual/R-devel/library/survival/html/survfit.formula.html
#
#######################################################################################


########################################
# Load library
########################################
library(survival)

# aml has data like
# > aml
# time status             x
# 1     9      1    Maintained
# 2    13      1    Maintained
# 3    13      0    Maintained


########################################
# Fit a Kaplan-Meier and plot it 
########################################
fit <- survfit(Surv(time, status) ~ x, data = aml) 
plot(fit, lty = 2:3) 
legend(100, .8, c("Maintained", "Nonmaintained"), lty = 2:3) 

################################################################################
# Fit a Cox proportional hazards model and plot the  
# predicted survival for a 60 year old 
################################################################################
fit <- coxph(Surv(futime, fustat) ~ age, data = ovarian) 
plot(survfit(fit, newdata=data.frame(age=60)),
     xscale=365.25, xlab = "Years", ylab="Survival") 



###########################################
# Better plots for tiem to event analysis
###########################################

# from
# https://rpkgs.datanovia.com/survminer/index.html
# install.packages("survminer")
library("survminer")
fit <- survfit(Surv(time, status) ~ sex, data = lung)

ggsurvplot(fit, data = lung)

ggsurvplot(fit, data = lung, censor.shape="|", censor.size = 4)

ggsurvplot(
          fit, 
          data = lung, 
          size = 1,                 # change line size
          palette = 
            c("#E7B800", "#2E9FDF"),# custom color palettes
          conf.int = TRUE,          # Add confidence interval
          pval = TRUE,              # Add p-value
          risk.table = TRUE,        # Add risk table
          risk.table.col = "strata",# Risk table color by groups
          legend.labs = 
            c("Male", "Female"),    # Change legend labels
          risk.table.height = 0.25, # Useful to change when you have multiple groups
          ggtheme = theme_bw()      # Change ggplot2 theme
)

