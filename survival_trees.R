# Script to build survival trees
#  adapted from
#  https://en.wikipedia.org/wiki/Survival_analysis
#  https://www.statmethods.net/advstats/cart.html

library(rpart)

head(stagec)

fit_tree <- rpart::rpart(formula = Surv(pgtime,pgstat) ~ age + eet + g2 + grade + gleason + ploidy,
                         data = stagec)

plot(fit_tree, uniform = TRUE, branch = .4, compress = TRUE)
text(fit_tree, use.n = TRUE)

print(fit_tree)
