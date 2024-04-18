rm(list = ls())

library(lme4)

# LOAD DATA --------------------------------------------------------------------

data <- read.table("03data/2024-04-15data_wrangled_no_missings.txt", header = TRUE)
data_per_article <- read.table("03data/2024-04-15data_per_article_with_stats.txt", header = TRUE)

# FUNCTION TO REPORT LM MODELS -------------------------------------------------

report_model <- function(model){
  
  # extract coefficients
  coef <- summary(model)$coefficients
  
  # calculate 95% CI (Wald)
  ci_summary <- confint(model, method = "Wald", level = .95)
  # remove unnecessary info:
  ci_summary2 <- ci_summary[-which(rownames(ci_summary) == ".sig01"), ]
  ci_lb <- round(ci_summary2[ , "2.5 %"], 3)
  ci_ub <- round(ci_summary2[ , "97.5 %"], 3)
  ci <- paste0("[", ci_lb, ", ", ci_ub, "]")
  
  # add CI to table & round to 3 dec
  coef2 <- data.frame(round(coef, 3), ci)
  
  # extract random effect
  random <- summary(model)$varcor
  
  return(list(fixed = coef2, random = random))
}

# ANALYSIS 1: Predict inconsistencies ------------------------------------------

# fit multilevel logistic model
lm_errors <- glmer(error ~ period*statcheck_journal + (1|source), 
            data = data, family = binomial(link = "logit"))

summary(lm_errors)

# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) [
#   glmerMod]
# Family: binomial  ( logit )
# Formula: error ~ period * statcheck_journal + (1 | source)
# Data: data
# 
# AIC      BIC   logLik deviance df.resid 
# 66374.3  66423.8 -33182.1  66364.3   147785 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -1.8508 -0.2591 -0.1779 -0.1320  9.3106 
# 
# Random effects:
#   Groups Name        Variance Std.Dev.
# source (Intercept) 1.662    1.289   
# Number of obs: 147790, groups:  source, 7315
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)              -3.11969    0.04563 -68.368  < 2e-16 ***
#   period                   -0.27533    0.07962  -3.458 0.000544 ***
#   statcheck_journal         0.12550    0.05347   2.347 0.018918 *  
#   period:statcheck_journal -0.71298    0.10598  -6.728 1.73e-11 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) period sttch_
# period      -0.533              
# sttchck_jrn -0.792  0.463       
# prd:sttchc_  0.409 -0.749 -0.500

save(lm_errors, file = "04analysis/02confirmatory/lm_errors.rda")

## extract relevant stats and report in apa style ------------------------------

# table
report_model(lm_errors)

# in text
coef <- summary(lm_errors)$coefficients

b3 <- coef["period:statcheck_journal", "Estimate"]
b3_round <- round(b3, 2)
se <- coef["period:statcheck_journal", "Std. Error"]
se_round <- round(se, 2)
z <- round(coef["period:statcheck_journal", "z value"], 2)
p <- round(coef["period:statcheck_journal", "Pr(>|z|)"], 3)

ci_low <- round(b3 - 1.96*se, 2)
ci_up <- round(b3 + 1.96*se, 2)

cat("b3 = ", b3_round, ", SE = ", se_round, ", 95% CI = [", ci_low, "; ", ci_up, "], Z = ", z, ", p = ", p,
    sep = "")

# odds & probabilities
round(exp(b3), 2)
round(exp(ci_low), 2)
round(exp(ci_up), 2)

round(exp(b3)/(1+exp(b3)), 2)
round(exp(ci_low)/(1+exp(ci_low)), 2)
round(exp(ci_up)/(1+exp(ci_up)), 2)

# ANALYSIS 2: Predict gross inconsistencies ------------------------------------

# fit multilevel logistic model
lm_dec_errors <- glmer(decision_error ~ period*statcheck_journal + (1|source), 
            data = data, family = binomial(link = "logit"))

summary(lm_dec_errors)
 
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) [
#   glmerMod]
# Family: binomial  ( logit )
# Formula: decision_error ~ period * statcheck_journal + (1 | source)
# Data: data
# 
# AIC      BIC   logLik deviance df.resid 
# 12396.0  12445.5  -6193.0  12386.0   147785 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -1.8300 -0.0197 -0.0176 -0.0143 15.8343 
# 
# Random effects:
#   Groups Name        Variance Std.Dev.
# source (Intercept) 14.75    3.84    
# Number of obs: 147790, groups:  source, 7315
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)               -8.0103     0.2415 -33.165   <2e-16 ***
#   period                    -0.4180     0.2515  -1.662   0.0966 .  
# statcheck_journal          0.2287     0.1616   1.415   0.1569    
# period:statcheck_journal  -0.8235     0.3481  -2.366   0.0180 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) period sttch_
# period      -0.302              
# sttchck_jrn -0.474  0.437       
# prd:sttchc_  0.216 -0.719 -0.459

save(lm_dec_errors, file = "04analysis/02confirmatory/lm_dec_errors.rda")

## extract relevant stats and report in apa style ------------------------------

# table
report_model(lm_dec_errors)

# in text
coef <- summary(lm_dec_errors)$coefficients

b3 <- coef["period:statcheck_journal", "Estimate"]
b3_round <- round(b3, 2)
se <- coef["period:statcheck_journal", "Std. Error"]
se_round <- round(se, 2)
z <- round(coef["period:statcheck_journal", "z value"], 2)
p <- round(coef["period:statcheck_journal", "Pr(>|z|)"], 3)

ci_low <- round(b3 - 1.96*se, 2)
ci_up <- round(b3 + 1.96*se, 2)

cat("b3 = ", b3_round, ", SE = ", se_round, ", 95% CI = [", ci_low, "; ", ci_up, "], Z = ", z, ", p = ", p,
    sep = "")

# odds & probabilities
round(exp(b3), 2)
round(exp(ci_low), 2)
round(exp(ci_up), 2)

round(exp(b3)/(1+exp(b3)), 2)
round(exp(ci_low)/(1+exp(ci_low)), 2)
round(exp(ci_up)/(1+exp(ci_up)), 2)
