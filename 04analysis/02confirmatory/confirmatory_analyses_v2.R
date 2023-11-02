rm(list = ls())

library(lme4)

# LOAD DATA --------------------------------------------------------------------

data <- read.table("03data/2023-06-15data_wrangled_no_missings.txt", header = TRUE)
data_per_article <- read.table("03data/2023-06-15data_per_article_with_stats.txt", header = TRUE)

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

# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: error ~ period * statcheck_journal + (1 | source)
# Data: data
# 
# AIC      BIC   logLik deviance df.resid 
# 66375.7  66425.2 -33182.8  66365.7   147785 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -1.8507 -0.2591 -0.1780 -0.1320  9.2984 
# 
# Random effects:
#   Groups Name        Variance Std.Dev.
# source (Intercept) 1.661    1.289   
# Number of obs: 147790, groups:  source, 7315
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)              -3.11954    0.04560 -68.409  < 2e-16 ***
#   period                   -0.27529    0.07948  -3.463 0.000533 ***
#   statcheck_journal         0.12603    0.05346   2.357 0.018403 *  
#   period:statcheck_journal -0.70579    0.10555  -6.687 2.28e-11 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) period sttch_
# period      -0.532              
# sttchck_jrn -0.791  0.462       
# prd:sttchc_  0.409 -0.750 -0.501

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

# ANALYSIS 2: Predict gross inconsistencies ------------------------------------

# fit multilevel logistic model
lm_dec_errors <- glmer(decision_error ~ period*statcheck_journal + (1|source), 
            data = data, family = binomial(link = "logit"))

summary(lm_dec_errors)
 
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: decision_error ~ period * statcheck_journal + (1 | source)
# Data: data
# 
# AIC      BIC   logLik deviance df.resid 
# 12395.1  12444.6  -6192.6  12385.1   147785 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -1.8289 -0.0198 -0.0176 -0.0143 15.8462 
# 
# Random effects:
#   Groups Name        Variance Std.Dev.
# source (Intercept) 14.69    3.833   
# Number of obs: 147790, groups:  source, 7315
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)               -8.0044     0.2461 -32.527   <2e-16 ***
#   period                    -0.4181     0.2533  -1.650   0.0989 .  
# statcheck_journal          0.2326     0.1628   1.428   0.1531    
# period:statcheck_journal  -0.8387     0.3510  -2.390   0.0169 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) period sttch_
# period      -0.305              
# sttchck_jrn -0.476  0.444       
# prd:sttchc_  0.222 -0.723 -0.465

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
