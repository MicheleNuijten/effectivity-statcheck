rm(list = ls())

library(lme4)

# LOAD DATA --------------------------------------------------------------------

data <- read.table("03data/2023-01-06data_wrangled_no_missings.txt", header = TRUE)
data_per_article <- read.table("03data/2023-01-06data_per_article_with_stats.txt", header = TRUE)

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
# 64316.7  64366.0 -32153.4  64306.7   141001 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -1.8506 -0.2609 -0.1790 -0.1343  9.3220 
# 
# Random effects:
#   Groups Name        Variance Std.Dev.
# source (Intercept) 1.66     1.288   
# Number of obs: 141006, groups:  source, 7097
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)              -3.11943    0.04582 -68.076  < 2e-16 ***
#   period                   -0.27511    0.07992  -3.442 0.000576 ***
#   statcheck_journal         0.13314    0.05376   2.477 0.013267 *  
#   period:statcheck_journal -0.72936    0.11100  -6.571    5e-11 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) period sttch_
# period      -0.534              
# sttchck_jrn -0.790  0.465       
# prd:sttchc_  0.395 -0.719 -0.482

save(lm_errors, file = "04analysis/02confirmatory/lm_errors.rda")

## extract relevant stats and report in apa style ------------------------------

coef <- summary(lm_errors)$coefficients

b3 <- coef["period:statcheck_journal", "Estimate"]
b3_round <- round(b3, 2)
se <- coef["period:statcheck_journal", "Std. Error"]
z <- round(coef["period:statcheck_journal", "z value"], 2)
p <- round(coef["period:statcheck_journal", "Pr(>|z|)"], 3)

ci_low <- round(b3 - 1.96*se, 2)
ci_up <- round(b3 + 1.96*se, 2)

cat("b3 = ", b3_round, ", 95% CI = [", ci_low, "; ", ci_up, "], Z = ", z, ", p = ", p,
    sep = "")

# ANALYSIS 1: Predict gross inconsistencies ------------------------------------

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
# 12167.1  12216.4  -6078.6  12157.1   141001 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -1.8096 -0.0207 -0.0186 -0.0152 15.8623 
# 
# Random effects:
#   Groups Name        Variance Std.Dev.
# source (Intercept) 13.9     3.728   
# Number of obs: 141006, groups:  source, 7097
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)               -7.9154     0.2466 -32.104   <2e-16 ***
#   period                    -0.4194     0.2483  -1.689   0.0912 .  
# statcheck_journal          0.2419     0.1594   1.518   0.1291    
# period:statcheck_journal  -0.7696     0.3577  -2.151   0.0315 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) period sttch_
# period      -0.292              
# sttchck_jrn -0.457  0.439       
# prd:sttchc_  0.203 -0.693 -0.444

save(lm_dec_errors, file = "04analysis/02confirmatory/lm_dec_errors.rda")

## extract relevant stats and report in apa style ------------------------------

coef <- summary(lm_dec_errors)$coefficients

b3 <- coef["period:statcheck_journal", "Estimate"]
b3_round <- round(b3, 2)
se <- coef["period:statcheck_journal", "Std. Error"]
z <- round(coef["period:statcheck_journal", "z value"], 2)
p <- round(coef["period:statcheck_journal", "Pr(>|z|)"], 3)

ci_low <- round(b3 - 1.96*se, 2)
ci_up <- round(b3 + 1.96*se, 2)

cat("b3 = ", b3_round, ", 95% CI = [", ci_low, "; ", ci_up, "], Z = ", z, ", p = ", p,
    sep = "")
