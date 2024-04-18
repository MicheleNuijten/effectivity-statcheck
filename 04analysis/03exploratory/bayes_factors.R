# exploratory analysis:
# calculate bayes factors for linear models

rm(list = ls())

library(BFpack)
library(lme4)

# LOAD DATA --------------------------------------------------------------------

data <- read.table("03data/2024-04-15data_wrangled_no_missings.txt", header = TRUE)
data_per_article <- read.table("03data/2024-04-15data_per_article_with_stats.txt", header = TRUE)

# ANALYSIS 1: Predict inconsistencies ------------------------------------------

# fit multilevel logistic model
lm_errors <- glmer(error ~ period*statcheck_journal + (1|source), 
                   data = data, family = binomial(link = "logit"))

summary(lm_errors)

##################

### Fit null model in order to compute the intraclass correlation
null <- glmer(error ~ (1|source), data = data, family = binomial(link = "logit"))

var_int <- as.numeric(summary(null)$varcor$source) # Variance of random intercept

### Compute Bayes factor
### Compute ICC in order to compute the effective sample size that is needed as 
# input for the BF() function (see p. 117 of Hox, Moerbeek, and Van de Schoot, 
# 2018 for the computation of the ICC)
icc <- var_int/(var_int+pi^2/3)

### Compute mean cluster size
tmp <- split(data, f = data$source)
cluster_size <- sapply(tmp, function(x) nrow(x))
mean_cluster_size <- mean(cluster_size)

### Compute effective sample size (equation 12.5 in Hox, Moerbeek and Van de 
# Schoot, 2018, p. 223)
design <- 1 + (mean_cluster_size-1)*icc
N <- nrow(data)
eff_N <- N/design

### Compute Bayes factor & posterior probabilities
bf1 <- BF(x = c(period_x_journal = 
                  coef(summary(lm_errors))["period:statcheck_journal","Estimate"]), 
          Sigma = 
            matrix(coef(summary(lm_errors))["period:statcheck_journal","Std. Error"]^2), 
          n = eff_N,
          hypothesis = "period_x_journal < 0")
bf1

# Posterior probabilities:
#   Pr(hypothesis|data)
# H1                   1
# H2                   0
# 
# Evidence matrix (Bayes factors):
#   H1           H2
# H1  1 115891448317
# H2  0            1
# 
# Hypotheses:
#   H1: period_x_journal<0
# H2: complement

# ANALYSIS 2: Predict decision inconsistencies ------------------------------------------

# fit multilevel logistic model
lm_dec_errors <- glmer(decision_error ~ period*statcheck_journal + (1|source), 
                       data = data, family = binomial(link = "logit"))

summary(lm_dec_errors)

##################

### Compute Bayes factor & posterior probabilities
bf2 <- BF(x = c(period_x_journal = 
                  coef(summary(lm_dec_errors))["period:statcheck_journal","Estimate"]), 
          Sigma = 
            matrix(coef(summary(lm_dec_errors))["period:statcheck_journal","Std. Error"]^2), 
          n = eff_N,
          hypothesis = "period_x_journal < 0")
bf2

# Posterior probabilities:
#   Pr(hypothesis|data)
# H1               0.991
# H2               0.009
# 
# Evidence matrix (Bayes factors):
#   H1      H2
# H1 1.000 110.249
# H2 0.009   1.000
# 
# Hypotheses:
#   H1: period_x_journal<0
# H2: complement