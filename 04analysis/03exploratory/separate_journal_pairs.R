rm(list = ls())

# load packages
library(tidyverse) 

# LOAD DATA --------------------------------------------------------------------

data <- read.table("03data/2024-04-15data_wrangled_no_missings.txt", header = TRUE)
data_per_article <- read.table("03data/2024-04-15data_per_article_with_stats.txt", header = TRUE)

# PS vs. JEPG --------------------------

# select relevant data
ps_jepg <- data %>% filter(journal %in% c("PS", "JEPG"))

# create dummy variable for journal
ps_jepg$experimental_journal <- ifelse(ps_jepg$journal == "PS", 1, 0)

# fit multilevel logistic model
lm_ps_jepg <- glmer(error ~ period*experimental_journal + (1|source), 
                    data = ps_jepg, family = binomial(link = "logit"))

summary(lm_ps_jepg)

# Fixed effects:
#                               Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)                 -3.15110    0.06363 -49.521  < 2e-16 ***
#   period                      -0.19417    0.09663  -2.009   0.0445 *  
#   experimental_journal         0.13245    0.07566   1.751   0.0800 .  
#   period:experimental_journal -0.91727    0.14295  -6.417 1.39e-10 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

### extract relevant stats and report in apa style -----------------------------

coef <- summary(lm_ps_jepg)$coefficients

b3 <- coef["period:experimental_journal", "Estimate"]
b3_round <- round(b3, 2)
se <- coef["period:experimental_journal", "Std. Error"]
z <- round(coef["period:experimental_journal", "z value"], 2)
p <- round(coef["period:experimental_journal", "Pr(>|z|)"], 3)

ci_low <- round(b3 - 1.96*se, 2)
ci_up <- round(b3 + 1.96*se, 2)

cat("b3 = ", b3_round, ", 95% CI = [", ci_low, "; ", ci_up, "], Z = ", z, ", p = ", p,
    sep = "")

## DECISION ERRORS -------------------------------------------------------------

lm_ps_jepg_dec <- glmer(decision_error ~ period*experimental_journal + (1|source), 
                        data = ps_jepg, family = binomial(link = "logit"))

summary(lm_ps_jepg_dec)

# Fixed effects:
#                               Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)                  -8.9835     0.3180 -28.253   <2e-16 ***
#   period                       -0.2307     0.3477  -0.664    0.507    
#   experimental_journal          0.3455     0.2667   1.295    0.195    
#   period:experimental_journal  -0.8002     0.5284  -1.514    0.130    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

### extract relevant stats and report in apa style -----------------------------

coef <- summary(lm_ps_jepg_dec)$coefficients

b3 <- coef["period:experimental_journal", "Estimate"]
b3_round <- round(b3, 2)
se <- coef["period:experimental_journal", "Std. Error"]
z <- round(coef["period:experimental_journal", "z value"], 2)
p <- round(coef["period:experimental_journal", "Pr(>|z|)"], 3)

ci_low <- round(b3 - 1.96*se, 2)
ci_up <- round(b3 + 1.96*se, 2)

cat("b3 = ", b3_round, ", 95% CI = [", ci_low, "; ", ci_up, "], Z = ", z, ", p = ", p,
    sep = "")


# JESP vs. JPSP --------------------------

# select relevant data
jesp_jpsp <- data %>% filter(journal %in% c("JESP", "JPSP"))

# create dummy variable for journal
jesp_jpsp$experimental_journal <- ifelse(jesp_jpsp$journal == "JESP", 1, 0)

# fit multilevel logistic model
lm_jesp_jpsp <- glmer(error ~ period*experimental_journal + (1|source), 
                      data = jesp_jpsp, family = binomial(link = "logit"))

summary(lm_jesp_jpsp)

# Fixed effects:
#                               Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)                 -3.08836    0.06556 -47.104  < 2e-16 ***
#   period                      -0.76223    0.17787  -4.285 1.82e-05 ***
#   experimental_journal         0.12024    0.07567   1.589    0.112    
#   period:experimental_journal -0.11674    0.20070  -0.582    0.561    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

### extract relevant stats and report in apa style -----------------------------

coef <- summary(lm_jesp_jpsp)$coefficients

b3 <- coef["period:experimental_journal", "Estimate"]
b3_round <- round(b3, 2)
se <- coef["period:experimental_journal", "Std. Error"]
z <- round(coef["period:experimental_journal", "z value"], 2)
p <- round(coef["period:experimental_journal", "Pr(>|z|)"], 3)

ci_low <- round(b3 - 1.96*se, 2)
ci_up <- round(b3 + 1.96*se, 2)

cat("b3 = ", b3_round, ", 95% CI = [", ci_low, "; ", ci_up, "], Z = ", z, ", p = ", p,
    sep = "")

## DECISION ERRORS -------------------------------------------------------------

# fit multilevel logistic model
lm_jesp_jpsp_dec <- glmer(decision_error ~ period*experimental_journal + (1|source), 
                          data = jesp_jpsp, family = binomial(link = "logit"))

summary(lm_jesp_jpsp_dec)

# Fixed effects:
#                               Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)                 -6.40249    0.22435 -28.538   <2e-16 ***
#   period                      -1.00277    0.44185  -2.269   0.0232 *  
#   experimental_journal         0.08706    0.17239   0.505   0.6135    
#   period:experimental_journal -0.37359    0.51238  -0.729   0.4659    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

### extract relevant stats and report in apa style -----------------------------

coef <- summary(lm_jesp_jpsp_dec)$coefficients

b3 <- coef["period:experimental_journal", "Estimate"]
b3_round <- round(b3, 2)
se <- coef["period:experimental_journal", "Std. Error"]
z <- round(coef["period:experimental_journal", "z value"], 2)
p <- round(coef["period:experimental_journal", "Pr(>|z|)"], 3)

ci_low <- round(b3 - 1.96*se, 2)
ci_up <- round(b3 + 1.96*se, 2)

cat("b3 = ", b3_round, ", 95% CI = [", ci_low, "; ", ci_up, "], Z = ", z, ", p = ", p,
    sep = "")

### JESP WITHOUT 2019 ----------------------------------------------------------

# select relevant data
jesp_jpsp_no2019 <- data %>% 
  filter(journal %in% c("JESP", "JPSP"),
         !(journal == "JESP" & year_published == 2019))

# create dummy variable for journal
jesp_jpsp_no2019$experimental_journal <- 
  ifelse(jesp_jpsp_no2019$journal == "JESP", 1, 0)

# fit multilevel logistic model
lm_jesp_jpsp_no2019 <- glmer(error ~ period*experimental_journal + (1|source), 
                      data = jesp_jpsp_no2019, family = binomial(link = "logit"))

summary(lm_jesp_jpsp_no2019)

# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                 -3.08578    0.06543 -47.163  < 2e-16 ***
#   period                      -0.76082    0.17708  -4.297 1.73e-05 ***
#   experimental_journal         0.13611    0.07568   1.798   0.0721 .  
# period:experimental_journal -0.13719    0.21079  -0.651   0.5152    


