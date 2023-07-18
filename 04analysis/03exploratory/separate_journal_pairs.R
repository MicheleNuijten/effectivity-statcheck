rm(list = ls())

# load packages
library(tidyverse) 

# LOAD DATA --------------------------------------------------------------------

data <- read.table("03data/2023-01-06data_wrangled_no_missings.txt", header = TRUE)
data_per_article <- read.table("03data/2023-01-06data_per_article_with_stats.txt", header = TRUE)

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
#   Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)                 -3.15112    0.06361 -49.536  < 2e-16 ***
#   period                      -0.19414    0.09652  -2.011   0.0443 *  
#   experimental_journal         0.13252    0.07562   1.753   0.0797 .  
#   period:experimental_journal -0.91734    0.14264  -6.431 1.27e-10 ***
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
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                  -8.9835     0.3154 -28.484   <2e-16 ***
# period                       -0.2307     0.3460  -0.667    0.505    
# experimental_journal          0.3456     0.2657   1.301    0.193    
# period:experimental_journal  -0.8003     0.5258  -1.522    0.128 

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
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                 -3.08564    0.06530 -47.254  < 2e-16 ***
#   period                      -0.76077    0.17671  -4.305 1.67e-05 ***
#   experimental_journal         0.13615    0.07555   1.802   0.0715 .  
# period:experimental_journal -0.10791    0.20989  -0.514   0.6072

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
#   Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)                  -6.2595     0.2062 -30.350   <2e-16 ***
#   period                       -0.9995     0.4248  -2.353   0.0186 *  
#   experimental_journal          0.1160     0.1659   0.699   0.4846    
#   period:experimental_journal  -0.3556     0.5216  -0.682   0.4953 

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


