#######################################################################
###    Power analysis for the effectiveness of statcheck project    ###
#######################################################################

rm(list=ls())

library(lme4)
library(reshape2)

# The model: 
# Logit[inconsistency=1|Time, statcheckJournal, Time*statcheckJournal] = 
# b0 + b1*Time + b2*statcheckJournal + b3*Time*statcheckJournal

# load data on which effect size estimates are based
# perform the regression analyses needed for the effect size estimates

######################################################################
############################# LOAD DATA ##############################
######################################################################

# LOAD STATCHECK DATA OF JOURNAL ARTICLES FROM T1
# THESE DATA WERE OBTAINED BY RUNNING THE scrape_html_dir.R SCRIPT
# WE NEED THESE DATA TO ESTABLISH A BASELINE OF THE ERRORS
data <- read.table("03data/2022-11-28data_wrangled.txt", header=TRUE, na.strings="NA")

# load data organized on article level
# see data_per_article_Automatic1Tail.R to see how this data set was created
data_per_article <- read.table("03data/2022-11-28data_per_article.txt", header = TRUE)

######################################################################
####################### ESTIMATE B0, B1, AND B2 ######################
######################################################################

# ESTIMATE B0 and B2

# b0 indicates the logit of inconsistencies in non-statcheck journals before 
# in period 0 (before statcheck was introduced in the other journals). At the
# time of preregistration, we did not yet make a distinction between the 
# different article categories in JPSP (we are using parts of an old sample), so
# b0 is an approximation

# data subset
data_baseline <- subset(data, period == 0)

(glm_baseline <- glmer(error ~ statcheck_journal + (1|source),
                       data = data_baseline, 
                       family = binomial(link = 'logit')))

# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: error ~ statcheck_journal + (1 | source)
# Data: data_baseline
# AIC       BIC    logLik  deviance  df.resid 
# 45254.06  45281.55 -22624.03  45248.06     70652 
# Random effects:
#   Groups Name        Std.Dev.
# source (Intercept) 1.155   
# Number of obs: 70655, groups:  source, 3536
# Fixed Effects:
#   (Intercept)  statcheck_journal  
# -2.52835            0.03863  


(b0_logit <- coef(summary(glm_baseline))[1])
# -2.528355

(b2_logit <- coef(summary(glm_baseline))[2])
# 0.03863407

#-------------------------------------------------------------------

# ESTIMATE B1

# based on the statcheck paper, we estimate the trend in the 5 APA journals to
# be b = -.11 (also following study 2 of Nuijten et al. 2017, Collabra)

b1_logit <- -.11

######################################################################
############################ ESTIMATE B3 #############################
######################################################################

# Try out different values of b3 to see the effect on power

#---------------------------------------------------------------------

# b3 based on an error probability equal PLOS in T0 (no effect)

b3_logit_0 <- 0

#---------------------------------------------------------------------

# b3 based on an error probability of .01
# this reflects an extreme confirmation of the hypothesis 
# that the error prevalence should have gone down after the open data policy

b3_logit_p.01 <- log(.01/.99)-(b0_logit+b1_logit+b2_logit)
b3_logits <- seq(b3_logit_0, b3_logit_p.01, length = 10)

######################################################################
########################### ESTIMATE T^2 #############################
######################################################################

# estimate variance in intercepts

summary(glm_baseline)

# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: error ~ statcheck_journal + (1 | source)
# Data: data_baseline
# 
# AIC      BIC   logLik deviance df.resid 
# 45254.1  45281.6 -22624.0  45248.1    70652 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -3.2798 -0.3440 -0.2485 -0.1854  7.2218 
# 
# Random effects:
#   Groups Name        Variance Std.Dev.
# source (Intercept) 1.333    1.155   
# Number of obs: 70655, groups:  source, 3536
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)       -2.52835    0.03546 -71.294   <2e-16 ***
#   statcheck_journal  0.03863    0.05546   0.697    0.486    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr)
# sttchck_jrn -0.557

t2_logit <- 1.33


######################################################################
####################### DEFINE NUMBER OF ARTICLES ####################
######################################################################

# The number of available articles in period 0 is known based on the previous
# sample that has been downloaded
# The number of available articles in period 1 is estimated

# function to list all articles in a directory
count_articles <- function(dir){
  subdirs <- list.dirs(dir)
  nr_files <- rep(NA, length(subdirs))
  
  for(subdir in seq(subdirs)){
    journal_year_dir <- paste(dir, subdirs[subdir], sep = "/")
    
    nr_files[subdir] <- length(list.files(journal_year_dir, pattern = "*.htm"))
  }
  
  nr_downloads <- sum(subdir, na.rm = TRUE)
  
  return(nr_downloads)
  
}

# JEPG 2003-2013
(nr_downloads_JEPG_0 <- count_articles("./01articles/JEPG"))
# 618

# JPSP 2003-2013
# for this study we will only include JPSP articles in the subsection ASC, which
# we estimate will be 1/3 of the previously downloaded sample 
(nr_downloads_JPSP_0 <- round((1/3) * count_articles("./01articles/JPSP"), 0))
# 526

# PS 2003-2013
(nr_downloads_PS_0 <- count_articles("./01articles/PS"))
# 2319

# JESP 2003-2013: we don't have these articles yet, so this is an estimate
# based on the number of articles in 2003: 61; extrapolate over 10 years
(nr_downloads_JESP_0 <- 61*10)
# 610

# for the number of articles in time period 1, we extrapolate based on the nr
# of articles per year in period 0; 2014 - oct 2022 = 8.8 years
(nr_downloads_JEPG_1 <- round(nr_downloads_JEPG_0/10*8.8, 0))
# 544
(nr_downloads_JPSP_1 <- round(nr_downloads_JPSP_0/10*8.8, 0))
# 463
(nr_downloads_PS_1 <- round(nr_downloads_PS_0/10*8.8, 0))
#2041
(nr_downloads_JESP_1 <- round(nr_downloads_JESP_0/10*8.8, 0))
#537

# add up journal categories for power analysis
(nr_downloads_statcheck_before <- sum(nr_downloads_PS_0, nr_downloads_JESP_0))
#2929
(nr_downloads_statcheck_after <- sum(nr_downloads_PS_1, nr_downloads_JESP_1))
# 2578
(nr_downloads_control_before <- sum(nr_downloads_JEPG_0, nr_downloads_JPSP_0))
# 1144
(nr_downloads_control_after <- sum(nr_downloads_JEPG_1, nr_downloads_JPSP_1))
# 1007

# To estimate the number of articles in which statcheck will be able to detect 
# statistics, we use the data from (Nuijten et al., 2016), in which statcheck 
# was used to check a large number of statistics in eight major psychology 
# journals, among which were PS, JEPG, and JPSP. Statcheck detected statistics 
# in 72.9% of PS articles, 69.3% of JEPG articles, and 85.1% of JPSP articles. 
# We averaged these percentages to estimate the percentage of JESP articles in 
# which statcheck would detect statistics (75.8%). We then used these 
# percentages to estimate the total number of articles statcheck will detect 
# results in, in this study. 
(nr_articles_PS_0 <- round(.729*nr_downloads_PS_0, 0))
(nr_articles_PS_1 <- round(.729*nr_downloads_PS_1, 0))
(nr_articles_JEPG_0 <- round(.693*nr_downloads_JEPG_0, 0))
(nr_articles_JEPG_1 <- round(.693*nr_downloads_JEPG_1, 0))
(nr_articles_JESP_0 <- round(.758*nr_downloads_JESP_0, 0))
(nr_articles_JESP_1 <- round(.758*nr_downloads_JESP_1, 0))
(nr_articles_JPSP_0 <- round(.851*nr_downloads_JPSP_0, 0))
(nr_articles_JPSP_1 <- round(.851*nr_downloads_JPSP_1, 0))

# add up journal categories for power analysis
(nr_articles_statcheck_before <- sum(nr_articles_PS_0, nr_articles_JESP_0))
(nr_articles_statcheck_after <- sum(nr_articles_PS_1, nr_articles_JESP_1))
(nr_articles_control_before <- sum(nr_articles_JEPG_0, nr_articles_JPSP_0))
(nr_articles_control_after <- sum(nr_articles_JEPG_1, nr_articles_JPSP_1))

#---------------------------------------------------------------------

# frequency distribution of the number of p-values within an article
# based on statcheck data of PS, JEPG and JPSP

nr_nhst_statcheck <- 
  data_per_article[data_per_article$statcheck_journal == 1, "nr_nhst"]
hist(nr_nhst_statcheck)
median(nr_nhst_statcheck)

nr_nhst_control <- 
  data_per_article[data_per_article$statcheck_journal == 0, "nr_nhst"]
hist(nr_nhst_control)
median(nr_nhst_control)

######################################################################
############################ RUN SIMULATION ##########################
######################################################################

reps <- 100

names_conditions <- c("control_before", "control_after", 
                      "statcheck_before","statcheck_after")
nr_articles <- c(nr_articles_control_before,
                 nr_articles_control_after,
                 nr_articles_statcheck_before,
                 nr_articles_statcheck_after)

# create empty matrices to store estimated regression coefficients in
b0_est <- b1_est <- b2_est <- b3_est <- numeric()

# create empty matrices to store estimated p values of the b coefficients in
p0_est <- p1_est <- p2_est <- p3_est <- numeric()

# create empty vector to store power in
power <- numeric()

#---------------------------------------------------------------------

# time the loop
start.time <- proc.time()

for (b3 in 1:length(b3_logits)){
  
  for (rep in 1:reps){
    
    # time every repetition
    rep.time <- proc.time()
    
    # empty lists to store inconsistencies per article in
    incons00 <- matrix(numeric(), nr_articles[1], max(nr_nhst_control))
    incons10 <- matrix(numeric(), nr_articles[2], max(nr_nhst_control))
    incons01 <- matrix(numeric(), nr_articles[3], max(nr_nhst_statcheck))
    incons11 <- matrix(numeric(), nr_articles[4], max(nr_nhst_statcheck))
    
    # estimate logits for articles in one of the four conditions
    # transform logits to probabilities
    # draw a paper with N results
    # calculate the number of inconsistencies in it
    
    for (k in 1:length(nr_articles)){
      
      if(names_conditions[k] == "control_before"){
        for(i in 1:nr_articles[k]){
          # draw random effect for article
          t2 <- rnorm(1, mean = 0, sd = sqrt(t2_logit))
          
          # estimate probability that p-value is an error
          y00 <- b0_logit + t2 
          p00 <- exp(y00)/(1+exp(y00))
          
          # draw the number of p-values in the specific article
          nr_nhst <- sample(nr_nhst_control, 1)
          
          # combine with probability of an error: how many errors?
          p_inconsistent <- rbinom(n = nr_nhst, size = 1, prob = p00)
          incons00[i, 1:length(p_inconsistent)] <- p_inconsistent
        }
      }
      
      if(names_conditions[k]=="control_after"){
        for(i in 1:nr_articles[k]){
          # draw random effect for article
          t2 <- rnorm(1, mean = 0, sd = sqrt(t2_logit))
          
          # estimate probability that a p-value is an error
          y10 <- b0_logit + b1_logit + t2
          p10 <- exp(y10)/(1+exp(y10))
          
          # draw the number of p-values in the specific article
          nr_nhst <- sample(nr_nhst_control,1)
          
          # combine with probability of an error: how many errors
          p_inconsistent <- rbinom(n = nr_nhst, size = 1, prob = p10)
          incons10[i, 1:length(p_inconsistent)] <- p_inconsistent
        }
      }
      
      if(names_conditions[k]=="statcheck_before"){
        for(i in 1:nr_articles[k]){
          # draw random effect for article
          t2 <- rnorm(1, mean = 0, sd = sqrt(t2_logit))

          # estimate probability that p-value is an error
          y01 <- b0_logit + b2_logit + t2
          p01 <- exp(y01)/(1+exp(y01))
          
          # draw the number of p-values in the specific article
          nr_nhst <- sample(nr_nhst_statcheck, 1)
          
          # combine with probability of an error: how many errors?
          p_inconsistent <- rbinom(n= nr_nhst, size = 1, prob = p01)
          incons01[i, 1:length(p_inconsistent)] <- p_inconsistent
        }
      }
      
      if(names_conditions[k]=="statcheck_after"){
        for(i in 1:nr_articles[k]){
          # draw random effect for article
          t2 <- rnorm(1, mean = 0, sd = sqrt(t2_logit))
          
          # estimate probability that a p-value is an error
          y11 <- b0_logit + b1_logit + b2_logit + b3_logits[b3] + t2
          p11 <- exp(y11)/(1+exp(y11))
          
          # draw number of p-values in specific article
          nr_nhst <- sample(nr_nhst_statcheck, 1)
          
          # combine with probability of an error: how many errors
          p_inconsistent <- rbinom(n = nr_nhst, size = 1, prob = p11)
          incons11[i, 1:length(p_inconsistent)] <- p_inconsistent
        }
      }
      
    }
    
    #---------------------------------------------------------------------
    
    # melt the four data frames
    incons00_melt <- melt(as.data.frame(t(incons00)))
    colnames(incons00_melt) <- c("article","p_inconsistent")
    incons00_melt <- incons00_melt[!is.na(incons00_melt$p_inconsistent),]
    incons00_melt <- cbind(articleID = paste0(0,0,incons00_melt$article),
                           incons00_melt,
                           period = rep(0,nrow(incons00_melt)),
                           statcheck_journal = rep(0,nrow(incons00_melt)))
    
    incons10_melt <- melt(as.data.frame(t(incons10)))
    colnames(incons10_melt) <- c("article","p_inconsistent")
    incons10_melt <- incons10_melt[!is.na(incons10_melt$p_inconsistent),]
    incons10_melt <- cbind(articleID = paste0(1,0,incons10_melt$article),
                           incons10_melt,
                           period = rep(1,nrow(incons10_melt)),
                           statcheck_journal = rep(0,nrow(incons10_melt)))
    
    incons01_melt <- melt(as.data.frame(t(incons01)))
    colnames(incons01_melt) <- c("article","p_inconsistent")
    incons01_melt <- incons01_melt[!is.na(incons01_melt$p_inconsistent),]
    incons01_melt <- cbind(articleID = paste0(0,1,incons01_melt$article),
                           incons01_melt,
                           period = rep(0,nrow(incons01_melt)),
                           statcheck_journal = rep(1,nrow(incons01_melt)))
    
    incons11_melt <- melt(as.data.frame(t(incons11)))
    colnames(incons11_melt) <- c("article","p_inconsistent")
    incons11_melt <- incons11_melt[!is.na(incons11_melt$p_inconsistent),]
    incons11_melt <- cbind(articleID = paste0(1,1,incons11_melt$article),
                           incons11_melt,
                           period = rep(1,nrow(incons11_melt)),
                           statcheck_journal = rep(1,nrow(incons11_melt)))
    
    # combine data frames into one
    data <- rbind(incons00_melt,
                  incons10_melt,
                  incons01_melt,
                  incons11_melt)
    
    #---------------------------------------------------------------------
    
    # fit logistic multilevel model on data to estimate b coefficients
    lm <- glmer(p_inconsistent ~ period*statcheck_journal + (1|articleID), data=data,
                family=binomial(link='logit'))
    
    # retrieve b coefficients
    b0_est[rep] <- coef(summary(lm))["(Intercept)","Estimate"]
    b1_est[rep] <- coef(summary(lm))["period","Estimate"]
    b2_est[rep] <- coef(summary(lm))["statcheck_journal","Estimate"]
    b3_est[rep] <- coef(summary(lm))["period:statcheck_journal","Estimate"]
    
    # retrieve p values of the b coefficients
    p0_est[rep] <- coef(summary(lm))["(Intercept)","Pr(>|z|)"]
    p1_est[rep] <- coef(summary(lm))["period","Pr(>|z|)"]
    p2_est[rep] <- coef(summary(lm))["statcheck_journal","Pr(>|z|)"]
    p3_est[rep] <- coef(summary(lm))["period:statcheck_journal","Pr(>|z|)"]
    
    # time each repetition
    end.rep.time <- proc.time()
    total.rep.time <- end.rep.time-rep.time
    
    cat("b3 =",b3,"; rep =", rep,"; time elapsed =",total.rep.time[3]/60,"minutes","\n")
    
  }
  
  
  #---------------------------------------------------------------------
  
  # calculate power for each k articles for coefficient b3
  power[b3] <- sum(p3_est<.05)/reps
  
  
  ### write away files
  write.table(p3_est, 
              paste0("02preregistration/", Sys.Date(), "power_b3=", b3, "_p3_est.txt"), 
              row.names = FALSE)
  write.table(b3_est, 
              paste0("02preregistration/", Sys.Date(), "power_b3", b3, "_b3_est.txt"), 
              row.names = FALSE)
  write.table(power, paste0("02preregistration/", Sys.Date(), "power_b3", b3, "_power.txt"),
              row.names = FALSE)
  
}

# time loop
end.time <- proc.time()
total.time <- end.time-start.time


#---------------------------------------------------------------------

# total probability of an inconsistency
logits <- b0_logit+b1_logit+b2_logit+b3_logits
exp(logits)/(1+exp(logits))


plot(b3_logits, power, type="b", xlab="b3(logit)", ylab="power")
abline(h=.8, lty=2)

