###############################
#Beta regression analysis
#Programmer: Miguel Acevedo
##################################

library(ggplot2)
library(lme4)
library(sjPlot)
library(sjmisc)
library(nlstools)
library(investr)
library(ggpubr)
library(betareg)
library(glmmTMB)
library(effects)
library(performance)
library(TMB)
library(boot)
library(truncnorm)

#Linear model

data <- read.csv("Dec30_Input_Output_ALL-1.csv",header=TRUE,
                 stringsAsFactors = TRUE) #input data from Walker provided in Dec24

head(data) #look at the data
names(data)

data$defit_prop=data$defit/100 #turn percentages into proportions
data$def50_prop=data$def50/100

#ensure groups are factors to use as RE
data$group=as.factor(data$group) 

#run model with polynomial deterministic function of degree 2
#beta distribution with logit link function (i.e., log (p/1-p))
mod_beta_re <- glmmTMB(defit_prop~def50_prop+I(def50_prop^2)+
                         (1|group),data=data,control = glmmTMBControl(rank_check = "adjust"),
                       family=beta_family(link="logit"))

#adding scale(size_km^2) does not significantly improve model fit

#Check model fit
r2(mod_beta_re)#conditional R^2 = 0.41

########################
#Visualization
#######################

#make model predictions
preds_re=as.data.frame(effect("def50_prop",mod_beta_re,xlevels=list(def50_prop=seq(0,1,by=0.01))))

#plot model predictions with 1:1 line representing H0
p_beta_re <- ggplot(preds_re, aes(x = def50_prop, y = fit)) +
  geom_line() +
  geom_smooth(aes(ymin = lower, ymax = upper), stat = "identity") +
  geom_point(data = data, aes(x = def50_prop, y = defit_prop, size = size_km2))+
  geom_abline(slope = 1, intercept = 0, linetype="dashed", color="blue")+
  xlab("Deforestation at 50km buffer")+
  ylab("Deforestation inside IT")+
  coord_cartesian(ylim = c(0,0.6))+
  theme_bw()

##########################################
#Deforestation predictions: Simulation
##########################################
set.seed(137)

source("neutral.R")
neutral_slope = coef(mod)[2]
neutral_sd = (summary(mod)$coefficients[,2][2])*sqrt(nobs(mod))

sim_list=list()

for (r in 1:100){
  
  nt = rtruncnorm(1,a=0,b=1,mean=neutral_slope,sd=neutral_sd)
  
  # Function to simulate from the fitted model
  nsim <- 1000  # number of simulations
  predictions <- simulate(mod_beta_re, nsim = nsim, seed = 137)
  
  # Calculate sum of differences for each simulation
  sum_diffs <- sapply(1:nsim, function(i) {
    sum((nt*data$def50_prop-predictions[[i]])*data$size_km2)
  })
  
  sim_list[[r]]=sum_diffs
  
  # Calculate confidence intervals from simulations
  ci <- quantile(sum_diffs, probs = c(0.025, 0.975))
  
  # Get point estimate
  fitted_vals <- predict(mod_beta_re, type = "response")
  point_estimate <- sum((nt*data$def50_prop-fitted_vals)*data$size_km2)
  
  
}

neutral_sims=numeric(0)

for (u in 1:length(sim_list)){
  
  neutral_sims=c(neutral_sims,sim_list[[u]])
  
}

mean_sim = mean(neutral_sims)
ci_sim <- quantile(neutral_sims, probs = c(0.025, 0.975))

# Print results
print(paste("Sum of differences:", round(mean_sim, 2)))
print(paste("95% CI:", round(ci_sim[1], 2), "to", round(ci_sim[2], 2)))

######################################
#Deforestation predictions: Bootstrap
#####################################
#nt=0.77 +/- 0.66

boot_list = list()

for (q in 1:100){
  
  # Method 2: Using non-parametric bootstrap
  boot_func <- function(data, indices) {
    # Resample data
    d <- data[indices,]
    
    # Fit model to resampled data
    tryCatch({
      mod <- glmmTMB(defit_prop~def50_prop+I(def50_prop^2)+
                       (1|group),data=data,control = glmmTMBControl(rank_check = "adjust"),
                     family=beta_family(link="logit"))
      pred <- predict(mod, type = "response")
      return(sum((rtruncnorm(1,a=0,b=1,mean=neutral_slope,sd=neutral_sd)*d$def50_prop - pred)*d$size_km2))
    }, error = function(e) NA)
  }
  
  # Perform bootstrap
  group_ids <- data$group
  boot_results <- boot(data = data, 
                       statistic = boot_func, 
                       R = 1000,
                       strata = group_ids)
  
  # Calculate bootstrap confidence intervals
  boot_ci <- boot.ci(boot_results, type = "basic")
  
  boot_list[[q]]=boot_results
  
  print(q)
  
}

neutral_boosts=numeric(0)

for (u in 1:length(boot_list)){
  
  neutral_boosts=c(neutral_boosts,boot_list[[u]]$t)
  
}

mean_neutral_boosts=mean(neutral_boosts)
median_neutral_boosts = median(neutral_boosts)
ci_boost <- quantile(neutral_boosts, probs = c(0.025, 0.975))

print(paste("Sum of differences:", round(mean_neutral_boosts, 2)))
print(paste("95% CI:", round(ci_boost[1], 2), "to", round(ci_boost[2], 2)))


