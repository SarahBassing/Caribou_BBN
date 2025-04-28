  #'  ---------------------------------
  #'  Making up stuff for caribou BBN
  #'  April 2025
  #'  Bassing
  #'  ---------------------------------

  #'  Load packages
  library(tidyverse)
  library(purrr)
  library(ggplot2)
  
  #'  Define levels for source pop N and AM phi
  abund.sourcePop <- seq(10, 200, by = 20)
  survival.AM <- c(1, 2, 3)
  #'  Create data frame containing full cross of all parent node levels
  df <- expand.grid(source.N = abund.sourcePop, survival.M = survival.AM) %>%
    tidyr::expand(source.N, survival.M) %>%
    mutate(source.N = factor(source.N),
           survival.M = factor(survival.M))
  
  #'  Define coefficients
  # alpha <- 6
  # beta.source <- -0.7 
  # beta.am.phi <- -0.7 
  # (df$y <- 1 / (1 + exp(alpha + beta.source*as.numeric(df$source.N) + beta.am.phi*as.numeric(df$survival.M))))
  alpha <- -8#-6
  beta.source <- 1#0.7 
  beta.am.phi <- 0.6#0.5 
  (df$y <- 1 / (1 + exp(-(alpha + beta.source*as.numeric(df$source.N) + beta.am.phi*as.numeric(df$survival.M)))))
  
  #'  Plot 
  ggplot(data = df, aes(x = source.N, y = y, group = survival.M)) + 
    geom_line(aes(color = survival.M)) +
    xlab("Source population abundance")+
    ylab("Pr(Adult male abundance = High)")+
    guides(color = guide_legend(title = "Adult male \nsurvival"))+
    scale_color_manual(values = c("red", "blue", "green"),
                       labels = c('Low', 'Medium', 'High'))
  
  ####  Conditional Probability Tables for each node  ####
  
  #####  Abundance_adultMales  #####
  #'  Predict probability of male abundance being low vs high under different 
  #'  levels of adult male survival (low, moderate, high) and the size of the 
  #'  source population (and thus individuals available for augmentation)
  AM_abundance <- function(AM.phi, phi.level) {  #SourceN, a, b1, b2, 
    #'  Holding AM.survival at a fixed value (level of male survival)
    AM.survival <- AM.phi 
    #'  Population size of source population
    N.sourcePop <- seq(10, 200, by = 20)
    #'  Center and scale N.sourcePop so values don't range too widely
    N.sourcePopz <- scale(N.sourcePop)
    
    #'  Define intercept and slope coefficients (that create hypothesized shape
    #'  of each probability curve)
    alpha <- 2 # Intercept for low abundance category 
    beta1 <- -2 # Slope coefficient for abundance of source population   
    beta2 <- -1 # Slope coefficient for adult male survival
    
    #'  Calculate probability of adult male abundance being low vs high given 
    #'  varying sizes of the source population and levels of adult male survival
    (p.AMnLo <- 1/(1+exp(-(alpha + beta1*N.sourcePopz + beta2*AM.survival))))
    #'  Probability of adult male abundance being high given low male survival over 
    #'  a range of population sizes of the source population
    p.AMnHi <- 1 - p.AMnLo
    
    #'  Create data frame with probabilities of adult male abundance being low vs high
    (p.AMn <- cbind(p.AMnLo, p.AMnHi))
    #'  Add source population covariate data to data frame and 
    df <- data.frame(N.sourcePop = N.sourcePop, phi = phi.level, p1 = p.AMnLo, p2 = p.AMnHi) 
    #'  Reformat data frame for easier plotting
    df_plot <- df %>% dplyr::select(-phi) %>%
      pivot_longer(cols = c('p1','p2'), names_to = "p", values_to = "prob")
    #'  Plot probability of adult male population being low or high, given adult
    #'  male survival is low, moderate, or high, across a range of source population sizes
    prediction_plot <- ggplot(df_plot, aes(x = N.sourcePop, y = prob)) + 
      ylim(0, 1)+
      geom_line(aes(color = p)) +
      xlab("Source population size")+
      ylab("Prob(Adult male N)")+
      ggtitle(paste("Adult male abundance when survival is", phi.level)) +
      theme(
        legend.position = "top",
        legend.justification = c("left"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
      scale_color_manual(name = '', labels = c('Low N', 'High N'),
                         values = c('red', 'blue', 'green'))
    
    #'  Plot relationship
    plot(prediction_plot)
    #'  Return predictions
    return(df)
    
  }
  #'  Calculate probability of adult male abundance being low or high given 
  #'  adult male survival is low (1), moderate (2), or high (3)
  p.AMn.phiLo <- AM_abundance(AM.phi = 1, phi.level = "low") #SourceN = seq(10, 200, by = 20), a = 2, b1 = -2, b2 = -1, 
  p.AMn.phiMod <- AM_abundance(AM.phi = 2, phi.level = "moderate")
  p.AMn.phiHi <- AM_abundance(AM.phi = 3, phi.level = "high")
  p.AMn <- bind_rows(p.AMn.phiLo, p.AMn.phiMod, p.AMn.phiHi) %>%
    mutate(phi = ifelse(phi == "low", "Low", phi),
           phi = ifelse(phi == "moderate", "Medium", phi),
           phi = ifelse(phi == "high", "High", phi),
           phi = factor(phi, levels = c("Low", "Medium", "High"))) %>%
    arrange(N.sourcePop, phi) 
  names(p.AMn) <- c("Abundance_sourcePop", "Survival_adultMale", "Low", "High")
  head(p.AMn)
  write_csv(p.AMn, "./Conditional_Probability_Tables/CPT_Abundance_adultMales.csv")
  
  
  #####  Abundance_adultFemales  #####
  #'  Predict probability of female abundance being low vs high under different 
  #'  levels of adult female survival (low, moderate, high) and the size of the 
  #'  source population (and thus individuals available for augmentation)
  AF_abundance <- function(AF.phi, phi.level) {  
    #'  Holding AF.survival at a fixed value (level of female survival)
    AF.survival <- AF.phi 
    #'  Population size of source population
    N.sourcePop <- seq(10, 200, by = 20)
    #'  Center and scale N.sourcePop so values don't range too widely
    N.sourcePopz <- scale(N.sourcePop)
    
    #'  Define intercept and slope coefficients (that create hypothesized shape
    #'  of each probability curve)
    alpha <- 2 # Intercept for low abundance category 
    beta1 <- -2 # Slope coefficient for abundance of source population   
    beta2 <- -1 # Slope coefficient for adult male survival
    
    #'  Calculate probability of adult female abundance being low vs high given 
    #'  varying sizes of the source population and levels of adult female survival
    (p.AFnLo <- 1/(1+exp(-(alpha + beta1*N.sourcePopz + beta2*AF.survival))))
    #'  Probability of adult female abundance being high given low female survival 
    #'  over a range of population sizes of the source population
    p.AFnHi <- 1 - p.AFnLo
    
    #'  Create data frame with probabilities of adult female abundance being low vs high
    (p.AFn <- cbind(p.AFnLo, p.AFnHi))
    #'  Add source population covariate data to data frame and 
    df <- data.frame(N.sourcePop = N.sourcePop, phi = phi.level, p1 = p.AFnLo, p2 = p.AFnHi) 
    #'  Reformat data frame for easier plotting
    df_plot <- df %>% dplyr::select(-phi) %>%
      pivot_longer(cols = c('p1','p2'), names_to = "p", values_to = "prob")
    #'  Plot probability of adult female population being low or high, given adult
    #'  female survival is low, moderate, or high, across a range of source population sizes
    prediction_plot <- ggplot(df_plot, aes(x = N.sourcePop, y = prob)) + 
      ylim(0, 1)+
      geom_line(aes(color = p)) +
      xlab("Source population size")+
      ylab("Prob(Adult female N)")+
      ggtitle(paste("Adult female abundance when survival is", phi.level)) +
      theme(
        legend.position = "top",
        legend.justification = c("left"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
      scale_color_manual(name = '', labels = c('Low N', 'High N'),
                         values = c('red', 'blue', 'green'))
    
    #'  Plot relationship
    plot(prediction_plot)
    #'  Return predictions
    return(df)
    
  }
  #'  Calculate probability of adult male abundance being low or high given 
  #'  adult male survival is low (1), moderate (2), or high (3)
  p.AFn.phiLo <- AF_abundance(AF.phi = 1, phi.level = "low") 
  p.AFn.phiMod <- AF_abundance(AF.phi = 2, phi.level = "moderate")
  p.AFn.phiHi <- AF_abundance(AF.phi = 3, phi.level = "high")
  p.AFn <- bind_rows(p.AFn.phiLo, p.AFn.phiMod, p.AFn.phiHi) %>%
    mutate(phi = ifelse(phi == "low", "Low", phi),
           phi = ifelse(phi == "moderate", "Medium", phi),
           phi = ifelse(phi == "high", "High", phi),
           phi = factor(phi, levels = c("Low", "Medium", "High"))) %>%
    arrange(N.sourcePop, phi) 
  names(p.AFn) <- c("Abundance_sourcePop", "Survival_adultFemale", "Low", "High")
  head(p.AFn)
  write_csv(p.AFn, "./Conditional_Probability_Tables/CPT_Abundance_adultFemales.csv")
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ####  OLD CODE - PROBABLY DON'T NEED  ####
  #'  Define nature nodes
  climate.change <- c("Bad", "Worse", "We fucked")
  # hab.quality <- data.frame(score = c("Poor", "Moderate", "High"), hq = c(-0.8, 0.0, 0.8))
  hab.quality <- data.frame(hq = c(-0.8, 0.0, 0.8))
  abund.altPrey <- c("Low", "High")
  # abund.pred <- data.frame(score = c("Low", "High"), npred = c(2, 10))
  abund.pred <- data.frame(npred = c(2, 10))
  survival.AM <- c("Low", "Medium", "High")
  survival.AF <- c("Low", "Medium", "High")
  survival.C <- c("Low", "Medium", "High")
  fecund.AF <- c("Low", "Medium", "High")
  abund.sourcePop <- c("Low", "High")
  immigration <- c("None", "Low", "High")
  abund.AM <- c("Low", "High")
  abund.AF <- c("Low", "High")
  abund.C <- c("Low", "High")
  abund.adult <- c("Low", "High")
  abund.total <- c("Low", "High")
  lambda <- c("Decreasing", "Stable", "Increasing")
  prob.success <- seq(0,1,0.1)
  
  #'  Define decision nodes
  pred.control <- c("None", "Low", "High")
  # sup.feed <- data.frame(score = c("No", "Yes"), feed = c(1, 0))
  sup.feed <- data.frame(feed = c(1, 0))
  maternal.pen <- c("No", "Yes")
  captive.breed <- c("No", "Yes")  
  augment <- c("None", "Low", "High")
  
  #'  Define utility node
  utility <- c()
  
  #'  Assign values to each state
  set.seed(26)
  hq <- runif(100, -1, 1) #' generate 100 habitat quality values ranging -1 to 1
  npred <- rpois(100, 5) #' generate 100 predator abundances w/ avg n = 5
  feed <- rbinom(100, 1, 0.5) #' generate 100 random decisions to feed (1) or not (0)
  mean.phi <- 0.70 #' average expected adult male survival probability 
  beta0 <- qlogis(mean.phi)
  beta.hq <- 3 #' effect of habitat quality
  beta.npred <- -1 #' effect of predator abundance
  beta.feed <- 1 #' effect of supplemental feeding
  #' linear model
  logit.phi <- beta0 + beta.hq*hq + beta.npred*npred + beta.feed*feed
  phi.m <- plogis(logit.phi)
  #'  visualize effects of parent nodes on child node across range of values
  plot(hq, phi.m)
  plot(npred, phi.m)
  plot(feed, phi.m)
  #'  create data frame containing full cross of all parent node values
  newdf <- expand.grid(hq = hab.quality, npred = abund.pred, feed = sup.feed) %>%
    tidyr::expand(hq, npred, feed)
  #' compute expected phi for new data based on defined relationship of parent nodes on child node
  phi.new <- c(NA)
  for(i in 1:nrow(newdf)) {
    phi.new[i] <- plogis(beta0 + beta.hq*newdf$hq[i] + beta.npred*newdf$npred[i] + beta.feed*newdf$feed[i])
  }
  newdf$expected.phi <- round(phi.new,3)
  plot(newdf$hq, newdf$expected.phi)
  plot(newdf$npred, newdf$expected.phi)
  plot(newdf$feed, newdf$expected.phi)
  
  