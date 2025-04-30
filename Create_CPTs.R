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
  alpha <- -8
  beta.source <- 1 
  beta.am.phi <- 0.6
  (df$y <- 1 / (1 + exp(-(alpha + beta.source*as.numeric(df$source.N) + beta.am.phi*as.numeric(df$survival.M)))))
  
  #'  Plot 
  ggplot(data = df, aes(x = source.N, y = y, group = survival.M)) + 
    geom_line(aes(color = survival.M)) +
    xlab("Source population abundance")+
    ylab("Pr(Adult male abundance = High)")+
    guides(color = guide_legend(title = "Adult male \nsurvival"))+
    scale_color_manual(values = c("red", "blue", "green"),
                       labels = c('Low', 'Medium', 'High'))
  
  #'  ------------------------------------------------
  ####  Conditional Probability Tables for each node  ####
  #'  ------------------------------------------------
  
  #'  -------------------------
  #####  Habitat_availability  #####
  #'  -------------------------
  #'  Predict probability of available habitat across range of binned areas based
  #'  on different levels of expected climate change scenarios
  
  #'  ----------------------
  #####  Abundance_altPrey  #####
  #'  ----------------------
  #'  Predict probability of low, medium, or high abundances of alternative prey
  #'  sources (e.g., deer, elk, moose) under varying levels of habitat availability
  
  #'  ------------------------
  #####  Abundance_predators  #####
  #'  ------------------------
  #'  Predict probability of low, medium, or high abundances of predators (e.g., wolf, 
  #'  mountain lion, bear) based on whether alternative prey populations are rare (0) 
  #'  or abundant (1) and whether predator control does not (0) or does (1) occur
  
  
  #'  ------------------------
  #####  Survival_adultMales  #####
  #'  ------------------------
  #'  Predict probability of male survival being low, medium, or high under 
  #'  different levels of predator abundance (low or high) and whether supplemental 
  #'  feeding occurs (yes or no) over a range of available suitable habitat
  AM_survival <- function(N.pred, Feeding, pred.level, feed.level) {   
    #'  Habitat availability (amount of suitable habitat in sq-km)
    habitat.avail <- seq(0, 500, by = 50)
    #'  Center and scale habitat.avail so values don't range too widely
    habitat.availz <- scale(habitat.avail)
    #'  Holding Abundance_predators at a fixed level 
    N.predator <- N.pred
    #'  Holding Supplemental feeding at a fixed level
    Supp.feeding <- Feeding
    
    #'  Define intercept and slope coefficients 
    #'  H: Availability of suitable habitat most important, predators second most,
    #'  supplemental feeding doesn't really matter for adult male survival
    alpha <- c(0.25, 2) # Intercepts for low and low/moderate survival categories
    beta1 <- -2 # Slope coefficient for habitat availability  
    beta2 <- 0.5 # Slope coefficient for predator abundance
    beta3 <- -0.25 # Slope coefficient for supplemental feeding
    
    #'  Calculate probability of adult male survival being low, moderate, or high 
    #'  given varying amounts of available habitat, levels of predator abundance, 
    #'  and whether supplemental feeding occurs
    (p.AMphiLo <- 1/(1 + exp(-(alpha[1] + beta1*habitat.availz + beta2*N.predator + beta3*Supp.feeding))))
    #'  Probability of being in low or moderate survival category given all of the above
    (p.AMphiLoMod <- 1/(1 + exp(-(alpha[2] + beta1*habitat.availz + beta2*N.predator + beta3*Supp.feeding))))
    #'  Probability of being in moderate survival category given all of the above
    (p.AMphiMod <- p.AMphiLoMod - p.AMphiLo)
    #'  Probability of being in high survival category given all of the above
    (p.AMphiHi <- 1 - (p.AMphiLoMod))
    
    #'  Create data frame with probabilities of adult male survival being low, medium, or high
    (p.AMphi <- cbind(p.AMphiLo, p.AMphiMod, p.AMphiHi))
    #'  Add habitat availability covariate data to data frame  
    df <- data.frame(habitat.avail = habitat.avail, pred = pred.level, food = feed.level, 
                     p1 = p.AMphiLo, p2 = p.AMphiMod, p3 = p.AMphiHi) 
    #'  Reformat data frame for easier plotting
    df_plot <- df %>% dplyr::select(-c(pred, food)) %>%
      pivot_longer(cols = c('p1','p2', 'p3'), names_to = "p", values_to = "prob")
    #'  Plot probability of adult male survival being low, medium, or high, given
    #'  habitat availability, predator abundance, and supplemental feeding
    prediction_plot <- ggplot(df_plot, aes(x = habitat.avail, y = prob)) + 
      ylim(0, 1)+
      geom_line(aes(color = p)) +
      xlab("Habitat suitability (sq-km)")+
      ylab("Prob(Adult male survival)")+
      ggtitle(paste("Adult male survival", feed.level, "suppemental feeding and predators are", pred.level)) +
      theme(
        legend.position = "top",
        legend.justification = c("left"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
      scale_color_manual(name = '', labels = c('Low phi', 'Moderate phi', 'High phi'),
                         values = c('red', 'blue', 'green'))
    
    #'  Plot relationship
    plot(prediction_plot)
    #'  Return predictions
    return(df)
    
  }
  #'  Calculate probability of adult male survival being low, moderate, or high given 
  #'  predators are rare (1) or abundant (2) and feeding does not (0) or does (1) occur
  p.AMphi.predLo.noFeed <- AM_survival(N.pred = 1, Feeding = 0, pred.level = "rare", feed.level = "without")
  p.AMphi.predLo.wFeed <- AM_survival(N.pred = 1, Feeding = 1, pred.level = "rare", feed.level = "with")
  p.AMphi.predHi.noFeed <- AM_survival(N.pred = 2, Feeding = 0, pred.level = "abundant", feed.level = "without")
  p.AMphi.predHi.wFeed <- AM_survival(N.pred = 2, Feeding = 1, pred.level = "abundant", feed.level = "with")
  p.AMphi <- bind_rows(p.AMphi.predLo.noFeed, p.AMphi.predLo.wFeed, p.AMphi.predHi.noFeed, p.AMphi.predHi.wFeed) %>%
    mutate(pred = ifelse(pred == "rare", "Low", pred),
           pred = ifelse(pred == "abundant", "High", pred),
           pred = factor(pred, levels = c("Low", "High")),
           food = ifelse(food == "without", "No", food),
           food = ifelse(food == "with", "Yes", food),
           food = factor(food, levels = c("No", "Yes"))) %>%
    arrange(habitat.avail, pred, food) 
  names(p.AMphi) <- c("Habitat_availability", "Aubdance_predators", "Supplemental_feeding", "Low", "Medium", "High")
  head(p.AMphi)
  write_csv(p.AMphi, "./Conditional_Probability_Tables/CPT_Survival_adultMales.csv")
  
  #'  --------------------------
  #####  Survival_adultFemales  #####
  #'  --------------------------
  #'  Predict probability of female survival being low, medium, or high under 
  #'  different levels of predator abundance (low or high) and whether supplemental 
  #'  feeding occurs (yes or no) over a range of available suitable habitat
  AF_survival <- function(N.pred, Feeding, pred.level, feed.level) {   
    #'  Habitat availability (amount of suitable habitat in sq-km)
    habitat.avail <- seq(0, 500, by = 50)
    #'  Center and scale habitat.avail so values don't range too widely
    habitat.availz <- scale(habitat.avail)
    #'  Holding Abundance_predators at a fixed level 
    N.predator <- N.pred
    #'  Holding Supplemental feeding at a fixed level
    Supp.feeding <- Feeding
    
    #'  Define intercept and slope coefficients 
    #'  H: Availability of suitable habitat most important, predators second most,
    #'  supplemental feeding third most for adult female survival
    alpha <- c(0.3, 3) # Intercepts for low and low/moderate survival categories
    beta1 <- -2.25 # Slope coefficient for habitat availability  
    beta2 <- 0.85 # Slope coefficient for predator abundance
    beta3 <- -0.75 # Slope coefficient for supplemental feeding
    
    #'  Calculate probability of adult female survival being low, moderate, or high 
    #'  given varying amounts of available habitat, levels of predator abundance, 
    #'  and whether supplemental feeding occurs
    (p.AFphiLo <- 1/(1 + exp(-(alpha[1] + beta1*habitat.availz + beta2*N.predator + beta3*Supp.feeding))))
    #'  Probability of being in low or moderate survival category given all of the above
    (p.AFphiLoMod <- 1/(1 + exp(-(alpha[2] + beta1*habitat.availz + beta2*N.predator + beta3*Supp.feeding))))
    #'  Probability of being in moderate survival category given all of the above
    (p.AFphiMod <- p.AFphiLoMod - p.AFphiLo)
    #'  Probability of being in high survival category given all of the above
    (p.AFphiHi <- 1 - (p.AFphiLoMod))
    
    #'  Create data frame with probabilities of adult female survival being low, medium, or high
    (p.AFphi <- cbind(p.AFphiLo, p.AFphiMod, p.AFphiHi))
    #'  Add habitat availability covariate data to data frame  
    df <- data.frame(habitat.avail = habitat.avail, pred = pred.level, food = feed.level, 
                     p1 = p.AFphiLo, p2 = p.AFphiMod, p3 = p.AFphiHi) 
    #'  Reformat data frame for easier plotting
    df_plot <- df %>% dplyr::select(-c(pred, food)) %>%
      pivot_longer(cols = c('p1','p2', 'p3'), names_to = "p", values_to = "prob")
    #'  Plot probability of adult female survival being low, medium, or high, given
    #'  habitat availability, predator abundance, and supplemental feeding
    prediction_plot <- ggplot(df_plot, aes(x = habitat.avail, y = prob)) + 
      ylim(0, 1)+
      geom_line(aes(color = p)) +
      xlab("Habitat suitability (sq-km)")+
      ylab("Prob(Adult female survival)")+
      ggtitle(paste("Adult female survival", feed.level, "suppemental feeding and predators are", pred.level)) +
      theme(
        legend.position = "top",
        legend.justification = c("left"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
      scale_color_manual(name = '', labels = c('Low phi', 'Moderate phi', 'High phi'),
                         values = c('red', 'blue', 'green'))
    
    #'  Plot relationship
    plot(prediction_plot)
    #'  Return predictions
    return(df)
    
  }
  #'  Calculate probability of adult female survival being low, medium, or high given 
  #'  predators are rare (1) or abundant (2) and feeding does not (0) or does (1) occur
  p.AFphi.predLo.noFeed <- AF_survival(N.pred = 1, Feeding = 0, pred.level = "rare", feed.level = "without")
  p.AFphi.predLo.wFeed <- AF_survival(N.pred = 1, Feeding = 1, pred.level = "rare", feed.level = "with")
  p.AFphi.predHi.noFeed <- AF_survival(N.pred = 2, Feeding = 0, pred.level = "abundant", feed.level = "without")
  p.AFphi.predHi.wFeed <- AF_survival(N.pred = 2, Feeding = 1, pred.level = "abundant", feed.level = "with")
  p.AFphi <- bind_rows(p.AFphi.predLo.noFeed, p.AFphi.predLo.wFeed, p.AFphi.predHi.noFeed, p.AFphi.predHi.wFeed) %>%
    mutate(pred = ifelse(pred == "rare", "Low", pred),
           pred = ifelse(pred == "abundant", "High", pred),
           pred = factor(pred, levels = c("Low", "High")),
           food = ifelse(food == "without", "No", food),
           food = ifelse(food == "with", "Yes", food),
           food = factor(food, levels = c("No", "Yes"))) %>%
    arrange(habitat.avail, pred, food) 
  names(p.AFphi) <- c("Habitat_availability", "Aubdance_predators", "Supplemental_feeding", "Low", "Medium", "High")
  head(p.AFphi)
  write_csv(p.AFphi, "./Conditional_Probability_Tables/CPT_Survival_adultFemales.csv")
  
  #'  ---------------------------
  #####  Fecundity_adultFemales  #####
  #'  ---------------------------
  #'  Predict probability of female fecundity being low, medium, or high depending 
  #'  on whether supplemental feeding occurs (yes or no) over a range of available 
  #'  suitable habitat
  AF_fecundity <- function(Feeding, feed.level) {   
    #'  Habitat availability (amount of suitable habitat in sq-km)
    habitat.avail <- seq(0, 500, by = 50)
    #'  Center and scale habitat.avail so values don't range too widely
    habitat.availz <- scale(habitat.avail)
    #'  Holding Supplemental feeding at a fixed level
    Supp.feeding <- Feeding
    
    #'  Define intercept and slope coefficients 
    #'  H: Availability of suitable habitat most important but supplemental feeding 
    #'  increases fecundity
    alpha <- c(0.25, 3) # Intercepts for low and low/moderate fecundity categories
    beta1 <- -3 # Slope coefficient for habitat availability  
    beta2 <- -1.5 # Slope coefficient for supplemental feeding
    
    #'  Calculate probability of adult female fecundity being low, moderate, or high 
    #'  given varying amounts of available habitat and whether supplemental feeding occurs
    (p.AFfecLo <- 1/(1 + exp(-(alpha[1] + beta1*habitat.availz + beta2*Supp.feeding))))
    #'  Probability of being in low or moderate survival category given all of the above
    (p.AFfecLoMod <- 1/(1 + exp(-(alpha[2] + beta1*habitat.availz + beta2*Supp.feeding))))
    #'  Probability of being in moderate survival category given all of the above
    (p.AFfecMod <- p.AFfecLoMod - p.AFfecLo)
    #'  Probability of being in high survival category given all of the above
    (p.AFfecHi <- 1 - (p.AFfecLoMod))
    
    #'  Create data frame with probabilities of adult female fecundity being low, medium, or high
    (p.AFfec <- cbind(p.AFfecLo, p.AFfecMod, p.AFfecHi))
    #'  Add habitat availability covariate data to data frame  
    df <- data.frame(habitat.avail = habitat.avail, food = feed.level, p1 = p.AFfecLo, 
                     p2 = p.AFfecMod, p3 = p.AFfecHi) 
    #'  Reformat data frame for easier plotting
    df_plot <- df %>% dplyr::select(-food) %>%
      pivot_longer(cols = c('p1','p2', 'p3'), names_to = "p", values_to = "prob")
    #'  Plot probability of adult female fecundity being low, medium, or high, 
    #'  given habitat availability and supplemental feeding
    prediction_plot <- ggplot(df_plot, aes(x = habitat.avail, y = prob)) + 
      ylim(0, 1)+
      geom_line(aes(color = p)) +
      xlab("Habitat suitability (sq-km)")+
      ylab("Prob(Adult female fecundity)")+
      ggtitle(paste("Adult female fecundity", feed.level, "suppemental feeding")) +
      theme(
        legend.position = "top",
        legend.justification = c("left"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
      scale_color_manual(name = '', labels = c('Low fecundity', 'Moderate fecundity', 'High fecundity'),
                         values = c('red', 'blue', 'green'))
    
    #'  Plot relationship
    plot(prediction_plot)
    #'  Return predictions
    return(df)
    
  }
  #'  Calculate probability of adult female fecundity being low, medium, or high given 
  #'  supplemental feeding does not (0) or does (1) occur
  p.AFfec.noFeed <- AF_fecundity(Feeding = 0, feed.level = "without")
  p.AFfec.wFeed <- AF_fecundity(Feeding = 1, feed.level = "with")
  p.AFfec <- bind_rows(p.AFfec.noFeed, p.AFfec.wFeed) %>%
    mutate(food = ifelse(food == "without", "No", food),
           food = ifelse(food == "with", "Yes", food),
           food = factor(food, levels = c("No", "Yes"))) %>%
    arrange(habitat.avail, food) 
  names(p.AFfec) <- c("Habitat_availability", "Supplemental_feeding", "Low", "Medium", "High")
  head(p.AFfec)
  write_csv(p.AFfec, "./Conditional_Probability_Tables/CPT_Fecundity_adultFemales.csv")
  
  #'  ------------------
  #####  Survival_calf  #####
  #'  ------------------
  #'  Predict probability of calf survival being low, medium, or high depending on 
  #'  different levels of female fecundity (low, medium, high), predator abundance 
  #'  (low or high), and whether maternal penning occurs (yes or no)
  YoY_survival <- function(N.pred, Feeding, pen.level, pred.level) {   
    #'  Adult female fecundity (low, medium, or high fecundity)
    AF.fecundity <- c(1, 2, 3)
    #'  Holding Abundance_predators at a fixed level 
    N.predator <- N.pred
    #'  Holding Supplemental feeding at a fixed level
    Supp.feeding <- Feeding
    
    #'  Define intercept and slope coefficients 
    #'  H: Adult female fecundity most important, closely followed by predator
    #'  abundance, but maternal penning can greatly offset the effects of predators
    alpha <- c(4, 8) # Intercepts for low and low/moderate fecundity categories
    beta1 <- -3 # Slope coefficient for adult female fecundity 
    beta2 <- 2 # Slope coefficient for predator abundance
    beta3 <- -1.5 # Slope coefficient for maternal penning
    
    #'  Calculate probability of calf survival being low, moderate, or high given 
    #'  different levels of adult female fecundity, predator abundance, and whether 
    #'  maternal penning occurs
    (p.YoYphiLo <- 1/(1 + exp(-(alpha[1] + beta1*AF.fecundity + beta2*N.predator + beta3*Supp.feeding))))
    #'  Probability of being in low or moderate survival category given all of the above
    (p.YoYphiLoMod <- 1/(1 + exp(-(alpha[2] + beta1*AF.fecundity + beta2*N.predator + beta3*Supp.feeding))))
    #'  Probability of being in moderate survival category given all of the above
    (p.YoYphiMod <- p.YoYphiLoMod - p.YoYphiLo)
    #'  Probability of being in high survival category given all of the above
    (p.YoYphiHi <- 1 - (p.YoYphiLoMod))
    
    #'  Create data frame with probabilities of calf survival being low, medium, or high
    (p.YoYphi <- cbind(p.YoYphiLo, p.YoYphiMod, p.YoYphiHi))
    #'  Add covariate data to data frame  
    df <- data.frame(AF.fecundity = AF.fecundity, pred = pred.level, pens = pen.level, 
                     p1 = p.YoYphiLo, p2 = p.YoYphiMod, p3 = p.YoYphiHi) %>%
      mutate(AF.fecundity = ifelse(AF.fecundity == 1, "Low", AF.fecundity),
             AF.fecundity = ifelse(AF.fecundity == 2, "Medium", AF.fecundity),
             AF.fecundity = ifelse(AF.fecundity == 3, "High", AF.fecundity),
             AF.fecundity = factor(AF.fecundity, levels = c("Low", "Medium", "High")))
    #'  Reformat data frame for easier plotting
    df_plot <- df %>% dplyr::select(-c(pred, pens)) %>%
      pivot_longer(cols = c('p1','p2', 'p3'), names_to = "p", values_to = "prob")
    #'  Plot probability of calf survival being low, medium, or high, given adult
    #'  female fecundity, predator abundance, and use of maternal penning
    prediction_plot <- ggplot(df_plot, aes(x = AF.fecundity, y = prob)) + 
      ylim(0, 1)+
      geom_point(aes(color = p)) +
      xlab("Adult female fecundity")+
      ylab("Prob(Calf survival)")+
      ggtitle(paste("Calf survival", pen.level, "maternal penning and predators are", pred.level)) +
      theme(
        legend.position = "top",
        legend.justification = c("left"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
      scale_color_manual(name = '', labels = c('Low phi', 'Moderate phi', 'High phi'),
                         values = c('red', 'blue', 'green'))
    
    #'  Plot relationship
    plot(prediction_plot)
    #'  Return predictions
    return(df)
    
  }
  #'  Calculate probability of adult female fecundity being low, medium, or high given 
  #'  supplemental feeding does not (0) or does (1) occur
  p.YoYphi.predLo.noPen <- YoY_survival(N.pred = 0, Feeding = 0, pen.level = "without", pred.level = "rare")
  p.YoYphi.predLo.wPen <- YoY_survival(N.pred = 0, Feeding = 1, pen.level = "with", pred.level = "rare")
  p.YoYphi.predHi.noPen <- YoY_survival(N.pred = 1, Feeding = 0, pen.level = "without", pred.level = "abundant")
  p.YoYphi.predHi.wPen <- YoY_survival(N.pred = 1, Feeding = 1, pen.level = "with", pred.level = "abundant")
  p.YoYphi <- bind_rows(p.YoYphi.predLo.noPen, p.YoYphi.predLo.wPen, p.YoYphi.predHi.noPen, p.YoYphi.predHi.wPen) %>%
    mutate(pred = ifelse(pred == "rare", "Low", pred),
           pred = ifelse(pred == "abundant", "High", pred),
           pred = factor(pred, levels = c("Low", "High")),
           pens = ifelse(pens == "without", "No", pens),
           pens = ifelse(pens == "with", "Yes", pens),
           pens = factor(pens, levels = c("No", "Yes"))) %>%
    arrange(AF.fecundity, pred, pens) 
  names(p.YoYphi) <- c("Fecundity_adultFemale", "Abundance_predators", "Maternal_penning", "Low", "Medium", "High")
  head(p.YoYphi)
  write_csv(p.YoYphi, "./Conditional_Probability_Tables/CPT_Survival_calf.csv")
  
  #'  -------------------------
  #####  Abundance_adultMales  #####
  #'  -------------------------
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
    
    #'  Define intercept and slope coefficients (that creates hypothesized shape
    #'  of each probability curve --- H: males don't contribute much even if survival is high)
    alpha <- 3 # Intercept for low abundance category
    beta1 <- -1#-2 # Slope coefficient for abundance of source population   
    beta2 <- -0.65#-1 # Slope coefficient for adult male survival
    
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
  
  #'  ---------------------------
  #####  Abundance_adultFemales  #####
  #'  ---------------------------
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
    
    #'  Define intercept and slope coefficients 
    #'  H: AF survival matters a lot where low phi == low N
    alpha <- 5 #4 # Intercept for low abundance category
    beta1 <- -1 #-0.70 # Slope coefficient for abundance of source population   
    beta2 <- -2#-1.75 # Slope coefficient for adult female survival
    
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
  
  
  #####  Natural immigration  #####
  #####  Abundance_adults  #####
  #####  Abundance_calves  #####
  #####  Abundance_caribou  #####
  #####  Abundance_adultAugmentation  #####
  #####  Lambda  #####
  #####  ProbSuccess  #####
  #####  Utility  #####
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  