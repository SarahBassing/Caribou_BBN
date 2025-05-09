  #'  ---------------------------------
  #'  Making up stuff for caribou BBN
  #'  April 2025
  #'  Bassing
  #'  ---------------------------------

  #'  Load packages
  library(tidyverse)
  library(purrr)
  library(ggplot2)
  
  #' #'  Define levels for source pop N and AM phi
  #' abund.sourcePop <- seq(10, 200, by = 20)
  #' survival.AM <- c(1, 2, 3)
  #' #'  Create data frame containing full cross of all parent node levels
  #' df <- expand.grid(source.N = abund.sourcePop, survival.M = survival.AM) %>%
  #'   tidyr::expand(source.N, survival.M) %>%
  #'   mutate(source.N = factor(source.N),
  #'          survival.M = factor(survival.M))
  #' 
  #' #'  Define coefficients
  #' alpha <- -8
  #' beta.source <- 1 
  #' beta.am.phi <- 0.6
  #' (df$y <- 1 / (1 + exp(-(alpha + beta.source*as.numeric(df$source.N) + beta.am.phi*as.numeric(df$survival.M)))))
  #' 
  #' #'  Plot 
  #' ggplot(data = df, aes(x = source.N, y = y, group = survival.M)) + 
  #'   geom_line(aes(color = survival.M)) +
  #'   xlab("Source population abundance")+
  #'   ylab("Pr(Adult male abundance = High)")+
  #'   guides(color = guide_legend(title = "Adult male \nsurvival"))+
  #'   scale_color_manual(values = c("red", "blue", "green"),
  #'                      labels = c('Low', 'Medium', 'High'))
  
  #'  ------------------------------------------------
  ####  Conditional Probability Tables for each node  ####
  #'  ------------------------------------------------
  
  #'  -------------------------
  #####  Habitat_availability  #####
  #'  -------------------------
  #'  Predict probability of available habitat across range of binned areas based
  #'  on different levels of expected climate change scenarios
  habitat <- function() {
    #'  Climate scenarios (1 = bad, 2 = worse, 3 = we fucked)
    climate <- c(1, 2, 3)
    
    #'  Define intercept and slope coefficients
    #'  H: The hotter the climate, the less suitable habitat will be available
    alpha <- c(-6, -5, -4, -3, -2, -1, 0, 1, 2) # Intercepts for low probability of being in each bin c(-5, -4, -3, -2, -1, 0, 1, 2, 3) making more negative shifts peak/slope of scenarios right
    beta1 <- 2.5 # Slope for climate effect scenario (increase beta1 to increase diff btwn scenarios)
    
    #'  Calculate probability of habitat availability being in each size bin given
    #'  climate change scenarios (bad, worse, and we fucked)
    (p.hab.0.50 <- 1/(1 + exp(-(alpha[1] + beta1*climate)))) # + beta2*climate + beta3*climate
    (p.hab.0.100 <- 1/(1 + exp(-(alpha[2] + beta1*climate))))
    (p.hab.0.150 <- 1/(1 + exp(-(alpha[3] + beta1*climate))))
    (p.hab.0.200 <- 1/(1 + exp(-(alpha[4] + beta1*climate))))
    (p.hab.0.250 <- 1/(1 + exp(-(alpha[5] + beta1*climate))))
    (p.hab.0.300 <- 1/(1 + exp(-(alpha[6] + beta1*climate))))
    (p.hab.0.350 <- 1/(1 + exp(-(alpha[7] + beta1*climate))))
    (p.hab.0.400 <- 1/(1 + exp(-(alpha[8] + beta1*climate))))
    (p.hab.0.450 <- 1/(1 + exp(-(alpha[9] + beta1*climate))))
    (p.hab.50.100 <- p.hab.0.100 - p.hab.0.50)
    (p.hab.100.150 <- p.hab.0.150 - p.hab.0.100)
    (p.hab.150.200 <- p.hab.0.200 - p.hab.0.150)
    (p.hab.200.250 <- p.hab.0.250 - p.hab.0.200)
    (p.hab.250.300 <- p.hab.0.300 - p.hab.0.250)
    (p.hab.300.350 <- p.hab.0.350 - p.hab.0.300)
    (p.hab.350.400 <- p.hab.0.400 - p.hab.0.350)
    (p.hab.400.450 <- p.hab.0.450 - p.hab.0.400)
    (p.hab.450.500 <- 1 - p.hab.0.450)
    
    #'  Create data frame with probabilities of prey abundance being low or high
    (p.hab <- cbind(p.hab.0.50, p.hab.50.100, p.hab.100.150, p.hab.150.200, p.hab.200.250, 
                    p.hab.250.300, p.hab.300.350, p.hab.350.400, p.hab.400.450, p.hab.450.500)) 
      
    #'  Add climate change covariate data to data frame
    (df <- data.frame(climate.scenario = climate, p1 = p.hab.0.50, p2 = p.hab.50.100, 
                      p3 = p.hab.100.150, p4 = p.hab.150.200, p5 = p.hab.200.250, 
                      p6 = p.hab.250.300, p7 = p.hab.300.350, p8 = p.hab.350.400, 
                      p9 = p.hab.400.450, p10 = p.hab.450.500) %>%
      mutate(climate.scenario = ifelse(climate.scenario == 1, "Bad", climate.scenario),
             climate.scenario = ifelse(climate.scenario == 2, "Worse", climate.scenario),
             climate.scenario = ifelse(climate.scenario == 3, "We fucked", climate.scenario),
             climate.scenario = factor(climate.scenario, levels = c("Bad", "Worse", "We fucked")),
             sum_to_one = rowSums(across(where(is.numeric)))))
    #'  Habitat availability (amount of suitable habitat in sq-km)
    habitat.avail <- seq(50, 500, by = 50)
    habitat <- rep(habitat.avail, 3)
    #'  Reformat data frame for easier plotting
    df_plot <- df %>% dplyr::select(-sum_to_one) %>%
      pivot_longer(cols = c('p1','p2','p3','p4','p5','p6','p7','p8','p9','p10'), 
                                   names_to = "p", values_to = "prob") %>%
      bind_cols(habitat) 
      # bind_cols(habitat.avail) 
    names(df_plot) <- c("climate.scenario", "p", "prob", "habitat.bin")
    
    #'  Plot probability of prey abundance being low or high, given level
    #'  of available habitat
    prediction_plot <- ggplot(df_plot, aes(x = habitat.bin, y = prob, group = climate.scenario)) + 
      ylim(0, 1) +
      geom_line(aes(color = climate.scenario)) +
      geom_point(aes(color = climate.scenario)) +
      xlab("Habitat availability bin")+
      ylab("Prob(Habitat availability bin)")+
      ggtitle(paste("Probability of being in each habitat bin given each climate scenario")) +
      theme(
        legend.position = "top",
        legend.justification = c("left"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) 
    
    #'  Plot relationship
    plot(prediction_plot)
    #'  Return predictions
    return(df)
  }
  p.habitat <- habitat()
  names(p.habitat) <- c("Habitat_availability", "0 to 50", "50 to 100", "100 to 150",
                        "150 to 200", "200 to 250", "250 to 300", "300 to 350",
                        "350 to 400", "400 to 450", "450 to 500", "sum_to_one")
  head(p.habitat)
  write_csv(p.habitat, "./Conditional_Probability_Tables/CPT_Habitat_availability.csv")
  
  #'  ----------------------
  #####  Abundance_altPrey  #####
  #'  ----------------------
  #'  Predict probability of low, medium, or high abundances of alternative prey
  #'  sources (e.g., deer, elk, moose) under varying levels of habitat availability
  Prey_abundance <- function() {
    #'  Habitat availability (amount of suitable habitat in sq-km)
    habitat.avail <- seq(50, 500, by = 50)
    #'  Center and scale habitat.avail so values don't range too widely
    habitat.availz <- scale(habitat.avail)
    
    #'  Define intercept and slope coefficients
    #'  H: Availability of suitable habitat increases abundance of alternative prey
    alpha <- 0 # Intercept for low prey abundance
    beta1 <- -2 # Slope for habitat availability effect
    
    #'  Calculate probability alternative prey abundance will be low vs high given 
    #'  varying amounts of available habitat
    (p.PreynLo <- 1/(1 + exp(-(alpha + beta1*habitat.availz))))
    (p.PreynHi <- 1 - p.PreynLo)
    
    #'  Create data frame with probabilities of prey abundance being low or high
    (p.Preyn <- cbind(p.PreynLo, p.PreynHi))
    #'  Add habitat availability covariate data to data frame
    df <- data.frame(habitat.avail = habitat.avail, p1 = p.PreynLo, p2 = p.PreynHi) 
    #'  Reformat data frame for easier plotting
    df_plot <- df %>% 
      pivot_longer(cols = c('p1','p2'), names_to = "p", values_to = "prob")
    
    #'  Plot probability of prey abundance being low or high, given level
    #'  of available habitat
    prediction_plot <- ggplot(df_plot, aes(x = habitat.avail, y = prob)) + 
      ylim(0, 1) +
      geom_line(aes(color = p)) +
      xlab("Habitat availability (sq-km)")+
      ylab("Prob(Prey abundance)")+
      ggtitle(paste("Prey abundance over varying levels of habitat availability")) +
      theme(
        legend.position = "top",
        legend.justification = c("left"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
      scale_color_manual(name = '', labels = c('Low prey N', 'High prey N'),
                         values = c('red', 'blue'))
    
    #'  Plot relationship
    plot(prediction_plot)
    #'  Return predictions
    return(df)
  }
  p.PreyN <- Prey_abundance() %>%
    mutate(sum_to_one = rowSums(.[2:3]))
  names(p.PreyN) <- c("Habitat_availability", "Low", "High", "sum_to_one")
  head(p.PreyN)
  write_csv(p.PreyN, "./Conditional_Probability_Tables/CPT_Abundance_altPrey.csv")
  
  #'  ------------------------
  #####  Abundance_predators  #####
  #'  ------------------------
  #'  Predict probability of low vs high abundances of predators (e.g., wolf, 
  #'  cougar, bear) based on whether alternative prey populations are rare (1) 
  #'  or abundant (2) and whether predator control does not (0) or does (1) occur
  Pred_abundance <- function(control, control.level) {
    #'  Alternative prey abundance (low, or high) 
    N.altPrey <- c(1, 2)
    #'  Holding whether predator control occurs at a fixed level
    Pred.control <- control
    
    #'  Define intercept and slope coefficients
    #'  H: More alternative prey increases predator abundance but predator control
    #'  efforts reduce predator abundance
    alpha <- 4.75 # Intercept for low predator abundance
    beta1 <- -3 # Slope for alternative prey effect
    beta2 <- 0.95 # Slope for predator control effect
    
    #'  Calculate probability predator abundance will be low vs high given different
    #'  levels of alternative prey sources (low or high) and whether predator 
    #'  control occurs (yes or no)
    (p.PrednLo <- 1/(1 + exp(-(alpha + beta1*N.altPrey + beta2*Pred.control))))
    (p.PrednHi <- 1 - p.PrednLo)
    
    #'  Create data frame with probabilities of predator abundance being low or high
    (p.Predn <- cbind(p.PrednLo, p.PrednHi))
    #'  Add prey and predator control covariate data to data frame  
    df <- data.frame(N.altPrey = N.altPrey, control = control.level, 
                     p1 = p.PrednLo, p2 = p.PrednHi) %>%
      mutate(N.altPrey = ifelse(N.altPrey == 1, "Low", "High"),
             N.altPrey = factor(N.altPrey, levels = c("Low", "High")))
    #'  Reformat data frame for easier plotting
    df_plot <- df %>% dplyr::select(-control) %>%
      pivot_longer(cols = c('p1','p2'), names_to = "p", values_to = "prob")
    
    #'  Plot probability of predator abundance being low or high, given level
    #'  of alternative prey sources and predator control
    prediction_plot <- ggplot(df_plot, aes(x = N.altPrey, y = prob)) + 
      ylim(0, 1) +
      geom_point(aes(color = p)) +
      xlab("Alternative prey abundance")+
      ylab("Prob(Predator abundance)")+
      ggtitle(paste("Predator abundance", control.level, "predator control")) +
      theme(
        legend.position = "top",
        legend.justification = c("left"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
      scale_color_manual(name = '', labels = c('Low predator N', 'High predator N'),
                         values = c('red', 'blue'))
    
    #'  Plot relationship
    plot(prediction_plot)
    #'  Return predictions
    return(df)
  }
  #'  Calculate probability of predator abundance being low or high given level
  #'  of alternative prey sources (low or high) and whether predator control
  #'  occurs (yes or no)
  p.PredN.noCon <- Pred_abundance(control = 0, control.level = "without")
  p.PredN.wCon <- Pred_abundance(control = 1, control.level = "with")
  p.PredN <- bind_rows(p.PredN.noCon, p.PredN.wCon) %>%
    mutate(control = ifelse(control == "without", "No", control),
           control = ifelse(control == "with", "Yes", control),
           control = factor(control, levels = c("No", "Yes")),
           sum_to_one = rowSums(across(where(is.numeric)))) %>%
    arrange(N.altPrey, control) 
  names(p.PredN) <- c("Abundance_altPrey", "Predator_control", "Low", "High", "sum_to_one")
  head(p.PredN)
  write_csv(p.PredN, "./Conditional_Probability_Tables/CPT_Abundance_predators.csv")
  
  #'  ------------------------
  #####  Survival_adultMales  #####
  #'  ------------------------
  #'  Predict probability of male survival being low, medium, or high under 
  #'  different levels of predator abundance (low or high) and whether supplemental 
  #'  feeding occurs (yes or no) over a range of available suitable habitat
  AM_survival <- function(N.pred, Feeding, pred.level, feed.level) {   
    #'  Habitat availability (amount of suitable habitat in sq-km)
    habitat.avail <- seq(50, 500, by = 50)
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
      scale_color_manual(name = '', labels = c('Low AM phi', 'Moderate AM phi', 'High AM phi'),
                         values = c('red', 'green', 'blue'))
    
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
           food = factor(food, levels = c("No", "Yes")),
           sum_to_one = rowSums(.[4:6])) %>%
    arrange(habitat.avail, pred, food) 
  names(p.AMphi) <- c("Habitat_availability", "Abundance_predators", "Supplemental_feeding", "Low", "Medium", "High", "sum_to_one")
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
    habitat.avail <- seq(50, 500, by = 50)
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
      scale_color_manual(name = '', labels = c('Low AF phi', 'Moderate AF phi', 'High AF phi'),
                         values = c('red', 'green', 'blue'))
    
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
           food = factor(food, levels = c("No", "Yes")),
           sum_to_one = rowSums(.[4:6])) %>%
    arrange(habitat.avail, pred, food) 
  names(p.AFphi) <- c("Habitat_availability", "Abundance_predators", "Supplemental_feeding", "Low", "Medium", "High", "sum_to_one")
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
    habitat.avail <- seq(50, 500, by = 50)
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
                         values = c('red', 'green', 'blue'))
    
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
           food = factor(food, levels = c("No", "Yes")),
           sum_to_one = rowSums(.[3:5])) %>%
    arrange(habitat.avail, food) 
  names(p.AFfec) <- c("Habitat_availability", "Supplemental_feeding", "Low", "Medium", "High", "sum_to_one")
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
      ylim(0, 1) +
      geom_point(aes(color = p), position = position_dodge(0.1)) +
      xlab("Adult female fecundity")+
      ylab("Prob(Calf survival)")+
      ggtitle(paste("Calf survival", pen.level, "maternal penning and predators are", pred.level)) +
      theme(
        legend.position = "top",
        legend.justification = c("left"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
      scale_color_manual(name = '', labels = c('Low calf phi', 'Moderate calf phi', 'High calf phi'),
                         values = c('red', 'green', 'blue'))
    
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
           pens = factor(pens, levels = c("No", "Yes")),
           sum_to_one = rowSums(across(where(is.numeric)))) %>%
    arrange(AF.fecundity, pred, pens) 
  names(p.YoYphi) <- c("Fecundity_adultFemale", "Abundance_predators", "Maternal_penning", "Low", "Medium", "High", "sum_to_one")
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
    #'  Plot probability of adult male abundance being low or high, given adult
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
      scale_color_manual(name = '', labels = c('Low AM N', 'High AM N'),
                         values = c('red', 'blue'))
    
    #'  Plot relationship
    plot(prediction_plot)
    #'  Return predictions
    return(df)
  }
  #'  Calculate probability of adult male abundance being low or high given 
  #'  adult male survival is low (1), moderate (2), or high (3)
  p.AMn.phiLo <- AM_abundance(AM.phi = 1, phi.level = "low") 
  p.AMn.phiMod <- AM_abundance(AM.phi = 2, phi.level = "moderate")
  p.AMn.phiHi <- AM_abundance(AM.phi = 3, phi.level = "high")
  p.AMn <- bind_rows(p.AMn.phiLo, p.AMn.phiMod, p.AMn.phiHi) %>%
    mutate(phi = ifelse(phi == "low", "Low", phi),
           phi = ifelse(phi == "moderate", "Medium", phi),
           phi = ifelse(phi == "high", "High", phi),
           phi = factor(phi, levels = c("Low", "Medium", "High")),
           sum_to_one = rowSums(.[3:4])) %>%
    arrange(N.sourcePop, phi) 
  names(p.AMn) <- c("Abundance_sourcePop", "Survival_adultMale", "Low", "High", "sum_to_one")
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
    #'  Plot probability of adult female abundance being low or high, given adult
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
      scale_color_manual(name = '', labels = c('Low AF N', 'High AF N'),
                         values = c('red', 'blue'))
    
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
           phi = factor(phi, levels = c("Low", "Medium", "High")),
           sum_to_one = rowSums(.[3:4])) %>%
    arrange(N.sourcePop, phi) 
  names(p.AFn) <- c("Abundance_sourcePop", "Survival_adultFemale", "Low", "High", "sum_to_one")
  head(p.AFn)
  write_csv(p.AFn, "./Conditional_Probability_Tables/CPT_Abundance_adultFemales.csv")

  #'  ------------------------
  #####  Natural immigration  #####
  #'  ------------------------
  #'  Predict probability of no, low, or moderate natural immigration from BC 
  #'  source populations to US under varying levels of source population
  immigration <- function() {
    #'  Population size of source population
    N.sourcePop <- seq(10, 200, by = 20)
    #'  Center and scale N.sourcePop so values don't range too widely
    N.sourcePopz <- scale(N.sourcePop)
    
    #'  Define intercept and slope coefficients
    #'  H: Size of source population increases level of natural immigration into US
    alpha <- c(2.25, 5) # Intercept for NO immigration
    beta1 <- -2 # Slope for source population effect
    
    #'  Calculate probability natural immigration will be none, low, or moderate 
    #'  given varying sizes of the source population
    (p.ImmnNo <- 1/(1 + exp(-(alpha[1] + beta1*N.sourcePopz))))
    (p.ImmnNoLo <- 1/(1 + exp(-(alpha[2] + beta1*N.sourcePopz))))
    (p.ImmnLo <- p.ImmnNoLo - p.ImmnNo)
    (p.ImmnMod <- 1 - p.ImmnNoLo)
    
    #'  Create data frame with probabilities of natural immigration being none, low, or moderate
    (p.Immn <- cbind(p.ImmnNo, p.ImmnLo, p.ImmnMod))
    #'  Add source population covariate data to data frame
    df <- data.frame(N.sourcePop = N.sourcePop, p1 = p.ImmnNo, p2 = p.ImmnLo, p3 = p.ImmnMod) %>%
      mutate(sum_to_one = rowSums(.[2:4]))
    #'  Reformat data frame for easier plotting
    df_plot <- df %>% dplyr::select(-sum_to_one) %>% 
      pivot_longer(cols = c('p1','p2', 'p3'), names_to = "p", values_to = "prob")
    
    #'  Plot probability of natural immigration being low or high, given level
    #'  of available habitat
    prediction_plot <- ggplot(df_plot, aes(x = N.sourcePop, y = prob)) + 
      ylim(0, 1) +
      geom_line(aes(color = p)) +
      xlab("Source population abundance")+
      ylab("Prob(Natural Immigration)")+
      ggtitle(paste("Natural immigration over varying sizes of source population")) +
      theme(
        legend.position = "top",
        legend.justification = c("left"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
      scale_color_manual(name = '', labels = c('No immigration', 'Low immigration', 'Mod immigration'),
                         values = c('red', 'green', 'blue'))
    
    #'  Plot relationship
    plot(prediction_plot)
    #'  Return predictions
    return(df)
  }
  p.ImmN <- immigration()
  names(p.ImmN) <- c("Abundance_sourcePop", "None", "Low", "Moderate", "sum_to_one")
  head(p.ImmN)
  write_csv(p.ImmN, "./Conditional_Probability_Tables/CPT_Natural_immigration.csv")  
  
  #'  ---------------------
  #####  Captive_breeding  #####
  #'  ---------------------
  #'  Probability of whether captive breeding occurs given the size of the source 
  #'  population
  cap_breeding <- function() {  
    #'  Population size of source population
    N.sourcePop <- seq(10, 200, by = 20)
    #'  Center and scale N.sourcePop so values don't range too widely
    N.sourcePopz <- scale(N.sourcePop)
    
    #'  Define intercept and slope coefficients
    #'  Whether captive breeding occurs depends on size of the source population
    alpha <- 0 # Intercept for No captive breeding
    beta1 <- 2 # Slope coefficient for abundance of source population   
    
    #'  Calculate probability of adult male abundance being low vs high given 
    #'  varying sizes of the source population and levels of adult male survival
    (p.CapBreedNo <- 1/(1+exp(-(alpha + beta1*N.sourcePopz))))
    #'  Probability of adult male abundance being high given low male survival over 
    #'  a range of population sizes of the source population
    p.CapBreedYes <- 1 - p.CapBreedNo
    
    #'  Create data frame with probabilities of whether captive breeding occurs
    (p.CapBreed <- cbind(p.CapBreedNo, p.CapBreedYes))
    #'  Add source population covariate data to data frame and 
    df <- data.frame(N.sourcePop = N.sourcePop, p1 = p.CapBreedNo, p2 = p.CapBreedYes) 
    #'  Reformat data frame for easier plotting
    df_plot <- df %>% 
      pivot_longer(cols = c('p1','p2'), names_to = "p", values_to = "prob")
    #'  Plot probability of captive breeding occuring given source population size
    prediction_plot <- ggplot(df_plot, aes(x = N.sourcePop, y = prob)) + 
      ylim(0, 1)+
      geom_line(aes(color = p)) +
      xlab("Source population size")+
      ylab("Prob(Captive Breeding)")+
      ggtitle(paste("Captive breeding depending on size of source population")) +
      theme(
        legend.position = "top",
        legend.justification = c("left"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
      scale_color_manual(name = '', labels = c('No', 'Yes'),
                         values = c('red', 'blue'))
    
    #'  Plot relationship
    plot(prediction_plot)
    #'  Return predictions
    return(df)
  }
  #'  Calculate probability of captive breeding given size of source population
  p.CapBreed <- cap_breeding() %>%
    mutate(sum_to_one = rowSums(.[2:3]))
  names(p.CapBreed) <- c("Abundance_sourcePop", "No", "Yes")
  head(p.CapBreed)
  write_csv(p.CapBreed, "./Conditional_Probability_Tables/CPT_Captive_breeding.csv")
  
  #'  --------------------------------
  #####  Abundance_adultAugmentation  #####
  #'  --------------------------------
  #'  Probability of augmenting caribou population with adults from the source
  #'  population given size of source population and whether captive breeding occurs (No, Yes)
  augment <- function(Cap.Breed, cap.level) {  
    #'  Holding captive.breeding at a fixed value (No vs Yes)
    captive.breeding <- Cap.Breed 
    #'  Population size of source population
    N.sourcePop <- seq(10, 200, by = 20)
    #'  Center and scale N.sourcePop so values don't range too widely
    N.sourcePopz <- scale(N.sourcePop)
    
    #'  Define intercept and slope coefficients 
    #'  H: Augmentation will depend on size of source population and whether captive
    #'  breeding occurs
    alpha <- c(0, 1) # Intercept for no augmentation category
    beta1 <- -1.5 # Slope coefficient for abundance of source population   
    beta2 <- 0.75 # Slope coefficient for captive breeding
    
    #'  Calculate probability of augmenting population with adults from source
    #'  population being none, low, or high given varying sizes of the source population 
    #'  and whether captive breeding occurs (no vs yes)
    (p.AugnNo <- 1/(1+exp(-(alpha[1] + beta1*N.sourcePopz + beta2*captive.breeding))))
    (p.AugnNoLo <- 1/(1+exp(-(alpha[2] + beta1*N.sourcePopz + beta2*captive.breeding))))
    (p.AugnLo <- p.AugnNoLo - p.AugnNo)
    (p.AugnHi <- 1 - p.AugnNoLo)
    
    #'  Create data frame with probabilities of augmentation being none, low or high
    (p.Augn <- cbind(p.AugnNo, p.AugnLo, p.AugnHi))
    #'  Add source population and captive breeding covariate data to data frame and 
    df <- data.frame(N.sourcePop = N.sourcePop, Captive.breeding = captive.breeding, 
                     p1 = p.AugnNo, p2 = p.AugnLo, p3 = p.AugnHi) 
    #'  Reformat data frame for easier plotting
    df_plot <- df %>% dplyr::select(-Captive.breeding) %>%
      pivot_longer(cols = c('p1','p2','p3'), names_to = "p", values_to = "prob")
    #'  Plot probability of augmentation being none, low, or high, given source 
    #'  population size and whether captive breeding occurs 
    prediction_plot <- ggplot(df_plot, aes(x = N.sourcePop, y = prob)) + 
      ylim(0, 1)+
      geom_line(aes(color = p)) +
      xlab("Source population size")+
      ylab("Prob(Adult augmentation)")+
      ggtitle(paste("Adult augementation when captive breeding", cap.level)) +
      theme(
        legend.position = "top",
        legend.justification = c("left"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
      scale_color_manual(name = '', labels = c('No augmentation', 'Low augmentation', 'High augmentation'),
                         values = c('red', 'green', 'blue'))
    
    #'  Plot relationship
    plot(prediction_plot)
    #'  Return predictions
    return(df)
  }
  #'  Calculate probability of adult augmentation given captive breeding and source
  #'  population
  p.Augn.BreedingN <- augment(Cap.Breed = 1, cap.level = "does not occur") 
  p.Augn.BreedingY <- augment(Cap.Breed = 2, cap.level = "occurs")
  p.Augn <- bind_rows(p.Augn.BreedingN, p.Augn.BreedingY) %>%
    mutate(Captive.breeding = ifelse(Captive.breeding == 1, "No", Captive.breeding),
           Captive.breeding = ifelse(Captive.breeding == 2, "Yes", Captive.breeding),
           Captive.breeding = factor(Captive.breeding, levels = c("No", "Yes")),
           sum_to_one = rowSums(.[3:5])) %>%
    arrange(N.sourcePop, Captive.breeding) 
  names(p.Augn) <- c("Abundance_sourcePop", "Captive_breeding", "None", "Low", "High", "sum_to_one")
  head(p.Augn)
  write_csv(p.Augn, "./Conditional_Probability_Tables/CPT_Abundance_adultAugment.csv")
  
  #'  ---------------------
  #####  Abundance_adults  #####
  #'  ---------------------
  #'  Probability of adult abundance being low or high given the level of adult 
  #'  male and adult female abundances and level of natural immigration (none, low, moderate)
  A_abundance <- function(AM.n, AF.n, AM.level, AF.level) {   
    #'  Natural immigration
    immigration <- c(1, 2, 3)
    #'  Holding Abundance_adultMales at a fixed level (low vs high)
    N.AM <- AM.n
    #'  Holding Abundance_adultFemales at a fixed level (low vs high)
    N.AF <- AF.n
    
    #'  Define intercept and slope coefficients 
    #'  H: Adult abundance increases most with adult female abundance, then adult
    #'  male abundance, and lastly it is supplemented with natural immigration
    alpha <- c(5) # Intercept for low adult abundance category
    beta1 <- -2 # Slope coefficient for adult male abundance
    beta2 <- -4 # Slope coefficient for adult female abundance
    beta3 <- -0.75 # Slope coefficient for natural immigration
    
    #'  Calculate probability of adult abundance being low given different levels 
    #'  of adult male abundance, adult female abundance, and natural immigration
    (p.AnLo <- 1/(1 + exp(-(alpha[1] + beta1*N.AM + beta2*N.AF + beta3*immigration))))
    #'  Probability of being in high abundance category given all of the above
    (p.AnHi <- 1 - (p.AnLo))
    
    #'  Create data frame with probabilities of adult abundance being low or high
    (p.AN <- cbind(p.AnLo, p.AnHi))
    #'  Add covariate data to data frame  
    df <- data.frame(Immigration = immigration, Abundance_AM = AM.level, Abundance_AF = AF.level, 
                     p1 = p.AnLo, p2 = p.AnHi) %>%
      mutate(Immigration = ifelse(Immigration == 1, "None", Immigration),
             Immigration = ifelse(Immigration == 2, "Low", Immigration),
             Immigration = ifelse(Immigration == 3, "Moderate", Immigration),
             Immigration = factor(Immigration, levels = c("None", "Low", "Moderate")))
    #'  Reformat data frame for easier plotting
    df_plot <- df %>% dplyr::select(-c(Abundance_AM, Abundance_AF)) %>%
      pivot_longer(cols = c('p1','p2'), names_to = "p", values_to = "prob")
    #'  Plot probability of adult abundance being low or high given abundance
    #'  of adult males, adult females, and natural immigration
    prediction_plot <- ggplot(df_plot, aes(x = Immigration, y = prob)) + 
      ylim(0, 1) +
      geom_point(aes(color = p), position = position_dodge(0.1)) +
      xlab("Natural immigration")+
      ylab("Prob(Adult abundance)")+
      ggtitle(paste("Adult abundance when adult males are", AM.level, "and \nadult females are", AF.level)) +
      theme(
        legend.position = "top",
        legend.justification = c("left"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
      scale_color_manual(name = '', labels = c('Low adult N', 'High adult N'),
                         values = c('red', 'blue'))
    
    #'  Plot relationship
    plot(prediction_plot)
    #'  Return predictions
    return(df)
  }
  #'  Calculate probability of adult abundance being low or high given adult male
  #'  abundance, adult female abundance, and natural immigration
  p.AN.AMLo.AFLo <- A_abundance(AM.n = 0, AF.n = 0, AM.level = "rare", AF.level = "rare")
  p.AN.AMLo.AFHi <- A_abundance(AM.n = 0, AF.n = 1, AM.level = "rare", AF.level = "abundant")
  p.AN.AMHi.AFLo <- A_abundance(AM.n = 1, AF.n = 0, AM.level = "abundant", AF.level = "rare")
  p.AN.AMHi.AFHi <- A_abundance(AM.n = 1, AF.n = 1, AM.level = "abundant", AF.level = "abundant")
  p.AN <- bind_rows(p.AN.AMLo.AFLo, p.AN.AMLo.AFHi, p.AN.AMHi.AFLo, p.AN.AMHi.AFHi) %>%
    mutate(Abundance_AM = ifelse(Abundance_AM == "rare", "Low", Abundance_AM),
           Abundance_AM = ifelse(Abundance_AM == "abundant", "High", Abundance_AM),
           Abundance_AM = factor(Abundance_AM, levels = c("Low", "High")),
           Abundance_AF = ifelse(Abundance_AF == "rare", "Low", Abundance_AF),
           Abundance_AF = ifelse(Abundance_AF == "abundant", "High", Abundance_AF),
           Abundance_AF = factor(Abundance_AF, levels = c("Low", "High")),
           sum_to_one = rowSums(across(where(is.numeric)))) %>%
    arrange(Immigration, Abundance_AM, Abundance_AF) 
  names(p.AN) <- c("Natural_immigration", "Abundance_adultMales", "Abundance_adultFemales", "Low", "High", "sum_to_one")
  head(p.AN)
  write_csv(p.AN, "./Conditional_Probability_Tables/CPT_Abundance_adults.csv")
  
  #'  ---------------------
  #####  Abundance_calves  #####
  #'  ---------------------
  #'  Probability of calf abundance being low or high given the level of adult 
  #'  female abundance and level calf survival (low, medium, or high)
  C_abundance <- function(AF.n, AF.level) {   
    #'  Calf survival categories
    phi <- c(1, 2, 3)
    #'  Holding Abundance_adultFemales at a fixed level (low vs high)
    N.AF <- AF.n
    
    #'  Define intercept and slope coefficients 
    #'  H: Calf survival influences calf abundance more than adult female abundance 
    alpha <- c(5) # Intercept for low calf abundance category
    beta1 <- -2 # Slope coefficient for calf survival
    beta2 <- -0.75 # Slope coefficient for adult female abundance
    
    #'  Calculate probability of calf abundance being low given different levels 
    #'  of adult female abundance and calf survival probabilities
    (p.YoYnLo <- 1/(1 + exp(-(alpha[1] + beta1*phi + beta2*N.AF))))
    #'  Probability of being in high abundance category given all of the above
    (p.YoYnHi <- 1 - (p.YoYnLo))
    
    #'  Create data frame with probabilities of calf abundance being low or high
    (p.YoYN <- cbind(p.YoYnLo, p.YoYnHi))
    #'  Add covariate data to data frame  
    df <- data.frame(Survival_calf = phi, Abundance_AF = AF.level, 
                     p1 = p.YoYnLo, p2 = p.YoYnHi) %>%
      mutate(Survival_calf = ifelse(Survival_calf == 1, "Low", Survival_calf),
             Survival_calf = ifelse(Survival_calf == 2, "Moderate", Survival_calf),
             Survival_calf = ifelse(Survival_calf == 3, "High", Survival_calf),
             Survival_calf = factor(Survival_calf, levels = c("Low", "Moderate", "High")))
    #'  Reformat data frame for easier plotting
    df_plot <- df %>% dplyr::select(-Abundance_AF) %>%
      pivot_longer(cols = c('p1','p2'), names_to = "p", values_to = "prob")
    #'  Plot probability of calf abundance being low or high given adult female 
    #'  abundance and calf survival
    prediction_plot <- ggplot(df_plot, aes(x = Survival_calf, y = prob)) + 
      ylim(0, 1) +
      geom_point(aes(color = p), position = position_dodge(0.1)) +
      xlab("Calf survival")+
      ylab("Prob(Calf abundance)")+
      ggtitle(paste("Calf abundance over varying levels of calf survival when \nadult females are", AF.level)) +
      theme(
        legend.position = "top",
        legend.justification = c("left"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
      scale_color_manual(name = '', labels = c('Low calf N', 'High calf N'),
                         values = c('red', 'blue'))
    
    #'  Plot relationship
    plot(prediction_plot)
    #'  Return predictions
    return(df)
  }
  #'  Calculate probability of calf abundance being low vs high given calf
  #'  survival and adult female abundance
  p.YoYn.AFLo <- C_abundance(AF.n = 0, AF.level = "rare")
  p.YoYn.AFHi <- C_abundance(AF.n = 1, AF.level = "abundant")
  p.YoYn <- bind_rows(p.YoYn.AFLo, p.YoYn.AFHi) %>%
    mutate(Abundance_AF = ifelse(Abundance_AF == "rare", "Low", Abundance_AF),
           Abundance_AF = ifelse(Abundance_AF == "abundant", "High", Abundance_AF),
           Abundance_AF = factor(Abundance_AF, levels = c("Low", "High")),
           sum_to_one = rowSums(across(where(is.numeric)))) %>%
    arrange(Survival_calf, Abundance_AF) 
  names(p.YoYn) <- c("Survival_calf", "Abundance_adultFemales", "Low", "High", "sum_to_one")
  head(p.YoYn)
  write_csv(p.YoYn, "./Conditional_Probability_Tables/CPT_Abundance_calves.csv")
  
  #'  ----------------------
  #####  Abundance_caribou  #####
  #'  ----------------------
  #'  Probability of caribou abundance being low or high given the level of adult 
  #'  abundance, calf abundance, and level of augmentation
  Total_abundance <- function(A.n, YoY.n, A.level, YoY.level) {   
    #'  Augmentation categories
    augment <- c(1, 2, 3)
    #'  Holding Abundance_adultFemales at a fixed level (low vs high)
    N.A <- A.n
    #'  Holding Abundance_calves at a fixed level (low vs high)
    N.C <- YoY.n
    
    #'  Define intercept and slope coefficients 
    #'  H: Adult abundance has largest effect, then calf abundance, then augmentation 
    alpha <- 5 # Intercept for low total abundance category
    beta1 <- -1 # Slope coefficient for augmentation
    beta2 <- -4.5 # Slope coefficient for adult abundance
    beta3 <- -1.5 # Slope coefficient for calf abundance
    
    #'  Calculate probability of total abundance being low given different levels 
    #'  of adult abundance, calf abundance, and augmentation probabilities
    (p.nLo <- 1/(1 + exp(-(alpha[1] + beta1*augment + beta2*N.A + beta3*N.C))))
    #'  Probability of being in high abundance category given all of the above
    (p.nHi <- 1 - (p.nLo))
    
    #'  Create data frame with probabilities of total abundance being low or high
    (p.N <- cbind(p.nLo, p.nHi))
    #'  Add covariate data to data frame  
    df <- data.frame(Augment = augment, Abundance_A = A.level, Abundance_C = YoY.level,
                     p1 = p.nLo, p2 = p.nHi) %>%
      mutate(Augment = ifelse(Augment == 1, "None", Augment),
             Augment = ifelse(Augment == 2, "Low", Augment),
             Augment = ifelse(Augment == 3, "High", Augment),
             Augment = factor(Augment, levels = c("None", "Low", "High")))
    #'  Reformat data frame for easier plotting
    df_plot <- df %>% dplyr::select(-c(Abundance_A, Abundance_C)) %>%
      pivot_longer(cols = c('p1','p2'), names_to = "p", values_to = "prob")
    #'  Plot probability of total abundance being low or high, given adult
    #'  abundance, calf abundance, and augmentation
    prediction_plot <- ggplot(df_plot, aes(x = Augment, y = prob)) + 
      ylim(0, 1) +
      geom_point(aes(color = p), position = position_dodge(0.1)) +
      xlab("Augmentation")+
      ylab("Prob(Total abundance)")+
      ggtitle(paste("Caribou abundance over varying levels of augementation when \nadult N is", A.level, "and calf N is", YoY.level)) +
      theme(
        legend.position = "top",
        legend.justification = c("left"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
      scale_color_manual(name = '', labels = c('Low total N', 'High total N'),
                         values = c('red', 'blue'))
    
    #'  Plot relationship
    plot(prediction_plot)
    #'  Return predictions
    return(df)
  }
  #'  Calculate probability of total abundance given adult and calf abundances as
  #'  well as augmentation
  p.n.ALo.CLo <- Total_abundance(A.n = 0, YoY.n = 0, A.level = "low", YoY.level = "low")
  p.n.ALo.CHi <- Total_abundance(A.n = 0, YoY.n = 1, A.level = "low", YoY.level = "high")
  p.n.AHi.CLo <- Total_abundance(A.n = 1, YoY.n = 0, A.level = "high", YoY.level = "low")
  p.n.AHi.CHi <- Total_abundance(A.n = 1, YoY.n = 1, A.level = "high", YoY.level = "high")
  p.N <- bind_rows(p.n.ALo.CLo, p.n.ALo.CHi, p.n.AHi.CLo, p.n.AHi.CHi) %>%
    mutate(Abundance_A = ifelse(Abundance_A == "low", "Low", Abundance_A),
           Abundance_A = ifelse(Abundance_A == "high", "High", Abundance_A),
           Abundance_A = factor(Abundance_A, levels = c("Low", "High")),
           Abundance_C = ifelse(Abundance_C == "low", "Low", Abundance_C),
           Abundance_C = ifelse(Abundance_C == "high", "High", Abundance_C),
           Abundance_C = factor(Abundance_C, levels = c("Low", "High")),
           sum_to_one = rowSums(across(where(is.numeric)))) %>%
    arrange(Augment, Abundance_A, Abundance_C) 
  names(p.N) <- c("Abundance_adultAugmentation", "Abundance_adults", "Abundance_calves", "Low", "High", "sum_to_one")
  head(p.N)
  write_csv(p.N, "./Conditional_Probability_Tables/CPT_Abundance_caribou.csv")
  
  #'  -----------
  #####  Lambda  #####
  #'  -----------
  #'  Probability of population growth decreasing, remaining stable, or increasing
  #'  given the size of the caribou population
  bou_lambda <- function(bebe.phi, n.bou, bebe.level, bou.level) {
    #'  Adult female survival
    AF.phi <- c(1,2,3)
    #'  Calf survival
    YoY.phi <- bebe.phi
    #'  Total abundance of caribou population
    caribou.n <- n.bou
    
    #'  Define intercepts and slop coefficients
    #'  H: Population growth most strongly affected by adult female survival, 
    #'  followed by total caribou population size and then calf survival 
    alpha <- c(5, 7)
    beta1 <- -2.5 # Slope for adult female survival
    beta2 <- -1.25 # Slope for calf survival
    beta3 <- -1  # Slope for total caribou abundance effect
    
    #'  Calculate probability of lambda being decreasing given AF & YoY phi and total abundance level
    #'  Probability of lambda being in decreasing category
    (p.lambda.decrease <- 1/(1 + exp(-(alpha[1] + beta1*AF.phi + beta2*YoY.phi + beta3*caribou.n))))
    #'  Probability of lambda being in decreasing or stable categories
    (p.lambda.dec.stable <- 1/(1 + exp(-(alpha[2] + beta1*AF.phi + beta2*YoY.phi + beta3*caribou.n))))
    #'  Probability of lambda being in stable category
    (p.lambda.stable <- p.lambda.dec.stable - p.lambda.decrease)
    #'  Probability of lambda being in increasing category
    (p.lambda.increasing <- 1 - p.lambda.dec.stable)
    
    #'  Create data frame with probabilities of prey abundance being low or high
    (p.lambda <- cbind(p.lambda.decrease, p.lambda.stable, p.lambda.increasing)) 
    
    #'  Add climate change covariate data to data frame
    (df <- data.frame(survival_AF = AF.phi, survival_C = YoY.phi, N_caribou = caribou.n, 
                      p1 = p.lambda.decrease, p2 = p.lambda.stable, p3 = p.lambda.increasing) %>%
        mutate(survival_AF = ifelse(survival_AF == 1, "Low", survival_AF),
               survival_AF = ifelse(survival_AF == 2, "Medium", survival_AF),
               survival_AF = ifelse(survival_AF == 3, "High", survival_AF),
               survival_AF = factor(survival_AF, levels = c("Low", "Medium", "High")),
               survival_C = ifelse(survival_C == 1, "Low", survival_C),
               survival_C = ifelse(survival_C == 2, "Medium", survival_C),
               survival_C = ifelse(survival_C == 3, "High", survival_C),
               survival_C = factor(survival_C, levels = c("Low", "Medium", "High")),
               N_caribou = ifelse(N_caribou == 0, "Low", N_caribou),
               N_caribou = ifelse(N_caribou == 1, "High", N_caribou),
               N_caribou = factor(N_caribou, levels = c("Low", "High")),
               sum_to_one = rowSums(across(where(is.numeric)))))

    #'  Reformat data frame for easier plotting
    df_plot <- df %>% dplyr::select(-c(survival_C, N_caribou, sum_to_one)) %>%
      pivot_longer(cols = c('p1','p2','p3'), 
                   names_to = "p", values_to = "prob") 
    names(df_plot) <- c("survival_AF", "p", "prob")
    
    #'  Plot probability of population growth decreasing, stable, or increasing given
    #'  adult female and calf survival and total caribou abundance
    prediction_plot <- ggplot(df_plot, aes(x = survival_AF, y = prob)) + 
      ylim(0, 1) +
      geom_point(aes(color = p), position = position_dodge(0.1)) +
      xlab("Adult female survival")+
      ylab("Prob(Lambda category)")+
      ggtitle(paste("Population growth given adult female survival \nwith", bebe.level,
                    "calf survival and", bou.level, "total caribou abundance")) +
      theme(
        legend.position = "top",
        legend.justification = c("left"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
      scale_color_manual(name = '', labels = c('Decreasing lambda', 'Stable lamda', 'Increasing lambda'),
                         values = c('red', 'green', 'blue'))
    
    #'  Plot relationship
    plot(prediction_plot)
    #'  Return predictions
    return(df)
  }
  #'  Calculate probability of lambda being decreasing, stable, or increasing given 
  #'  adult female and calf survival (low = 1, medium = 2, high = 3) and total 
  #'  caribou abundance (low = 0, high = 1)
  p.lambda.YoYLo.nLo <- bou_lambda(bebe.phi = 1, n.bou = 0, bebe.level = "low", bou.level = "low")
  p.lambda.YoYLo.nHi <- bou_lambda(bebe.phi = 1, n.bou = 1, bebe.level = "low", bou.level = "high")
  p.lambda.YoYMed.nLo <- bou_lambda(bebe.phi = 2, n.bou = 0, bebe.level = "medium", bou.level = "low")
  p.lambda.YoYMed.nHi <- bou_lambda(bebe.phi = 2, n.bou = 1, bebe.level = "medium", bou.level = "high")
  p.lambda.YoYHi.nLo <- bou_lambda(bebe.phi = 3, n.bou = 0, bebe.level = "high", bou.level = "low")
  p.lambda.YoYHi.nHi <- bou_lambda(bebe.phi = 3, n.bou = 1, bebe.level = "high", bou.level = "high")
  p.lambda <- bind_rows(p.lambda.YoYLo.nLo, p.lambda.YoYLo.nHi, p.lambda.YoYMed.nLo, 
                       p.lambda.YoYMed.nHi, p.lambda.YoYHi.nLo, p.lambda.YoYHi.nHi) %>%
    arrange(survival_AF, survival_C, N_caribou) 
  names(p.lambda) <- c("Survival_adultFemale", "Survival_calf", "Abundance_caribou", 
                      "Decreasing", "Stable", "Increasing", "sum_to_one")
  head(p.lambda)
  write_csv(p.lambda, "./Conditional_Probability_Tables/CPT_Lambda.csv")
  
  #'  ----------------
  #####  ProbSuccess  #####
  #'  ----------------
  #'  Probability of reintroduction success given whether the population is most 
  #'  likely to be decreasing, stable, or increasing
  PrSuccess <- function() {
    #'  Lambda categories (1 = decreasing, 2 = stable, 3 = increasing)
    lambda <- c(1, 2, 3)
    
    #'  Define intercept and slope coefficients
    #'  H: increasing lambda leads to highest success probability, stable lambda
    #'  is less successful, and decreasing lambda is failure
    alpha <- c(3, 4, 5, 6, 7, 8, 9, 10, 11) # Intercepts for low probability of being in each bin 
    beta1 <- -3.5 # -4 # Slope for lambda categories
    
    #'  Calculate probability of success given different lambda categories
    (p.success.0.1 <- 1/(1 + exp(-(alpha[1] + beta1*lambda)))) 
    (p.success.0.2 <- 1/(1 + exp(-(alpha[2] + beta1*lambda))))
    (p.success.0.3 <- 1/(1 + exp(-(alpha[3] + beta1*lambda))))
    (p.success.0.4 <- 1/(1 + exp(-(alpha[4] + beta1*lambda))))
    (p.success.0.5 <- 1/(1 + exp(-(alpha[5] + beta1*lambda))))
    (p.success.0.6 <- 1/(1 + exp(-(alpha[6] + beta1*lambda))))
    (p.success.0.7 <- 1/(1 + exp(-(alpha[7] + beta1*lambda))))
    (p.success.0.8 <- 1/(1 + exp(-(alpha[8] + beta1*lambda))))
    (p.success.0.9 <- 1/(1 + exp(-(alpha[9] + beta1*lambda))))
    (p.success.1.2 <- p.success.0.2 - p.success.0.1)
    (p.success.2.3 <- p.success.0.3 - p.success.0.2)
    (p.success.3.4 <- p.success.0.4 - p.success.0.3)
    (p.success.4.5 <- p.success.0.5 - p.success.0.4)
    (p.success.5.6 <- p.success.0.6 - p.success.0.5)
    (p.success.6.7 <- p.success.0.7 - p.success.0.6)
    (p.success.7.8 <- p.success.0.8 - p.success.0.7)
    (p.success.8.9 <- p.success.0.9 - p.success.0.8)
    (p.success.9.10 <- 1 - p.success.0.9)
    
    #'  Create data frame with probabilities of success
    (p.success <- cbind(p.success.0.1, p.success.1.2, p.success.2.3, p.success.3.4, p.success.4.5, 
                    p.success.5.6, p.success.6.7, p.success.7.8, p.success.8.9, p.success.9.10)) 
    
    #'  Add climate change covariate data to data frame
    (df <- data.frame(lambda = lambda, p1 = p.success.0.1, p2 = p.success.1.2, 
                      p3 = p.success.2.3, p4 = p.success.3.4, p5 = p.success.4.5, 
                      p6 = p.success.5.6, p7 = p.success.6.7, p8 = p.success.7.8, 
                      p9 = p.success.8.9, p10 = p.success.9.10) %>%
        mutate(lambda = ifelse(lambda == 1, "Decreasing", lambda),
               lambda = ifelse(lambda == 2, "Stable", lambda),
               lambda = ifelse(lambda == 3, "Increasing", lambda),
               lambda = factor(lambda, levels = c("Decreasing", "Stable", "Increasing")),
               sum_to_one = rowSums(across(where(is.numeric)))))
    #'  Lambda values
    lambda.val <- seq(0.1, 1, by = 0.1)
    lambda <- rep(lambda.val, 3)
    #'  Reformat data frame for easier plotting
    df_plot <- df %>% dplyr::select(-sum_to_one) %>%
      pivot_longer(cols = c('p1','p2','p3','p4','p5','p6','p7','p8','p9','p10'), 
                   names_to = "p", values_to = "prob") %>%
      bind_cols(lambda) 
    names(df_plot) <- c("lambda.scenario", "p", "prob", "Success")
    
    #'  Plot probability of prey abundance being low or high, given level
    #'  of available habitat
    prediction_plot <- ggplot(df_plot, aes(x = Success, y = prob, group = lambda.scenario)) + 
      ylim(0, 1) +
      geom_line(aes(color = lambda.scenario)) +
      geom_point(aes(color = lambda.scenario)) +
      xlab("Success bin")+
      ylab("Prob(Success bin)")+
      ggtitle(paste("Probability of success given each lambda scenario")) +
      theme(
        legend.position = "top",
        legend.justification = c("left"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) 
    
    #'  Plot relationship
    plot(prediction_plot)
    #'  Return predictions
    return(df)
  }
  p.PrSuccess <- PrSuccess()
  names(p.PrSuccess) <- c("Lambda", "0 to 0.1", "0.1 to 0.2", "0.2 to 0.3",
                        "0.3 to 0.4", "0.4 to 0.5", "0.5 to 0.6", "0.6 to 0.7", 
                        "0.7 to 0.8","0.8 to 0.9", "0.9 to 1", "sum_to_one")
  head(p.PrSuccess)
  write_csv(p.PrSuccess, "./Conditional_Probability_Tables/CPT_ProbSuccess.csv")
  
  #'  ------------
  #####  Utility  #####
  #'  ------------
  #'  How much utility do we get out of the decision given the probability of success?
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  