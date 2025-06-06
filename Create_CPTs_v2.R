  #'  ---------------------------------
  #'  Making up stuff for caribou BBN
  #'  April 2025
  #'  Bassing
  #'  ---------------------------------

  #'  Load packages
  library(tidyverse)
  library(purrr)
  library(ggplot2)
  library(patchwork)
  
  #'  ------------------------------------------------
  ####  Conditional Probability Tables for each node  ####
  #'  ------------------------------------------------
  #'  -------------------------
  #####  Habitat_availability  #####
  #'  -------------------------
  #'  Predict probability of available habitat across range of binned areas based
  #'  on different levels of expected climate change scenarios and levels of timber harvest
  habitat <- function(timber, timber.level) {
    #'  Climate scenarios (1 = bad, 2 = worse, 3 = we fucked)
    climate <- c(1, 2, 3)
    #'  Levels of timber harvest
    timber.extract <- timber
    
    #'  Define intercept and slope coefficients
    #'  H: The hotter the climate, the less suitable habitat will be available
    alpha <- c(-6, -5, -4, -3, -2, -1, 0, 1, 2) # Intercepts for low probability of being in each bin c(-5, -4, -3, -2, -1, 0, 1, 2, 3) making more negative shifts peak/slope of scenarios right
    beta1 <- 0.75 # Climate effect scenario (increase beta1 to increase diff btwn scenarios)
    beta2 <- 2 # Timber harvest effect
    
    #'  Calculate probability of habitat availability being in each size bin given
    #'  climate change scenarios (bad, worse, and we fucked)
    (p.hab.0.50 <- 1/(1 + exp(-(alpha[1] + beta1*climate + beta2*timber.extract)))) 
    (p.hab.0.100 <- 1/(1 + exp(-(alpha[2] + beta1*climate + beta2*timber.extract))))
    (p.hab.0.150 <- 1/(1 + exp(-(alpha[3] + beta1*climate + beta2*timber.extract))))
    (p.hab.0.200 <- 1/(1 + exp(-(alpha[4] + beta1*climate + beta2*timber.extract))))
    (p.hab.0.250 <- 1/(1 + exp(-(alpha[5] + beta1*climate + beta2*timber.extract))))
    (p.hab.0.300 <- 1/(1 + exp(-(alpha[6] + beta1*climate + beta2*timber.extract))))
    (p.hab.0.350 <- 1/(1 + exp(-(alpha[7] + beta1*climate + beta2*timber.extract))))
    (p.hab.0.400 <- 1/(1 + exp(-(alpha[8] + beta1*climate + beta2*timber.extract))))
    (p.hab.0.450 <- 1/(1 + exp(-(alpha[9] + beta1*climate + beta2*timber.extract))))
    (p.hab.50.100 <- p.hab.0.100 - p.hab.0.50)
    (p.hab.100.150 <- p.hab.0.150 - p.hab.0.100)
    (p.hab.150.200 <- p.hab.0.200 - p.hab.0.150)
    (p.hab.200.250 <- p.hab.0.250 - p.hab.0.200)
    (p.hab.250.300 <- p.hab.0.300 - p.hab.0.250)
    (p.hab.300.350 <- p.hab.0.350 - p.hab.0.300)
    (p.hab.350.400 <- p.hab.0.400 - p.hab.0.350)
    (p.hab.400.450 <- p.hab.0.450 - p.hab.0.400)
    (p.hab.450.500 <- 1 - p.hab.0.450)
    
    #'  Add climate change covariate data to data frame
    (df <- data.frame(climate.scenario = climate, timber.harvest = timber.extract, 
                      p1 = p.hab.0.50, p2 = p.hab.50.100, p3 = p.hab.100.150, 
                      p4 = p.hab.150.200, p5 = p.hab.200.250, p6 = p.hab.250.300, 
                      p7 = p.hab.300.350, p8 = p.hab.350.400, p9 = p.hab.400.450, p10 = p.hab.450.500) %>%
      mutate(climate.scenario = ifelse(climate.scenario == 1, "Bad", climate.scenario),
             climate.scenario = ifelse(climate.scenario == 2, "Worse", climate.scenario),
             climate.scenario = ifelse(climate.scenario == 3, "We fucked", climate.scenario),
             climate.scenario = factor(climate.scenario, levels = c("Bad", "Worse", "We fucked")),
             timber.harvest = timber.level,
             sum_to_one = rowSums(across(where(is.numeric)))))
    #'  Habitat availability (amount of suitable habitat in sq-km)
    habitat.avail <- seq(50, 500, by = 50)
    habitat <- rep(habitat.avail, 3)
    #'  Reformat data frame for easier plotting
    df_plot <- df %>% dplyr::select(-sum_to_one) %>%
      pivot_longer(cols = c('p1','p2','p3','p4','p5','p6','p7','p8','p9','p10'), 
                                   names_to = "p", values_to = "prob") %>%
      bind_cols(habitat) 
    names(df_plot) <- c("climate.scenario", "timber.harvest", "p", "prob", "habitat.bin")
    df_plot <- mutate(df_plot, habitat.bin = as.factor(habitat.bin))
    
    #'  Plot probability of each habitat availability bin given each climate scenario and a harvest level
    #'  Lots of categories so hard to visualize
    prediction_plotv1 <- ggplot(df_plot, aes(x = climate.scenario, y = prob, group = habitat.bin)) + 
      ylim(0, 1) +
      geom_line(aes(color = habitat.bin)) +
      geom_point(aes(color = habitat.bin)) +
      xlab("Climate scenario")+
      ylab("Prob(Habitat availability bin)")+
      ggtitle(paste(timber.level, "timber harvest")) +
      theme(
        legend.position = "top",
        legend.justification = c("left"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))
    #'  Re-plotting a little backwards but in a way that's easier to visualize 
    #'  the relationships by grouping by climate scenarios and plotting across
    #'  habitat bins
    prediction_plotv2 <- ggplot(df_plot, aes(x = habitat.bin, y = prob, group = climate.scenario)) +
      ylim(0, 1) +
      geom_line(aes(color = climate.scenario)) +
      geom_point(aes(color = climate.scenario)) +
      xlab("Habitat availability bin")+
      ylab("Prob(Habitat availability bin)")+
      ggtitle(paste(timber.level, "timber harvest")) +
      theme(
        legend.position = "top",
        legend.justification = c("left"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))
    
    #' #'  Plot relationship
    #' plot(prediction_plotv1)
    #' plot(prediction_plotv2)
    
    #'  List all outputs
    out <- list(df, prediction_plotv1, prediction_plotv2)
    #'  Return predictions
    return(out)
  }
  p.habitat.timberLow <- habitat(timber = 1, timber.level = "Low") 
  p.habitat.timberMod <- habitat(timber = 2, timber.level = "Moderate") 
  p.habitat.timberHi <- habitat(timber = 3, timber.level = "High") 
  #'  Visualize 
  (p.habitat.plotsv1 <- p.habitat.timberLow[[2]] + p.habitat.timberMod[[2]] + 
      p.habitat.timberHi[[2]] + plot_annotation(title = 'Habitat availability given varying climate scenarios and...') +
      plot_layout(guides = "collect") & theme(legend.position = 'top'))
  (p.habitat.plotsv2 <- p.habitat.timberLow[[3]] + p.habitat.timberMod[[3]] + 
      p.habitat.timberHi[[3]] + plot_annotation(title = 'Habitat availability given varying climate scenarios and...') +
      plot_layout(guides = "collect") & theme(legend.position = 'top'))
  #'  Create full CPT
  p.habitat <- bind_rows(p.habitat.timberLow[[1]], p.habitat.timberMod[[1]], p.habitat.timberHi[[1]])
  names(p.habitat) <- c("Habitat_availability", "Timber_harvest", "0 to 50", "50 to 100", "100 to 150",
                        "150 to 200", "200 to 250", "250 to 300", "300 to 350",
                        "350 to 400", "400 to 450", "450 to 500", "sum_to_one")
  head(p.habitat)
  write_csv(p.habitat, filename = "./Conditional_Probability_Tables/CPT_v2_Habitat_availability.csv")
  ggsave("./Conditional_Probability_Tables/CPT_plots_habitat.tiff", p.habitat.plotsv1, 
         units = "in", width = 12, height = 6, dpi = 400, device = 'tiff', compression = "lzw") 
  
  #'  ----------------------
  #####  Abundance_altPrey  #####
  #'  ----------------------
  #'  Predict probability of low, medium, or high abundances of alternative prey
  #'  sources (e.g., deer, elk, moose) under varying climate scenarios & levels of timber harvest
  Prey_abundance <- function(timber, timber.level) {
    #'  Climate scenarios (1 = bad, 2 = worse, 3 = we fucked)
    climate <- c(1, 2, 3)
    #'  Levels of timber harvest
    timber.extract <- timber
    
    #'  Define intercept and slope coefficients
    #'  Greater timber harvest increases abundance of alternative prey but more intense
    #'  climate scenarios reduce abundance of alternative prey
    alpha <- c(-2, 0) # Intercept for low prey abundance
    beta1 <- 2 # Slope for climate effect
    beta2 <- -3.5 # Slope for timber harvest
    beta3 <- 0.85 # Slope for interaction between climate and harvest effects
    
    #'  Calculate probability alternative prey abundance will be low, medium, or high given 
    #'  varying climate scenarios and levels of harvest
    (p.PreynLo <- 1/(1 + exp(-(alpha[1] + beta1*climate + beta2*timber.extract + beta3*climate*timber.extract))))
    (p.PreynLoMed <- 1/(1 + exp(-(alpha[2] + beta1*climate + beta2*timber.extract + beta3*climate*timber.extract))))
    (p.PreynMed <- p.PreynLoMed - p.PreynLo)
    (p.PreynHi <- 1 - p.PreynLoMed)
    
    #'  Create data frame with probabilities of prey abundance being low or high
    (p.Preyn <- cbind(p.PreynLo, p.PreynMed, p.PreynHi))
    #'  Add habitat availability covariate data to data frame
    df <- data.frame(climate.scenario = climate, timber.harvest = timber.extract, 
                     p1 = p.PreynLo, p2 = p.PreynMed, p3 = p.PreynHi) %>%
      mutate(climate.scenario = ifelse(climate.scenario == 1, "Bad", climate.scenario),
             climate.scenario = ifelse(climate.scenario == 2, "Worse", climate.scenario),
             climate.scenario = ifelse(climate.scenario == 3, "World on fire", climate.scenario),
             climate.scenario = factor(climate.scenario, levels = c("Bad", "Worse", "World on fire")),
             timber.harvest = timber.level,
             sum_to_one = rowSums(across(where(is.numeric))))
    #'  Reformat data frame for easier plotting
    df_plot <- df %>% dplyr::select(-sum_to_one) %>%
      pivot_longer(cols = c('p1','p2', 'p3'), names_to = "p", values_to = "prob")
    
    #'  Plot probability of prey abundance being low or high, given level
    #'  of available habitat
    prediction_plot <- ggplot(df_plot, aes(x = climate.scenario, y = prob, group = p)) + 
      ylim(0, 1) +
      geom_line(aes(color = p)) +
      geom_point(aes(color = p)) +
      xlab("Climate change scenario")+
      ylab("Prob(Prey abundance)")+
      ggtitle(paste(timber.level, "timber harvest")) +
      theme(
        legend.position = "top",
        legend.justification = c("left"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
      scale_color_manual(name = '', labels = c('Low prey N', 'Medium prey N', 'High prey N'),
                         values = c('red', 'green', 'blue'))
    
    #' #'  Plot relationship
    #' plot(prediction_plot)
    #'  List
    out <- list(df, prediction_plot)
    #'  Return predictions
    return(out)
  }
  p.PreyN.Low <- Prey_abundance(timber = 1, timber.level = "Low") 
  p.PreyN.Med <- Prey_abundance(timber = 2, timber.level = "Medium") 
  p.PreyN.Hi <- Prey_abundance(timber = 3, timber.level = "High") 
  #'  Visualize
  (p.PreyN.plots <- p.PreyN.Low[[2]] + p.PreyN.Med[[2]] + p.PreyN.Hi[[2]] + 
      plot_annotation(title = 'Abundance of alternative prey given interactive effects of varying climate scenarios and...') +
      plot_layout(guides = "collect") & theme(legend.position = 'top'))
  #'  Create full CPT
  p.PreyN <- bind_rows(p.PreyN.Low[[1]], p.PreyN.Med[[1]], p.PreyN.Hi[[1]])
  names(p.PreyN) <- c("Changing_climate", "Timber_harvest", "Low", "Medium", "High", "sum_to_one")
  head(p.PreyN)
  write_csv(p.PreyN, "./Conditional_Probability_Tables/CPT_v2_Abundance_altPrey.csv")
  ggsave("./Conditional_Probability_Tables/CPT_plots_Abundance_altPrey.tiff", p.PreyN.plots, 
         units = "in", width = 10, height = 12, dpi = 400, device = 'tiff', compression = "lzw") 
  
  
  #'  ------------------------
  #####  Abundance_predators  #####
  #'  ------------------------
  #'  Predict probability of low vs high abundances of predators (e.g., wolf, 
  #'  cougar, bear) based on whether alternative prey populations are rare (1) 
  #'  or abundant (2) and whether predator control does not (0) or does (1) occur
  Pred_abundance <- function(control, control.level) {
    #'  Alternative prey abundance (low, medium or high) 
    N.altPrey <- c(1, 2, 3)
    #'  Holding whether predator control occurs at a fixed level
    Pred.control <- control
    
    #'  Define intercept and slope coefficients
    #'  H: More alternative prey increases predator abundance but predator control
    #'  efforts reduce predator abundance
    alpha <- c(4.75, 7) # Intercept for low and low-medium predator abundance
    beta1 <- -3 # Slope for alternative prey effect
    beta2 <- 1.75 # Slope for predator control effect
    
    #'  Calculate probability predator abundance will be low vs high given different
    #'  levels of alternative prey sources (low or high) and whether predator 
    #'  control occurs (yes or no)
    (p.PrednLo <- 1/(1 + exp(-(alpha[1] + beta1*N.altPrey + beta2*Pred.control))))
    (p.PrednLoMed <- 1/(1 + exp(-(alpha[2] + beta1*N.altPrey + beta2*Pred.control))))
    (p.PrednMed <- p.PrednLoMed - p.PrednLo) 
    (p.PrednHi <- 1 - p.PrednLoMed)
    
    #'  Create data frame with probabilities of predator abundance being low or high
    (p.Predn <- cbind(p.PrednLo, p.PrednMed, p.PrednHi))
    #'  Add prey and predator control covariate data to data frame  
    df <- data.frame(N.altPrey = N.altPrey, control = control.level, 
                     p1 = p.PrednLo, p2 = p.PrednMed, p3 = p.PrednHi) %>%
      mutate(N.altPrey = ifelse(N.altPrey == 1, "Low", N.altPrey),
             N.altPrey = ifelse(N.altPrey == 2, "Medium", N.altPrey),
             N.altPrey = ifelse(N.altPrey == 3, "High", N.altPrey),
             N.altPrey = factor(N.altPrey, levels = c("Low", "Medium", "High")))
    #'  Reformat data frame for easier plotting
    df_plot <- df %>% dplyr::select(-control) %>%
      pivot_longer(cols = c('p1','p2', 'p3'), names_to = "p", values_to = "prob")
    
    #'  Plot probability of predator abundance being low or high, given level
    #'  of alternative prey sources and predator control
    prediction_plot <- ggplot(df_plot, aes(x = N.altPrey, y = prob)) + 
      ylim(0, 1) +
      geom_point(aes(color = p), position = position_dodge(0.1)) +
      xlab("Alternative prey abundance")+
      ylab("Prob(Predator abundance)")+
      ggtitle(paste(control.level, "predator control")) +
      theme(
        legend.position = "top",
        legend.justification = c("left"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
      scale_color_manual(name = '', labels = c('Low predator N', 'Medium predator N', 'High predator N'),
                         values = c('red', 'green', 'blue'))
    
    #' #'  Plot relationship
    #' plot(prediction_plot)
    #'  List
    out <- list(df, prediction_plot)
    #'  Return predictions
    return(out)
  }
  #'  Calculate probability of predator abundance being low or high given level
  #'  of alternative prey sources (low or high) and whether predator control
  #'  occurs (yes or no)
  p.PredN.noCon <- Pred_abundance(control = 0, control.level = "Without")
  p.PredN.wCon <- Pred_abundance(control = 1, control.level = "With")
  #'  Visualize
  (p.PredN.plots <- p.PredN.noCon[[2]] + p.PredN.wCon[[2]] + 
      plot_annotation(title = 'Predator abundance given interactive effects of alternative prey abundance...') +
      plot_layout(guides = "collect") & theme(legend.position = 'top'))
  #'  Create CPT
  p.PredN <- bind_rows(p.PredN.noCon[[1]], p.PredN.wCon[[1]]) %>%
    mutate(control = ifelse(control == "Without", "No", control),
           control = ifelse(control == "With", "Yes", control),
           control = factor(control, levels = c("No", "Yes")),
           sum_to_one = rowSums(across(where(is.numeric)))) %>%
    arrange(N.altPrey, control) 
  names(p.PredN) <- c("Abundance_altPrey", "Predator_control", "Low", "Medium", "High", "sum_to_one")
  head(p.PredN)
  write_csv(p.PredN, "./Conditional_Probability_Tables/CPT_v2_Abundance_predators.csv")
  ggsave("./Conditional_Probability_Tables/CPT_plots_Abundance_predators.tiff", p.PredN.plots, 
         units = "in", width = 12, height = 6, dpi = 400, device = 'tiff', compression = "lzw") 
  
  
  #'  -------------------
  #####  Predation_rate  #####
  #'  -------------------
  #'  Predict probability of predation rate on caribou given predator abundance
  #'  and road density
  predation <- function(pred, pred.level) {
    #'  Level of road density (1 = low, 2 = medium, 3 = high)
    road <- c(1, 2, 3)
    #'  Level of predator abundance
    N.pred <- pred
    
    #'  Define intercept and slope coefficients 
    #'  H: predation rate increases with predator abundance and increasing road density,
    #'  which interaction where predator abundance is less important at lower road densities
    alpha <- c(8, 9, 10, 11, 12, 13, 14, 15, 16) # Intercepts for low and low/moderate predation categories
    beta1 <- -1.5 # Slope for road effect
    beta2 <- -2.5 # Slope for predator abundance effect
    beta3 <- -0.35 # Slope for interaction between predator abundance and road density
    
    #'  Calculate probability of predation rate given road density, predator abundance,
    #'  and an interaction between the two
    (p.pred.0.10 <- 1/(1 + exp(-(alpha[1] + beta1*road + beta2*N.pred + beta3*road*N.pred)))) 
    (p.pred.0.20 <- 1/(1 + exp(-(alpha[2] + beta1*road + beta2*N.pred + beta3*road*N.pred))))
    (p.pred.0.30 <- 1/(1 + exp(-(alpha[3] + beta1*road + beta2*N.pred + beta3*road*N.pred))))
    (p.pred.0.40 <- 1/(1 + exp(-(alpha[4] + beta1*road + beta2*N.pred + beta3*road*N.pred))))
    (p.pred.0.50 <- 1/(1 + exp(-(alpha[5] + beta1*road + beta2*N.pred + beta3*road*N.pred))))
    (p.pred.0.60 <- 1/(1 + exp(-(alpha[6] + beta1*road + beta2*N.pred + beta3*road*N.pred))))
    (p.pred.0.70 <- 1/(1 + exp(-(alpha[7] + beta1*road + beta2*N.pred + beta3*road*N.pred))))
    (p.pred.0.80 <- 1/(1 + exp(-(alpha[8] + beta1*road + beta2*N.pred + beta3*road*N.pred))))
    (p.pred.0.90 <- 1/(1 + exp(-(alpha[9] + beta1*road + beta2*N.pred + beta3*road*N.pred))))
    (p.pred.10.20 <- p.pred.0.20 - p.pred.0.10)
    (p.pred.20.30 <- p.pred.0.30 - p.pred.0.20)
    (p.pred.30.40 <- p.pred.0.40 - p.pred.0.30)
    (p.pred.40.50 <- p.pred.0.50 - p.pred.0.40)
    (p.pred.50.60 <- p.pred.0.60 - p.pred.0.50)
    (p.pred.60.70 <- p.pred.0.70 - p.pred.0.60)
    (p.pred.70.80 <- p.pred.0.80 - p.pred.0.70)
    (p.pred.80.90 <- p.pred.0.90 - p.pred.0.80)
    (p.pred.90.100 <- 1 - p.pred.0.90)
    
    #'  Add climate change covariate data to data frame
    (df <- data.frame(road.density = road, pred.abundance = N.pred, 
                      p1 = p.pred.0.10, p2 = p.pred.10.20, p3 = p.pred.20.30, 
                      p4 = p.pred.30.40, p5 = p.pred.40.50, p6 = p.pred.50.60, 
                      p7 = p.pred.60.70, p8 = p.pred.70.80, p9 = p.pred.80.90, p10 = p.pred.90.100) %>%
        mutate(road.density = ifelse(road.density == 1, "Low", road.density),
               road.density = ifelse(road.density == 2, "Moderate", road.density),
               road.density = ifelse(road.density == 3, "High", road.density),
               road.density = factor(road.density, levels = c("Low", "Moderate", "High")),
               pred.abundance = pred.level,
               sum_to_one = rowSums(across(where(is.numeric)))))
    #'  Predation rate
    pred.rate <- seq(10, 100, by = 10)
    predation <- rep(pred.rate, 3)
    #'  Reformat data frame for easier plotting
    df_plot <- df %>% dplyr::select(-sum_to_one) %>%
      pivot_longer(cols = c('p1','p2','p3','p4','p5','p6','p7','p8','p9','p10'), 
                   names_to = "p", values_to = "prob") %>%
      bind_cols(predation) 
    names(df_plot) <- c("Road_density", "Abundance_predators", "p", "prob", "predation.bin")
    df_plot <- mutate(df_plot, predation.bin = as.factor(predation.bin))
    
    #'  Plot probability of each predation rate bin given road density and predator abundance levels
    prediction_plotv1 <- ggplot(df_plot, aes(x = Road_density, y = prob, group = predation.bin)) + 
      ylim(0, 1) +
      geom_line(aes(color = predation.bin)) +
      geom_point(aes(color = predation.bin)) +
      xlab("Road density")+
      ylab("Prob(Predation rate bin)")+
      ggtitle(paste(pred.level, "predator abundance")) +
      theme(
        legend.position = "top",
        legend.justification = c("left"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))
    #'  Re-plotting a little backwards but in a way that's easier to visualize 
    #'  the relationships by grouping by road density and plotting across
    #'  predation bins    
    df_plot <- mutate(df_plot, predation.bin = predation)
    prediction_plotv2 <- ggplot(df_plot, aes(x = predation.bin, y = prob, group = Road_density)) + 
      ylim(0, 1) +
      geom_line(aes(color = Road_density)) +
      geom_point(aes(color = Road_density)) +
      xlab("Predation rate bin")+
      ylab("Prob(Predation rate bin)")+
      ggtitle(paste(pred.level, "predator abundance")) +
      theme(
        legend.position = "top",
        legend.justification = c("left"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) 
    
    #' #'  Plot relationship
    #' plot(prediction_plotv1)
    #' plot(prediction_plotv2)
    
    #'  List all outputs
    out <- list(df, prediction_plotv1, prediction_plotv2)
    #'  Return predictions
    return(out)
  }
  p.predation.NpredLow <- predation(pred = 1, pred.level = "Low") 
  p.predation.NpredMod <- predation(pred = 2, pred.level = "Moderate") 
  p.predation.NpredHi <- predation(pred = 3, pred.level = "High") 
  #'  Visualize 
  (p.predation.plotsv1 <- p.predation.NpredLow[[2]] + p.predation.NpredMod[[2]] + 
      plot_annotation(title = 'Predation rate given interaction between road density and...') +
      p.predation.NpredHi[[2]] + plot_layout(guides = "collect") & theme(legend.position = 'top'))
  (p.predation.plotsv2 <- p.predation.NpredLow[[3]] + p.predation.NpredMod[[3]] + 
      plot_annotation(title = 'Predation rate given interaction between road density and...') +
      p.predation.NpredHi[[3]] + plot_layout(guides = "collect") & theme(legend.position = 'top'))
  #'  Create full CPT
  p.predation <- bind_rows(p.predation.NpredLow[[1]], p.predation.NpredMod[[1]], p.predation.NpredHi[[1]])
  names(p.predation) <- c("Road_density", "Abundance_predators", "0 to 10", "10 to 20", "20 to 30",
                        "30 to 40", "40 to 50", "50 to 60", "60 to 70",
                        "70 to 80", "80 to 90", "90 to 100", "sum_to_one")
  head(p.predation)
  write_csv(p.predation, "./Conditional_Probability_Tables/CPT_v2_Predation_Rate.csv")
  ggsave("./Conditional_Probability_Tables/CPT_plots_predation.tiff", p.predation.plotsv1, 
         units = "in", width = 12, height = 6, dpi = 400, device = 'tiff', compression = "lzw") 
  
  
  #'  -------------------
  #####  Body_condition  #####
  #'  -------------------
  #'  Predict probability of body condition being poor, moderate, or good, given
  #'  availability of quality habitat, level of human recreation and disease/parasites
  myhotbody <- function(human.rec, disease, human.level, disease.prev) {
    #'  Habitat availability
    habitat <- seq(50, 500, by = 50)
    habitat.z <- scale(habitat)
    #'  Human recreation/activity level
    human <- human.rec
    #'  Disease and parasite prevalence
    sick <- disease
    
    #'  Define intercept and slope coefficients 
    #'  H: Caribou body condition is influenced most strongly by habitat availability,
    #'  then by stress induced disease/parasite prevalence (low v high) and then
    #'  by the level of human activity (low, moderate, high)
    alpha <- c(-3, 0) # Intercepts for low and low/moderate body condition categories
    beta1 <- -3 # Slope coefficient for habitat effect
    beta2 <- 1.25 # Slope coefficient for human activity effect
    beta3 <- 2 # Slope coefficient for disease/parasite effect
    
    #'  Calculate probability of adult male survival being poor, moderate, or good
    #'  given varying amounts of available habitat, levels of predator abundance, 
    #'  and whether supplemental feeding occurs
    (p.BodyLo <- 1/(1 + exp(-(alpha[1] + beta1*habitat.z + beta2*human + beta3*sick))))
    #'  Probability of being in low or moderate body condition category given all of the above
    (p.BodyLoMod <- 1/(1 + exp(-(alpha[2] + beta1*habitat.z + beta2*human + beta3*sick))))
    #'  Probability of being in moderate body condition category given all of the above
    (p.BodyMod <- p.BodyLoMod - p.BodyLo)
    #'  Probability of being in high body condition category given all of the above
    (p.BodyHi <- 1 - (p.BodyLoMod))
    
    #'  Create data frame with probabilities of caribou body condition being poor, moderate, or good
    df <- data.frame(habitat.avail = habitat, human.activity = human.level, disease = disease.prev, 
                     p1 = p.BodyLo, p2 = p.BodyMod, p3 = p.BodyHi) 
    #'  Reformat data frame for easier plotting
    df_plot <- df %>% dplyr::select(-c(human.activity, disease)) %>%
      pivot_longer(cols = c('p1','p2', 'p3'), names_to = "p", values_to = "prob")
    #'  Plot probability of caribou body condition being poor, moderate, or good, given
    #'  habitat availability, human recreation/activity, and disease/parasite prevalence
    prediction_plot <- ggplot(df_plot, aes(x = habitat.avail, y = prob)) + 
      ylim(0, 1)+
      geom_line(aes(color = p)) +
      xlab("Habitat availability")+
      ylab("Prob(Body condition)")+
      ggtitle(paste(disease.prev, "disease prevalance and\n", human.level, "human recreation")) +
      theme(
        legend.position = "top",
        legend.justification = c("left"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
      scale_color_manual(name = '', labels = c('Poor condition', 'Moderate condition', 'Good condition'),
                         values = c('red', 'green', 'blue'))
    #' #'  Plot relationship
    #' plot(prediction_plot)
    #'  List
    out <- list(df, prediction_plot)
    #'  Return predictions
    return(out)
  }
  #'  Calculate probability of caribou body condition being poor, moderate, or good,
  #'  given habitat availability, human recreation/activity level, and level of
  #'  disease/parasite prevalence (0 = low, 1 = high)
  p.bodyCon.humanLo.sickLo <- myhotbody(human = 1, disease = 0, human.level = "low", disease.prev = "Low")
  p.bodyCon.humanLo.sickHi <- myhotbody(human = 1, disease = 1, human.level = "low", disease.prev = "High")
  p.bodyCon.humanMod.sickLo <- myhotbody(human = 2, disease = 0, human.level = "moderate", disease.prev = "Low")
  p.bodyCon.humanMod.sickHi <- myhotbody(human = 2, disease = 1, human.level = "moderate", disease.prev = "High")
  p.bodyCon.humanHi.sickLo <- myhotbody(human = 3, disease = 0, human.level = "high", disease.prev = "Low")
  p.bodyCon.humanHi.sickHi <- myhotbody(human = 3, disease = 1, human.level = "high", disease.prev = "High")
  #'  Visualize
  (p.bodyCon.plots <- p.bodyCon.humanLo.sickLo[[2]] + p.bodyCon.humanLo.sickHi[[2]] +
      p.bodyCon.humanMod.sickLo[[2]] + p.bodyCon.humanMod.sickHi[[2]] +
      p.bodyCon.humanHi.sickLo[[2]] + p.bodyCon.humanHi.sickHi[[2]] + plot_layout(ncol = 2) +
      plot_annotation(title = 'Caribou body condition given effects of habitat availability...') +
      plot_layout(guides = "collect") & theme(legend.position = 'top'))
  #'  Create CPT
  p.bodyCon <- bind_rows(p.bodyCon.humanLo.sickLo[[1]], p.bodyCon.humanLo.sickHi[[1]], p.bodyCon.humanMod.sickLo[[1]], 
                       p.bodyCon.humanMod.sickHi[[1]], p.bodyCon.humanHi.sickLo[[1]], p.bodyCon.humanHi.sickHi[[1]]) %>%
    mutate(human.activity = ifelse(human.activity == "low", "Low", human.activity),
           human.activity = ifelse(human.activity == "moderate", "Moderate", human.activity),
           human.activity = ifelse(human.activity == "high", "High", human.activity),
           human.activity = factor(human.activity, levels = c("Low", "Moderate", "High")),
           disease = ifelse(disease == "low", "Low", disease),
           disease = ifelse(disease == "high", "High", disease),
           disease = factor(disease, levels = c("Low", "High")),
           sum_to_one = rowSums(.[4:6])) %>%
    arrange(habitat.avail, human.activity, disease) 
  names(p.bodyCon) <- c("Habitat_availability", "Human_recreation", "Disease_Parasites", "Poor", "Moderate", "Good", "sum_to_one")
  head(p.bodyCon)
  write_csv(p.bodyCon, "./Conditional_Probability_Tables/CPT_v2_Body_condition.csv")
  ggsave("./Conditional_Probability_Tables/CPT_plots_Body_condition.tiff", p.bodyCon.plots, 
         units = "in", width = 10, height = 12, dpi = 400, device = 'tiff', compression = "lzw") 
  
  
  #'  ------------------------
  #####  Survival_adultMales  #####
  #'  ------------------------
  #'  Predict probability of male survival being low, medium, or high under 
  #'  different levels of predation rates (ranging 0 - 100), body condition, and
  #'  whether supplemental feeding occurs
  AM_survival <- function(bod, Feeding, feed.level, condition.level) {   
    #'  Predation rate
    predation <- seq(10, 100, by = 10)
    #'  Center and scale habitat.avail so values don't range too widely
    predation.z <- scale(predation)
    #'  Holding body condition at a fixed level 
    little.fatty <- bod
    #'  Holding Supplemental feeding at a fixed level
    Supp.feeding <- Feeding
    
    #'  Define intercept and slope coefficients 
    #'  H: Survival decreases with increasing predation rate, followed by lower
    #'  body condition, but the body condition effect is countered to some degree
    #'  with supplemental feeding (interactive)
    alpha <- c(3, 5) # Intercepts for low and low/moderate survival categories
    beta1 <- 3 # Slope coefficient for predation rate
    beta2 <- -1.25 # Slope coefficient for body condition
    beta3 <- -0.25 # Slope coefficient for supplemental feeding
    beta4 <- -0.25 # Interaction between body condition and supplemental feeding
    
    #'  Calculate probability of adult male survival being low, moderate, or high 
    #'  given varying amounts of available habitat, levels of predator abundance, 
    #'  and whether supplemental feeding occurs
    (p.AMphiLo <- 1/(1 + exp(-(alpha[1] + beta1*predation.z + beta2*little.fatty + beta3*Supp.feeding + beta4*little.fatty*Supp.feeding))))
    #'  Probability of being in low or moderate survival category given all of the above
    (p.AMphiLoMod <- 1/(1 + exp(-(alpha[2] + beta1*predation.z + beta2*little.fatty + beta3*Supp.feeding + beta4*little.fatty*Supp.feeding))))
    #'  Probability of being in moderate survival category given all of the above
    (p.AMphiMod <- p.AMphiLoMod - p.AMphiLo)
    #'  Probability of being in high survival category given all of the above
    (p.AMphiHi <- 1 - (p.AMphiLoMod))
    
    #'  Create data frame with probabilities of adult male survival being low, medium, or high
    df <- data.frame(predation.rate = predation, body.condition = condition.level, food = feed.level, 
                     p1 = p.AMphiLo, p2 = p.AMphiMod, p3 = p.AMphiHi) 
    #'  Reformat data frame for easier plotting
    df_plot <- df %>% dplyr::select(-c(body.condition, food)) %>%
      pivot_longer(cols = c('p1','p2', 'p3'), names_to = "p", values_to = "prob")
    #'  Plot probability of adult male survival being low, medium, or high, given
    #'  habitat availability, predator abundance, and supplemental feeding
    prediction_plot <- ggplot(df_plot, aes(x = predation.rate, y = prob)) + 
      ylim(0, 1)+
      geom_line(aes(color = p)) +
      xlab("Predation rate")+
      ylab("Prob(Adult male survival)")+
      ggtitle(paste(feed.level, "suppemental feeding and \nbody condition is", condition.level)) +
      theme(
        legend.position = "top",
        legend.justification = c("left"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
      scale_color_manual(name = '', labels = c('Low AM phi', 'Moderate AM phi', 'High AM phi'),
                         values = c('red', 'green', 'blue'))
    
    #' #'  Plot relationship
    #' plot(prediction_plot)
    #'  List
    out <- list(df, prediction_plot)
    #'  Return predictions
    return(out)
  }
  #'  Calculate probability of adult male survival being low, moderate, or high given 
  #'  poor (1), moderate (2) or good (3) body condition & feeding does not (0) or does (1) occur
  p.AMphi.condPoor.noFeed <- AM_survival(bod = 1, Feeding = 0, condition.level = "poor", feed.level = "Without")
  p.AMphi.condPoor.wFeed <- AM_survival(bod = 1, Feeding = 1, condition.level = "poor", feed.level = "With")
  p.AMphi.condMod.noFeed <- AM_survival(bod = 2, Feeding = 0, condition.level = "moderate", feed.level = "Without")
  p.AMphi.condMod.wFeed <- AM_survival(bod = 2, Feeding = 1, condition.level = "moderate", feed.level = "With")
  p.AMphi.condGood.noFeed <- AM_survival(bod = 3, Feeding = 0, condition.level = "good", feed.level = "Without")
  p.AMphi.condGood.wFeed <- AM_survival(bod = 3, Feeding = 1, condition.level = "good", feed.level = "With")
  #'  Visualize
  (p.AMphi.plots <- p.AMphi.condPoor.noFeed[[2]] + p.AMphi.condPoor.wFeed[[2]] +
      p.AMphi.condMod.noFeed[[2]] + p.AMphi.condMod.wFeed[[2]] +
      p.AMphi.condGood.noFeed[[2]] + p.AMphi.condGood.wFeed[[2]] + 
      plot_annotation(title = 'Adult male survival given predation rate...') +
      plot_layout(ncol = 2) + plot_layout(guides = "collect") & theme(legend.position = 'top'))
  #'  Create CPT
  p.AMphi <- bind_rows(p.AMphi.condPoor.noFeed[[1]], p.AMphi.condPoor.wFeed[[1]], p.AMphi.condMod.noFeed[[1]], 
                       p.AMphi.condMod.wFeed[[1]], p.AMphi.condGood.noFeed[[1]], p.AMphi.condGood.wFeed[[1]]) %>%
    mutate(body.condition = ifelse(body.condition == "poor", "Poor", body.condition),
           body.condition = ifelse(body.condition == "moderate", "Moderate", body.condition),
           body.condition = ifelse(body.condition == "good", "Good", body.condition),
           body.condition = factor(body.condition, levels = c("Poor", "Moderate", "Good")),
           food = ifelse(food == "Without", "No", food),
           food = ifelse(food == "With", "Yes", food),
           food = factor(food, levels = c("No", "Yes")),
           sum_to_one = rowSums(.[4:6])) %>%
    arrange(predation.rate, body.condition, food) 
  names(p.AMphi) <- c("Predation_rate", "Body_condition", "Supplemental_feeding", "Low", "Medium", "High", "sum_to_one")
  head(p.AMphi)
  write_csv(p.AMphi, "./Conditional_Probability_Tables/CPT_v2_Survival_adultMales.csv")
  ggsave("./Conditional_Probability_Tables/CPT_plots_Survival_adultMales.tiff", p.AMphi.plots, 
         units = "in", width = 10, height = 12, dpi = 400, device = 'tiff', compression = "lzw") 
  
  #'  --------------------------
  #####  Survival_adultFemales  #####
  #'  --------------------------
  #'  Predict probability of female survival being low, medium, or high under 
  #'  different levels of predation rates (ranging 0 - 100), body condition, and
  #'  whether supplemental feeding occurs
  AF_survival <- function(bod, Feeding, feed.level, condition.level) {   
    #'  Predation rate
    predation <- seq(10, 100, by = 10)
    #'  Center and scale habitat.avail so values don't range too widely
    predation.z <- scale(predation)
    #'  Holding body condition at a fixed level 
    little.fatty <- bod
    #'  Holding Supplemental feeding at a fixed level
    Supp.feeding <- Feeding
    
    #'  Define intercept and slope coefficients 
    #'  H: Survival decreases with increasing predation rate, followed by lower
    #'  body condition, but the body condition effect is countered to some degree
    #'  with supplemental feeding (interactive)
    alpha <- c(2, 6) # Intercepts for low and low/moderate survival categories
    beta1 <- 4 # Slope coefficient for predation rate  
    beta2 <- -1 # Slope coefficient for body condition
    beta3 <- -0.25 # Slope coefficient for supplemental feeding
    beta4 <- -0.35 # Interaction between body condition and supplemental feeding
    
    #'  Calculate probability of adult female survival being low, moderate, or high 
    #'  given varying amounts of available habitat, levels of predator abundance, 
    #'  and whether supplemental feeding occurs
    (p.AFphiLo <- 1/(1 + exp(-(alpha[1] + beta1*predation.z + beta2*little.fatty + beta3*Supp.feeding + beta4*little.fatty*Supp.feeding))))
    #'  Probability of being in low or moderate survival category given all of the above
    (p.AFphiLoMod <- 1/(1 + exp(-(alpha[2] + beta1*predation.z + beta2*little.fatty + beta3*Supp.feeding + beta4*little.fatty*Supp.feeding))))
    #'  Probability of being in moderate survival category given all of the above
    (p.AFphiMod <- p.AFphiLoMod - p.AFphiLo)
    #'  Probability of being in high survival category given all of the above
    (p.AFphiHi <- 1 - (p.AFphiLoMod))
    
    #'  Create data frame with probabilities of adult male survival being low, medium, or high
    df <- data.frame(predation.rate = predation, body.condition = condition.level, food = feed.level, 
                     p1 = p.AFphiLo, p2 = p.AFphiMod, p3 = p.AFphiHi) 
    #'  Reformat data frame for easier plotting
    df_plot <- df %>% dplyr::select(-c(body.condition, food)) %>%
      pivot_longer(cols = c('p1','p2', 'p3'), names_to = "p", values_to = "prob")
    #'  Plot probability of adult remale survival being low, medium, or high, given
    #'  habitat availability, predator abundance, and supplemental feeding
    prediction_plot <- ggplot(df_plot, aes(x = predation.rate, y = prob)) +  
      ylim(0, 1)+
      geom_line(aes(color = p)) +
      xlab("Predation rate")+
      ylab("Prob(Adult female survival)")+
      ggtitle(paste(feed.level, "suppemental feeding and \nbody condition is", condition.level)) +
      theme(
        legend.position = "top",
        legend.justification = c("left"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
      scale_color_manual(name = '', labels = c('Low AF phi', 'Moderate AF phi', 'High AF phi'),
                         values = c('red', 'green', 'blue'))
    
    #' #'  Plot relationship
    #' plot(prediction_plot)
    #'  List
    out <- list(df, prediction_plot)
    #'  Return predictions
    return(out)
  }
  #'  Calculate probability of adult female survival being low, moderate, or high given 
  #'  poor (1), moderate (2) or good (3) body condition & feeding does not (0) or does (1) occur
  p.AFphi.condPoor.noFeed <- AF_survival(bod = 1, Feeding = 0, condition.level = "poor", feed.level = "Without")
  p.AFphi.condPoor.wFeed <- AF_survival(bod = 1, Feeding = 1, condition.level = "poor", feed.level = "With")
  p.AFphi.condMod.noFeed <- AF_survival(bod = 2, Feeding = 0, condition.level = "moderate", feed.level = "Without")
  p.AFphi.condMod.wFeed <- AF_survival(bod = 2, Feeding = 1, condition.level = "moderate", feed.level = "With")
  p.AFphi.condGood.noFeed <- AF_survival(bod = 3, Feeding = 0, condition.level = "good", feed.level = "Without")
  p.AFphi.condGood.wFeed <- AF_survival(bod = 3, Feeding = 1, condition.level = "good", feed.level = "With")
  #'  Visualize
  (p.AFphi.plots <- p.AFphi.condPoor.noFeed[[2]] + p.AFphi.condPoor.wFeed[[2]] +
      p.AFphi.condMod.noFeed[[2]] + p.AFphi.condMod.wFeed[[2]] +
      p.AFphi.condGood.noFeed[[2]] + p.AFphi.condGood.wFeed[[2]] + plot_layout(ncol = 2) +
      plot_annotation(title = 'Adult female survival given predation rate...') +
      plot_layout(guides = "collect") & theme(legend.position = 'top'))
  #'  Create CPT
  p.AFphi <- bind_rows(p.AFphi.condPoor.noFeed[[1]], p.AFphi.condPoor.wFeed[[1]], p.AFphi.condMod.noFeed[[1]], 
                       p.AFphi.condMod.wFeed[[1]], p.AFphi.condGood.noFeed[[1]], p.AFphi.condGood.wFeed[[1]]) %>%
    mutate(body.condition = ifelse(body.condition == "poor", "Poor", body.condition),
           body.condition = ifelse(body.condition == "moderate", "Moderate", body.condition),
           body.condition = ifelse(body.condition == "good", "Good", body.condition),
           body.condition = factor(body.condition, levels = c("Poor", "Moderate", "Good")),
           food = ifelse(food == "Without", "No", food),
           food = ifelse(food == "With", "Yes", food),
           food = factor(food, levels = c("No", "Yes")),
           sum_to_one = rowSums(.[4:6])) %>%
    arrange(predation.rate, body.condition, food) 
  names(p.AFphi) <- c("Predation_rate", "Body_condition", "Supplemental_feeding", "Low", "Medium", "High", "sum_to_one")
  head(p.AFphi)
  write_csv(p.AFphi, "./Conditional_Probability_Tables/CPT_v2_Survival_adultFemales.csv")
  ggsave("./Conditional_Probability_Tables/CPT_plots_Survival_adultFemales.tiff", p.AFphi.plots, 
         units = "in", width = 10, height = 12, dpi = 400, device = 'tiff', compression = "lzw") 
  
  #'  ---------------------------
  #####  Fecundity_adultFemales  #####
  #'  ---------------------------
  #'  Predict probability of female fecundity being low, medium, or high depending 
  #'  on whether supplemental feeding occurs (yes or no) over a range of available 
  #'  suitable habitat
  AF_fecundity <- function(Feeding, feed.level) {   
    #'  Caribou body condition (1 = poor, 2 = moderate, 3 = good)
    little.fatty <- c(1, 2, 3)
    #'  Holding Supplemental feeding at a fixed level
    Supp.feeding <- Feeding
    
    #'  Define intercept and slope coefficients 
    #'  H: Body condition is most important but supplemental feeding also increases fecundity
    alpha <- c(4, 5) # Intercepts for low and low/moderate fecundity categories
    beta1 <- -2 # Slope coefficient for body condition 
    beta2 <- -1.5 # Slope coefficient for supplemental feeding
    
    #'  Calculate probability of adult female fecundity being low, moderate, or high 
    #'  given varying levels of body condition and whether supplemental feeding occurs
    (p.AFfecLo <- 1/(1 + exp(-(alpha[1] + beta1*little.fatty + beta2*Supp.feeding))))
    #'  Probability of being in low or moderate fecundity category given all of the above
    (p.AFfecLoMod <- 1/(1 + exp(-(alpha[2] + beta1*little.fatty + beta2*Supp.feeding))))
    #'  Probability of being in moderate fecundity category given all of the above
    (p.AFfecMod <- p.AFfecLoMod - p.AFfecLo)
    #'  Probability of being in high fecundity category given all of the above
    (p.AFfecHi <- 1 - (p.AFfecLoMod))
    
    #'  Create data frame with probabilities of adult female fecundity being low, medium, or high
    (p.AFfec <- cbind(p.AFfecLo, p.AFfecMod, p.AFfecHi))
    #'  Add habitat availability covariate data to data frame  
    df <- data.frame(body.cond = little.fatty, food = feed.level, p1 = p.AFfecLo, 
                     p2 = p.AFfecMod, p3 = p.AFfecHi) 
    #'  Reformat data frame for easier plotting
    df_plot <- df %>% dplyr::select(-food) %>%
      pivot_longer(cols = c('p1','p2', 'p3'), names_to = "p", values_to = "prob") %>%
      mutate(body.cond = ifelse(body.cond == 1, "Poor", body.cond),
             body.cond = ifelse(body.cond == 2, "Moderate", body.cond),
             body.cond = ifelse(body.cond == 3, "Little fatty", body.cond),
             body.cond = factor(body.cond, levels = c("Poor", "Moderate", "Little fatty")))
    #'  Plot probability of adult female fecundity being low, medium, or high, 
    #'  given body condition and supplemental feeding
    prediction_plot <- ggplot(df_plot, aes(x = body.cond, y = prob)) + 
      ylim(0, 1)+
      # geom_line(aes(color = p)) +
      geom_point(aes(color = p), position = position_dodge(0.1)) +
      xlab("Caribou body condition")+
      ylab("Prob(Adult female fecundity)")+
      ggtitle(paste(feed.level, "suppemental feeding")) +
      theme(
        legend.position = "top",
        legend.justification = c("left"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
      scale_color_manual(name = '', labels = c('Low fecundity', 'Moderate fecundity', 'High fecundity'),
                         values = c('red', 'green', 'blue'))
    
    #' #'  Plot relationship
    #' plot(prediction_plot)
    #'  List
    out <- list(df, prediction_plot)
    #'  Return predictions
    return(out)
  }
  #'  Calculate probability of adult female fecundity being low, medium, or high 
  #'  under varying levels of body condition, given supplemental feeding does (1)
  #'  or does not (0) occur
  p.AFfec.noFeed <- AF_fecundity(Feeding = 0, feed.level = "Without")
  p.AFfec.wFeed <- AF_fecundity(Feeding = 1, feed.level = "With")
  #'  Visualize
  (p.AFfec.plots <- p.AFfec.noFeed[[2]] + p.AFfec.wFeed[[2]] +
      plot_annotation(title = 'Adult female fecundity given body condition...') +
      plot_layout(guides = "collect") & theme(legend.position = 'top'))
  #'  Create CPT
  p.AFfec <- bind_rows(p.AFfec.noFeed[[1]], p.AFfec.wFeed[[1]]) %>%
    mutate(body.cond = ifelse(body.cond == 1, "Poor", body.cond),
           body.cond = ifelse(body.cond == 2, "Moderate", body.cond),
           body.cond = ifelse(body.cond == 3, "Good", body.cond),
           body.cond = factor(body.cond, levels = c("Poor", "Moderate", "Good")),
           food = ifelse(food == "Without", "No", food),
           food = ifelse(food == "With", "Yes", food),
           food = factor(food, levels = c("No", "Yes")),
           sum_to_one = rowSums(.[3:5])) %>%
    arrange(body.cond, food) 
  names(p.AFfec) <- c("Body_condition", "Supplemental_feeding", "Low", "Medium", "High", "sum_to_one")
  head(p.AFfec)
  write_csv(p.AFfec, "./Conditional_Probability_Tables/CPT_v2_Fecundity_adultFemales.csv")
  ggsave("./Conditional_Probability_Tables/CPT_plots_Fecundity_adultFemales.tiff", p.AFfec.plots, 
         units = "in", width = 12, height = 6, dpi = 400, device = 'tiff', compression = "lzw") 
  
  #'  ------------------
  #####  Survival_calf  #####
  #'  ------------------
  #'  Predict probability of calf survival being low, medium, or high depending on 
  #'  different levels of female fecundity (low, medium, high), predator abundance 
  #'  (low or high), and whether maternal penning occurs (yes or no)
  YoY_survival <- function(pen, pen.level) {   
    #'  Predation rate
    predation <- seq(10, 100, by = 10)
    #'  Center and scale habitat.avail so values don't range too widely
    predation.z <- scale(predation)
    #'  Holding maternal penning at a fixed level
    maternal.pen <- pen
    
    #'  Define intercept and slope coefficients 
    #'  H: Predation rate most important but maternal penning can greatly offset 
    #'  the effects of predators 
    alpha <- c(1, 4) # Intercepts for low and low/moderate calf survival categories
    beta1 <- 2 # Slope coefficient for predator abundance 
    beta2 <- -1.75 # Slope coefficient for maternal penning
    
    #'  Calculate probability of calf survival being low, moderate, or high given 
    #'  different levels of predation rate and whether maternal penning occurs
    (p.YoYphiLo <- 1/(1 + exp(-(alpha[1] + beta1*predation.z + beta2*maternal.pen))))
    #'  Probability of being in low or moderate survival category given all of the above
    (p.YoYphiLoMod <- 1/(1 + exp(-(alpha[2] + beta1*predation.z + beta2*maternal.pen))))
    #'  Probability of being in moderate survival category given all of the above
    (p.YoYphiMod <- p.YoYphiLoMod - p.YoYphiLo)
    #'  Probability of being in high survival category given all of the above
    (p.YoYphiHi <- 1 - (p.YoYphiLoMod))
    
    #'  Create data frame with probabilities of calf survival being low, medium, or high
    (p.YoYphi <- cbind(p.YoYphiLo, p.YoYphiMod, p.YoYphiHi))
    #'  Add covariate data to data frame  
    df <- data.frame(pred.rate = predation, maternal_pen = pen.level, 
                     p1 = p.YoYphiLo, p2 = p.YoYphiMod, p3 = p.YoYphiHi)
    #'  Reformat data frame for easier plotting
    df_plot <- df %>% dplyr::select(-maternal_pen) %>%
      pivot_longer(cols = c('p1','p2', 'p3'), names_to = "p", values_to = "prob")
    #'  Plot probability of calf survival being low, medium, or high, given adult
    #'  female fecundity, predator abundance, and use of maternal penning
    prediction_plot <- ggplot(df_plot, aes(x = pred.rate, y = prob)) + 
      ylim(0, 1) +
      geom_line(aes(color = p)) +
      xlab("Predation rate")+
      ylab("Prob(Calf survival)")+
      ggtitle(paste(pen.level, "maternal penning")) +
      theme(
        legend.position = "top",
        legend.justification = c("left"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
      scale_color_manual(name = '', labels = c('Low calf phi', 'Moderate calf phi', 'High calf phi'),
                         values = c('red', 'green', 'blue'))
    
    #' #'  Plot relationship
    #' plot(prediction_plot)
    #'  List
    out <- list(df, prediction_plot)
    #'  Return predictions
    return(out)
  }
  #'  Calculate probability of adult female fecundity being low, medium, or high given 
  #'  supplemental feeding does not (0) or does (1) occur
  p.YoYphi.noPen <- YoY_survival(pen = 0, pen.level = "Without")
  p.YoYphi.wPen <- YoY_survival(pen = 1, pen.level = "With")
  #'  Visualize
  (p.AFfec.plots <- p.YoYphi.noPen[[2]] + p.YoYphi.wPen[[2]] + 
      plot_annotation(title = 'Calf survival given predation rate...') +
      plot_layout(guides = "collect") & theme(legend.position = 'top'))
  #'  Create CPT
  p.YoYphi <- bind_rows(p.YoYphi.noPen[[1]], p.YoYphi.wPen[[1]]) %>%
    mutate(maternal_pen = ifelse(maternal_pen == "Without", "No", maternal_pen),
           maternal_pen = ifelse(maternal_pen == "With", "Yes", maternal_pen),
           maternal_pen = factor(maternal_pen, levels = c("No", "Yes")),
           sum_to_one = rowSums(.[3:5])) %>%
    arrange(pred.rate, maternal_pen) 
  names(p.YoYphi) <- c("Predation_rate", "Maternal_penning", "Low", "Medium", "High", "sum_to_one")
  head(p.YoYphi)
  write_csv(p.YoYphi, "./Conditional_Probability_Tables/CPT_v2_Survival_calf.csv")
  ggsave("./Conditional_Probability_Tables/CPT_plots_Survival_calf.tiff", p.AFfec.plots, 
         units = "in", width = 12, height = 6, dpi = 400, device = 'tiff', compression = "lzw") 
  
  #'  -------------------------
  #####  Abundance_adultMales  #####
  #'  -------------------------
  #'  Predict probability of male abundance being low, medium, or high under 
  #'  different levels of adult male survival (low, moderate, high) and the
  #'  relative number of adult caribou translocated (assuming a fixed proportion
  #'  of translocated adults are male)
  AM_abundance <- function(move_it_boys, trans.level) {  
    #'  Adult male survival (1 = low, 2 = moderate, 3 = high)
    AM.survival <- c(1, 2, 3)
    #'  Holding translocation fixed
    N.translocate <- move_it_boys
    
    #'  Define intercept and slope coefficients 
    #'  Interaction - if not translocation then survial does not matter, there are no bou
    alpha <- c(4, 6) # Intercept for low and low/medium abundance categories
    beta1 <- -0.65 # Slope coefficient for adult male survival
    beta2 <- -1 # Slope coefficient for translocation
    beta3 <- -0.65 # Slope coefficient for interaction between survival and translocation
    
    #'  Calculate probability of adult male abundance being low, medium, high given
    #'  varying levels of adult male survival and translocation
    (p.AMnLo <- 1/(1+exp(-(alpha[1] + beta1*AM.survival + beta2*N.translocate + beta3*AM.survival*N.translocate))))
    (p.AMnLoMed <- 1/(1+exp(-(alpha[2] + beta1*AM.survival + beta2*N.translocate + beta3*AM.survival*N.translocate))))
    (p.AMnMed <- p.AMnLoMed - p.AMnLo)
    (p.AMnHi <- 1 - p.AMnLoMed)

    #'  Create data frame with probabilities of adult male abundance being low, medium, high
    (p.AMn <- cbind(p.AMnLo, p.AMnMed, p.AMnHi))
    #'  Add source population covariate data to data frame and
    df <- data.frame(translocation = trans.level, survival = AM.survival, 
                     p1 = p.AMnLo, p2 = p.AMnMed, p3 = p.AMnHi) %>%
      mutate(translocation = factor(translocation, levels = c("No", "Few", "Many")),
             survival = ifelse(survival == 1, "Low", survival),
             survival = ifelse(survival == 2, "Medium", survival),
             survival = ifelse(survival == 3, "High", survival),
             survival = factor(survival, levels = c("Low", "Medium", "High")))
    #'  Reformat data frame for easier plotting
    df_plot <- df %>% dplyr::select(-translocation) %>%
      pivot_longer(cols = c('p1','p2', 'p3'), names_to = "p", values_to = "prob")
    #'  Plot probability of adult male abundance being low, medium, high, given adult
    #'  male survival and level of translocation
    prediction_plot <- ggplot(df_plot, aes(x = survival, y = prob)) +
      ylim(0, 1)+
      # geom_line(aes(color = p)) +
      geom_point(aes(color = p), position = position_dodge(0.1)) +
      xlab("Adult male survival level")+
      ylab("Prob(Adult male N)")+
      ggtitle(paste(trans.level, "adults are translocated")) +
      theme(
        legend.position = "top",
        legend.justification = c("left"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
      scale_color_manual(name = '', labels = c('Low AM N', 'Medium AM N', 'High AM N'),
                         values = c('red', 'green', 'blue'))

    #' #'  Plot relationship
    #' plot(prediction_plot)
    #'  List
    out <- list(df, prediction_plot)
    #'  Return predictions
    return(out)
  }
  #'  Calculate probability of adult male abundance being low, medium, or high given
  #'  adult male survival and level of translocation
  p.AMn.transNo <- AM_abundance(move_it_boys = 0, trans.level = "No")
  p.AMn.transFew <- AM_abundance(move_it_boys = 1, trans.level = "Few")
  p.AMn.transMany <- AM_abundance(move_it_boys = 2, trans.level = "Many")
  #'  Visualize
  (p.AMn.plots <- p.AMn.transNo[[2]] + p.AMn.transFew[[2]] + p.AMn.transMany[[2]] + 
      plot_annotation(title = 'Adult male abundance (N) given interactions between adult male survival and') +
      plot_layout(guides = "collect") & theme(legend.position = 'top'))
  #'  Create CPT
  p.AMn <- bind_rows(p.AMn.transNo[[1]], p.AMn.transFew[[1]], p.AMn.transMany[[1]]) %>%
    mutate(sum_to_one = rowSums(.[3:5])) %>%
    arrange(translocation, survival)
  names(p.AMn) <- c("Abundance_translocateAdult", "Survival_adultMale", "Low", "Medium", "High", "sum_to_one")
  head(p.AMn)
  write_csv(p.AMn, "./Conditional_Probability_Tables/CPT_v2_Abundance_adultMales.csv")
  ggsave("./Conditional_Probability_Tables/CPT_plots_Abundance_adultMales.tiff", p.AMn.plots, 
         units = "in", width = 12, height = 6, dpi = 400, device = 'tiff', compression = "lzw")
  
  #'  ---------------------------
  #####  Abundance_adultFemales  #####
  #'  ---------------------------
  #'  Predict probability of female abundance being low, medium, or high under 
  #'  different levels of adult male survival (low, moderate, high) and the
  #'  relative number of adult caribou translocated (assuming a fixed proportion
  #'  of translocated adults are female)
  AF_abundance <- function(move_it_ladies, trans.level) {  
    #'  Adult female survival (1 = low, 2 = moderate, 3 = high)
    AF.survival <- c(1, 2, 3)
    #'  Holding translocation fixed
    N.translocate <- move_it_ladies

    #'  Define intercept and slope coefficients
    #'  Interaction - if not translocation then survival does not matter, there are no bou
    alpha <- c(4, 6) # Intercept for low abundance category
    beta1 <- -0.65 # Slope coefficient for adult female survival
    beta2 <- -1 # Slope coefficient for translocation
    beta3 <- -0.65 # Slope coefficient for interaction between survival and translocation
    
    #'  Calculate probability of adult female abundance being low, medium, high given
    #'  varying levels of adult female survival and translocation
    (p.AFnLo <- 1/(1+exp(-(alpha[1] + beta1*AF.survival + beta2*N.translocate + beta3*AF.survival*N.translocate))))
    (p.AFnLoMed <- 1/(1+exp(-(alpha[2] + beta1*AF.survival + beta2*N.translocate + beta3*AF.survival*N.translocate))))
    (p.AFnMed <- p.AFnLoMed - p.AFnLo)
    (p.AFnHi <- 1 - p.AFnLoMed)

    #'  Create data frame with probabilities of adult female abundance being low, medium, high
    (p.AFn <- cbind(p.AFnLo, p.AFnMed, p.AFnHi))
    #'  Add source population covariate data to data frame and
    df <- data.frame(translocation = trans.level, survival = AF.survival, 
                     p1 = p.AFnLo, p2 = p.AFnMed, p3 = p.AFnHi) %>%
      mutate(translocation = factor(translocation, levels = c("No", "Few", "Many")),
             survival = ifelse(survival == 1, "Low", survival),
             survival = ifelse(survival == 2, "Medium", survival),
             survival = ifelse(survival == 3, "High", survival),
             survival = factor(survival, levels = c("Low", "Medium", "High")))
    #'  Reformat data frame for easier plotting
    df_plot <- df %>% dplyr::select(-translocation) %>%
      pivot_longer(cols = c('p1','p2', 'p3'), names_to = "p", values_to = "prob")
    #'  Plot probability of adult female abundance being low, medium, high, given adult
    #'  female survival and level of translocation
    prediction_plot <- ggplot(df_plot, aes(x = survival, y = prob)) +
      ylim(0, 1)+
      geom_point(aes(color = p), position = position_dodge(0.1)) +
      xlab("Adult female survival level")+
      ylab("Prob(Adult female N)")+
      ggtitle(paste(trans.level, "adults are translocated")) +
      theme(
        legend.position = "top",
        legend.justification = c("left"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
      scale_color_manual(name = '', labels = c('Low AF N', 'Medium AF N', 'High AF N'),
                         values = c('red', 'green', 'blue'))

    #' #'  Plot relationship
    #' plot(prediction_plot)
    #'  List
    out <- list(df, prediction_plot)
    #'  Return predictions
    return(out)
  }
  #'  Calculate probability of adult male abundance being low, medium,or  high given
  #'  adult female survival and level of translocation
  p.AFn.transNo <- AF_abundance(move_it_ladies = 0, trans.level = "No")
  p.AFn.transFew <- AF_abundance(move_it_ladies = 1, trans.level = "Few")
  p.AFn.transMany <- AF_abundance(move_it_ladies = 2, trans.level = "Many")
  #'  Visualize
  (p.AFn.plots <- p.AFn.transNo[[2]] + p.AFn.transFew[[2]] + p.AFn.transMany[[2]] + 
      plot_annotation(title = 'Adult female abundance (N) given interactions between adult female survival and') +
      plot_layout(guides = "collect") & theme(legend.position = 'top'))
  #'  Create CPT
  p.AFn <- bind_rows(p.AFn.transNo[[1]], p.AFn.transFew[[1]], p.AFn.transMany[[1]]) %>%
    mutate(sum_to_one = rowSums(.[3:5])) %>%
    arrange(translocation, survival)
  names(p.AFn) <- c("Abundance_translocateAdults", "Survival_adultFemale", "Low", "Medium", "High", "sum_to_one")
  head(p.AFn)
  write_csv(p.AFn, "./Conditional_Probability_Tables/CPT_v2_Abundance_adultFemales.csv")
  ggsave("./Conditional_Probability_Tables/CPT_plots_Abundance_adultFemales.tiff", p.AFn.plots, 
         units = "in", width = 12, height = 6, dpi = 400, device = 'tiff', compression = "lzw")
  
  #'  ---------------------
  #####  Abundance_adults  #####
  #'  ---------------------
  #'  Probability of adult abundance being low, medium, or high given the level 
  #'  of adult male and adult female abundances 
  A_abundance <- function(AF.n, AF.level) {   
    #'  Adult male abundance
    n.AM <- c(1, 2, 3)
    #'  Holding adult female abundance at a fixed levels
    n.AF <- AF.n
    
    #'  Define intercept and slope coefficients 
    alpha <- c(4, 6) # Intercept for low and low/medium abundance categories
    beta1 <- -1.1 # Slope coefficient for adult male abundance
    beta2 <- -1.5 # Slope coefficient for adult female abundance
    
    #'  Calculate probability of adult abundance being low, medium, or high given 
    #'  different levels of adult male and female abundance
    (p.AnLo <- 1/(1 + exp(-(alpha[1] + beta1*n.AM + beta2*n.AF))))
    (p.AnLoMed <- 1/(1 + exp(-(alpha[2] + beta1*n.AM + beta2*n.AF))))
    (p.AnMed <- p.AnLoMed - p.AnLo)
    (p.AnHi <- 1 - p.AnLoMed)
    
    #'  Create data frame with probabilities of adult abundance being low, medium, or high
    (p.AN <- cbind(p.AnLo, p.AnMed, p.AnHi))
    #'  Add covariate data to data frame  
    df <- data.frame(abundance_AM = n.AM, abundance_AF = AF.level, 
                     p1 = p.AnLo, p2 = p.AnMed, p3 = p.AnHi) %>%
      mutate(abundance_AM = ifelse(abundance_AM == 1, "Low", abundance_AM),
             abundance_AM = ifelse(abundance_AM == 2, "Medium", abundance_AM),
             abundance_AM = ifelse(abundance_AM == 3, "High", abundance_AM),
             abundance_AM = factor(abundance_AM, levels = c("Low", "Medium", "High")))
    #'  Reformat data frame for easier plotting
    df_plot <- df %>% dplyr::select(-c(abundance_AF)) %>%
      pivot_longer(cols = c('p1','p2','p3'), names_to = "p", values_to = "prob")
    #'  Plot probability of adult abundance being low or high given abundance
    #'  of adult males, adult females, and natural immigration
    prediction_plot <- ggplot(df_plot, aes(x = abundance_AM, y = prob)) + 
      ylim(0, 1) +
      geom_point(aes(color = p), position = position_dodge(0.1)) +
      xlab("Level of adult male abundance")+
      ylab("Prob(Adult abundance)")+
      ggtitle(paste(AF.level, "adult female abundance")) +
      theme(
        legend.position = "top",
        legend.justification = c("left"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
      scale_color_manual(name = '', labels = c('Low adult N', 'Medium adult N', 'High adult N'),
                         values = c('red', 'green', 'blue'))
    
    #' #'  Plot relationship
    #' plot(prediction_plot)
    #'  List
    out <- list(df, prediction_plot)
    #'  Return predictions
    return(out)
  }
  #'  Calculate probability of adult abundance being low, medium, or high given 
  #'  adult male and female abundance
  p.AN.FnLo <- A_abundance(AF.n = 1, AF.level = "Low")
  p.AN.FnMed <- A_abundance(AF.n = 2, AF.level = "Medium")
  p.AN.FnHi <- A_abundance(AF.n = 3, AF.level = "High")
  #'  Visualize
  (p.AN.plots <- p.AN.FnLo[[2]] + p.AN.FnMed[[2]] + p.AN.FnHi[[2]] + 
      plot_annotation(title = 'Adult abundance (N) given adult male abundance and') +
      plot_layout(guides = "collect") & theme(legend.position = 'top'))
  #'  Create CPT
  p.AN <- bind_rows(p.AN.FnLo[[1]], p.AN.FnMed[[1]], p.AN.FnHi[[1]]) %>%
    mutate(sum_to_one = rowSums(across(where(is.numeric)))) %>%
    arrange(abundance_AM, abundance_AF) 
  names(p.AN) <- c("Abundance_adultMales", "Abundance_adultFemales", "Low", "Medium", "High", "sum_to_one")
  head(p.AN)
  write_csv(p.AN, "./Conditional_Probability_Tables/CPT_Abundance_adults.csv")
  ggsave("./Conditional_Probability_Tables/CPT_plots_Abundance_adults.tiff", p.AN.plots, 
         units = "in", width = 12, height = 6, dpi = 400, device = 'tiff', compression = "lzw")
  
  
  #'  ---------------------
  #####  Abundance_calves  #####
  #'  ---------------------
  #'  Probability of calf abundance being low, medium, or high given the levels 
  #'  of adult female abundance, adult female fecundity, and calf survival 
  C_abundance <- function(AF.n, fecund, AF.level, fecund.level) {   
    #'  Calf survival categories
    phi <- c(1, 2, 3)
    #'  Holding Abundance_adultFemales at a fixed level 
    N.AF <- AF.n
    #'  Holding female fecundity at a fixed level
    fecund.AF <- fecund
    
    #'  Define intercept and slope coefficients 
    #'  H: Adult female abundance most important (can't have calves without
    #'  adult females), followed by calf survival, then adult female fecundity
    alpha <- c(4, 7) # Intercept for low and low/medium calf abundance categories
    beta1 <- -0.95 # Slope coefficient for calf survival
    beta2 <- -1.25 # Slope coefficient for adult female abundance
    beta3 <- -0.85 # Slope coefficient for adult female fecundity
    
    #'  Calculate probability of calf abundance being low given different levels 
    #'  of adult female abundance, calf survival probabilities, and female fecundity
    (p.YoYnLo <- 1/(1 + exp(-(alpha[1] + beta1*phi + beta2*N.AF + beta3*fecund.AF))))
    (p.YoYnLoMed <- 1/(1 + exp(-(alpha[2] + beta1*phi + beta2*N.AF + beta3*fecund.AF))))
    (p.YoYnMed <- p.YoYnLoMed - p.YoYnLo)
    (p.YoYnHi <- 1 - p.YoYnLoMed)
    
    #'  Create data frame with probabilities of calf abundance being low, medium, or high
    (p.YoYN <- cbind(p.YoYnLo, p.YoYnMed, p.YoYnHi))
    #'  Add covariate data to data frame  
    df <- data.frame(Survival_calf = phi, Abundance_AF = AF.level, Fecundity_AF = fecund.level,
                     p1 = p.YoYnLo, p2 = p.YoYnMed, p3 = p.YoYnHi) %>%
      mutate(Survival_calf = ifelse(Survival_calf == 1, "Low", Survival_calf),
             Survival_calf = ifelse(Survival_calf == 2, "Moderate", Survival_calf),
             Survival_calf = ifelse(Survival_calf == 3, "High", Survival_calf),
             Survival_calf = factor(Survival_calf, levels = c("Low", "Moderate", "High")))
    #'  Reformat data frame for easier plotting
    df_plot <- df %>% dplyr::select(-c(Abundance_AF, Fecundity_AF)) %>%
      pivot_longer(cols = c('p1','p2','p3'), names_to = "p", values_to = "prob")
    #'  Plot probability of calf abundance being low or high given adult female 
    #'  abundance, fecundity, and calf survival
    prediction_plot <- ggplot(df_plot, aes(x = Survival_calf, y = prob)) + 
      ylim(0, 1) +
      geom_point(aes(color = p), position = position_dodge(0.1)) +
      xlab("Calf survival")+
      ylab("Prob(Calf abundance)")+
      ggtitle(paste(AF.level, "female abundance \nand", fecund.level, "female fecundity")) +
      theme(
        legend.position = "top",
        legend.justification = c("left"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
      scale_color_manual(name = '', labels = c('Low calf N', 'Medium calf N', 'High calf N'),
                         values = c('red', 'green', 'blue'))
    
    #' #'  Plot relationship
    #' plot(prediction_plot)
    #'  List
    out <- list(df, prediction_plot)
    #'  Return predictions
    return(out)
  }
  #'  Calculate probability of calf abundance being low, medium, or high given calf
  #'  survival, adult female abundance, and fecundity
  p.YoYn.AFLo.fecLo <- C_abundance(AF.n = 1, fecund = 1, AF.level = "Low", fecund.level = "low")
  p.YoYn.AFLo.fecMed <- C_abundance(AF.n = 1, fecund = 2, AF.level = "Low", fecund.level = "medium")
  p.YoYn.AFLo.fecHi <- C_abundance(AF.n = 1, fecund = 3, AF.level = "Low", fecund.level = "high")
  p.YoYn.AFMed.fecLo <- C_abundance(AF.n = 2, fecund = 1, AF.level = "Medium", fecund.level = "low")
  p.YoYn.AFMed.fecMed <- C_abundance(AF.n = 2, fecund = 2, AF.level = "Medium", fecund.level = "medium")
  p.YoYn.AFMed.fecHi <- C_abundance(AF.n = 2, fecund = 3, AF.level = "Medium", fecund.level = "high")
  p.YoYn.AFHi.fecLo <- C_abundance(AF.n = 3, fecund = 1, AF.level = "High", fecund.level = "low")
  p.YoYn.AFHi.fecMed <- C_abundance(AF.n = 3, fecund = 2, AF.level = "High", fecund.level = "medium")
  p.YoYn.AFHi.fecHi <- C_abundance(AF.n = 3, fecund = 3, AF.level = "High", fecund.level = "high")
  #'  Visualize
  (p.YoYn.plots <- p.YoYn.AFLo.fecLo[[2]] + p.YoYn.AFLo.fecMed[[2]] + p.YoYn.AFLo.fecHi[[2]] + 
      p.YoYn.AFMed.fecLo[[2]] + p.YoYn.AFMed.fecMed[[2]] + p.YoYn.AFMed.fecHi[[2]] +
      p.YoYn.AFHi.fecLo[[2]] + p.YoYn.AFHi.fecMed[[2]] + p.YoYn.AFHi.fecHi[[2]] +
      plot_layout(ncol = 3) + plot_annotation(title = 'Calf abundance (N) given calf survival...') +
      plot_layout(guides = "collect") & theme(legend.position = 'top'))
  #'  Create CPT
  p.YoYn <- bind_rows(p.YoYn.AFLo.fecLo[[1]], p.YoYn.AFLo.fecMed[[1]], p.YoYn.AFLo.fecHi[[1]],
                      p.YoYn.AFMed.fecLo[[1]], p.YoYn.AFMed.fecMed[[1]], p.YoYn.AFMed.fecHi[[1]],
                      p.YoYn.AFHi.fecLo[[1]], p.YoYn.AFHi.fecMed[[1]], p.YoYn.AFHi.fecHi[[1]]) %>%
    arrange(Survival_calf, Abundance_AF, Fecundity_AF) %>%
    mutate(sum_to_one = rowSums(across(where(is.numeric))))
  names(p.YoYn) <- c("Survival_calf", "Abundance_adultFemale", "Fecundity_adultFemale", "Low", "Medium", "High", "sum_to_one")
  head(p.YoYn)
  write_csv(p.YoYn, "./Conditional_Probability_Tables/CPT_v2_Abundance_calves.csv")
  ggsave("./Conditional_Probability_Tables/CPT_plots_Abundance_calves.tiff", p.YoYn.plots, 
         units = "in", width = 10, height = 12, dpi = 400, device = 'tiff', compression = "lzw")
  
  #'  ------------------------
  #####  Natural immigration  #####
  #'  ------------------------
  #'  Predict probability of no, low, or moderate natural immigration from BC 
  #'  source populations to US under varying levels of source population
  immigration <- function() {
    #'  Population size of source population (1 = very low, 2 = low, 3 = medium, 4 = high)
    N.sourcePop <- c(1, 2, 3, 4)
    
    #'  Define intercept and slope coefficients
    #'  H: Size of source population increases level of natural immigration into US
    alpha <- c(4, 7) # Intercept for NO immigration
    beta1 <- -0.75 # Slope for source population effect
    
    #'  Calculate probability natural immigration will be none, low, or moderate 
    #'  given varying sizes of the source population
    (p.ImmnNo <- 1/(1 + exp(-(alpha[1] + beta1*N.sourcePop))))
    (p.ImmnNoLo <- 1/(1 + exp(-(alpha[2] + beta1*N.sourcePop))))
    (p.ImmnLo <- p.ImmnNoLo - p.ImmnNo)
    (p.ImmnMod <- 1 - p.ImmnNoLo)
    
    #'  Create data frame with probabilities of natural immigration being none, low, moderate, or high
    (p.Immn <- cbind(p.ImmnNo, p.ImmnLo, p.ImmnMod))
    #'  Add source population covariate data to data frame
    df <- data.frame(N.sourcePop = N.sourcePop, p1 = p.ImmnNo, p2 = p.ImmnLo, p3 = p.ImmnMod) %>%
      mutate(N.sourcePop = ifelse(N.sourcePop == 1, "Very low", N.sourcePop),
             N.sourcePop = ifelse(N.sourcePop == 2, "Low", N.sourcePop),
             N.sourcePop = ifelse(N.sourcePop == 3, "Medium", N.sourcePop),
             N.sourcePop = ifelse(N.sourcePop == 4, "High", N.sourcePop),
             N.sourcePop = factor(N.sourcePop, levels = c("Very low", "Low", "Medium", "High")),
             sum_to_one = rowSums(.[2:4]))
    #'  Reformat data frame for easier plotting
    df_plot <- df %>% dplyr::select(-sum_to_one) %>% 
      pivot_longer(cols = c('p1','p2','p3'), names_to = "p", values_to = "prob")
    
    #'  Plot probability of natural immigration being low or high, given level
    #'  of available habitat
    prediction_plot <- ggplot(df_plot, aes(x = N.sourcePop, y = prob)) + 
      ylim(0, 1) +
      geom_point(aes(color = p), position = position_dodge(0.1)) +
      xlab("Source population abundance")+
      ylab("Prob(Natural Immigration)")+
      ggtitle("Natural immigration based on relative size of source population") +
      theme(
        legend.position = "top",
        legend.justification = c("left"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
      scale_color_manual(name = '', labels = c('No immigration', 'Low immigration', 'Mod immigration'),
                         values = c('red', 'green', 'blue'))
    
    #'  Plot relationship
    plot(prediction_plot)
    #'  List
    out <- list(df, prediction_plot)
    #'  Return predictions
    return(out)
  }
  p.ImmN <- immigration()
  names(p.ImmN[[1]]) <- c("Abundance_sourcePop", "None", "Low", "Moderate", "sum_to_one")
  head(p.ImmN[[1]])
  write_csv(p.ImmN[[1]], "./Conditional_Probability_Tables/CPT_v2_Natural_immigration.csv") 
  ggsave("./Conditional_Probability_Tables/CPT_plots_Natural_immigration.tiff", p.ImmN[[2]], 
         units = "in", width = 6, height = 6, dpi = 400, device = 'tiff', compression = "lzw")
  
  #'  ----------------------
  #####  Abundance_caribou  #####
  #'  ----------------------
  #'  Probability of caribou abundance being low, medium, or high given the level of adult 
  #'  abundance, calf abundance, and level of natural immigration
  Total_abundance <- function(YoY.n, immigration, YoY.level, imm.level) {   
    #'  Abundance_adult 
    N.A <- c(1, 2, 3)
    #'  Holding Abundance_calves at a fixed level 
    N.C <- YoY.n
    #'  Holding Natural_immigration at a fixed level
    N.imm <- immigration
    
    #'  Define intercept and slope coefficients 
    #'  H: Adult abundance has largest effect, then calf abundance, then natural immigration 
    alpha <- c(5,7) # Intercept for low and low/medium abundance categories
    beta1 <- -1.5 # Slope coefficient for adult abundance
    beta2 <- -1 # Slope coefficient for calf abundance
    beta3 <- -0.5 # Slope coefficient for immigration
    
    #'  Calculate probability of total abundance being low given different levels 
    #'  of adult abundance, calf abundance, and immigration probabilities
    (p.nLo <- 1/(1 + exp(-(alpha[1] + beta1*N.A + beta2*N.C + beta3*N.imm))))
    (p.nLoMed <- 1/(1 + exp(-(alpha[2] + beta1*N.A + beta2*N.C + beta3*N.imm))))
    (p.nMed <- p.nLoMed - p.nLo)
    (p.nHi <- 1 - p.nLoMed)
    
    #'  Create data frame with probabilities of total abundance being low or high
    (p.N <- cbind(p.nLo, p.nMed, p.nHi))
    #'  Add covariate data to data frame  
    df <- data.frame(Abundance_A = N.A, Abundance_C = YoY.level, Nat_Imm = imm.level,
                     p1 = p.nLo, p2 = p.nMed, p3 = p.nHi) %>%
      mutate(Abundance_A = ifelse(Abundance_A == 1, "Low", Abundance_A),
             Abundance_A = ifelse(Abundance_A == 2, "Medium", Abundance_A),
             Abundance_A = ifelse(Abundance_A == 3, "High", Abundance_A),
             Abundance_A = factor(Abundance_A, levels = c("Low", "Medium", "High")))
    #'  Reformat data frame for easier plotting
    df_plot <- df %>% dplyr::select(-c(Abundance_C, Nat_Imm)) %>%
      pivot_longer(cols = c('p1','p2','p3'), names_to = "p", values_to = "prob")
    #'  Plot probability of total abundance being low or high, given adult
    #'  abundance, calf abundance, and augmentation
    prediction_plot <- ggplot(df_plot, aes(x = Abundance_A, y = prob)) + 
      ylim(0, 1) +
      geom_point(aes(color = p), position = position_dodge(0.1)) +
      xlab("Adult abundance")+
      ylab("Prob(Total abundance)")+
      ggtitle(paste(YoY.level, "calf abundance \nand", imm.level, "natural immigration")) +
      theme(
        legend.position = "top",
        legend.justification = c("left"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
      scale_color_manual(name = '', labels = c('Low total N', 'Medium total N', 'High total N'),
                         values = c('red', 'green', 'blue'))
    
    #' #'  Plot relationship
    #' plot(prediction_plot)
    #'  List
    out <- list(df, prediction_plot)
    #'  Return predictions
    return(out)
  }
  #'  Calculate probability of total abundance given adult and calf abundances as
  #'  well as augmentation
  p.n.CLo.ImmNo <- Total_abundance(YoY.n = 1, immigration = 0, YoY.level = "Low", imm.level = "no")
  p.n.CLo.ImmLo <- Total_abundance(YoY.n = 1, immigration = 1, YoY.level = "Low", imm.level = "low")
  p.n.CLo.ImmMod <- Total_abundance(YoY.n = 1, immigration = 2, YoY.level = "Low", imm.level = "moderate")
  p.n.CMed.ImmNo <- Total_abundance(YoY.n = 2, immigration = 0, YoY.level = "Medium", imm.level = "no")
  p.n.CMed.ImmLo <- Total_abundance(YoY.n = 2, immigration = 1, YoY.level = "Medium", imm.level = "low")
  p.n.CMed.ImmMod <- Total_abundance(YoY.n = 2, immigration = 2, YoY.level = "Medium", imm.level = "moderate")
  p.n.CHi.ImmNo <- Total_abundance(YoY.n = 3, immigration = 0, YoY.level = "High", imm.level = "no")
  p.n.CHi.ImmLo <- Total_abundance(YoY.n = 3, immigration = 1, YoY.level = "High", imm.level = "low")
  p.n.CHi.ImmMod <- Total_abundance(YoY.n = 3, immigration = 2, YoY.level = "High", imm.level = "moderate")
  #'  Visualize
  (p.N.plots <- p.n.CLo.ImmNo[[2]] + p.n.CLo.ImmLo[[2]] + p.n.CLo.ImmMod[[2]] + 
      p.n.CMed.ImmNo[[2]] + p.n.CMed.ImmLo[[2]] + p.n.CMed.ImmMod[[2]] +
      p.n.CHi.ImmNo[[2]] + p.n.CHi.ImmLo[[2]] + p.n.CHi.ImmMod[[2]] +
      plot_layout(ncol = 3) + plot_annotation(title = 'Total abundance (N) given adult abundance...') +
      plot_layout(guides = "collect") & theme(legend.position = 'top'))
  #'  Create CPT
  p.N <- bind_rows(p.n.CLo.ImmNo[[1]], p.n.CLo.ImmLo[[1]], p.n.CLo.ImmMod[[1]], 
                   p.n.CMed.ImmNo[[1]], p.n.CMed.ImmLo[[1]], p.n.CMed.ImmMod[[1]],
                   p.n.CHi.ImmNo[[1]], p.n.CHi.ImmLo[[1]], p.n.CHi.ImmMod[[1]]) %>%
    mutate(sum_to_one = rowSums(across(where(is.numeric)))) %>%
    arrange(Abundance_A, Abundance_C, Nat_Imm) 
  names(p.N) <- c("Abundance_adults", "Abundance_calves", "Natural_immigration", "Low", "Medium", "High", "sum_to_one")
  head(p.N)
  write_csv(p.N, "./Conditional_Probability_Tables/CPT_v2_Abundance_caribou.csv")
  ggsave("./Conditional_Probability_Tables/CPT_plots_Abundance_caribou.tiff", p.N.plots, 
         units = "in", width = 10, height = 12, dpi = 400, device = 'tiff', compression = "lzw")
  
  #'  -----------------------
  #####  Recovery Potential  #####
  #'  -----------------------
  #'  Recovery potential given total caribou abundance and environmental stochasticity (acts of god)
  RecoveryPotential <- function(chaos, chaos.level) {
    #'  Total caribou abundance
    N <- c(1, 2, 3)
    #'  Holding stochasticity at a fixed level (haha)
    stoch <- chaos
    
    #'  Define intercept and slope coefficients
    #'  H: recovery potential influenced most by caribou abundance but can be undone
    #'  by environmental stochasticity
    alpha <- c(4, 6, 8, 10) # Intercepts for caribou abundance levels
    beta1 <- -5 # Slope for caribou abundance
    beta2 <- 1 # Slope for stochasticity
    beta3 <- 0.75 # Slope for interaction betwen abundance and stochasticity - things
    # are REALLY bad if you have low abundance and high stochasticity
    
    #'  Calculate probability of success given different lambda categories
    (p.recovery.vLo <- 1/(1 + exp(-(alpha[1] + beta1*N + beta2*stoch + beta3*N*stoch)))) 
    (p.recovery.vLoLo <- 1/(1 + exp(-(alpha[2] + beta1*N + beta2*stoch + beta3*N*stoch)))) 
    (p.recovery.vLoMod <- 1/(1 + exp(-(alpha[3] + beta1*N + beta2*stoch + beta3*N*stoch)))) 
    (p.recovery.vLoHi <- 1/(1 + exp(-(alpha[4] + beta1*N + beta2*stoch + beta3*N*stoch)))) 
    (p.recovery.Lo <- p.recovery.vLoLo - p.recovery.vLo)
    (p.recovery.Mod <- p.recovery.vLoMod - p.recovery.vLoLo)
    (p.recovery.Hi <- p.recovery.vLoHi - p.recovery.vLoMod)
    (p.recovery.vHi <- 1 - p.recovery.vLoHi)
    
    #'  Add climate change covariate data to data frame
    (df <- data.frame(total.N = N, acts_o_god = chaos.level, p1 = p.recovery.vLo, p2 = p.recovery.Lo, 
                      p3 = p.recovery.Mod, p4 = p.recovery.Hi, p5 = p.recovery.vHi) %>%
        mutate(total.N = ifelse(total.N == 1, "Low", total.N),
               total.N = ifelse(total.N == 2, "Medium", total.N),
               total.N = ifelse(total.N == 3, "High", total.N),
               total.N = factor(total.N, levels = c("Low", "Medium", "High")),
               sum_to_one = rowSums(across(where(is.numeric)))))
    #'  Reformat data frame for easier plotting
    df_plot <- df %>% dplyr::select(-c(acts_o_god, sum_to_one)) %>%
      pivot_longer(cols = c('p1','p2','p3','p4','p5'), 
                   names_to = "p", values_to = "prob")
    #'  Plot probability of recovery potential being very low, low, moderate, high, or very high
    prediction_plot <- ggplot(df_plot, aes(x = total.N, y = prob)) + 
      ylim(0, 1) +
      # geom_line(aes(color = p)) +
      geom_point(aes(color = p)) +
      xlab("Total caribou abundance")+
      ylab("Prob(Recovery potential)")+
      ggtitle(paste(chaos.level, "\nenvironmental stochasticity")) +
      theme(
        legend.position = "top",
        legend.justification = c("left"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
      scale_color_manual(name = '', labels = c('Very low Potential', 'Low Potential', 
                                               'Moderate Potential', 'High Potential', 'Very High Potential'),
                         values = c('purple', 'red', 'orange', 'green', 'blue'))
    
    #' #'  Plot relationship
    #' plot(prediction_plot)
    #'  List
    out <- list(df, prediction_plot)
    #'  Return predictions
    return(out)
  }
  p.PrSuccess.chaosRare <- RecoveryPotential(chaos = 1, chaos.level = "Rare")
  p.PrSuccess.chaosOccasional <- RecoveryPotential(chaos = 2, chaos.level = "Occasional")
  p.PrSuccess.chaosAllDayEveryDay <- RecoveryPotential(chaos = 3, chaos.level = "All Day Every Day")
  #'  Visualize
  (p.PrSuccess.plots <- p.PrSuccess.chaosRare[[2]] + p.PrSuccess.chaosOccasional[[2]] + 
      p.PrSuccess.chaosAllDayEveryDay[[2]] + 
      plot_annotation(title = 'Recovery potential given total caribou abundance and') +
      plot_layout(guides = "collect") & theme(legend.position = 'top'))
  #'  Create CPT
  p.PrSuccess <- bind_rows(p.PrSuccess.chaosRare[[1]], p.PrSuccess.chaosOccasional[[1]], 
                           p.PrSuccess.chaosAllDayEveryDay[[1]]) %>%
    arrange(total.N, acts_o_god)
  names(p.PrSuccess) <- c("Abundance_caribou", "Acts_of_God", "Very Low", "Low", "Moderate", "High", "Very High", "sum_to_one")
  head(p.PrSuccess)
  write_csv(p.PrSuccess, "./Conditional_Probability_Tables/CPT_v2_Reovery_potential.csv")
  ggsave("./Conditional_Probability_Tables/CPT_plots_Recovery_potential.tiff", p.PrSuccess.plots, 
         units = "in", width = 12, height = 6, dpi = 400, device = 'tiff', compression = "lzw")
  
  #'  ------------
  #####  Utility  #####
  #'  ------------
  #'  How much utility do we get if we decide to translocate adult male and/or 
  #'  adult female caribou, given the recovery potential and relative size of 
  #'  the source population?
  Recovery_potential <- c("Very Low", "Low", "Moderate", "High", "Very High")
  Abundance_translocateAdults <- c("None", "Few", "Many")
  Abundance_sourcePop <- c("Very Low", "Low", "Medium", "High")
  utility_setup <- expand.grid(Recovery = Recovery_potential, 
                         translocateA = Abundance_translocateAdults, 
                         sourcePop = Abundance_sourcePop) %>%
    tidyr::expand(Recovery, translocateA, sourcePop) 
  dim(utility_setup)
  head(utility_setup)
  #'  Save data frame to be filled in outside of R
  write_csv(utility_setup, file = "./Conditional_Probability_Tables/utility_fillable.csv")
  
 
  
  
  
  #' ######  Abundance_translocateMale - Chance  ######
  #' move_males <- function() {
  #'   #'  Population size of source population
  #'   N.sourcePop <- seq(50, 500, by = 50)
  #'   N.sourcePop.z <- scale(N.sourcePop)
  #'   
  #'   #'  Define intercept and slope coefficients
  #'   alpha <- c(-2, -1.25, -0.5, 0.25, 0.75, 1.5, 2.5, 3.75, 5)
  #'   beta1 <- -3
  #'   
  #'   #'  Conditional probabilities
  #'   #'  When sourcePop is low, prob of translocating 0 males is high
  #'   #'  As sourcePop increases, prob of translocating 0 males decreases and prob
  #'   #'  of translocating more males increases
  #'   (p.transM.0 <- 1/(1 + exp(-(alpha[1] + beta1*N.sourcePop.z))))
  #'   (p.transM.0to5 <- 1/(1 + exp(-(alpha[2] + beta1*N.sourcePop.z))))
  #'   (p.transM.0to10 <- 1/(1 + exp(-(alpha[3] + beta1*N.sourcePop.z))))
  #'   (p.transM.0to15 <- 1/(1 + exp(-(alpha[4] + beta1*N.sourcePop.z))))
  #'   (p.transM.0to20 <- 1/(1 + exp(-(alpha[5] + beta1*N.sourcePop.z))))
  #'   (p.transM.0to25 <- 1/(1 + exp(-(alpha[6] + beta1*N.sourcePop.z))))
  #'   (p.transM.0to30 <- 1/(1 + exp(-(alpha[7] + beta1*N.sourcePop.z))))
  #'   (p.transM.0to35 <- 1/(1 + exp(-(alpha[8] + beta1*N.sourcePop.z))))
  #'   (p.transM.0to40 <- 1/(1 + exp(-(alpha[9] + beta1*N.sourcePop.z))))
  #'   (p.transM.5 <- p.transM.0to5 - p.transM.0)
  #'   (p.transM.10 <- p.transM.0to10 - p.transM.0to5)
  #'   (p.transM.15 <- p.transM.0to15 - p.transM.0to10)
  #'   (p.transM.20 <- p.transM.0to20 - p.transM.0to15)
  #'   (p.transM.25 <- p.transM.0to25 - p.transM.0to20)
  #'   (p.transM.30 <- p.transM.0to30 - p.transM.0to25)
  #'   (p.transM.35 <- p.transM.0to35 - p.transM.0to30)
  #'   (p.transM.40 <- p.transM.0to40 - p.transM.0to35)
  #'   (p.transM.45 <- 1 - p.transM.0to40)
  #'   
  #'   #'  Create data frame with probabilities
  #'   (df <- data.frame(source.pop = N.sourcePop, p1 = p.transM.0, p2 = p.transM.5, 
  #'                     p3 = p.transM.10, p4 = p.transM.15, p5 = p.transM.20, 
  #'                     p6 = p.transM.25, p7 = p.transM.30, p8 = p.transM.35, 
  #'                     p9 = p.transM.40, p10 = p.transM.45) %>%
  #'       mutate(sum_to_one = rowSums(.[2:11])))
  #'   
  #'   #'  Number of translocated male caribou
  #'   N.transM <- seq(0, 45, by = 5)
  #'   transM <- rep(N.transM, 10)
  #'   #'  Reformat data frame for easier plotting
  #'   df_plot <- df %>% dplyr::select(-sum_to_one) %>%
  #'     pivot_longer(cols = c('p1','p2','p3','p4','p5','p6','p7','p8','p9','p10'), 
  #'                  names_to = "p", values_to = "prob") %>%
  #'     bind_cols(transM) 
  #'   names(df_plot) <- c("Abundance_sourcePop", "p", "prob", "Abundance_translocateM")
  #'   df_plot <- mutate(df_plot, Abundance_translocateM = as.factor(Abundance_translocateM))
  #'   
  #'   #'  Plot probability of translocating N male caribou given size of source population
  #'   prediction_plot <- ggplot(df_plot, aes(x = Abundance_sourcePop, y = prob, group = Abundance_translocateM)) + 
  #'     ylim(0, 1) +
  #'     geom_line(aes(color = Abundance_translocateM)) +
  #'     geom_point(aes(color = Abundance_translocateM)) +
  #'     xlab("Size of source population")+
  #'     ylab("Prob(Translocating adult males)")+
  #'     ggtitle(paste("Probability of translocating N adult males given source population")) +
  #'     theme(
  #'       legend.position = "top",
  #'       legend.justification = c("left"),
  #'       legend.box.just = "right",
  #'       legend.margin = margin(6, 6, 6, 6)) 
  #'   
  #'   #'  Plot relationship
  #'   plot(prediction_plot)
  #'   #'  Return predictions
  #'   return(df)
  #' }
  #' p.transM <- move_males()
  #' names(p.transM) <- c("Abundance_sourcePop", "N = 0", "N = 5", "N = 10", "N = 15", 
  #'                      "N = 20", "N = 25", "N = 30", "N = 35", "N = 40", "N = 45", "sum_to_one")
  #' head(p.transM)
  #' write_csv(p.transM, "./Conditional_Probability_Tables/CPT_v2_Abundance_translocateMale.csv")  
  #' 
  #' ######  Abundance_translocateFemale  ######
  #' move_females <- function() {
  #'   #'  Population size of source population
  #'   N.sourcePop <- seq(50, 500, by = 50)
  #'   N.sourcePop.z <- scale(N.sourcePop)
  #'   
  #'   #'  Define intercept and slope coefficients
  #'   alpha <- c(-1.25, -0.15, 0.85, 1.75, 2.75, 3.25, 4.5, 6, 7, 10)
  #'   beta1 <- -3
  #'   
  #'   #'  Conditional probabilities
  #'   #'  When sourcePop is low, prob of translocating 0 females is high
  #'   #'  As sourcePop increases, prob of translocating 0 females decreases and prob
  #'   #'  of translocating more males increases
  #'   (p.transF.0 <- 1/(1 + exp(-(alpha[1] + beta1*N.sourcePop.z))))
  #'   (p.transF.0to5 <- 1/(1 + exp(-(alpha[2] + beta1*N.sourcePop.z))))
  #'   (p.transF.0to10 <- 1/(1 + exp(-(alpha[3] + beta1*N.sourcePop.z))))
  #'   (p.transF.0to15 <- 1/(1 + exp(-(alpha[4] + beta1*N.sourcePop.z))))
  #'   (p.transF.0to20 <- 1/(1 + exp(-(alpha[5] + beta1*N.sourcePop.z))))
  #'   (p.transF.0to25 <- 1/(1 + exp(-(alpha[6] + beta1*N.sourcePop.z))))
  #'   (p.transF.0to30 <- 1/(1 + exp(-(alpha[7] + beta1*N.sourcePop.z))))
  #'   (p.transF.0to35 <- 1/(1 + exp(-(alpha[8] + beta1*N.sourcePop.z))))
  #'   (p.transF.0to40 <- 1/(1 + exp(-(alpha[9] + beta1*N.sourcePop.z))))
  #'   (p.transF.5 <- p.transF.0to5 - p.transF.0)
  #'   (p.transF.10 <- p.transF.0to10 - p.transF.0to5)
  #'   (p.transF.15 <- p.transF.0to15 - p.transF.0to10)
  #'   (p.transF.20 <- p.transF.0to20 - p.transF.0to15)
  #'   (p.transF.25 <- p.transF.0to25 - p.transF.0to20)
  #'   (p.transF.30 <- p.transF.0to30 - p.transF.0to25)
  #'   (p.transF.35 <- p.transF.0to35 - p.transF.0to30)
  #'   (p.transF.40 <- p.transF.0to40 - p.transF.0to35)
  #'   (p.transF.45 <- 1 - p.transF.0to40)
  #'   
  #'   #'  Create data frame with probabilities
  #'   (df <- data.frame(source.pop = N.sourcePop, p1 = p.transF.0, p2 = p.transF.5, 
  #'                     p3 = p.transF.10, p4 = p.transF.15, p5 = p.transF.20, 
  #'                     p6 = p.transF.25, p7 = p.transF.30, p8 = p.transF.35, 
  #'                     p9 = p.transF.40, p10 = p.transF.45) %>%
  #'       mutate(sum_to_one = rowSums(.[2:11])))
  #'   
  #'   #'  Number of translocated female caribou
  #'   N.transF <- seq(0, 45, by = 5)
  #'   transF <- rep(N.transM, 10)
  #'   #'  Reformat data frame for easier plotting
  #'   df_plot <- df %>% dplyr::select(-sum_to_one) %>%
  #'     pivot_longer(cols = c('p1','p2','p3','p4','p5','p6','p7','p8','p9','p10'), 
  #'                  names_to = "p", values_to = "prob") %>%
  #'     bind_cols(transM) 
  #'   names(df_plot) <- c("Abundance_sourcePop", "p", "prob", "Abundance_translocateF")
  #'   df_plot <- mutate(df_plot, Abundance_translocateF = as.factor(Abundance_translocateF))
  #'   
  #'   #'  Plot probability translocating N female caribou given size of source population
  #'   prediction_plot <- ggplot(df_plot, aes(x = Abundance_sourcePop, y = prob, group = Abundance_translocateF)) + 
  #'     ylim(0, 1) +
  #'     geom_line(aes(color = Abundance_translocateF)) +
  #'     geom_point(aes(color = Abundance_translocateF)) +
  #'     xlab("Size of source population")+
  #'     ylab("Prob(Translocating adult males)")+
  #'     ggtitle(paste("Probability of translocating N adult females given source population")) +
  #'     theme(
  #'       legend.position = "top",
  #'       legend.justification = c("left"),
  #'       legend.box.just = "right",
  #'       legend.margin = margin(6, 6, 6, 6)) 
  #'   
  #'   #'  Plot relationship
  #'   plot(prediction_plot)
  #'   #'  Return predictions
  #'   return(df)
  #' }
  #' p.transF <- move_females()
  #' names(p.transF) <- c("Abundance_sourcePop", "N = 0", "N = 5", "N = 10", "N = 15", 
  #'                      "N = 20", "N = 25", "N = 30", "N = 35", "N = 40", "N = 45", "sum_to_one")
  #' head(p.transF)
  #' write_csv(p.transF, "./Conditional_Probability_Tables/CPT_v2_Abundance_translocateFemale.csv")  
  
  
  
  
  
  
  
  
  