cat("\014")
# Graph for Treatment Effects on Policy Support
# Lucas Kitzmueller

# This R script estimates treatment effects on the support for different 
# protective policies, and visualizes (a) the baseline support, (b) treatment effects, 
# and (c) the support with treatment effects in a combined plot (figure 11).

# Input: 03_clean_data_with_weights.dta (data pre-processed in Stata)
# Output: 06_treatment_policy_support.pdf

# Intro -------------------
  
  # Libraries
  library(tidyverse) #used
  library(haven) # used
  library(labelled) # used
  library(ggpubr) # used
  library(lfe) # used
  library(broom) # used
  library(scales) # used
  library(viridis) # used
  library(srvyr) # used
  library(glue) # used
  library(estimatr) # used
  recode <- dplyr::recode # undo masking of dplyr

  # Load data
  setwd("~/Dropbox (Harvard University)/SYPA/")
  df <- read_dta("01_data/02_clean/03_clean_data_with_weights.dta")

  # Set global parameters
  label_text_size = 5
  text_size = 12
  
# Helper functions -----------------

  # Estimate treatment effects, controlling for baseline characteristics, using survey weights 
  estimate_effects <- function(dep_var,data) {
    control_variable_names <- str_subset(colnames(data), "_mi_*") 
    control_variable_names <- paste(control_variable_names,collapse = " + ")
    formula <- glue(dep_var, " ~ treatment + fo_probability + risk_percentile + " ,
                    control_variable_names, " + as.factor(bl_occupation)")
    fit <- lm_robust(as.formula(formula), data = df_pp, weights = final_weight,
                     se_type = "stata")
    summary(fit)
    
    label_character <- paste(dep_var)
    result_vector <- c(label_character,
                       round(fit$coefficients[2], digits = 3), 
                       (round(fit$coefficients[2]+1.96*fit$std.error[2], digits = 3)), 
                       (round(fit$coefficients[2]-1.96*fit$std.error[2], digits = 3)),
                       round(fit$std.error[2], digits = 3),
                       fit$p.value[2] # include p-values, can use for Benjamini-Hochberg adjustment
    )
    result_vector
    return(result_vector)
  }


# Select relevant data --------------------------
  df_pp <- df %>%
    filter(bl_occupation != 33) %>%
    filter(!is.na(treatment)) %>%
    filter(pptime >= pptimecutoff) %>%
    select(c(final_weight,treatment,bl_occupation,starts_with("pp_"),contains("_mi_"), fo_probability, risk_percentile)) 

# Create summary data of control group / baseline (panel A) -----------------
  
  # Reshaping to long format
  df_pp_c <- df_pp %>%
    select(c(final_weight,treatment,starts_with("pp_"))) %>%
    gather("policy","support", -c(final_weight,treatment))
  
  # Calculate averages using survey weights
  df_pp_c <- df_pp_c %>%
    filter(treatment == 0) %>%
    filter(policy != "pp_marketpols") %>%
    as_survey_design(weights = c(final_weight)) %>%
    group_by(policy) %>%
    summarize(n = survey_mean(support, na.rm = T, vartype = "ci"))
  
# Estimate treatment effects (panel B) ---------------------------
  
  # Estimate effect on each policy measure (could loop here)
  result1 <- estimate_effects("pp_training", df_pp)
  result2 <- estimate_effects("pp_wealthtax", df_pp)
  result3 <- estimate_effects("pp_council", df_pp)
  result4 <- estimate_effects("pp_hartiv", df_pp)
  result5 <- estimate_effects("pp_uib", df_pp)
  result6 <- estimate_effects("pp_robotax", df_pp)
  result7 <- estimate_effects("pp_funding", df_pp)
  result8 <- estimate_effects("pp_buergergeld", df_pp)
  result9 <- estimate_effects("pp_remaining", df_pp)
  
  # Combine estimates in one data frame
  df_pp_t <- as_tibble(rbind(result1, result2, result3, 
                             result4, result5, result6,
                             result7, result8, result9))
  df_pp_t$V2 <- as.numeric(df_pp_t$treatment)
  df_pp_t$V3 <- as.numeric(df_pp_t$V3)
  df_pp_t$V4 <- as.numeric(df_pp_t$V4)
  df_pp_t$policy <- df_pp_t$V1
  df_pp_t

  # Join datasets
  df_pp <- left_join(df_pp_c,df_pp_t)
  df_pp

# Policy support in control group + treatment effects (panel C) ---------------------------
  
  # Sum baseline support and treatment estimate
  df_pp$treat_a_cont <- df_pp$n + df_pp$V2
  df_pp$treat_a_cont_upp <- df_pp$n + df_pp$V3
  df_pp$treat_a_cont_low <- df_pp$n + df_pp$V4
  df_pp

# Data preprocessing for graph ------------------------  
      
  # create dummy variable for fill (so that control is included in the legend)
  df_pp$dummy <- "."
  
  # Round values  
  df_pp <- df_pp %>%
    mutate(mean_rounded_control = round(n, 2)) %>%
    mutate(mean_rounded_effect = round(V2, 2)) %>%
    mutate(mean_rounded_treatment = round(treat_a_cont, 2))
  df_pp

  # Reorder descending
  df_pp <- df_pp %>%
    mutate(policy = fct_relevel(policy, "pp_remaining", "pp_uib","pp_buergergeld","pp_funding","pp_robotax","pp_hartiv","pp_wealthtax","pp_training","pp_council"))
  
  # Add indicator variables for the distinction between market policies and redistribution policies
  df_pp <- mutate(df_pp, market_pol=if_else(policy %in% c("pp_council","pp_robotax","pp_buergergeld","pp_funding"), "market-income policy", "redistribution policy"))
  df_pp <- mutate(df_pp, market_pol_treat=if_else(policy %in% c("pp_council","pp_robotax","pp_buergergeld","pp_funding"), "support with treatment effect \n (market policy)", "support with treatment effect \n (redistribution policy)"))
  
  # Add labels for different policies
  df_pp$policy <- recode(df_pp$policy, "pp_training" = "Financial support \n for retraining")
  df_pp$policy <-  recode(df_pp$policy, "pp_wealthtax" = "Introduce \n wealth tax")
  df_pp$policy <-  recode(df_pp$policy, "pp_council" = "Strengthen \n workers' council")
  df_pp$policy <-  recode(df_pp$policy, "pp_hartiv" = "More generous \n unemployment insurance")
  df_pp$policy <-  recode(df_pp$policy, "pp_uib" = "Universal Basic Income")
  df_pp$policy <-  recode(df_pp$policy, "pp_robotax" = "'Robot Tax'")
  df_pp$policy <-  recode(df_pp$policy, "pp_funding" = "Directed research \n funding")
  df_pp$policy <-  recode(df_pp$policy, "pp_buergergeld" = "Social Wealth Fund \n with yearly dividends")
  df_pp$policy <-  recode(df_pp$policy, "pp_remaining" = "None of the policies")
  df_pp

# Visualization code for each panel  ------------------------  
  
  # Panel A - Baseline support
  p_pp1 <- df_pp %>%
    ggplot(aes(x = policy)) +
    geom_point(aes(y = n, fill = dummy), stat = "identity") +
    geom_text(aes(y = n, label = mean_rounded_control), vjust = 1.5, hjust = 0.5, color = "black", show.legend = FALSE, size = label_text_size) +
    theme_minimal()+
    coord_flip() +
    scale_y_continuous(name = "Baseline Support \n (on 5-point Likert scale)", 
                       limits = c(0,4))  +
    scale_x_discrete(name = "Policy Measure") +
    labs(title="Support in Control Group",
         color = "") +
    scale_fill_manual(name = "", values="black", label="baseline support") +
    scale_color_brewer(palette="Set1") + 
    theme(axis.text.y  = element_text(size=text_size),
          legend.position="bottom",
          legend.title = element_blank(),
          legend.text=element_text(size=text_size),
          axis.text.x  = element_text(size=text_size),
          axis.title.x = element_text(size = text_size),
          axis.title.y = element_text(size = text_size))
  p_pp1
  
  # Panel B - Treatment effects
  p_pp2 <- df_pp %>%
    ggplot(aes(x = policy, y = V2)) +
    geom_hline(yintercept = 0, color = "black", size=0.5, linetype = "dotted") +
    geom_point(aes(color = market_pol), stat = "identity") +
    geom_errorbar(aes(ymin = V3, max = V4, color = market_pol), width = 0.2) +
    geom_text(aes(label = mean_rounded_effect, color = market_pol), vjust = -0.75, hjust = 0.5, show.legend = FALSE, size = label_text_size) +
    theme_minimal()+
    coord_flip() +
    scale_y_continuous(name = "Treatments effects \n (in units of 5-point Likert scale)", 
                       limits = c(-2,2))  +
    scale_x_discrete(name = "Policy Measure") +
    labs(title="   Treatment Effects",
         color = "") +
    scale_color_brewer(palette="Set1") + 
    theme(legend.position="bottom",
          legend.title = element_blank(),
          axis.text.y  = element_text(size=text_size),
          axis.text.x  = element_text(size=text_size),
          axis.title.x = element_text(size = text_size),
          axis.title.y = element_text(size = text_size))
  p_pp2
  
  # Panel C - Policy Support with Treatment 
  p_pp3 <- df_pp %>%
    ggplot(aes(x = policy, y = treat_a_cont)) +
    geom_point(aes(color = market_pol_treat), stat = "identity") +
    geom_errorbar(aes(ymin = treat_a_cont_low, max = treat_a_cont_upp, color = market_pol_treat), width = 0.2) +
    geom_point(aes(y = n, fill = dummy), stat = "identity") +
    geom_text(aes(label = mean_rounded_treatment, color = market_pol_treat), vjust = -0.75, hjust = 0.5, show.legend = FALSE, size = label_text_size) +
    theme_minimal()+
    coord_flip() +
    scale_y_continuous(name = "Support with Treatment \n (on 5-point Likert scale)", 
                       limits = c(0,4))  +
    scale_x_discrete(name = "Policy Measure") +
    labs(title="   Support with Treatment Effects",
         color = "") +
    scale_fill_manual(name = "", values="black", label="baseline support") +
    scale_color_brewer(palette="Set1") + 
    theme(legend.position="bottom", 
          legend.title = element_blank(),
          legend.text=element_text(size=text_size),
          axis.text.y  = element_text(size=text_size),
          axis.text.x  = element_text(size=text_size),
          axis.title.x = element_text(size = text_size),
          axis.title.y = element_text(size = text_size))
  p_pp3

# Combine panels into one figure and save -----------------------
  
  # Save legend from graph 3
  leg <- get_legend(p_pp3)
  leg
  
  # Arrange panels
  figure <- ggarrange(p_pp1 + theme(axis.ticks.y = element_blank(),
                                    plot.margin=unit(c(0.2,0.2,0.2,0.2), "cm")), 
  p_pp2 + theme(axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                axis.title.y = element_blank(),
                plot.margin=unit(c(0.2,0.2,0.2,0.2), "cm")), 
  p_pp3 + theme(axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                axis.title.y = element_blank(),
                plot.margin=unit(c(0.2,0.2,0.2,0.2), "cm")) ,
  leg,
  nrow = 2, ncol = 3,
  widths = c(1.45, 1, 1), heights = c(8,0.1),
  legend.grob = leg,
  legend="bottom",align = "h",
  labels = c("A","B","C",""))
  figure
  
  ggsave("02_output/04_graphs/06_treatment_policy_support.pdf",
         width = 14,
         height = 7)
  
# End of script.