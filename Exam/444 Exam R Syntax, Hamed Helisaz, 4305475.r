library(tidyverse)
library(readr)
library(haven)
library(magrittr)
library(car)
library(patchwork)
library(corrplot)
library(rlang)
library(broom)
library(DescTools)   # For ratio statistics (like COD, PRD) and non-parametric tests
library(tidyr)       # For data tidying
library(broom)       # For tidying model outputs
library(dplyr)
library(ggplot2)
library(knitr)
library(summarytools) # For detailed descriptive statistics
library(gridExtra)   # For arranging multiple plots
library(GGally)      # For pairs plots (scatter/histo/correlation)
rm(list = ls())

# -----------------------------------------------------------------------------
# R Code for BUSI 444 Case Study Examination - Cost Model Analysis ####
# -----------------------------------------------------------------------------

# Loading Data ####
sales_data <- read_sav('Exam/BUSI 444 Term 2 Case Study 466 Marilu EXAM.sav')

# Convert column names to lower case for consistency
# I will resume mentioning those the same in Report too
colnames(sales_data) <- tolower(colnames(sales_data))

# Preliminary Data Screening & Preparation ####

num_records <- nrow(sales_data)
cat("Total Records:", num_records, "\n")


# Date Range
min_month <- min(sales_data[['month']], na.rm = TRUE)
max_month <- max(sales_data[['month']], na.rm = TRUE)
cat("Sales Date Range:", as.character(as.Date(paste0('2017-',min_month,'-01'))), 
    "to", 
    as.character(as.Date(paste0('2017-',max_month,'-01'))), "\n")

# Neighbourhoods
neighbourhoods <- unique(sales_data$neigh)
cat("Neighbourhoods included:", paste(sort(neighbourhoods), collapse = ", "), "\n")

# Convert relevant categorical variables to factors
factor_cols <- c("neigh", "heattype", "baseste", "garagefl", "carptfl",
                 "mbmclsdv", "gar_type", "conduit", "alley",
                 "sidewalk", "culdesac", "corner", "wooded", "pie_shpe", "slope",
                 "abv_road", "splitlvl") # Add others if needed
# Check which columns exist before converting
factor_cols_exist <- factor_cols[factor_cols %in% colnames(sales_data)]
sales_data <- sales_data %>%
  mutate(across(all_of(factor_cols_exist), as.factor))

# Check for missing values in key columns
key_cols <- c("saleprice", "landval", "imprval", "totalval", "mbyrblt", "mbeffyr",
              "flrarea1", "bsmtarea", "gar_area", "lotsize", "neigh", "conduit",
              "mbmancls") # mbmancls seems like quality class
missing_summary <- colSums(is.na(sales_data[, key_cols[key_cols %in% colnames(sales_data)]]))
cat("Missing values summary for key columns:\n")
print(missing_summary[missing_summary > 0])

# Basic descriptive stats for key continuous variables
numeric_cols <- c("saleprice", "landval", "imprval", "totalval", "mbyrblt",
                  "flrarea1", "bsmtarea", "gar_area", "lot_size", "linmcls")
numeric_cols_exist <- numeric_cols[numeric_cols %in% colnames(sales_data)]
print(summary(sales_data[, numeric_cols_exist]))

# Define Age variable (using mbyrblt - year built)
# Assuming valuation year is 2017 based on valuation date July 1, 2017
valuation_year <- 2017
sales_data <- sales_data %>%
  mutate(
    age = ifelse(mbyrblt > 0 & mbyrblt <= valuation_year, valuation_year - mbyrblt, NA)
  )
cat("\nAge variable created (based on 2017 - mbyrblt).\n")
print(summary(sales_data$age))


## Descriptive Analysis #### 
### Numerical Columns ####
numeric_summary <- summary(sales_data[, numeric_cols])
print(numeric_summary)


### Factorial Columns (Frequency Table)####
frequency_tables <- list()
for (col in factor_cols_exist) {
  cat("Variable:", col, "\n")
  # Using count for a cleaner table output
  freq_table <- sales_data %>%
    count(!!sym(col), sort = TRUE) %>%
    mutate(percentage = scales::percent(n / sum(n), accuracy = 0.1))
  frequency_tables[[col]] <- freq_table
  print(as.data.frame(freq_table)) # Print as sales_data frame for better alignment
  cat("\n") # Add space between tables
}

font_size<-12
# Variable Review ####
## Time Adjustment ####
cat("\n--- Ratio Statistics for SAR (Sale Price / Total Assessed Value) ---\n")
n <- nrow(sales_data)
# Calculate indices for binomial confidence interval for the median
alpha <- 0.05
sales_data%<>%mutate(sar = saleprice/totalval)


### Ratio Summary ####
ratio_summary_fn <- function(data, var_to_summarize, group_var = NULL, alpha = 0.05) {
  
  summarise_data <- function(df, var, alpha) {
    n <- nrow(df)
    sorted_df <- sort(df[[var]])
    lower_index <- qbinom(alpha / 2, n, 0.5)
    upper_index <- qbinom(1 - alpha / 2, n, 0.5)
    lb_median_ci <- sorted_df[lower_index]
    ub_median_ci <- sorted_df[upper_index]
    if (lower_index < 1 || upper_index > n || lower_index >= upper_index) {
      warning("Could not calculate valid binomial confidence interval indices for the median.")
      lb_median_ci <- NA
      ub_median_ci <- NA
      actual_coverage <- NA}else{
      actual_coverage <- (pbinom(upper_index -1, n, 0.5) - pbinom(lower_index - 1, n, 0.5)) * 100
      }
    mean_val <- mean(df[[var]], na.rm = TRUE)
    sd_val <- sd(df[[var]], na.rm = TRUE)
    median_val <- median(df[[var]], na.rm = TRUE)
    lb_mean <- mean_val - qt(1 - alpha / 2, df = n - 1) * sd_val / sqrt(n)
    ub_mean <- mean_val + qt(1 - alpha / 2, df = n - 1) * sd_val / sqrt(n)
    cod <- (mean(abs(df[[var]] - median_val), na.rm = TRUE) / median_val) * 100
    cv <- (sd_val / mean_val) * 100
    
    data.frame(
      Mean = round(mean_val, 2),
      LB_mean = round(lb_mean, 2),
      UB_mean = round(ub_mean, 2),
      Median = round(median_val, 2),
      Min = round(min(df[[var]], na.rm = TRUE), 2),
      Max = round(max(df[[var]], na.rm = TRUE), 2),
      COD = round(cod, 2),
      CV = round(cv, 2),
      LB_median = round(lb_median_ci, 2),
      UB_median = round(ub_median_ci, 2),
      Actual_Coverage_Median_CI = round(actual_coverage, 2)
    )
  }
  
  if (!is.null(group_var)) {
    data %>%
      group_by(across(all_of(group_var))) %>%
      summarise(across(all_of(var_to_summarize), ~ summarise_data(cur_data(), var_to_summarize, alpha)))
  } else {
    summarise_data(data, var_to_summarize, alpha)
  }
}
ratio_summary <- ratio_summary_fn(sales_data, "sar")

# Print the summary table
print(ratio_summary, row.names = FALSE)


### sar vs month plot ####

month_labels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")


lm_model <- lm(sar ~ month, data = sales_data)
r_squared <- summary(lm_model)$r.squared

sar_time_plot <- ggplot(sales_data, aes(x = month, y = sar)) +
  geom_point(alpha = 0.5) + # Points
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "blue") + # Linear trend line
  scale_x_continuous(breaks = 1:12, labels = month_labels) + # Ensure all months are shown as ticks
  labs(
    title = "SAR variation with sale month",
    x = "",
    y = "SAR"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12), # Set font size for all text elements
    axis.title = element_text(size = 12), # Set font size for axis titles
    axis.text = element_text(size = 12), # Set font size for axis text
    plot.title = element_text(size = 12), # Set font size for plot title
    legend.text = element_text(size = 12) # Set font size for legend text
  ) + geom_label(aes(x = 10, y = max(sar, na.rm = TRUE), 
                     label = paste("R² =", round(r_squared, 2))), 
                 size = 5, hjust = 1, fill = "white", color = "red")


# print(sar_time_plot) # Display plot
ggsave("Exam/figure_sar_vs_month.jpg", plot = sar_time_plot, width = 7, height = 5) # Save plot


# R-squared from linear model
lm_model <- lm(sar ~ month, data = sales_data)
r_squared <- summary(lm_model)$r.squared
cat("R-squared for linear model (SAR ~ Sale Month):", round(r_squared, 3), "\n")

### asr Rank ####
mean_ranks <- sales_data %>%
  mutate(sar_rank = rank(sar)) %>%
  group_by(month) %>%
  summarise(mean_rank = mean(sar_rank), n = n(), .groups = 'drop')

cat("Mean SAR Rank by Sale Month:\n")
print(as.data.frame(mean_ranks))
cat("\n")

# Perform Kruskal-Wallis test
kw_test_result <- kruskal.test(sar ~ as.factor(month), data = sales_data)
cat("Kruskal-Wallis Test Results:\n")
print(kw_test_result)

kw_p_value <- kw_test_result$p.value
cat("\nAsymptotic Significance (p-value):", round(kw_p_value, 3), "\n")


### adjustment based on asr mean ####
asr_mean <- as.data.frame(sales_data%>%group_by(month)%>%summarise(sar = mean(sar), .groups = 'drop'))%>%
  mutate(sar_july_1st = (mean(as.numeric(sales_data[sales_data['month'] == 6,'sar', drop = TRUE])) + 
                        mean(as.numeric(sales_data[sales_data['month'] == 7,'sar', drop = TRUE])))/2)%>%
  mutate(factor = sar_july_1st / sar )

sales_data %<>% left_join(asr_mean [,c('month', 'factor')], by = 'month')%>%
  mutate(saleprice_time_adjusted = saleprice * factor)%>%select(-factor)

kruskal.test(sales_data$saleprice_time_adjusted, sales_data$month)
sales_data%<>%mutate(sar_time_adjusted = saleprice_time_adjusted/ totalval)


View(ratio_summary_fn(sales_data, 'sar_time_adjusted', 'month'))

# Unit Value Method - Land Value ####

imprv_adjustment_factor <- coef(lm(saleprice_time_adjusted ~ landval + imprval - 1,data = sales_data))[['imprval']]

sales_data%<>%mutate(imprval_time_adjusted = imprval * imprv_adjustment_factor,
                     landval_residual = saleprice_time_adjusted - imprval_time_adjusted)

median_size_factor <- median(sales_data$landval_residual/ sales_data$lot_size)

sales_data%<>%mutate(size_fact = landval_residual / lot_size / median_size_factor,
                     lot_size2 = lot_size/10000)

sales_data%>%ggplot(aes(x = lot_size2, y = size_fact))+geom_point()


# Fit models
linear_model <- lm(size_fact ~ lot_size2, data = sales_data)
quadratic_model <- lm(size_fact ~ poly(lot_size2, 2), data = sales_data)
cubic_model <- lm(size_fact ~ poly(lot_size2, 3), data = sales_data)
power_model <- lm(log(size_fact) ~ log(lot_size2), data = sales_data)

# Get model summaries
linear_summary <- summary(linear_model)
quadratic_summary <- summary(quadratic_model)
cubic_summary <- summary(cubic_model)
power_summary <- summary(power_model)


get_model_stats <- function(model_summary) {
  list(
    R_squared = model_summary$r.squared,
    Adjusted_R_squared = model_summary$adj.r.squared,
    F_statistic = model_summary$fstatistic[1],
    DF1 = model_summary$fstatistic[2],
    DF2 = model_summary$fstatistic[3]
  )
}

# Extract statistics for each model
linear_stats <- get_model_stats(linear_summary)
quadratic_stats <- get_model_stats(quadratic_summary)
cubic_stats <- get_model_stats(cubic_summary)
power_stats <- get_model_stats(power_summary)

# Print model statistics
linear_stats
quadratic_stats
cubic_stats
power_stats


# Plot the data and fitted curves

library(ggplot2)
library(dplyr)


# Second plot: Scatter plot with fitted curves
fitted_curves_plot <- sales_data %>%
  ggplot(aes(x = lot_size2, y = size_fact)) +
  geom_point(color = "black", size = 2,alpha = 0.5) +
  stat_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "blue", aes(linetype = "Linear")) +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "red", aes(linetype = "Quadratic")) +
  stat_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE, color = "green", aes(linetype = "Cubic")) +
  stat_smooth(method = "lm", formula = y ~ log(x), se = FALSE, color = "purple", aes(linetype = "Power Law")) +
  labs(title = "Size Factor with Fitted Curves",
       x = "Lot Size2 (/10,000)",
       y = "Size Factor",
       linetype = "Model") +
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash"),
                        labels = c("Linear", "Quadratic", "Cubic", "Power Law")) +
  theme_minimal()


# Save the fitted curves plot as a JPG
ggsave("Exam/fitted_curves_plot.jpg", plot = fitted_curves_plot, width = 8, height = 6, dpi = 300)


## Canculating genlandpow ####
sales_data%<>%mutate(genlandpow = exp(predict(power_model, newdata = sales_data)) * lot_size * median_size_factor,
                     land_ratio = landval_residual / genlandpow)

## Land Ratio Adjustments ####
### Conduit ####
kruskal.test(sales_data$land_ratio, sales_data$conduit)

conduit_land_adjustment_before <- ratio_summary_fn(sales_data, 'land_ratio', 'conduit')

sales_data%<>%
  mutate(genlandpow_conduit = case_when(
    conduit == 1 ~ genlandpow * conduit_land_adjustment_before[conduit_land_adjustment_before['conduit']==1,]$land_ratio$Median,
    conduit == 0 ~ genlandpow * conduit_land_adjustment_before[conduit_land_adjustment_before['conduit']==0,]$land_ratio$Median),
    land_ratio_conduit = landval_residual / genlandpow_conduit,
  )

conduit_land_adjustment_after <- ratio_summary_fn(sales_data, 'land_ratio_conduit', 'conduit')
kruskal.test(sales_data$land_ratio_conduit, sales_data$conduit)

### Other (binary) ####



perform_kruskal_tests <- function(data, binary_vars, target_var) {
  results <- data.frame(
    Variable = character(),
    P_Value = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (var in binary_vars) {
    if (length(unique(data[[var]])) <= 1) {
      warning(paste("Variable", var, "has only one unique value. Skipping."))
      next
    }
    
    # Kruskal-Wallis test
    formula <- as.formula(paste(target_var, "~", var))
    test_result <- kruskal.test(formula, data = data)
    
    results <- rbind(results, data.frame(
      Variable = var,
      P_Value = test_result$p.value,
      stringsAsFactors = FALSE
    ))
  }
  
  return(results)
}
  

bin_var <- c('conduit', 'alley','sidewalk',
             'culdesac','corner','wooded',
             'pie_shpe','slope','abv_road')

perform_kruskal_tests(sales_data, bin_var, 'land_ratio_conduit')

lapply(X = bin_var, function (x) ratio_summary_fn(sales_data, 'land_ratio_conduit',x))

### Correlation Matrix ####

chi_square_df <- sales_data[,names(sales_data) %in% bin_var]

View(cor(chi_square_df%>%mutate(across(everything(), as.numeric))))
chisq.test(chi_square_df$conduit, chi_square_df$alley)
table(chi_square_df$conduit, chi_square_df$alley)
###


### Adjust for Neigh ####
# Combine all into one data frame

kruskal.test(sales_data$land_ratio_conduit, sales_data$neigh)

neigh_land_adjustment_before <- ratio_summary_fn(sales_data, 'land_ratio_conduit', 'neigh')

sales_data%<>%
  mutate(genlandpow_neigh = case_when(
    # neigh == 713 ~ genlandpow_conduit * neigh_land_adjustment_before[neigh_land_adjustment_before['neigh']==713,]$land_ratio_conduit$Median,
    neigh == 715 ~ genlandpow_conduit * neigh_land_adjustment_before[neigh_land_adjustment_before['neigh']==715,]$land_ratio_conduit$Median,
    # neigh == 716 ~ genlandpow_conduit * neigh_land_adjustment_before[neigh_land_adjustment_before['neigh']==716,]$land_ratio_conduit$Median,
    neigh == 731 ~ genlandpow_conduit * neigh_land_adjustment_before[neigh_land_adjustment_before['neigh']==731,]$land_ratio_conduit$Median,
    # neigh == 732 ~ genlandpow_conduit * neigh_land_adjustment_before[neigh_land_adjustment_before['neigh']==732,]$land_ratio_conduit$Median,
    neigh == 734 ~ genlandpow_conduit * neigh_land_adjustment_before[neigh_land_adjustment_before['neigh']==734,]$land_ratio_conduit$Median,
    neigh == 736 ~ genlandpow_conduit * neigh_land_adjustment_before[neigh_land_adjustment_before['neigh']==736,]$land_ratio_conduit$Median,
    neigh == 737 ~ genlandpow_conduit * neigh_land_adjustment_before[neigh_land_adjustment_before['neigh']==737,]$land_ratio_conduit$Median,
    TRUE ~ genlandpow_conduit),
    land_ratio_neigh = landval_residual / genlandpow_neigh
  )

neigh_land_adjustment_after <- ratio_summary_fn(sales_data, 'land_ratio_neigh', 'neigh')
kruskal.test(sales_data$land_ratio_neigh, sales_data$neigh)

# Find the RCNLD ####
# Main Building RCN
sales_data %<>%
  mutate(
    basement_type = case_when(
      bsmtarea > 0  ~ "Full",   # Has basement area
      crwlarea > 0  ~ "Crawl",  # No basement, but has crawl space area
      TRUE          ~ "None"    # Neither basement nor crawl space area
    ),
    stories = if_else(flrarea2 > 0, 2, 1), # 2 stories if area on 2nd floor > 0
    sty1_const = case_when(
      stories == 1 & basement_type == "Full"  ~ 51500,
      stories == 1 & basement_type == "Crawl" ~ 45500,
      TRUE                                    ~ 0
    ),
    sty2_const = case_when(
      stories == 2 & basement_type == "Full"  ~ 51500 + 16300, # 67800
      stories == 2 & basement_type == "Crawl" ~ 45500 + 16300, # 61800
      TRUE                                    ~ 0 # No constant if 1-storey
    ),
    sty1_area_cost = if_else(stories == 1,
                             flrarea1 * 42.50,
                             0),
    sty2_area_cost = if_else(stories == 2,
                             flrarea1 * 42.50 + flrarea2 * 33.40,
                             0),
    basement_area_cost = ifelse(basement_type == "Full", bsmtarea * 21.25, 0),
    crawl_area_cost = ifelse(basement_type == "Crawl", crwlarea * 14.90, 0),
    finished_basement_cost = ifelse(bsmtfin > 0, bsmtfin * 15.60, 0),
    hotwater_cost = if_else(heattype %in% c(7, 8),
                            flrarea1 * 4.40 + flrarea2 * 3.30, # flrarea2 is 0 for 1-storey
                            0),
    fireplace_cost = fireplcs * 3850,
    bath_cost = 4000 * baths,
    mb_cost = sty1_const + sty2_const+
      sty1_area_cost + sty2_area_cost+
      basement_area_cost + crawl_area_cost+
      finished_basement_cost + hotwater_cost +
      fireplace_cost + bath_cost)

sales_data <- sales_data %>%
  mutate(
    MB_factor = case_when(
      mbmancls %in% c(40, 41, 42) ~ 0.507,
      mbmancls %in% c(50, 51, 52) ~ 0.767,
      mbmancls %in% c(80, 81, 82) ~ 0.783,
      mbmancls %in% c(90, 91, 92) ~ 0.945,
      mbmancls %in% c(140, 141, 142) ~ 1.000,
      mbmancls %in% c(145, 146, 147) ~ 1.220,
      mbmancls %in% c(150, 151, 152) ~ 1.190,
      TRUE                        ~ 1.000 # Default/fallback
    ),
    mb_rcn_manual = mb_cost * MB_factor
  )

# --- 9. Calculate Garage RCN ---
sales_data <- sales_data %>%
  mutate(
    GR_factor = case_when(
      gr_mancl %in% c(910, 911) ~ 0.450,
      gr_mancl %in% c(920, 921) ~ 1.000,
      gr_mancl %in% c(930, 931) ~ 1.200,
      gr_mancl %in% c(940, 941) ~ 1.420,
      TRUE                     ~ 1.000 # Default if gr_mancl is missing or 0
    ),
    garage_raw_cost = case_when(
      gar_type == 1 ~ 11600 + 30 * gar_area, # Attached
      gar_type == 2 ~ 16300 + 30 * gar_area, # Detached
      gar_type == 3 ~ 7100,                  # Basement
      TRUE          ~ 0                      # No garage or other type
    ),
    # Ensure factor isn't applied if there's no garage cost
    gr_rcn_manual = ifelse(garage_raw_cost > 0, garage_raw_cost * GR_factor, 0)
  )

# --- 10. Calculate Carport RCN ---
sales_data <- sales_data %>%
  mutate(
    CP_factor = case_when(
      cp_mancl %in% c(910, 911) ~ 0.450,
      cp_mancl %in% c(920, 921) ~ 1.000,
      cp_mancl %in% c(930, 931) ~ 1.200,
      cp_mancl %in% c(940, 941) ~ 1.420,
      TRUE                     ~ 1.000 # Default if cp_mancl is missing or 0
    ),
    carport_raw_cost = ifelse(cp_area > 0, 5850 + cp_area * 22, 0),
    # Ensure factor isn't applied if there's no carport cost
    cp_rcn_manual = ifelse(carport_raw_cost > 0, carport_raw_cost * CP_factor, 0)
  )

# --- 11. Calculate Total RCN ---
# Sum of Main Building, Garage, and Carport RCNs
sales_data <- sales_data %>%
  mutate(
    rcn_manual = mb_rcn_manual + gr_rcn_manual + cp_rcn_manual
  )

# --- Display first few rows of calculated RCN components ---
cat("\n--- Sample Calculated RCN Components ---\n")

cat("\n--- RCN Calculation Complete --- \n")


# You now have a new column ‘RCN’ in the dataframe:

## Depreciation ####
sales_data%<>%mutate(effage = 2017 - mbeffyr)

sales_data%<>%mutate(imprrsid_manual = saleprice_time_adjusted - genlandpow_neigh,
               obsdeprn = 1- (imprrsid_manual/(rcn_manual)))

obsdep_effage_plot <- sales_data%>%ggplot(aes(x = effage, y = obsdeprn))+geom_point(alpha = 0.5)+
  geom_smooth(method = "lm", formula = y ~ poly(x,2), se = FALSE, 
              aes(x = effage, y = obsdeprn, color = "Quadratic"))+
  geom_smooth(method = "lm", formula = y ~ poly(x,3), se = FALSE, 
              aes(x = effage, y = obsdeprn, color = "Cubic"))+
  labs ( color = "", title = "Observed depreciation vs effective year")+
  theme_minimal()


ggsave("Exam/obs_dep_1.jpg", plot = obsdep_effage_plot, width = 8, height = 6, dpi = 300)


#### Capping effective yaer for Depreciation ####

sales_data%<>%mutate(effage2 = pmin(effage, 40))

obsdep_effage_plot_2 <- sales_data %>%
  filter(obsdeprn > 0) %>%
  ggplot(aes(x = effage2, y = obsdeprn)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE, 
              aes(color = "Quadratic (raw)")) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2) + I(x^3), se = FALSE, 
              aes(color = "Cubic (raw)")) +
  labs(color = "", title = "Observed depreciation vs capped effective year") +
  theme_minimal()


quad_model <- lm(obsdeprn ~ effage2 + I(effage2^2), data = sales_data %>% filter(obsdeprn > 0))
cubic_model <- lm(obsdeprn ~ effage2 + I(effage2^2) + I(effage2^3), data = sales_data %>% filter(obsdeprn > 0))

summary(quad_model)
summary(cubic_model)


ggsave("Exam/obs_dep_2.jpg", plot = obsdep_effage_plot_2, width = 8, height = 6, dpi = 300)

sales_data%<>%mutate(pctgood = 1 - predict(quad_model, newdata = sales_data))

### PCTGOOD ####

pctgood_plot <- sales_data%>%ggplot(aes(x = effage2, y= pctgood))+geom_point(color = 'red')+
  labs(title = 'Pctgood vs effage2 (effage capped at 40 years)')+
  theme_minimal()

ggsave("Exam/pctgood_plot.jpg", plot = pctgood_plot, width = 8, height = 6, dpi = 300)

## Improvement Value Adjustment ####

### Correlation Matrix ####
bin_var_impr <- c("heattype", "baseste", "garagefl", "carptfl",
                  "gar_type","splitlvl") # Add others if needed

chi_square_df_imp <- sales_data[,names(sales_data) %in% bin_var_impr]

View(cor(chi_square_df_imp%>%mutate(across(everything(), as.numeric))))
chisq.test(chi_square_df_imp$garagefl, chi_square_df_imp$gar_type)
table(chi_square_df_imp$garagefl, chi_square_df_imp$gar_type)


sales_data%<>%mutate(imprval_rcndl = rcn_manual * pctgood + ob1mktval,
                     imprval_ratio = imprrsid_manual / imprval_rcndl)

ratio_summary_fn(sales_data, 'imprval_ratio')


perform_kruskal_tests(sales_data, bin_var_impr, 'imprval_ratio')

lapply(X = bin_var, function (x) ratio_summary_fn(sales_data, 'imprval_ratio',x))

### Adjustment for Garage Type ####
garage_adjustment_before <- ratio_summary_fn(sales_data, 'imprval_ratio', 'gar_type')

sales_data%<>%mutate(imprval_rcndl_gar = case_when(
            gar_type == 2 ~ imprval_rcndl * garage_adjustment_before[garage_adjustment_before['gar_type']==2,]$imprval_ratio$Median,
            gar_type == 3 ~ imprval_rcndl * garage_adjustment_before[garage_adjustment_before['gar_type']==3,]$imprval_ratio$Median,
            TRUE ~ imprval_rcndl
),imprval_ratio_gar = imprrsid_manual / imprval_rcndl_gar)

garage_adjustment_after <- ratio_summary_fn(sales_data, 'imprval_ratio_gar', 'gar_type')
perform_kruskal_tests(sales_data%>%filter(heattype!= '1'), 'gar_type', 'imprval_ratio_gar')

perform_kruskal_tests(sales_data, bin_var_impr, 'imprval_ratio_gar')

heattype_adjustment_before <- ratio_summary_fn(sales_data%>%mutate(heattype = ifelse(heattype=='1','2', heattype)), 
                 'imprval_ratio_gar', 'heattype')

sales_data%<>%mutate(imprval_rcndl_heattype = case_when(
  heattype == 3 ~ imprval_rcndl_gar * heattype_adjustment_before[heattype_adjustment_before['heattype']==3,]$imprval_ratio_gar$Median,
  heattype == 7 ~ imprval_rcndl_gar * heattype_adjustment_before[heattype_adjustment_before['heattype']==7,]$imprval_ratio_gar$Median,
  TRUE ~ imprval_rcndl_gar
),imprval_ratio_gar_heattype = imprrsid_manual / imprval_rcndl_heattype)

perform_kruskal_tests(sales_data, bin_var_impr, 'imprval_ratio_gar')

heattype_adjustment_after <- ratio_summary_fn(sales_data%>%mutate(heattype = ifelse(heattype=='1','2', heattype)), 
                                               'imprval_ratio_gar_heattype', 'heattype')

perform_kruskal_tests(sales_data, bin_var_impr, 'imprval_ratio_gar_heattype')


### Testing ration for building characteristics ####
building_characteristics_vars<- c("carptfl", "crawlfl", "fireplcs", "bedrooms", "familyrm",
                                  "baseste", "bath100", "gr_mancl", "cp_mancl", "sup_heat",
                                  "stories", "mbmancls")


chi_square_df_building <- sales_data[,names(sales_data) %in% building_characteristics_vars]
View(cor(chi_square_df_building%>%mutate(across(everything(), as.numeric))))




perform_kruskal_tests(sales_data, 'stories', 'imprval_ratio_gar_heattype')
stories_adjustment_before <- ratio_summary_fn(sales_data, 'imprval_ratio_gar_heattype', 'stories')

perform_kruskal_tests(sales_data, 'mbmancls', 'imprval_ratio_gar_heattype')
ratio_summary_fn(sales_data, 'imprval_ratio_gar_heattype', 'mbmancls')

mbmanclss_adjustment_before <- ratio_summary_fn(sales_data%>%mutate(mbmancls_2 = case_when(
  mbmancls %in% c('145', '147' ,'150') ~ '15',
  mbmancls %in% c('140', '142') ~ '14',
  mbmancls %in% c('90', '92') ~ '9',
  TRUE ~ '8')),'imprval_ratio_gar_heattype', 'mbmancls_2') 

perform_kruskal_tests(sales_data%>%mutate(mbmancls = case_when(
  mbmancls %in% c('145', '147' ,'150') ~ '15',
  mbmancls %in% c('140', '142') ~ '14',
  mbmancls %in% c('90', '92') ~ '9',
  TRUE ~ '8')), 'mbmancls', 'imprval_ratio_gar_heattype')


sales_data%<>%mutate(mbmancls_2 = case_when(
  mbmancls %in% c('145', '147' ,'150') ~ '15',
  mbmancls %in% c('140', '142') ~ '14',
  mbmancls %in% c('90', '92') ~ '9',
  TRUE ~ '8'))%>%
  mutate(imprval_rcndl_heattype_mbmancls = case_when(
  mbmancls_2 == '8' ~ imprval_rcndl_heattype * mbmanclss_adjustment_before[mbmanclss_adjustment_before['mbmancls_2']=='8',]$imprval_ratio_gar_heattype$Median,
  TRUE ~ imprval_rcndl_heattype
),imprval_ratio_gar_heattype_mbmancls = imprrsid_manual / imprval_rcndl_heattype_mbmancls)

mbmanclss_adjustment_after <- ratio_summary_fn(sales_data,'imprval_ratio_gar_heattype_mbmancls', 'mbmancls_2') 

perform_kruskal_tests(sales_data, 'mbmancls_2', 'imprval_ratio_gar_heattype_mbmancls')

building_characteristics_vars<- c("carptfl", "crawlfl", "fireplcs", "bedrooms", "familyrm",
                             "baseste", "bath100", "gr_mancl", "cp_mancl", "sup_heat")

perform_kruskal_tests(sales_data, building_characteristics_vars, 'imprval_ratio_gar_heattype_mbmancls')


familyrm_adjustment_before <- ratio_summary_fn(sales_data, 'imprval_ratio_gar_heattype_mbmancls', 'familyrm')
perform_kruskal_tests(sales_data, 'familyrm', 'imprval_ratio_gar_heattype_mbmancls')

sales_data%<>%mutate(imprval_rcndl_heattype_mbmancls_familyrm = case_when(
  familyrm == 0 ~ imprval_rcndl_heattype_mbmancls * familyrm_adjustment_before[familyrm_adjustment_before['familyrm']==0,]$imprval_ratio_gar_heattype_mbmancls$Median,
  TRUE ~ imprval_rcndl_heattype_mbmancls),
imprval_ratio_gar_heattype_mbmancls_familyrm = imprrsid_manual / imprval_rcndl_heattype_mbmancls_familyrm)

perform_kruskal_tests(sales_data, 'familyrm', 'imprval_ratio_gar_heattype_mbmancls_familyrm')
familyrm_adjustment_after <- ratio_summary_fn(sales_data, 'imprval_ratio_gar_heattype_mbmancls_familyrm', 'familyrm')

perform_kruskal_tests(sales_data, building_characteristics_vars, 'imprval_ratio_gar_heattype_mbmancls_familyrm')

bedroom_adjustment_before <- ratio_summary_fn(sales_data, 'imprval_ratio_gar_heattype_mbmancls_familyrm', 'bedrooms')
perform_kruskal_tests(sales_data, 'bedrooms', 'imprval_ratio_gar_heattype_mbmancls_familyrm')

sales_data%<>%mutate(imprval_rcndl_heattype_mbmancls_familyrm_bedroom = case_when(
  bedrooms == 2 ~ imprval_rcndl_heattype_mbmancls_familyrm * bedroom_adjustment_before[bedroom_adjustment_before['bedrooms']==2,]$imprval_ratio_gar_heattype_mbmancls_familyrm$Median,
  TRUE ~ imprval_rcndl_heattype_mbmancls_familyrm),
  imprval_ratio_gar_heattype_mbmancls_familyrm_bedroom = imprrsid_manual / imprval_rcndl_heattype_mbmancls_familyrm_bedroom)

bedroom_adjustment_after <- ratio_summary_fn(sales_data, 'imprval_ratio_gar_heattype_mbmancls_familyrm_bedroom', 'bedrooms')
perform_kruskal_tests(sales_data, 'bedrooms', 'imprval_ratio_gar_heattype_mbmancls_familyrm_bedroom')

perform_kruskal_tests(sales_data, building_characteristics_vars, 'imprval_ratio_gar_heattype_mbmancls_familyrm_bedroom')


# Testing cost model ####
sales_data%<>%mutate(imprval_rcndl_final = imprval_rcndl_heattype_mbmancls_familyrm_bedroom,
                     land_value_final = genlandpow_neigh,
                     costval = imprval_rcndl_final + land_value_final,
                     costasr = saleprice_time_adjusted/ costval)

ratio_summary_fn(sales_data, 'costasr')

  
lm_model <- lm(costval ~ saleprice_time_adjusted, data = sales_data)
r_squared <- summary(lm_model)$r.squared

costval_sale_price <- ggplot(sales_data, aes(x = saleprice_time_adjusted/100000, 
                                             y = costval/100000)) +
  geom_point(alpha = 0.5) + # Points
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "blue") + # Linear trend line
  labs(
    title = "Cost Value versus Time Adjusted Sale Price",
    x = "Time Adjusted Sale Price ($100,000)",
    y = "Cost Value ($100,000)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = font_size), # Set font size for all text elements
    axis.title = element_text(size = font_size), # Set font size for axis titles
    axis.text = element_text(size = font_size), # Set font size for axis text
    plot.title = element_text(size = font_size), # Set font size for plot title
    legend.text = element_text(size = font_size) # Set font size for legend text
  ) + geom_label(aes(x = max(saleprice_time_adjusted/100000, na.rm = TRUE)*0.9, 
                     y = max(costval/100000, na.rm = TRUE)*0.9, 
                     label = paste("R² =", round(r_squared, 2))), 
                 size = 5, hjust = 1, fill = "white", color = "red")


ggsave("Exam/costval_sale_price.jpg", plot = costval_sale_price, width = 8, height = 6, dpi = 300)

perform_kruskal_tests(sales_data, 'costasr', 'neigh')

neigh_adjustment_costasr_before <- ratio_summary_fn(sales_data, 'costasr','neigh')

sales_data%<>%mutate(costval_neigh = case_when(
  neigh == "734" ~ costval * neigh_adjustment_costasr_before[neigh_adjustment_costasr_before['neigh']=="734",]$costasr$Median,
  neigh == "731" ~ costval * neigh_adjustment_costasr_before[neigh_adjustment_costasr_before['neigh']=="731",]$costasr$Median,
  TRUE ~ costval
),costasr_neigh = saleprice_time_adjusted / costval_neigh)

perform_kruskal_tests(sales_data, 'costasr_neigh', 'neigh')

neigh_adjustment_costasr_after <- ratio_summary_fn(sales_data, 'costasr_neigh','neigh')

ratio_summary_fn(sales_data, 'costasr_neigh')

### Final Testing for Cost Model ####

lm_model <- lm(costasr_neigh ~ bsmtarea, data = sales_data)
r_squared <- summary(lm_model)$r.squared
ncostasr_basemnet_area_plot <- ggplot(sales_data, aes(x = bsmtarea, 
                                             y = costasr_neigh)) +
  geom_point(alpha = 0.5) + # Points
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "blue") + # Linear trend line
  labs(
    title = "Neighborhood Adjusted Cost ASR versus Basement Area",
    x = "Basement Area",
    y = "Neighborhood Adjusted Cost ASR"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = font_size), # Set font size for all text elements
    axis.title = element_text(size = font_size), # Set font size for axis titles
    axis.text = element_text(size = font_size), # Set font size for axis text
    plot.title = element_text(size = font_size), # Set font size for plot title
    legend.text = element_text(size = font_size) # Set font size for legend text
  ) + geom_label(aes(x = max(bsmtarea, na.rm = TRUE)*0.9, 
                     y = max(costasr_neigh, na.rm = TRUE)*0.9, 
                     label = paste("R² =", round(r_squared, 2))), 
                 size = 5, hjust = 1, fill = "white", color = "red")
ggsave("Exam/ncostasr_basemenet_area.jpg", plot = ncostasr_basemnet_area_plot, width = 8, height = 6, dpi = 300)


ncostasr_lot_area_plot <- ggplot(sales_data, aes(x = lot_size, 
                                                      y = costasr_neigh)) +
  geom_point(alpha = 0.5) + # Points
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "blue") + # Linear trend line
  labs(
    title = "Neighborhood Adjusted Cost ASR versus Lot Area",
    x = "Lot Area",
    y = "Neighborhood Adjusted Cost ASR"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = font_size), # Set font size for all text elements
    axis.title = element_text(size = font_size), # Set font size for axis titles
    axis.text = element_text(size = font_size), # Set font size for axis text
    plot.title = element_text(size = font_size), # Set font size for plot title
    legend.text = element_text(size = font_size) # Set font size for legend text
  ) + geom_label(aes(x = max(lot_size, na.rm = TRUE)*0.9, 
                     y = max(costasr_neigh, na.rm = TRUE)*0.9, 
                     label = paste("R² =", round(r_squared, 2))), 
                 size = 5, hjust = 1, fill = "white", color = "red")
ggsave("Exam/ncostasr_lot_area.jpg", plot = ncostasr_lot_area_plot, width = 8, height = 6, dpi = 300)


ncostasr_fireplace_plot <- ggplot(sales_data, aes(x = as.factor(fireplcs), 
                                                 y = costasr_neigh)) +
  geom_boxplot(aes(fill = as.factor(fireplcs)))+
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "blue") + # Linear trend line
  labs(
    title = "Neighborhood Adjusted Cost ASR versus Fireplaces",
    x = "Fireplaces",
    y = "Neighborhood Adjusted Cost ASR",
    fill = "Fireplaces"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = font_size), # Set font size for all text elements
    axis.title = element_text(size = font_size), # Set font size for axis titles
    axis.text = element_text(size = font_size), # Set font size for axis text
    plot.title = element_text(size = font_size), # Set font size for plot title
    legend.text = element_text(size = font_size) # Set font size for legend text
  ) 
  
  
ggsave("Exam/ncostasr_fireplce.jpg", plot = ncostasr_fireplace_plot, width = 8, height = 6, dpi = 300)

# -----------------------------------------------------------------------------
# R Code for BUSI 444 Case Study Examination - Sale Comparison Model Analysis ####
# -----------------------------------------------------------------------------

output_figure_dpi <- 300
figure_font_size <- 12 # Adjust base font size for plots

# --- Load Data (Assuming 'sales_data' dataframe exists) ---
# Ensure your 'sales_data' dataframe is loaded and contains the columns listed.

# --- Calculate Effective Age ---
if ("mbeffyr" %in% names(sales_data)) {
  sales_data <- sales_data %>% 
    mutate(effage = 2017 - mbyrblt)
  print("Calculated 'effage' = 2017 - mbeffyr")
} else {
  warning("'mbeffyr' column not found. Cannot calculate 'effage'. Check variable names.")
}

# --- Define Variable Groups and Readable Names ---
dependent_var <- "saleprice_time_adjusted"

# Continuous variables likely relevant as predictors
continuous_vars <- c("flrarea1","flrarea2", "lot_size", "effage", "bedrooms", "baths", 
                     "fireplcs", "gar_area", "bsmtarea", 'crwlarea') 

# Categorical/Factor variables likely relevant as predictors
categorical_vars <- c("neigh", "mbmancls", "heattype", "gar_type", "conduit", 
                      "alley", "sidewalk", "corner", "pie_shpe") 



# Mapping for readable names
readable_names <- list(
  "saleprice_time_adjusted" = "Time Adjusted Sale Price",
  "flrarea1" = "Floor Area",
  "lot_size" = "Lot Size",
  "effage" = "Effective Age",
  "bedrooms" = "Bedrooms",
  "baths" = "Bathrooms",
  "fireplcs" = "Fireplaces",
  "gar_area" = "Garage Area",
  "bsmtarea" = "Basement Area",
  "neigh" = "Neighbourhood",
  "mbmancls" = "Main Building Class",
  "heattype" = "Heating Type",
  "gar_type" = "Garage Type",
  "conduit" = "Conduit",
  "alley" = "Alley",
  "sidewalk" = "Sidewalk",
  "corner" = "Corner",
  "pie_shpe" = "Pie-Shaped",
  "ASR_neigh_2" = "Neighborhood Adjusted ASR"
  # Add other mappings as needed
)

# Function to get readable name, fallback to original if not found
get_readable_name <- function(var_name) {
  return(readable_names[[var_name]] %||% var_name)
}

# --- Data Preparation ---
all_vars_needed <- unique(c(dependent_var, continuous_vars, categorical_vars))
missing_vars <- setdiff(all_vars_needed, names(sales_data))
if(length(missing_vars) > 0) {
  print(paste("Warning: The following specified variables are not in sales_data:", paste(missing_vars, collapse=", ")))
}

continuous_vars <- intersect(continuous_vars, names(sales_data))
categorical_vars <- intersect(categorical_vars, names(sales_data))

if (!dependent_var %in% names(sales_data)) {
  stop(paste("Dependent variable '", dependent_var, "' not found in sales_data!"))
}

if (length(categorical_vars) > 0) {
  sales_data <- sales_data %>%
    mutate(across(all_of(categorical_vars), as.factor))
}

### Descriptive Statistics ####
# (Keeping this section as before - no changes requested here)
print("--- Overall Descriptive Statistics ---")
cols_for_summary <- intersect(c(dependent_var, continuous_vars, categorical_vars), names(sales_data))

numeric_summary <- summary(sales_data[, cols_for_summary])
print(numeric_summary)


readable_names <- list(
  "saleprice_time_adjusted" = "Time Adjusted Sale Price",
  "flrarea1" = "Floor Area1",
  "flrarea2" = "Floor Area2",
  "crwlarea" = "Crawl Area",
  "lot_size" = "Lot Size",
  "effage" = "Effective Age",
  "bedrooms" = "Bedrooms",
  "baths" = "Bathrooms",
  "fireplcs" = "Fireplaces",
  "gar_area" = "Garage Area",
  "bsmtarea" = "Basement Area",
  "neigh" = "Neighbourhood",
  "mbmancls" = "Main Building Class",
  "heattype" = "Heating Type",
  "gar_type" = "Garage Type",
  "conduit" = "Conduit",
  "alley" = "Alley Access",
  "sidewalk" = "Sidewalk",
  "corner" = "Corner Lot",
  "pie_shpe" = "Pie-shaped",
  'adjusted_land_size' = 'Adjusted Land Size',
  'dlin_flrarea1' = "Depreciated Linearized Floor Area 1",
  'dlin_flrarea2' = "Depreciated Linearized Floor Area 2",
  'dlin_bsmtfin' = "Depreciated Linearized Finished Basement Area",
  'dlin_gar_area_attached' = "Depreciated Linearized Garage Area Attached",
  'dlin_gar_area_detached' = "Depreciated Linearized Garage Area Detached",
  'dlin_gar_area_basement' = "Depreciated Linearized Garage Area Basement",
  'dlin_cp_area' = 'Depreciated Linearized Carport Area',
  'gas_heattype' = 'Heattype with Gas Fuel',
  'oil_heattype' = 'Heattype with Oil Fuel',
  'electric_heattype' = 'Heattype with Electric Fuel'
  # Add other mappings as needed
)

get_readable_name <- function(var_name) {
  return(readable_names[[var_name]] %||% var_name)
}

### Scatter Plots ####

if (!dir.exists("figures")) {
  dir.create("figures")
}

# a) Continuous Variables vs. TASP (Scatter Plots with Linear Fit and R^2)
print("--- Generating Individual Scatter Plots (Continuous Vars vs TASP) ---")
plot_list_scatter <- list()

for (var in continuous_vars) {
  if (var %in% names(sales_data)) {
    
    # Filter out NA values for this pair of variables for lm and plotting range
    plot_data <- sales_data %>% 
      select(y = all_of(dependent_var), x = all_of(var)) %>% 
      filter(!is.na(y) & !is.na(x))
    
    # Ensure there's data left to plot
    if(nrow(plot_data) > 1) {
      
      # Calculate R-squared
      r_squared <- NA # Default if lm fails
      tryCatch({
        lm_model <- lm(y ~ x, data = plot_data)
        r_squared <- summary(lm_model)$r.squared
      }, error = function(e) {
        print(paste("Could not fit linear model for", dependent_var, "vs", var, ":", e$message))
      })
      
      # Define readable labels
      readable_y <- get_readable_name(dependent_var)
      readable_x <- get_readable_name(var)
      plot_title <- paste(readable_y, "vs.", readable_x)
      r_squared_label <- if (!is.na(r_squared)) paste("R² =", round(r_squared, 2)) else "R² = NA"
      
      # Determine label position (adjust multipliers as needed)
      x_pos <- max(plot_data$x, na.rm = TRUE) * 0.95
      y_pos <- max(plot_data$y, na.rm = TRUE) * 0.95
      
      # Create Plot
      p <- ggplot(plot_data, aes(x = x, y = y)) +
        geom_point(alpha = 0.5, shape = 16, color = "black") + # Darker points
        geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "blue") + # Linear trend line
        geom_label(aes(x = x_pos, y = y_pos, label = r_squared_label), 
                   size = figure_font_size / 3, # Adjust label size relative to base font size
                   hjust = 1, vjust = 1, # Adjust justification
                   fill = "white", color = "red", label.padding = unit(0.15, "lines")) +
        scale_y_continuous(labels = scales::comma) + 
        scale_x_continuous(labels = scales::comma) + 
        labs(
          title = plot_title,
          x = readable_x,
          y = readable_y
        ) +
        theme_minimal() +
        theme( # Apply base font size
          text = element_text(size = figure_font_size), 
          axis.title = element_text(size = figure_font_size), 
          axis.text = element_text(size = figure_font_size), 
          plot.title = element_text(size = figure_font_size, face = "bold"), 
          legend.text = element_text(size = figure_font_size) 
        )
      
      plot_list_scatter[[var]] <- p
      
      # Save individual plot as JPEG
      file_name <- paste0("figures/scatter_", dependent_var, "_vs_", var, ".jpg") # Changed extension
      ggsave(file_name, plot = p, width = 8, height = 6, dpi = output_figure_dpi, units = "in")
      print(paste("Saved:", file_name))
      # print(p) # Optionally display plot immediately
      
    } else {
      print(paste("Not enough valid data points to plot", dependent_var, "vs", var))
    }
    
  } else {
    print(paste("Column '", var, "' not found for scatter plot."))
  }
}

### Box Plot ####
# b) Categorical / Discrete Variables vs. TASP (Box Plots)
# Using the explicitly defined boxplot_vars list now

# Continuous variables likely relevant as predictors
continuous_vars <- c("flrarea1", "lot_size", "effage", "bedrooms", "baths", 
                     "fireplcs", "gar_area", "bsmtarea","flrarea2","bsmtfin",
                     "crwlarea") 

# Categorical/Factor variables likely relevant as predictors
categorical_vars <- c("heattype", "gar_type", "conduit", 
                      "alley", "sidewalk", "corner", "pie_shpe") 

# Variables for Box Plotting (Categorical + Discrete Numeric)
boxplot_vars <- c("heattype", "gar_type", "conduit", 
                  "baths", "bedrooms", "fireplcs","baseste") # ADDED baths, bedrooms, fireplcs here

correlation_vars <- c(continuous_vars, "conduit", 
                      "alley", "sidewalk", "corner", "pie_shpe",
                      "baseste")

continuous_vars <- intersect(continuous_vars, names(sales_data))
categorical_vars <- intersect(categorical_vars, names(sales_data))
boxplot_vars <- intersect(boxplot_vars, names(sales_data)) # Check boxplot vars exist
correlation_vars <- intersect(correlation_vars, names(sales_data)) # Check boxplot vars exist


print("--- Generating Box Plots (Categorical/Discrete Vars vs TASP) ---")
plot_list_box <- list()

for (var in boxplot_vars) { # Iterate through the specific list for box plots
  if (var %in% names(sales_data)) {
    
    # Filter data and ensure the variable is treated as a factor for plotting
    plot_data_cat <- sales_data %>% 
      filter(!is.na(.data[[var]]) & !is.na(.data[[dependent_var]])) %>%
      mutate(x_var_factor = factor(.data[[var]])) # Convert to factor for plot axis
    
    if(nrow(plot_data_cat) > 0) {
      n_levels <- n_distinct(plot_data_cat$x_var_factor, na.rm = TRUE)
      angle_x = if(n_levels > 10) 45 else 0 
      hjust_x = if(n_levels > 10) 1 else 0.5
      
      readable_y <- get_readable_name(dependent_var)
      readable_x <- get_readable_name(var) # Get readable name for original variable
      plot_title <- paste(readable_y, "Distribution by", readable_x)
      
      # Plot using the factor version for the x-axis
      p <- ggplot(plot_data_cat, aes(x = x_var_factor, y = .data[[dependent_var]])) +
        geom_boxplot(fill = "lightblue", outlier.shape = 21, outlier.alpha = 0.5, outlier.size = 1.5) +
        scale_y_continuous(labels = scales::comma) +
        labs(
          title = plot_title,
          x = readable_x, # Use readable name for axis label
          y = readable_y
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = angle_x, hjust = hjust_x),
          text = element_text(size = figure_font_size), axis.title = element_text(size = figure_font_size), 
          axis.text = element_text(size = figure_font_size), plot.title = element_text(size = figure_font_size, face = "bold"), 
          legend.text = element_text(size = figure_font_size) 
        ) 
      plot_list_box[[var]] <- p
      
      file_name <- paste0("figures/boxplot_", dependent_var, "_by_", var, ".jpg") # Save using original var name
      ggsave(file_name, plot = p, width = max(7, n_levels*0.5), height = 5, dpi = output_figure_dpi, units = "in")
      print(paste("Saved:", file_name))
      
    } else {
      print(paste("Not enough valid data points to plot boxplot for", dependent_var, "by", var))
    }
  } else {
    print(paste("Column '", var, "' not found for box plot."))
  }
}

### correlation Matrix ####
correlation_data <- sales_data %>% 
  select(all_of(correlation_vars))%>%
  mutate(across(everything(), as.numeric))

# Calculate correlation matrix
cor_matrix_predictors <- cor(correlation_data, use = "pairwise.complete.obs") # Handle missing values

View(cor_matrix_predictors)
### Variable Transofrmation ###
sales_data%<>%mutate(adjusted_land_size = land_value_final / median_size_factor,
                     dlin_flrarea1 = MB_factor * flrarea1 * pctgood,
                     dlin_flrarea2 = MB_factor * flrarea2 * pctgood,
                     dlin_bsmtfin = MB_factor * bsmtfin * pctgood,
                     dlin_gar_area_attached = CP_factor * gar_area * (gar_type == 1) * pctgood,
                     dlin_gar_area_detached = CP_factor * gar_area * (gar_type == 2) * pctgood,
                     dlin_gar_area_basement = CP_factor * flrarea1 * (gar_type == 3) * pctgood,
                     dlin_cp_area = CP_factor * cp_area * pctgood,
                     gas_heattype = ifelse(heattype %in% c(1,2,7),1,0),
                     oil_heattype = ifelse(heattype %in% c(3,8),1,0),
                     electric_heattype = ifelse(heattype %in% c(4,5,6),1,0))

#### Variable Transsformation Examination ####
plot_list_scatter <- list()

for (var in c('adjusted_land_size','dlin_flrarea1','dlin_flrarea2',
              'dlin_bsmtfin','dlin_gar_area_attached','dlin_gar_area_detached',
              'dlin_gar_area_basement',
              'dlin_cp_area')) {
  if (var %in% names(sales_data)) {
    
    # Filter out NA values for this pair of variables for lm and plotting range
    plot_data <- sales_data %>% 
      select(y = all_of(dependent_var), x = all_of(var)) %>% 
      filter(!is.na(y) & !is.na(x))
    
    # Ensure there's data left to plot
    if(nrow(plot_data) > 1) {
      
      # Calculate R-squared
      r_squared <- NA # Default if lm fails
      tryCatch({
        lm_model <- lm(y ~ x, data = plot_data)
        r_squared <- summary(lm_model)$r.squared
      }, error = function(e) {
        print(paste("Could not fit linear model for", dependent_var, "vs", var, ":", e$message))
      })
      
      # Define readable labels
      readable_y <- get_readable_name(dependent_var)
      readable_x <- get_readable_name(var)
      plot_title <- paste(readable_y, "vs.", readable_x)
      r_squared_label <- if (!is.na(r_squared)) paste("R² =", round(r_squared, 2)) else "R² = NA"
      
      # Determine label position (adjust multipliers as needed)
      x_pos <- max(plot_data$x, na.rm = TRUE) * 0.95
      y_pos <- max(plot_data$y, na.rm = TRUE) * 0.95
      
      # Create Plot
      p <- ggplot(plot_data, aes(x = x, y = y)) +
        geom_point(alpha = 0.5, shape = 16, color = "black") + # Darker points
        geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "blue") + # Linear trend line
        geom_label(aes(x = x_pos, y = y_pos, label = r_squared_label), 
                   size = figure_font_size / 3, # Adjust label size relative to base font size
                   hjust = 1, vjust = 1, # Adjust justification
                   fill = "white", color = "red", label.padding = unit(0.15, "lines")) +
        scale_y_continuous(labels = scales::comma) + 
        scale_x_continuous(labels = scales::comma) + 
        labs(
          title = plot_title,
          x = readable_x,
          y = readable_y
        ) +
        theme_minimal() +
        theme( # Apply base font size
          text = element_text(size = figure_font_size), 
          axis.title = element_text(size = figure_font_size), 
          axis.text = element_text(size = figure_font_size), 
          plot.title = element_text(size = figure_font_size, face = "bold"), 
          legend.text = element_text(size = figure_font_size) 
        )
      
      plot_list_scatter[[var]] <- p
      
      # Save individual plot as JPEG
      file_name <- paste0("figures/scatter_", dependent_var, "_vs_", var, ".jpg") # Changed extension
      ggsave(file_name, plot = p, width = 8, height = 6, dpi = output_figure_dpi, units = "in")
      print(paste("Saved:", file_name))
      # print(p) # Optionally display plot immediately
      
    } else {
      print(paste("Not enough valid data points to plot", dependent_var, "vs", var))
    }
    
  } else {
    print(paste("Column '", var, "' not found for scatter plot."))
  }
}


plot_list_box <- list()

for (var in c('gas_heattype','oil_heattype','electric_heattype')) { # Iterate through the specific list for box plots
  if (var %in% names(sales_data)) {
    
    # Filter data and ensure the variable is treated as a factor for plotting
    plot_data_cat <- sales_data %>% 
      filter(!is.na(.data[[var]]) & !is.na(.data[[dependent_var]])) %>%
      mutate(x_var_factor = factor(.data[[var]])) # Convert to factor for plot axis
    
    if(nrow(plot_data_cat) > 0) {
      n_levels <- n_distinct(plot_data_cat$x_var_factor, na.rm = TRUE)
      angle_x = if(n_levels > 10) 45 else 0 
      hjust_x = if(n_levels > 10) 1 else 0.5
      
      readable_y <- get_readable_name(dependent_var)
      readable_x <- get_readable_name(var) # Get readable name for original variable
      plot_title <- paste(readable_y, "Distribution by", readable_x)
      
      # Plot using the factor version for the x-axis
      p <- ggplot(plot_data_cat, aes(x = x_var_factor, y = .data[[dependent_var]])) +
        geom_boxplot(fill = "lightblue", outlier.shape = 21, outlier.alpha = 0.5, outlier.size = 1.5) +
        scale_y_continuous(labels = scales::comma) +
        labs(
          title = plot_title,
          x = readable_x, # Use readable name for axis label
          y = readable_y
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = angle_x, hjust = hjust_x),
          text = element_text(size = figure_font_size), axis.title = element_text(size = figure_font_size), 
          axis.text = element_text(size = figure_font_size), plot.title = element_text(size = figure_font_size, face = "bold"), 
          legend.text = element_text(size = figure_font_size) 
        ) 
      plot_list_box[[var]] <- p
      
      file_name <- paste0("figures/boxplot_", dependent_var, "_by_", var, ".jpg") # Save using original var name
      ggsave(file_name, plot = p, width = max(7, n_levels*0.5), height = 5, dpi = output_figure_dpi, units = "in")
      print(paste("Saved:", file_name))
      
    } else {
      print(paste("Not enough valid data points to plot boxplot for", dependent_var, "by", var))
    }
  } else {
    print(paste("Column '", var, "' not found for box plot."))
  }
}

### Model Calibration ####

common_vars <- c('adjusted_land_size','dlin_flrarea1','dlin_flrarea2',
                 'dlin_bsmtfin','dlin_gar_area_attached','dlin_gar_area_detached',
                 'dlin_gar_area_basement',
                 'dlin_cp_area','gas_heattype','oil_heattype',
                 'electric_heattype',
                 "baths", "bedrooms", "fireplcs","baseste",'crwlarea')

model_1_var <- c('conduit')

model_2_var <- c('alley')

model_3_var <- c('bsmtarea','conduit')

model_4_var <- c('bsmtarea','alley')


### Model Summary for Four Models ####
common_vars <- c('adjusted_land_size','dlin_flrarea1','dlin_flrarea2',
                 'dlin_bsmtfin','dlin_gar_area_attached','dlin_gar_area_detached',
                 'dlin_gar_area_basement','dlin_cp_area','oil_heattype',
                 'electric_heattype','baths','bedrooms','fireplcs','baseste','crwlarea')

model_1_var <- c('conduit')
model_2_var <- c('alley')
model_3_var <- c('bsmtarea', 'conduit')
model_4_var <- c('bsmtarea', 'alley')

model_vars_list <- list(model_1_var, model_2_var, model_3_var, model_4_var)

# Run models and store results
results <- list()

for (i in 1:4) {
  model_vars <- c(common_vars, model_vars_list[[i]])
  formula_str <- paste("saleprice_time_adjusted ~", paste(model_vars, collapse = " + "))
  model <- lm(as.formula(formula_str), data = sales_data)
  
  # Summary stats
  model_summary <- summary(model)
  
  # VIF and Tolerance
  vif_vals <- vif(model)
  tolerance_vals <- 1 / vif_vals
  
  results[[paste0("Model_", i)]] <- list(
    formula = formula_str,
    summary = model_summary,
    vif = vif_vals,
    tolerance = tolerance_vals
  )
}

# Print model comparison summary
for (i in 1:4) {
  cat("\n==========", paste0("Model_", i), "==========\n")
  cat("Formula: ", results[[i]]$formula, "\n")
  
  # Extract summary
  model_sum <- results[[i]]$summary
  fstat <- model_sum$fstatistic
  df1 <- fstat["numdf"]  # numerator degrees of freedom
  df2 <- fstat["dendf"]  # denominator degrees of freedom
  
  cat("R: ", sqrt(model_sum$r.squared), "\n")
  cat("R^2: ", model_sum$r.squared, "\n")
  cat("Adjusted R^2: ", model_sum$adj.r.squared, "\n")
  cat("F-statistic: ", fstat[1], "\n")
  cat("Degrees of Freedom: df1 =", df1, ", df2 =", df2, "\n")
  cat("p-value: ", pf(fstat[1], df1, df2, lower.tail = FALSE), "\n\n")
  
  cat("Coefficients:\n")
  print(coef(model_sum))
  
  cat("\nVIF:\n")
  print(results[[i]]$vif)
  
  cat("\nTolerance:\n")
  print(results[[i]]$tolerance)
  cat("\n")
}

### Model 2 is selected ####
### Data split ####
n_total <- nrow(sales_data)

# Calculate exact sizes
n_train <- floor(2/3 * n_total)
n_test <- n_total - n_train  # just in case rounding affected it

# Sort by the 'random' column to ensure consistent split
sales_data_sorted <- sales_data %>% arrange(random)

# Split the data
train_data <- sales_data_sorted[1:n_train, ]
test_data  <- sales_data_sorted[(n_train + 1):n_total, ]

# Check sizes
cat("Training set size:", nrow(train_data), "\n")  # should be exactly 2/3 of total
cat("Testing set size:", nrow(test_data), "\n")    # should be the rest (1/3)

#### Stepwise regression
# Combine predictors
model_vars <- c(common_vars, model_2_var)
data_train_model2 <- train_data %>% select(all_of(c("saleprice_time_adjusted", model_vars)))

# Create full model formula
full_formula <- as.formula(paste("saleprice_time_adjusted ~", paste(model_vars, collapse = " + ")))

full_model <- lm(saleprice_time_adjusted ~ ., data = data_train_model2)
null_model <- lm(saleprice_time_adjusted ~ 1, data = data_train_model2)

# Initialize list to store intermediate models
intermediate_models <- list()

# Function to capture intermediate models
capture_models <- function(model, direction) {
  intermediate_models <<- append(intermediate_models, list(model))
}

step_model <- step(
  full_model,
  direction = "both",
  trace = 1,
  scope = list(lower = null_model, upper = full_model),
  k = 2,
  steps = 1000,
  keep = capture_models
)

# Refit models and assign names
model_formulas <- lapply(intermediate_models, formula)
models <- lapply(model_formulas, function(fml) lm(fml, data = data_train_model2))
names(models) <- paste0("Step ", seq_along(models))

# Collect and display summary, VIF, and tolerance for each model
for (i in seq_along(models)) {
  model <- models[[i]]
  cat("\n=========================\n")
  cat("Step:", i, "|", names(models)[i], "\n")
  cat("=========================\n")
  
  # Standard model summary
  print(summary(model))
  
  # VIF and Tolerance
  if (length(coef(model)) > 1) {  # Only if model has more than intercept
    cat("\n--- VIF and Tolerance ---\n")
    vif_values <- vif(model)
    tolerance <- 1 / vif_values
    vif_table <- data.frame(Variable = names(vif_values),
                            VIF = round(vif_values, 3),
                            Tolerance = round(tolerance, 3))
    print(vif_table)
  } else {
    cat("Model only has intercept — skipping VIF/Tolerance.\n")
  }
}

# Final selected model
final_model <- intermediate_models[[length(intermediate_models)]]

#### Casewise Diagnosis Model ####
standardized_residuals <- rstandard(final_model)

# Identify observations with residuals > 3 or < -3
outliers <- unique(which(abs(standardized_residuals) > 3))

# Extract outlier details
outlier_data <- data_train_model2[outliers, ]
outlier_resids <- standardized_residuals[outliers]

# Combine for report
casewise_report <- data.frame(
  Row = outliers,
  Std_Residual = round(outlier_resids, 3),
  Predicted = round(predict(final_model)[outliers], 2),
  Actual = round(data_train_model2$saleprice_time_adjusted[outliers], 2),
  Residual = round(data_train_model2$saleprice_time_adjusted[outliers] - predict(final_model)[outliers], 2)
)

# View the results
print(casewise_report)
non_outlier_indices <- which(abs(standardized_residuals) <= 3)

# Filter the training dataset to exclude outliers
data_train_model2_cleaned <- data_train_model2[non_outlier_indices, ]

refit_model_cleaned <- lm(formula(final_model), data = data_train_model2_cleaned)

# Summary of coefficients
cat("=== Linear Model Summary ===\n")
summary_refit <- summary(refit_model_cleaned)
print(summary_refit)

# VIF and Tolerance
if (length(coef(refit_model_cleaned)) > 1) {
  cat("\n=== VIF and Tolerance ===\n")
  vif_vals <- vif(refit_model_cleaned)
  tolerance_vals <- 1 / vif_vals
  vif_table <- data.frame(
    Variable = names(vif_vals),
    VIF = round(vif_vals, 3),
    Tolerance = round(tolerance_vals, 3)
  )
  print(vif_table)
} else {
  cat("Model only has intercept — skipping VIF/Tolerance.\n")
}


#### Recheck for casewise diagnosis 
final_model_2 <- refit_model_cleaned
standardized_residuals <- rstandard(final_model_2)

# Identify observations with residuals > 3 or < -3
outliers <- unique(which(abs(standardized_residuals) > 3))

 
## Model Testing ####
train_data_cleaned <- sales_data_sorted[non_outlier_indices, ]
train_data_cleaned$saleprice_time_adjusted_predicted = predict(final_model_2)

mean(train_data_cleaned$saleprice_time_adjusted_predicted)
mean(train_data_cleaned$saleprice_time_adjusted)

## ASR Testing ####
train_data_cleaned%<>%
  mutate(ASR = saleprice_time_adjusted_predicted / saleprice_time_adjusted) 

neigh_adjusted_sale_price_before <- ratio_summary_fn(train_data_cleaned, 'ASR','neigh')
perform_kruskal_tests(train_data_cleaned, 'ASR', 'neigh')


train_data_cleaned%<>%mutate(saleprice_time_adjusted_predicted_neigh = 
                               case_when(
                                 neigh == '734' ~ saleprice_time_adjusted_predicted / neigh_adjusted_sale_price_before[neigh_adjusted_sale_price_before['neigh']=='734',]$ASR$Median,
                                 TRUE ~ saleprice_time_adjusted_predicted
                               ),
                             ASR_neigh = saleprice_time_adjusted_predicted_neigh / saleprice_time_adjusted)

kruskal.test(train_data_cleaned$ASR_neigh, train_data_cleaned$neigh)

neigh_adjusted_sale_price_after <- ratio_summary_fn(train_data_cleaned, 'ASR_neigh','neigh')

kruskal.test(train_data_cleaned$ASR_neigh, train_data_cleaned$heattype)

test_data  <- sales_data_sorted[(n_train + 1):n_total, ]
test_data$saleprice_time_adjusted_predicted = predict(final_model_2, newdata = test_data)

test_data%<>%
  mutate(ASR = saleprice_time_adjusted_predicted / saleprice_time_adjusted) 

test_data%<>%mutate(saleprice_time_adjusted_predicted_neigh = 
                               case_when(
                                 neigh == '734' ~ saleprice_time_adjusted_predicted / neigh_adjusted_sale_price_before[neigh_adjusted_sale_price_before['neigh']=='734',]$ASR$Median,
                                 TRUE ~ saleprice_time_adjusted_predicted
                               ),
                             ASR_neigh = saleprice_time_adjusted_predicted_neigh / saleprice_time_adjusted)

ratio_summary_fn(test_data, 'ASR_neigh')

sales_data_clean <- rbind(test_data, train_data_cleaned)
ratio_summary_fn(sales_data_clean, 'ASR_neigh')

kruskal.test(sales_data_clean$ASR_neigh, sales_data_clean$neigh)
neigh_adjusted_sale_price_all_before <- ratio_summary_fn(sales_data_clean, 'ASR_neigh','neigh')

sales_data_clean%<>%mutate(saleprice_time_adjusted_predicted_neigh_2 = 
                      case_when(
                        neigh == '731' ~ saleprice_time_adjusted_predicted_neigh / neigh_adjusted_sale_price_all_before[neigh_adjusted_sale_price_all_before['neigh']=='731',]$ASR_neigh$Median,
                        neigh == '713' ~ saleprice_time_adjusted_predicted_neigh / neigh_adjusted_sale_price_all_before[neigh_adjusted_sale_price_all_before['neigh']=='713',]$ASR_neigh$Median,
                        TRUE ~ saleprice_time_adjusted_predicted_neigh
                      ),
                    ASR_neigh_2 = saleprice_time_adjusted_predicted_neigh_2 / saleprice_time_adjusted)


lm_model <- lm(saleprice_time_adjusted_predicted_neigh_2 ~ saleprice_time_adjusted, data = sales_data_clean)
r_squared <- summary(lm_model)$r.squared


sale_comparison_sale_price <- ggplot(sales_data_clean, aes(x = saleprice_time_adjusted/100000, 
                                             y = saleprice_time_adjusted_predicted_neigh_2/100000)) +
  geom_point(alpha = 0.5) + # Points
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "blue") + # Linear trend line
  labs(
    title = "Predicted Sale Price from MRA versus Time Adjusted Sale Price",
    x = "Time Adjusted Sale Price ($100,000)",
    y = "Predicted Sale Price ($100,000)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = font_size), # Set font size for all text elements
    axis.title = element_text(size = font_size), # Set font size for axis titles
    axis.text = element_text(size = font_size), # Set font size for axis text
    plot.title = element_text(size = font_size), # Set font size for plot title
    legend.text = element_text(size = font_size) # Set font size for legend text
  ) + geom_label(aes(x = max(saleprice_time_adjusted/100000, na.rm = TRUE)*0.9, 
                     y = max(saleprice_time_adjusted_predicted_neigh_2/100000, na.rm = TRUE)*0.9, 
                     label = paste("R² =", round(r_squared, 2))), 
                 size = 5, hjust = 1, fill = "white", color = "red")

ggsave("Exam/sale_comparison_sale_price.jpg", 
       plot = sale_comparison_sale_price, width = 8, height = 6, dpi = 300)



kruskal.test(sales_data_clean$ASR_neigh_2, sales_data_clean$neigh)
neigh_adjusted_sale_price_all_after<- ratio_summary_fn(sales_data_clean, 'ASR_neigh_2','neigh')


perform_kruskal_tests(sales_data_clean, boxplot_vars,'ASR_neigh_2')
dependent_var <- "ASR_neigh_2"

#### Scatter plot ####
if (!dir.exists("figures")) {
  dir.create("figures")
}

# a) Continuous Variables vs. TASP (Scatter Plots with Linear Fit and R^2)
print("--- Generating Individual Scatter Plots (Continuous Vars vs TASP) ---")
plot_list_scatter <- list()

for (var in continuous_vars) {
  if (var %in% names(sales_data_clean)) {
    
    # Filter out NA values for this pair of variables for lm and plotting range
    plot_data <- sales_data_clean %>% 
      select(y = all_of(dependent_var), x = all_of(var)) %>% 
      filter(!is.na(y) & !is.na(x))
    
    # Ensure there's data left to plot
    if(nrow(plot_data) > 1) {
      
      # Calculate R-squared
      r_squared <- NA # Default if lm fails
      tryCatch({
        lm_model <- lm(y ~ x, data = plot_data)
        r_squared <- summary(lm_model)$r.squared
      }, error = function(e) {
        print(paste("Could not fit linear model for", dependent_var, "vs", var, ":", e$message))
      })
      
      # Define readable labels
      readable_y <- get_readable_name(dependent_var)
      readable_x <- get_readable_name(var)
      plot_title <- paste(readable_y, "vs.", readable_x)
      r_squared_label <- if (!is.na(r_squared)) paste("R² =", round(r_squared, 2)) else "R² = NA"
      
      # Determine label position (adjust multipliers as needed)
      x_pos <- max(plot_data$x, na.rm = TRUE) * 0.95
      y_pos <- max(plot_data$y, na.rm = TRUE) * 0.95
      
      # Create Plot
      p <- ggplot(plot_data, aes(x = x, y = y)) +
        geom_point(alpha = 0.5, shape = 16, color = "black") + # Darker points
        geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "blue") + # Linear trend line
        geom_label(aes(x = x_pos, y = y_pos, label = r_squared_label), 
                   size = figure_font_size / 3, # Adjust label size relative to base font size
                   hjust = 1, vjust = 1, # Adjust justification
                   fill = "white", color = "red", label.padding = unit(0.15, "lines")) +
        scale_y_continuous(labels = scales::comma) + 
        scale_x_continuous(labels = scales::comma) + 
        labs(
          title = plot_title,
          x = readable_x,
          y = readable_y
        ) +
        theme_minimal() +
        theme( # Apply base font size
          text = element_text(size = figure_font_size), 
          axis.title = element_text(size = figure_font_size), 
          axis.text = element_text(size = figure_font_size), 
          plot.title = element_text(size = figure_font_size, face = "bold"), 
          legend.text = element_text(size = figure_font_size) 
        )
      
      plot_list_scatter[[var]] <- p
      
      # Save individual plot as JPEG
      file_name <- paste0("figures/scatter_", dependent_var, "_vs_", var, ".jpg") # Changed extension
      ggsave(file_name, plot = p, width = 8, height = 6, dpi = output_figure_dpi, units = "in")
      print(paste("Saved:", file_name))
      # print(p) # Optionally display plot immediately
      
    } else {
      print(paste("Not enough valid data points to plot", dependent_var, "vs", var))
    }
    
  } else {
    print(paste("Column '", var, "' not found for scatter plot."))
  }
}

ratio_summary_fn(sales_data_clean,'ASR_neigh_2')
