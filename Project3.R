library(tidyverse)
library(readr)
library(haven)
library(magrittr)
library(car)
library(patchwork)
library(corrplot)
library(rlang)
library(broom)
rm(list = ls())

data <- read_sav('Data/Apartment.sav')
## COV Analysis ##
data%<>%mutate(SP_UNIT = price/units,
               SP_SQFT = price/area,
               GIM = price/eff_inc)

View(data.frame(SP_UNIT_COV = sd(data$SP_UNIT)/ mean(data$SP_UNIT),
           SQ_SQFT_COV = sd(data$SP_SQFT)/ mean(data$SP_SQFT),
           GIM_COV = sd(data$GIM)/mean(data$GIM)))

## Transformation 
data%<>%mutate(
  SML_UNIT = ifelse(units<=18 , 1, 0),
  SP_UNIT = price/units,
  SP_SQFT = price/area,
  CAP_RATE = noi/price,
  GIM = price/eff_inc,
  LOT_AREA = lotsize/area,
  AREA_UNIT = area/units,
  EXP_EGI = expenses/eff_inc,
  GI_UNI = gross_in/units,
  NO_UNIT = noi/units,
  NBHD_1 = ifelse(nbhd == 1 , 1, 0),
  NBHD_3 = ifelse(nbhd == 3 , 1, 0),
  NBHD_4 = ifelse(nbhd == 4 , 1, 0),
  CONDITION_1 = ifelse(conditn == 1 , 1, 0),
  CONDITION_3 = ifelse(conditn == 3 , 1, 0),
  STORY_2 = ifelse(story == 2 , 1, 0),
  STORY_4 = ifelse(story == 4 , 1, 0)
)

## Correlation Coefficient 
data%>%select(SP_UNIT, SP_SQFT, CAP_RATE, GIM, effage, deprec, LOT_AREA, AREA_UNIT, EXP_EGI)%>%cor(method = "pearson")
colnames(data) <- toupper(colnames(data))
### Scatter plots in the main text
FIG_1 <- data%>%ggplot(aes(y = SP_UNIT, x = EFFAGE)) + geom_point() + geom_smooth(method = 'lm', se = FALSE)
summary(lm(SP_UNIT ~ EFFAGE, data = data))$r.squared

FIG_2 <- data%>%ggplot(aes(y = GIM, x = DEPREC)) + geom_point() + geom_smooth(method = 'lm', se = FALSE)
summary(lm(GIM ~ DEPREC, data = data))$r.squared

FIG_3 <- data%>%ggplot(aes(y = CAP_RATE, x = LOT_AREA)) + geom_point() + geom_smooth(method = 'lm', se = FALSE)
summary(lm(CAP_RATE ~ LOT_AREA, data = data))$r.squared

FIG_4 <- data%>%ggplot(aes(y = SP_SQFT, x = AREA_UNIT)) + geom_point() + geom_smooth(method = 'lm', se = FALSE)
summary(lm(SP_SQFT ~ AREA_UNIT, data = data))$r.squared

(FIG_1 | FIG_2) / (FIG_3 | FIG_4) 

### Scatter plots in the Appendix
FIG_1 <- data%>%ggplot(aes(y = SP_UNIT, x = DEPREC)) + geom_point() + 
  geom_smooth(method = 'lm', se = FALSE)

FIG_2 <- data%>%ggplot(aes(y = SP_UNIT, x = LOT_AREA)) + geom_point() + 
  geom_smooth(method = 'lm', se = FALSE)+labs(y= "")

FIG_3 <- data%>%ggplot(aes(y = SP_UNIT, x = AREA_UNIT)) + geom_point() + 
  geom_smooth(method = 'lm', se = FALSE)+labs(y= "")

(FIG_1 | FIG_2 | FIG_3) 

FIG_1 <- data%>%ggplot(aes(y = GIM, x = EFFAGE)) + geom_point() + 
  geom_smooth(method = 'lm', se = FALSE)

FIG_2 <- data%>%ggplot(aes(y = GIM, x = LOT_AREA)) + geom_point() + 
  geom_smooth(method = 'lm', se = FALSE)+labs(y= "")

FIG_3 <- data%>%ggplot(aes(y = GIM, x = AREA_UNIT)) + geom_point() + 
  geom_smooth(method = 'lm', se = FALSE)+labs(y= "")

(FIG_1 | FIG_2 | FIG_3) 

FIG_1 <- data%>%ggplot(aes(y = CAP_RATE, x = EFFAGE)) + geom_point() + 
  geom_smooth(method = 'lm', se = FALSE)

FIG_2 <- data%>%ggplot(aes(y = CAP_RATE, x = DEPREC)) + geom_point() + 
  geom_smooth(method = 'lm', se = FALSE)+labs(y= "")

FIG_3 <- data%>%ggplot(aes(y = CAP_RATE, x = AREA_UNIT)) + geom_point() + 
  geom_smooth(method = 'lm', se = FALSE)+labs(y= "")

(FIG_1 | FIG_2 | FIG_3) 

FIG_1 <- data%>%ggplot(aes(y = SP_SQFT, x = EFFAGE)) + geom_point() + 
  geom_smooth(method = 'lm', se = FALSE)

FIG_2 <- data%>%ggplot(aes(y = SP_SQFT, x = DEPREC)) + geom_point() + 
  geom_smooth(method = 'lm', se = FALSE)+labs(y= "")

FIG_3 <- data%>%ggplot(aes(y = SP_SQFT, x = LOT_AREA)) + geom_point() + 
  geom_smooth(method = 'lm', se = FALSE)+labs(y= "")

(FIG_1 | FIG_2 | FIG_3) 

### Box plots in the main text
FIG_1 <- data%>%ggplot(aes(y = GIM, x = as.factor(STORY))) + geom_boxplot() + labs(x = "STORY") 

FIG_2 <- data%>%ggplot(aes(y = GIM, x = as.factor(ELEVATOR))) + geom_boxplot() + labs(x = "ELEVATOR") 

FIG_3 <- data%>%ggplot(aes(y = GIM, x = as.factor(NBHD))) + geom_boxplot() + labs(x = "NBHD") 

FIG_4 <- data%>%ggplot(aes(y = GIM, x = as.factor(CONDITN))) + geom_boxplot() + labs(x = "CONDITIONS") 

(FIG_1 | FIG_2) / (FIG_3 | FIG_4) 

### Box plots in the Appendix

FIG_1 <- data%>%ggplot(aes(y = GIM, x = as.factor(UNITS))) + geom_boxplot() + labs(x = "UNITS") 
FIG_2 <- data%>%ggplot(aes(y = GIM, x = as.factor(BACH))) + geom_boxplot() + labs(x = "BACH") 

(FIG_1) / (FIG_2) 


FIG_1 <- data%>%ggplot(aes(y = GIM, x = as.factor(UND_PARK))) + geom_boxplot() + labs(x = "UND_PARK") 

FIG_3 <- data%>%ggplot(aes(y = GIM, x = as.factor(SML_UNIT))) + geom_boxplot() + labs(x = "SML_UNIT") 

FIG_4 <- data%>%ggplot(aes(y = GIM, x = as.factor(HEATTYPE))) + geom_boxplot() + labs(x = "HEATTYPE") 

(FIG_1 | FIG_3 | FIG_4) 

## Correlation between Variables
data_model <- data%>%select(UNITS, EFFAGE, DEPREC, UND_PARK, ELEVATOR, 
                            LOT_AREA, AREA_UNIT, EXP_EGI, NBHD_1, NBHD_3, NBHD_4,
                            CONDITION_1, CONDITION_3, STORY_2, STORY_4, SML_UNIT,GIM,
                            HEATTYPE, BACH)
data_model%>%cor(method = "pearson")%>%View()

## Fitting different models based on Groups 1-4
data_group_1 <- data_model%>% select(UND_PARK, ELEVATOR, LOT_AREA, AREA_UNIT, NBHD_1, NBHD_3, NBHD_4,
                                     STORY_2, STORY_4, HEATTYPE, BACH, UNITS, EFFAGE, GIM)

model_group_1 <- lm(GIM ~ ., data = data_group_1)

data_group_2 <- data_model%>% select(UND_PARK, ELEVATOR, LOT_AREA, AREA_UNIT, NBHD_1, NBHD_3, NBHD_4,
                                     STORY_2, STORY_4, HEATTYPE, BACH, SML_UNIT, CONDITION_1, GIM, CONDITION_3)

model_group_2 <- lm(GIM ~ ., data = data_group_2)

data_group_3 <- data_model%>% select(UND_PARK, ELEVATOR, LOT_AREA, AREA_UNIT, NBHD_1, NBHD_3, GIM, NBHD_4,
                                     STORY_2, STORY_4, HEATTYPE, BACH, SML_UNIT, EFFAGE)

model_group_3 <- lm(GIM ~ ., data = data_group_3)

data_group_4 <- data_model%>% select(UND_PARK, ELEVATOR, LOT_AREA, AREA_UNIT, NBHD_1, NBHD_3, GIM, NBHD_4,
                                     STORY_2, STORY_4, HEATTYPE, BACH, UNITS, CONDITION_1, CONDITION_3)

model_group_4 <- lm(GIM ~ ., data = data_group_4)

get_model_stats <- function(model) {
  vif_values <- vif(model)         # Calculate VIF
  max_vif <- max(vif_values)       # Maximum VIF
  summary_model <- summary(model)
  
  r_squared <- summary_model$r.squared
  adj_r_squared <- summary_model$adj.r.squared
  SEE <- sqrt(mean(summary_model$residuals^2))  # Standard Error of Estimate
  f_stat <- summary_model$fstatistic[1]         # F-statistic
  df1 <- summary_model$fstatistic[2]            # Model df
  df2 <- summary_model$fstatistic[3]            # Residual df
  sig_value <- pf(f_stat, df1, df2, lower.tail = FALSE)  # Significance (p-value)
  
  return(c(r_squared, adj_r_squared, SEE, f_stat, sig_value, max_vif))
}
models <- list(model_group_1, model_group_2, model_group_3, model_group_4)

model_stats <- t(sapply(models, get_model_stats))
model_stats_df <- as.data.frame(model_stats)
colnames(model_stats_df) <- c("R^2", "Adjusted R^2", "SEE", "F", "Sig.", "Max VIF")
rownames(model_stats_df) <- c("Model 1", "Model 2", "Model 3", "Model 4")
model_stats_df

## Based on the results, I am selecting model 3 
## Coefficients for Model 3

model <- model_group_3
model_summary <- summary(model)
coefficients_table <- as.data.frame(model_summary$coefficients)

# Create base table
result_table <- data.frame(
  Variable = rownames(coefficients_table),
  Coefficient = coefficients_table$Estimate,
  Standard_Coefficient = coefficients_table$Estimate / sd(model$model[[1]]),  # Standardized Coefficient
  t_Statistic = coefficients_table$`t value`,
  Significance = coefficients_table$`Pr(>|t|)`
)

# Calculate VIF and Tolerance
vif_values <- vif(model)
tolerance <- 1 / vif_values

# Add VIF and Tolerance to the table
result_table$Tolerance <- c(NA, tolerance)  # NA for intercept
result_table$VIF <- c(NA, vif_values)       # NA for intercept

# View the result table
View(result_table)

anova(model_group_3)


## VIF for one of the variables (UND_PARK) is more than 3.33 
## Therefore, I should remove the parameter and try again. 

model_group_3_m <- lm(GIM ~ .-UND_PARK, data = data_group_3)
model <- model_group_3_m
model_summary <- summary(model)
coefficients_table <- as.data.frame(model_summary$coefficients)

# Create base table
result_table <- data.frame(
  Variable = rownames(coefficients_table),
  Coefficient = coefficients_table$Estimate,
  Standard_Coefficient = coefficients_table$Estimate / sd(model$model[[1]]),  # Standardized Coefficient
  t_Statistic = coefficients_table$`t value`,
  Significance = coefficients_table$`Pr(>|t|)`
)

# Calculate VIF and Tolerance
vif_values <- vif(model)
tolerance <- 1 / vif_values

# Add VIF and Tolerance to the table
result_table$Tolerance <- c(NA, tolerance)  # NA for intercept
result_table$VIF <- c(NA, vif_values)       # NA for intercept

# View the result table
View(result_table)

anova(model)

## All good for now. I proceed to Stepwise method to chose the variables 
## that are statistically significant 

# Stepwise regression using AIC for model selection
data_train <- data_group_3%>%select(-UND_PARK)

full_model <- lm(GIM ~ ., data = data_train)
null_model <- lm(GIM ~ 1, data = data_train)


# Initialize a list to store intermediate models
intermediate_models <- list()

# Custom function to capture intermediate models
capture_models <- function(model, direction) {
  intermediate_models <<- append(intermediate_models, list(model))
}

# Perform stepwise regression with tracing
step_model <- step(full_model, direction = "both", trace = 1, scope = list(lower = null_model, upper = full_model), k = 2, 
                   steps = 1000, keep = capture_models)

# Extract formulas and refit models
model_formulas <- lapply(intermediate_models, formula)
models <- lapply(model_formulas, function(formula) lm(formula, data = data))

# Assign names to the models
names(models) <- paste0("Model", seq_along(models))

model_stats_step <- bind_rows(lapply(models, glance)) %>%
  mutate(Model = names(models)) %>%
  select(Model, r.squared, adj.r.squared, sigma, statistic) %>%
  rename(
    `R Squared` = r.squared,
    `Adjusted R Squared` = adj.r.squared,
    `Std. Error of the Estimate` = sigma,
    `F Statistic` = statistic
  ) %>%
  mutate(R = sqrt(`R Squared`)) %>%
  select(Model, R, `R Squared`, `Adjusted R Squared`, `Std. Error of the Estimate`, `F Statistic`)

View(model_stats_step)

coefficients_steps <- model_stats_step %>%
  mutate(Variables = sapply(models, function(model) {
    terms <- labels(terms(model))  # Extract variable names
    paste(terms, collapse = ", ")  # Combine into a single string
  })) %>%
  select(Model, Variables)

View(coefficients_steps)

## Final Model
final_model <- intermediate_models[[length(intermediate_models)]]

# Get ANOVA table for the final model
anova_table <- anova(final_model)
SSE_total = anova_table$`Sum Sq`
Df_totak = anova_table$Df


data.frame(SSE = c(sum(SSE_total[1:length(SSE_total)-1]),SSE_total[length(SSE_total)], sum(SSE_total)),
           df =  c(sum(Df_totak[1:length(Df_totak)-1]),Df_totak[length(Df_totak)], sum(Df_totak)),
           F_value = c(mean(anova_table$`F value`, na.rm = TRUE), NULL, NULL))

# Get Coefficient

model <- final_model
model_summary <- summary(model)

# Extract coefficients
coefficients_table <- as.data.frame(model_summary$coefficients)
coefficients_table
anova(model)

# Extract Coefficients for all the steps 

# Extract formulas and refit models
model_formulas <- lapply(intermediate_models, formula)
models <- lapply(model_formulas, function(formula) lm(formula, data = data))

# Assign names to the models
names(models) <- paste0("Model", seq_along(models))

# Function to extract coefficients from a model
extract_coefficients <- function(model) {
  coef_table <- as.data.frame(summary(model)$coefficients)
  coef_table$Term <- rownames(coef_table)
  return(coef_table)
}

# Extract coefficients for each model
all_coefficients <- lapply(models, extract_coefficients)

# Combine coefficients into a single data frame
combined_coefficients <- lapply(seq_along(all_coefficients), function(i) {
  coef_df <- all_coefficients[[i]]
  coef_df$Model <- names(models)[i]
  return(coef_df)
}) %>%
  bind_rows() %>%
  select(Model, Term, everything())

# Create a list to store the tables for each model
model_tables <- list()

# Loop through each unique model and create a table
for (model_name in unique(combined_coefficients$Model)) {
  model_data <- combined_coefficients %>%
    filter(Model == model_name) %>%
    select(-Model) # Remove the Model column for each table
  
  model_tables[[model_name]] <- model_data
}

# Print the tables
for (model_name in names(model_tables)) {
  cat("Coefficients for", model_name, ":\n")
  print(model_tables[[model_name]])
  cat("\n") }


model <- final_model
model_summary <- summary(model)
coefficients_table <- as.data.frame(model_summary$coefficients)

# Create base table
result_table <- data.frame(
  Variable = rownames(coefficients_table),
  Coefficient = coefficients_table$Estimate,
  Standard_Coefficient = coefficients_table$Estimate / sd(model$model[[1]]),  # Standardized Coefficient
  t_Statistic = coefficients_table$`t value`,
  Significance = coefficients_table$`Pr(>|t|)`
)

# Calculate VIF and Tolerance
vif_values <- vif(model)
tolerance <- 1 / vif_values

# Add VIF and Tolerance to the table
result_table$Tolerance <- c(NA, tolerance)  # NA for intercept
result_table$VIF <- c(NA, vif_values)       # NA for intercept

# View the result table
View(result_table)

### Prediction
data$GIM_PRED = predict(final_model, data = data)
data$GIM_RATIO = data$GIM_PRED /data$GIM

data$EFF_INC_PRED = data$PRICE / data$GIM_PRED
data$EFF_INC_RATIO = data$EFF_INC_PRED/data$EFF_INC

data$PRICE_PRED = data$EFF_INC * data$GIM_PRED
data$PRICE_RATIO = data$PRICE_PRED/data$PRICE

ratio_stat <- function(data, x){
  x <- enquo(x)
  output <- data%>%summarise(
                   MEAN= mean(!!x),
                   MEDIAN = median(!!x),
                   STD.DEV = sd(!!x),
                   COV = sd(!!x)/mean(!!x),
                   PERCENTILE_10 = quantile(!!x, probs = 0.10),
                   PERCENTILE_25 = quantile(!!x, probs = 0.25),
                   PERCENTILE_75 = quantile(!!x, probs = 0.75),
                   PERCENTILE_90 = quantile(!!x, probs = 0.90))
  return(output)
}

rbind(ratio_stat(data, GIM_RATIO),
      ratio_stat(data, EFF_INC_RATIO),
      ratio_stat(data, PRICE_RATIO))%>%t()

KW_test <- function (data, x, y){
x <- enquo(x)
y <- enquo(y)

x_vals <- eval_tidy(x, data)
y_vals <- eval_tidy(y, data)

test_result <- kruskal.test(x_vals, y_vals)

data.frame(
  title = paste0(quo_name(y)), 
  df =test_result$parameter[['df']],
  p.value = test_result$p.value,
  CH = test_result$statistic[['Kruskal-Wallis chi-squared']]
)
}

rbind(
  KW_test(data, PRICE_RATIO, NBHD),
  KW_test(data, PRICE_RATIO, STORY),
  KW_test(data, PRICE_RATIO, CONDITN),
  KW_test(data, PRICE_RATIO, SML_UNIT),
  KW_test(data, PRICE_RATIO, ELEVATOR),
  KW_test(data, PRICE_RATIO, BACH)
)
