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


data <- read_sav("Data/BUSI 444 Term 2 Case Study 466 Marilu Project 2.sav")

data%<>%mutate(SAR = saleprice/totalval)

SAR_month <- data%>%group_by(month)%>%summarise(SAR = mean(SAR), .groups = 'drop')
SAR_July_1<-(SAR_month[SAR_month['month']==6,'SAR'] + SAR_month[SAR_month['month']==7,'SAR'])$SAR/2
SAR_month%<>%mutate(Factor = SAR_July_1 / SAR)  

data%<>%mutate(
  saleprice_time_adjusted = case_when(
    month == 3 ~ saleprice * 1.013,
    month == 4 ~ saleprice * 0.999,
    month == 5 ~ saleprice * 1.001,
    month == 6 ~ saleprice * 1.009,
    month == 7 ~ saleprice * 0.991,
    month == 8 ~ saleprice * 1.011,
    month == 9 ~ saleprice * 0.977,
    month == 10 ~ saleprice * 0.979
  )
)
  
kruskal.test(data$saleprice_time_adjusted, data$month)

View(SAR_month)

n<-nrow(data)
lower_index <- qbinom(0.05 / 2, n, 0.5)
upper_index <- qbinom(1- 0.05/2, n, 0.5)

landval_per_square <- sales_data$landval / sales_data$lot_size
sizefact <- landsize_per_square/ median(landsize_per_square)


epsilon_values <- seq(0.5, 2, by=0.1)  # Try values between 0.5 and 2
errors <- numeric(length(epsilon_values))

for (i in seq_along(epsilon_values)) {
  e <- epsilon_values[i]
  adjusted_size <- sizefact_eq^(e/2)  # Apply exponent
  errors[i] <- sum((adjusted_size - data$sizefact)^2)  # Replace expected_values with real data
}
best_epsilon <- epsilon_values[which.min(errors)]
print(best_epsilon)



data%>%ggplot(aes(x = lot_size/10000, y = sizefact))+ geom_point()+
  geom_line(aes(x=lot_size/10000, y= sizefact_eq), size = 1, color = 'red')+
  labs(x = 'Lot area in sq feet (divided by 10,000)', y= 'Size Factor',
       title = 'Size Factor Vs. Lot Area')

## Fitting Quadratic, Cubic, and Power models to the data
power_model <- lm(log(sizefact) ~ log(lot_size/10000), data = data)

a <- exp(coef(power_model)[1])  # Intercept
b <- coef(power_model)[2]       # Slope
data <- data %>%
  mutate(adjs_fac = a * ((lot_size/10000)^b),
         lot_size_adjusted = lot_size * adjs_fac)

## Manual Classes 
data %<>%
  mutate(
    linmcls_cp = case_when(
      cp_mancl %in% c(910,911) ~ 0.450,
      cp_mancl %in% c(920,921) ~ 1.000,
      cp_mancl %in% c(930,931) ~ 1.200,
      cp_mancl %in% c(940,941) ~ 1.420,
      TRUE                     ~ 1.000
    ),
    linmcls_gr = case_when(
      gr_mancl %in% c(910,911) ~ 0.450,
      gr_mancl %in% c(920,921) ~ 1.000,
      gr_mancl %in% c(930,931) ~ 1.200,
      gr_mancl %in% c(940,941) ~ 1.420,
      TRUE                     ~ 1.000
    ))

rank_no <- function (data, x) {
  x <- enquo(x)
  output <- rbind(
    data%>%mutate(Rank = rank(!!x), value = corner)%>%group_by(value)%>%
      summarise(count = n(), MeanRank = mean(Rank)),
    data%>%mutate(Rank = rank(!!x), value = culdesac)%>%group_by(value)%>%
      summarise(count = n(), MeanRank = mean(Rank)),
    data%>%mutate(Rank = rank(!!x), value = alley)%>%group_by(value)%>%
      summarise(count = n(), MeanRank = mean(Rank)),
    data%>%mutate(Rank = rank(!!x), value = wooded)%>%group_by(value)%>%
      summarise(count = n(), MeanRank = mean(Rank)),
    data%>%mutate(Rank = rank(!!x), value = pie_shpe)%>%group_by(value)%>%
      summarise(count = n(), MeanRank = mean(Rank)),
    data%>%mutate(Rank = rank(!!x), value = slope)%>%group_by(value)%>%
      summarise(count = n(), MeanRank = mean(Rank)),
    data%>%mutate(Rank = rank(!!x), value = abv_road)%>%group_by(value)%>%
      summarise(count = n(), MeanRank = mean(Rank)))
  return(output)
}

ratio_statistics <- function (data, x, y) {
  x <- enquo(x)
  y <- enquo(y)
  
  output <- round(cbind(data%>%
                          arrange(!!x)%>%
                          group_by(!!y, .drop = FALSE)%>%reframe(
                            Mean = mean(!!x),
                            LB_mean = mean(!!x) - 1.96 * sd(!!x)/sqrt(n()),
                            UB_mean = mean(!!x) + 1.96 * sd(!!x)/sqrt(n()),
                            Median = median(!!x),
                            Min = min(!!x),
                            Max = max(!!x),
                            COD = (mean(abs(!!x-median(!!x)))/median(!!x)) * 100,
                            CV = sd(!!x)/mean(!!x)*100,
                            LB_mode = (!!x)[max(qbinom(0.05 / 2, n(), 0.5),1)], 
                            UB_mode = (!!x)[max(qbinom(1-0.05 / 2, n(), 0.5),1)],
                            Actual_coverage = (pbinom(max(qbinom(1-0.05 / 2, n(), 0.5),1), n(), 0.5) - 
                                                 pbinom(max(qbinom(0.05 / 2, n(), 0.5),1), n(), 0.5))*100)),2)
  return (output)
}
  
### EDA 

data%<>%mutate(flrarea1_adjusted = linmcls * flrarea1,
               flrarea2_adjusted = linmcls * flrarea2,
               area1_2_adjusted = linmcls * area1_2,
               totarea_adjusted = linmcls * totarea,
               bsmtfin_adjusted = linmcls * bsmtfin,
               gar_area_adjusted = linmcls_gr * gar_area,
               cp_area_adjusted = linmcls_cp * cp_area,
               effage = 2017 - mbeffyr)%>%
  mutate(neigh_713 = ifelse(neigh == 713, 1,0),
         neigh_715 = ifelse(neigh == 715, 1,0),
         neigh_731 = ifelse(neigh == 731, 1,0),
         neigh_732 = ifelse(neigh == 732, 1,0),
         neigh_734 = ifelse(neigh == 734, 1,0),
         neigh_736 = ifelse(neigh == 736, 1,0),
         neigh_737 = ifelse(neigh == 737, 1,0))


data_model <- data%>%select(bedrooms, baths, familyrm, fireplcs, flrarea1_adjusted,flrarea2_adjusted, 
                            bsmtfin_adjusted, crwlarea, bsmtarea, unfinbmt, area1_2_adjusted,totarea_adjusted, baseste,
                            heattype, splitlvl,lot_size_adjusted,
                            corner, wooded, pie_shpe, slope, abv_road, culdesac,effage,
                            conduit, alley, sidewalk, gar_area_adjusted, garagefl, cp_area_adjusted, carptfl,
                            saleprice_time_adjusted, neigh_713, neigh_715, neigh_731, neigh_732, neigh_734, neigh_736, neigh_737)

View(cor(data_model))

data%>%group_by(neigh)%>%summarise(N = n(),
                                   Mean = mean(saleprice_time_adjusted),
                                   std.Deviation = sd(saleprice_time_adjusted),
                                   .groups = 'drop')

scatter_plots <- function (data, x, y_label){
  x <- enquo(x)
  fig <- data%>%ggplot(aes(x= !!x, y= saleprice_time_adjusted/1000))+geom_point()+
    geom_smooth(method = 'lm', se = FALSE)
  
  if (y_label) {
    fig <- fig + labs(y = "Time Adjusted Sale Price ($K)")
  } else {
    fig <- fig + labs(y = "")
  }
  
  formula <- new_formula(
    lhs = quote(saleprice_time_adjusted),
    rhs = get_expr(x),
    env = caller_env()
  )
  
  r_squared <- summary(lm(formula, data = data))$r.squared
  return (list(figure = fig, r_sq = r_squared))
}

box_plots <- function (data, x, y_label){
  x <- enquo(x)
  
  data %<>%mutate(!!x := as.factor(!!x))
  
  fig <- data%>%
    ggplot(aes(x= !!x, y= saleprice_time_adjusted/1000))+geom_boxplot()
  
  
  if (y_label) {
    fig <- fig + labs(y = "Time Adjusted Sale Price ($K)")
  } else {
    fig <- fig + labs(y = "")
  }
  
  return (list(figure = fig))
}


### Descriptive Analysis

descriptive_analyzis <- sapply(X= names(data_model), FUN = function (x) {
  
  return(list(              Min = min(data_model[[x]], na.rm = TRUE),
                            Max = max(data_model[[x]], na.rm = TRUE),
                            Mean = mean(data_model[[x]], na.rm = TRUE),
                            Median = median (data_model[[x]], na.rm = TRUE)
  ))
  
})

View(as.data.frame(descriptive_analyzis)%>%t%>%as.data.frame()%>%filter(Max > 10))

Descriptive_cols <- as.data.frame(descriptive_analyzis)%>%t%>%as.data.frame()%>%filter(Max <= 10)


frequency_table <- lapply(row.names(Descriptive_cols), function(x) {
  # Summarise the count for each group in the column
  result <- data_model %>%
    group_by(!!sym(x)) %>%
    summarise(count = n(), .groups = 'drop')
} )


## Fixing heattype 

data%<>%mutate(heattype = case_when(
  heattype == 1 ~ 2,
  heattype == 5 ~ 6,
  TRUE ~ heattype
))

## Effective Age
effective_year_plot <- scatter_plots(data, effage, TRUE)$figure
effective_year_r_sq <- scatter_plots(data, effage, TRUE)$r_sq



alley_plot <- box_plots(data, alley, TRUE)$figure 

## Crawl Area
crawl__plot <- scatter_plots(data%>%filter(crwlarea>0), crwlarea, TRUE)$figure
crawl_r_sq <- scatter_plots(data%>%filter(crwlarea>0), crwlarea, TRUE)$r_sq

## Side Walk

Side_walk <- box_plots(data, sidewalk, FALSE)$figure

## Basement Presence
Bastement_presence <- box_plots(data, baseste, FALSE)$figure

## Above Road

Above_rd <- box_plots(data, abv_road, TRUE)$figure

## Slope

Slope <- box_plots(data, slope, FALSE)$figure


## Wooden

Wooded <-  box_plots(data, wooded, FALSE)$figure

## Corner

Corner <- box_plots(data, corner, TRUE)$figure



## Heattype

Heat_type <- box_plots(data, heattype, TRUE)$figure
## Carport Presence

Carport_presence <- box_plots(data, carptfl, FALSE)$figure
## Crawl Area

Crawl_area__plot <- scatter_plots(data%>%filter(crwlarea>0), crwlarea, FALSE)$figure
Crawl_area_r_sq <- scatter_plots(data%>%filter(crwlarea>0), crwlarea, TRUE)$r_sq


## Carport Area

Carport_area__plot <- scatter_plots(data%>%filter(cp_area_adjusted>0), cp_area_adjusted, FALSE)$figure
Carport_area_r_sq <- scatter_plots(data%>%filter(cp_area_adjusted>0), cp_area_adjusted, TRUE)$r_sq



## Unfinished Basement 

unfinished_area_plot <- scatter_plots(data%>%filter(unfinbmt>0), unfinbmt, FALSE)$figure
unfinished_area_r_sq <- scatter_plots(data%>%filter(unfinbmt>0), unfinbmt, TRUE)$r_sq


## Pie_shape

Pie_shape <- box_plots(data, pie_shpe, FALSE)$figure


## Garage Area


gar_area_plot <- scatter_plots(data%>%filter(gar_area_adjusted>0), gar_area_adjusted, TRUE)$figure
gar_area_r_sq <- scatter_plots(data%>%filter(gar_area_adjusted>0), gar_area_adjusted, TRUE)$r_sq

## Floor Area 2

floor_2_plot <- scatter_plots(data%>%filter(flrarea2_adjusted>0), flrarea2_adjusted, FALSE)$figure
floor_2_r_sq <- scatter_plots(data%>%filter(flrarea2_adjusted>0), flrarea2_adjusted, TRUE)$r_sq


## Garage Presence

Garage_presence <- box_plots(data, garagefl, FALSE)$figure

## Split-level Presence

Split_presence <- box_plots(data, splitlvl, FALSE)$figure
## Culdesac Presence

Culdesac_presence <- box_plots(data, culdesac, TRUE)$figure
## Bedrooms

Bedrooms <- box_plots(data, bedrooms, FALSE)$figure
## Lot Size

lot_plot <- scatter_plots(data%>%filter(lot_size_adjusted>0), lot_size_adjusted, TRUE)$figure
lot_r_sq <- scatter_plots(data%>%filter(lot_size_adjusted>0), lot_size_adjusted, TRUE)$r_sq

## FamilyRoom


Family_room <- box_plots(data, familyrm, FALSE)$figure

## Basement Finished

Basement_plot <- scatter_plots(data%>%filter(bsmtfin_adjusted>0), bsmtfin_adjusted, FALSE)$figure
Basement_r_sq <- scatter_plots(data%>%filter(bsmtfin_adjusted>0), bsmtfin_adjusted, FALSE)$r_sq


## Fireplcs

Fireplcs <- box_plots(data, fireplcs, TRUE)$figure

## Conduits

Conduits <- box_plots(data, conduit, FALSE)$figure


## Basement Area

Basement_area_plot <- scatter_plots(data%>%filter(bsmtarea>0), bsmtarea, FALSE)$figure
Basement_area_r_sq <- scatter_plots(data%>%filter(bsmtarea>0), bsmtarea, TRUE)$r_sq


## Baths

Bath_plot <- box_plots(data, baths , TRUE)$figure
## Total Area

Total_area_plot <- scatter_plots(data%>%filter(totarea_adjusted>0), totarea_adjusted, TRUE)$figure
Total_area_r_sq <- scatter_plots(data%>%filter(totarea_adjusted>0), totarea_adjusted, TRUE)$r_sq

## Area_1_2

Area_1_2_plot <- scatter_plots(data%>%filter(area1_2_adjusted>0), area1_2_adjusted, FALSE)$figure
Area_1_2_r_sq <- scatter_plots(data%>%filter(area1_2_adjusted>0), area1_2_adjusted, TRUE)$r_sq


## Floor Area_1

Floor_1_plot <- scatter_plots(data%>%filter(flrarea1_adjusted>0), flrarea1_adjusted, FALSE)$figure
Floor_1_r_sq <- scatter_plots(data%>%filter(flrarea1_adjusted>0), flrarea1_adjusted, TRUE)$r_sq


## Green

(Total_area_plot | Area_1_2_plot | Floor_1_plot) / 
  (Bath_plot | Basement_area_plot | effective_year_plot)

Total_area_r_sq
Area_1_2_r_sq
Floor_1_r_sq
Basement_area_r_sq
effective_year_r_sq

box_plots(data, neigh, TRUE)$figure


## Yellow

(Culdesac_presence | Bedrooms | Family_room)/
  (Fireplcs | Conduits |Pie_shape)



(lot_plot | Basement_plot | unfinished_area_plot)/
  (gar_area_plot | floor_2_plot | Garage_presence)

lot_r_sq  
Basement_r_sq
unfinished_area_r_sq
gar_area_r_sq
floor_2_r_sq

Basement_r_sq
crawl_r_sq

Corner | Wooded | Pie_shape
## White


(Heat_type | Carport_presence | Crawl_area__plot)/
  (alley_plot | Split_presence |Bastement_presence)/
  (Above_rd | Slope | Side_walk)

Carport_area_r_sq
Crawl_area_r_sq

## Data Model
data_model <- data%>%select(bedrooms, baths, familyrm, fireplcs, flrarea1_adjusted,flrarea2_adjusted, 
                            bsmtfin_adjusted, crwlarea, bsmtarea, unfinbmt, area1_2_adjusted,totarea_adjusted, baseste,
                            heattype, splitlvl,lot_size_adjusted,
                            corner, wooded, pie_shpe, slope, abv_road, culdesac,effage,
                            conduit, alley, sidewalk, gar_area_adjusted, garagefl, cp_area_adjusted, carptfl,
                            neigh,
                            saleprice_time_adjusted)

data_group_1 <- data_model%>% select(familyrm, flrarea2_adjusted, lot_size_adjusted,
                                     fireplcs, baseste, pie_shpe,
                                     heattype, effage, culdesac,
                                     splitlvl, sidewalk, garagefl,
                                     saleprice_time_adjusted, 
                                     flrarea1_adjusted, bsmtfin_adjusted, crwlarea, alley,
                                     bedrooms, baths)

model_group_1 <- lm(saleprice_time_adjusted ~ ., data = data_group_1)

data_group_2 <- data_model%>% select(familyrm, flrarea2_adjusted, lot_size_adjusted,
                                     fireplcs, baseste, pie_shpe,
                                     heattype, effage, culdesac,
                                     splitlvl, sidewalk, garagefl,
                                     saleprice_time_adjusted, 
                                     bsmtarea, bsmtfin_adjusted, conduit,
                                     bedrooms, baths)

model_group_2 <- lm(saleprice_time_adjusted ~ ., data = data_group_2)


data_group_3 <- data_model%>% select(familyrm, flrarea2_adjusted, lot_size_adjusted,
                                     fireplcs, baseste, pie_shpe,
                                     heattype, effage, culdesac,
                                     splitlvl, sidewalk, garagefl,
                                     saleprice_time_adjusted, 
                                     totarea_adjusted,	crwlarea,	conduit,
                                     bedrooms, baths)
model_group_3 <- lm(saleprice_time_adjusted ~ ., data = data_group_3)



data_group_4 <- data_model%>% select(familyrm, flrarea2_adjusted, lot_size_adjusted,
                                     fireplcs, baseste, pie_shpe,
                                     heattype, effage, culdesac,
                                     splitlvl, sidewalk, garagefl,
                                     saleprice_time_adjusted,
                                     area1_2_adjusted, bsmtfin_adjusted	, crwlarea	,conduit,
                                     bedrooms, baths)



model_group_4 <- lm(saleprice_time_adjusted ~ ., data = data_group_4)




model_names <- paste0("model_group_", 1:4)
adjusted_r2 <- c()
mean_sse <- c()
f_stat <- c()
p_value <- c()

# Loop through each model and extract metrics
for (i in 1:4) {
  # Get the model dynamically
  model <- get(model_names[i])
  model_summary <- summary(model)
  
  # Extract residual sum of squares (SSE)
  sse <- sum(model_summary$residuals^2)
  
  # Residual degrees of freedom (df)
  df_residual <- model_summary$df[2]
  
  # Calculate Mean SSE (MSE)
  mean_sse[i] <- sse / df_residual
  
  # Extract other metrics
  adjusted_r2[i] <- model_summary$adj.r.squared
  f_stat[i] <- model_summary$fstatistic[1]
  p_value[i] <- pf(model_summary$fstatistic[1], 
                   model_summary$fstatistic[2], 
                   model_summary$fstatistic[3], 
                   lower.tail = FALSE)
}

# Create the final data frame
results_table <- data.frame(
  Group = model_names,
  Adjusted_R2 = adjusted_r2,
  Mean_SSE = mean_sse,
  F_Statistic = f_stat,
  Significance = p_value
)

print(results_table)


model <- model_group_4
model_summary <- summary(model)

# Extract coefficients
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



## MODEL/TEST Split 

data_model <- cbind(data_group_4, random = data$random, neigh = data$neigh)

data_train <- data_model%>%arrange(random)%>%filter(row_number() <= 320)%>%select(-random)
data_test <- data_model%>%arrange(random)%>%filter(row_number() > 320)%>%select(-random)

## Model 


# Stepwise regression using AIC for model selection
full_model <- lm(saleprice_time_adjusted ~ ., data = data_train%>%select(-neigh))
null_model <- lm(saleprice_time_adjusted ~ 1, data = data_train%>%select(-neigh))


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

# Create base table
result_table <- data.frame(
  Variable = rownames(coefficients_table),
  Coefficient = coefficients_table$Estimate,
  Standard_Coefficient = coefficients_table$Estimate / sd(model$model[[1]]),  # Standardized Coefficient
  t_Statistic = coefficients_table$`t value`,
  Significance = coefficients_table$`Pr(>|t|)`
)

vif_values <- vif(model)
tolerance <- 1 / vif_values

# Add VIF and Tolerance to the table
result_table$Tolerance <- c(NA, tolerance)  # NA for intercept
result_table$VIF <- c(NA, vif_values)       # NA for intercept

# View the result table
View(result_table)

# Casewwise Diagnosis 
model <- final_model

# Calculate standardized residuals
std_residuals <- rstandard(model)

# Get the response variable (y) and predicted values
response_variable <- model$model$saleprice_time_adjusted  # Actual response variable
predicted_values <- fitted(model)   # Predicted values from the model
residuals <- residuals(model)       # Residuals (actual - predicted)

# Create a data frame with casewise diagnostics
diagnostics_df <- data.frame(
  Case = 1:length(std_residuals),  # Case number
  Std_Residuals = std_residuals,   # Standardized residuals
  Response_Variable = response_variable,  # Actual response variable
  Predicted_Values = predicted_values,    # Predicted values
  Residuals = residuals                   # Residuals
)

# Filter cases with standardized residuals > 3 or < -3
diagnosed_cases <- diagnostics_df %>%
  filter(Std_Residuals > 3 | Std_Residuals < -3)

# Display the diagnosed cases
print(diagnosed_cases)

# Residuals 
predicted_values <- fitted(model)  # Predicted values
residuals <- residuals(model)      # Residuals
std_predicted <- scale(predicted_values)  # Standardized predicted values
std_residuals <- rstandard(model)  # Standardized residuals

# Create a data frame with the required metrics
metrics_table <- data.frame(
  Metric = c("Predicted Values", "Residuals", "Std. Predicted Values", "Std. Residuals"),
  Min = c(min(predicted_values), min(residuals), min(std_predicted), min(std_residuals)),
  Max = c(max(predicted_values), max(residuals), max(std_predicted), max(std_residuals)),
  Mean = c(mean(predicted_values), mean(residuals), mean(std_predicted), mean(std_residuals)),
  Std_Deviation = c(sd(predicted_values), sd(residuals), sd(std_predicted), sd(std_residuals)),
  Count = c(length(predicted_values), length(residuals), length(std_predicted), length(std_residuals))
)

# Rename columns for better readability
colnames(metrics_table) <- c("Metric", "Min", "Max", "Mean", "Std. Deviation", "Count")

# Display the table
print(metrics_table)

model_summary <- summary(model)
r_value <- sqrt(model_summary$r.squared)
r_squared <- model_summary$r.squared
adj_r_squared <- model_summary$adj.r.squared
std_error <- model_summary$sigma

metrics_table_2 <- data.frame(
  Metric = c("R", "R-squared", "Adjusted R-squared", "Std. Error of the Estimate"),
  Value = c(r_value, r_squared, adj_r_squared, std_error)
)

# Display both tables
print("Table 1: Predicted Values, Residuals, Std. Predicted, Std. Residuals")
print(t(metrics_table_2))

data_train <- data%>%arrange(random)%>%filter(row_number() <= 320)


data_train$Prediction <- predict(model, newdata = data_train)
data_train%<>%mutate(ASR = Prediction/saleprice_time_adjusted)

data_test$Prediction <- predict(model, newdata = data_test)
data_test%<>%mutate(ASR = Prediction/saleprice_time_adjusted)

data$Prediction <- predict(model, newdata = data)
data%<>%mutate(ASR = Prediction/saleprice_time_adjusted)

data_train%>%mutate(Rank = rank(ASR), value = neigh)%>%group_by(value)%>%
  summarise(count = n(), MeanRank = mean(Rank))

## Neighborhood Adjustment 
kruskal.test(data_train$ASR, data_train$neigh)

ratio_statistics(data_train, ASR, neigh)

data_train%<>%mutate(Prediction_neigh = case_when(
  neigh == 715 ~ Prediction / 1.08,
  neigh == 731 ~ Prediction / 0.83,
  neigh == 734 ~ Prediction / 1.31,
  TRUE ~ Prediction),
  ASR_neigh = Prediction_neigh/saleprice_time_adjusted)

data%<>%mutate(Prediction_neigh = case_when(
  neigh == 715 ~ Prediction / 1.08,
  neigh == 731 ~ Prediction / 0.83,
  neigh == 734 ~ Prediction / 1.31,
  TRUE ~ Prediction),
  ASR_neigh = Prediction_neigh/saleprice_time_adjusted)

kruskal.test(data_train$ASR_neigh, data_train$neigh)
ratio_statistics(data_train, ASR_neigh, neigh)

## Manual Class Adjustment
kruskal.test(data_train$ASR_neigh, data_train$mbmancls)

ratio_statistics(data_train, ASR_neigh, mbmancls)


data_train%<>%mutate(Prediction_neigh_mclass = case_when(
  mbmancls == 40 ~ Prediction_neigh / 0.92 ,
  mbmancls == 51 ~ Prediction_neigh / 0.85 ,
  mbmancls == 142 ~ Prediction_neigh / 0.91 ,
  TRUE ~ Prediction_neigh),
  ASR_neigh_mclass = Prediction_neigh_mclass/saleprice_time_adjusted)

data%<>%mutate(Prediction_neigh_mclass = case_when(
  mbmancls == 40 ~ Prediction_neigh / 0.92 ,
  mbmancls == 51 ~ Prediction_neigh / 0.85 ,
  mbmancls == 142 ~ Prediction_neigh / 0.91 ,
  TRUE ~ Prediction_neigh),
  ASR_neigh_mclass = Prediction_neigh_mclass/saleprice_time_adjusted)

kruskal.test(data_train$ASR_neigh_mclass, data_train$mbmancls)
ratio_statistics(data_train, ASR_neigh_mclass, mbmancls)

## Bedroom

kruskal.test(data_train$ASR_neigh_mclass, data_train$bedrooms)
ratio_statistics(data_train, ASR_neigh_mclass, bedrooms)

## Heattyoe

kruskal.test(data_train$ASR_neigh_mclass, data_train$heattype)
ratio_statistics(data_train, ASR_neigh_mclass, heattype)

## Culdesac

kruskal.test(data_train$ASR_neigh_mclass, data_train$culdesac)
ratio_statistics(data_train, ASR_neigh_mclass, culdesac)

## Alley

kruskal.test(data_train$ASR_neigh_mclass, data_train$alley)
ratio_statistics(data_train, ASR_neigh_mclass, alley)


## Entire Dataset
ratio_statistics(data, ASR_neigh_mclass)


## Modeling ASR
data_asr <- data%>%select(colnames(data_group_4), ASR_neigh_mclass)%>%
  select(-saleprice_time_adjusted)

asr_model <- lm(ASR_neigh_mclass ~ .,data = data_asr)

model <- asr_model
model_summary <- summary(model)

# Extract coefficients
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


## conduit

kruskal.test(data$ASR_neigh_mclass, data$conduit)
ratio_statistics(data, ASR_neigh_mclass, conduit)

## sidewalk

kruskal.test(data$ASR_neigh_mclass, data$sidewalk)
ratio_statistics(data, ASR_neigh_mclass, sidewalk)

data%<>%mutate(Prediction_neigh_mclass_sidewalk = case_when(
  sidewalk  == 1 ~ Prediction_neigh_mclass / 1.06 ,
  TRUE ~ Prediction_neigh_mclass),
  ASR_neigh_mclass_sidewalk = Prediction_neigh_mclass_sidewalk/saleprice_time_adjusted)

kruskal.test(data$ASR_neigh_mclass_sidewalk, data$sidewalk)
ratio_statistics(data, ASR_neigh_mclass_sidewalk, sidewalk)


## effage and flrarea2_adjusted

effage_plot_2 <- data%>%ggplot(aes(x = effage, y = ASR_neigh_mclass_sidewalk)) + 
  geom_point()+ geom_smooth(method = "lm", se = FALSE)+
  labs(y = "Adjusted ASR")

summary(lm(ASR_neigh_mclass_sidewalk ~ effage, data = data))$r.squared

flrarea2_plot<- data%>%ggplot(aes(x = flrarea2_adjusted, y = ASR_neigh_mclass_sidewalk)) + 
  geom_point()+ geom_smooth(method = "lm", se = FALSE)+
  labs(y = "Adjusted ASR")

summary(lm(ASR_neigh_mclass_sidewalk ~ flrarea2_adjusted, data = data))$r.squared

effage_plot_2 | flrarea2_plot

### Final check with neighborhood 

kruskal.test(data_train$ASR_neigh_mclass, data_train$neigh)
