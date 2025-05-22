library(tidyverse)
library(readr)
library(haven)
library(magrittr)
library(car)
library(patchwork)
rm(list = ls())

data <- read_sav("Data/BUSI 444 Term 1 Case Study 446 Marilu Project 1.sav")

data%<>%mutate(SAR = saleprice/totalval)

n<-nrow(data)
lower_index <- qbinom(0.05 / 2, n, 0.5)
upper_index <- qbinom(1- 0.05/2, n, 0.5)


## Find ratio statistics for SAR
round(cbind(data%>%summarise(Mean = mean(SAR),
                 LB_mean = mean(SAR) - 1.96 * sd(SAR)/sqrt(n()),
                 UB_mean = mean(SAR) + 1.96 * sd(SAR)/sqrt(n()),
                 Median = median(SAR),
                 Min = min(SAR),
                 Max = max(SAR),
                 COD = (mean(abs(SAR-median(SAR)))/median(SAR)) * 100,
                 CV = sd(SAR)/mean(SAR)*100),
      data.frame(LB_mode = sort(data$SAR)[lower_index], 
                 UB_mode = sort(data$SAR)[upper_index],
                 Actual_coverage = (pbinom(upper_index, n, 0.5) - pbinom(lower_index, n, 0.5))*100)),2)

data%>%ggplot(aes(x = month, y= SAR))+geom_point()+geom_smooth(method = "lm", formula = y ~ x)+
  labs(title = "SAR variation with sale month")

lm_model <- lm(SAR ~ month, data = data)
r_squared <- summary(lm_model)$r.squared

## Performing Kruskal-Walli Test
data%>%mutate(SAR_rank=rank(SAR))%>%group_by(month)%>%summarise(mean_rank = mean(SAR_rank), n(), .groups = 'drop')

kruskal.test(data$SAR ~ as.factor(data$month))

## Calculating Land Residual value
lan_model <- lm(saleprice ~ landval + imprval-1, data = data)
summary(lan_model)

## Calculating time adjusted improved price
data%<>%mutate(adjimprv = 1.04 * imprval)


## Calculating land residual
data%<>%mutate(landresid = saleprice - adjimprv)

data%>%ggplot(aes(x = lot_size, y = landresid))+geom_point()+
  labs(x = 'Lot area in sq feet', y= 'land residuals',
       title = 'Land Residual Vs. Lot Area')


data%>%ggplot(aes(x = lot_size, y = landresid/lot_size))+geom_point()+
  labs(x = 'Lot area in sq feet', y= 'land residuals per sqaure feet',
       title = 'Land Residual per Square Feet Vs. Lot Area')

data%<>%mutate(landsize_per_square = landresid/lot_size)

## Find ratio statistics for landsize_per_square
round(cbind(data%>%summarise(Mean = mean(landsize_per_square),
                             LB_mean = mean(landsize_per_square) - 1.96 * sd(landsize_per_square)/sqrt(n()),
                             UB_mean = mean(landsize_per_square) + 1.96 * sd(landsize_per_square)/sqrt(n()),
                             Median = median(landsize_per_square),
                             Min = min(landsize_per_square),
                             Max = max(landsize_per_square),
                             COD = (mean(abs(landsize_per_square-median(landsize_per_square)))/median(landsize_per_square)) * 100,
                             CV = sd(landsize_per_square)/mean(landsize_per_square)*100),
            data.frame(LB_mode = sort(data$landsize_per_square)[lower_index], 
                       UB_mode = sort(data$landsize_per_square)[upper_index],
                       Actual_coverage = (pbinom(upper_index, n, 0.5) - pbinom(lower_index, n, 0.5))*100)),2)

## median for landsize_per_square = 7.83
data%<>% mutate(sizefact = landsize_per_square/median(landsize_per_square))
data%>%ggplot(aes(x = lot_size/10000, y = sizefact))+ geom_point()+
  labs(x = 'Lot area in sq feet (divided by 10,000)', y= 'Size Factor',
       title = 'Size Factor Vs. Lot Area')

## Fitting Quadratic, Cubic, and Power models to the data
power_model <- lm(log(sizefact) ~ log(lot_size/10000), data = data)
a <- exp(coef(power_model)[1])  # Intercept
b <- coef(power_model)[2]       # Slope
data <- data %>%
  mutate(adjsfactpow = a * ((lot_size/10000)^b))


data%>%ggplot(aes(x = lot_size/10000))+ geom_point(aes(y = sizefact))+
  labs(x = 'Lot area in sq feet (divided by 10,000)', y= 'Size Factor',
       title = 'Size Factor Vs. Lot Area')+
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, 
              aes(y = sizefact, color = "Linear Model"), size = 1)+
  geom_smooth(method = "lm", formula = y ~ poly(x,2), se = FALSE, 
              aes(y = sizefact, color = "Quadratic Model"), size = 1)+
  geom_smooth(method = "lm", formula = y ~ poly(x,3), se = FALSE, 
              aes(y = sizefact, color = "Cubic Model"), size = 1)+
  geom_line(aes(y = adjsfactpow, color = "Power Law"), size = 1)


quadratic_model <- lm(sizefact ~ poly(lot_size/10000,2), data = data)
cubic_model <- lm(sizefact ~ poly(lot_size/10000,3), data = data)
linear_model <- lm(sizefact ~ lot_size, data = data)

summary(linear_model)
summary(quadratic_model)
summary(cubic_model)
summary(power_model)

## Displaying power model prediction for size factor and lot area
data%>%ggplot(aes(x = lot_size/10000, y = adjsfactpow))+ geom_line(size = 1, color = 'darkred')+
  labs (x = 'Lot area in sq feet (divided by 10,000)',
        y = 'Power Model Prediction',
        title = 'Size Factor Estimation by Power Model')

data%<>%mutate(lot_size_adjusted = lot_size * adjsfactpow)

data%<>%mutate(genlandpow = adjsfactpow * lot_size * median(landsize_per_square))

data%>%ggplot(aes(x = lot_size, y = genlandpow))+geom_point()+labs(x = "Lot area in sq feet")

data%<>%mutate(genlanpow_ratio = landresid/genlandpow)


### Chi-squared Test to compare genlandpow

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


man_whiteney <- function (data , x){
  x <- enquo(x)
  output <- 
    data.frame("corner" = wilcox.test(data%>%filter(corner==1)%>%pull(!!x), 
                                      data%>%filter(corner==0)%>%pull(!!x))$p.value[1],
               "culdesac" = wilcox.test(data%>%filter(culdesac==1)%>%pull(!!x), 
                                        data%>%filter(culdesac==0)%>%pull(!!x))$p.value[1],
               "alley" = wilcox.test(data%>%filter(alley==1)%>%pull(!!x), 
                                     data%>%filter(alley==0)%>%pull(!!x))$p.value[1],
               "wooded" = wilcox.test(data%>%filter(wooded==1)%>%pull(!!x), 
                                      data%>%filter(wooded==0)%>%pull(!!x))$p.value[1],
               "pie_shpe" = wilcox.test(data%>%filter(pie_shpe==1)%>%pull(!!x), 
                                        data%>%filter(pie_shpe==0)%>%pull(!!x))$p.value[1],
               "slope" = wilcox.test(data%>%filter(slope==1)%>%pull(!!x), 
                                     data%>%filter(slope==0)%>%pull(!!x))$p.value[1],
               "abv_road" = wilcox.test(data%>%filter(abv_road==1)%>%pull(!!x), 
                                        data%>%filter(abv_road==0)%>%pull(!!x))$p.value[1])%>%t()
  
  return(output)
  
}

### Culdesac seems to have correlation with other covariates and 
### so it is better to adjust gelanpow based on that
ratio_statistics <- function (data, x, y) {
  x <- enquo(x)
  y <- enquo(y)
  
output <- round(cbind(data%>%
                                arrange(!!x)%>%
                             group_by(!!y)%>%summarise(
                             Mean = mean(!!x),
                             LB_mean = mean(!!x) - 1.96 * sd(!!x)/sqrt(n()),
                             UB_mean = mean(!!x) + 1.96 * sd(!!x)/sqrt(n()),
                             Median = median(!!x),
                             Min = min(!!x),
                             Max = max(!!x),
                             COD = (mean(abs(!!x-median(!!x)))/median(!!x)) * 100,
                             CV = sd(!!x)/mean(!!x)*100,
                             LB_mode = (!!x)[qbinom(0.05 / 2, n(), 0.5)], 
                             UB_mode = (!!x)[qbinom(1-0.05 / 2, n(), 0.5)],
                       Actual_coverage = (pbinom(qbinom(1-0.05 / 2, n(), 0.5), n(), 0.5) - 
                                            pbinom(qbinom(0.05 / 2, n(), 0.5), n(), 0.5))*100)),2)
return (output)
}
## The results show that the lots with uledac needs to have 20% more genlanpow

rank_no(data, genlanpow_ratio)
man_whiteney(data, genlanpow_ratio)
ratio_statistics(data%>%filter(culdesac == 0), genlanpow_ratio, alley)


data%<>%mutate(genlandpow_culde_adjusted = 
                 case_when(culdesac == 1 ~ 1.2 * genlandpow,
                           culdesac == 0 ~ 1.01 *genlandpow))%>%
        mutate(genlanpow_ratio_culde_adjusted = landresid/genlandpow_culde_adjusted)


rank_no(data, genlanpow_ratio_culde_adjusted)

man_whiteney(data, genlanpow_ratio_culde_adjusted)

ratio_statistics(data, genlanpow_ratio_culde_adjusted, culdesac)

ratio_statistics(data, genlanpow_ratio_culde_adjusted, alley)


data%<>%mutate(genlandpow_culde_alley_adjusted = 
                 case_when(alley == 0 ~ 1.07 * genlandpow_culde_adjusted,
                           alley == 1 ~ 0.91 *genlandpow_culde_adjusted))%>%
  mutate(genlanpow_ratio_culde_alley_adjusted = landresid/genlandpow_culde_alley_adjusted)


rank_no(data, genlanpow_ratio_culde_alley_adjusted)

man_whiteney(data, genlanpow_ratio_culde_alley_adjusted)

ratio_statistics(data, genlanpow_ratio_culde_alley_adjusted, abv_road)


data%<>%mutate(genlandpow_culde_alley_road_adjusted = 
                 case_when(abv_road == 0 ~ 1.00 * genlandpow_culde_alley_adjusted,
                           abv_road == 1 ~ 0.94 *genlandpow_culde_alley_adjusted))%>%
  mutate(genlanpow_ratio_culde_alley_road_adjusted = landresid/genlandpow_culde_alley_road_adjusted)


man_whiteney(data, genlanpow_ratio_culde_alley_road_adjusted)

ratio_statistics(data, genlanpow_ratio_culde_alley_road_adjusted, culdesac)

data%<>%mutate(genlandpow_culde_alley_road_culde_adjusted = 
                 case_when(culdesac == 0 ~ 1.01 * genlandpow_culde_alley_road_adjusted,
                           culdesac == 1 ~ 0.96 *genlandpow_culde_alley_road_adjusted))%>%
  mutate(genlanpow_ratio_culde_alley_road_culde_adjusted = landresid/genlandpow_culde_alley_road_culde_adjusted)

rank_no(data, genlanpow_ratio_culde_alley_road_culde_adjusted)

man_whiteney(data, genlanpow_ratio_culde_alley_road_culde_adjusted)

ratio_statistics(data, genlanpow_ratio_culde_alley_road_culde_adjusted, neigh)

data%<>%mutate(genlandpow_culde_alley_road_culde_neigh_adjusted = 
                 case_when(neigh == 713 ~ 0.94 * genlandpow_culde_alley_road_culde_adjusted,
                           neigh == 715 ~ 0.86 * genlandpow_culde_alley_road_culde_adjusted,
                           neigh == 716 ~ 1.1 * genlandpow_culde_alley_road_culde_adjusted,
                           neigh == 732 ~ 1.15 * genlandpow_culde_alley_road_culde_adjusted,
                           neigh == 734 ~ 0.78 * genlandpow_culde_alley_road_culde_adjusted,
                           neigh == 736 ~ 0.8 * genlandpow_culde_alley_road_culde_adjusted,
                           TRUE ~ genlandpow_culde_alley_road_culde_adjusted))%>%
  mutate(genlanpow_ratio_culde_alley_road_culde_neigh_adjusted = landresid/genlandpow_culde_alley_road_culde_neigh_adjusted)

ratio_statistics(data, genlanpow_ratio_culde_alley_road_culde_neigh_adjusted, neigh)

data%<>%rename(genlanpow_ratio_final = genlanpow_ratio_culde_alley_road_culde_neigh_adjusted,
               genlandpow_final = genlandpow_culde_alley_road_culde_neigh_adjusted)

data%>%ggplot(aes(x = genlanpow_ratio_final)) + geom_histogram(fill = 'lightblue',color = 'orange')+
  labs(y = 'Frequency', x = 'Land Residual Ratio')


data%<>%mutate(imprrsid = saleprice - genlandpow_final,
               obsdeprn = 1- (imprrsid/(mb_rcn + cp_rcn + gr_rcn + ob1mktval)),
               effage = 2017 - mbeffyr)

data%>%ggplot(aes(x = effage, y = obsdeprn))+geom_point()+
  geom_smooth(method = "lm", formula = y ~ poly(x,2), se = FALSE, 
              aes(x = effage, y = obsdeprn, color = "Quadratic"), size = 1.5)+
  geom_smooth(method = "lm", formula = y ~ poly(x,3), se = FALSE, 
              aes(x = effage, y = obsdeprn, color = "Cubic"), size = 1.5)+
  labs ( color = "")

data%<>%filter(obsdeprn >= 0 )

quadratic_model <- lm(obsdeprn ~ poly(effage,2), data = data)
cubic_model <- lm(obsdeprn ~ poly(effage,3), data = data)

summary(quadratic_model)
summary(cubic_model)

data%<>%mutate(effage2 = pmin(effage, 40))

new_data <- data.frame(effage = data$effage2)

data%<>%left_join(data.frame(effage = data$effage,
                             pctgood = 1- predict(quadratic_model, newdata = new_data))%>%distinct(), by = 'effage')

data%>%ggplot(aes(x = effage, y = pctgood))+geom_point()


##########################
## New Construction Data
##########################


#--- 1. Identify basement type and # of storeys ---
#    Assumes BMST_AREA > 0 means “full basement,”
#    CRWLAREA > 0 means “crawl,”
#    FLRAREA2 > 0 means a 2‐storey house.
#    Adjust as needed for your data.

data %<>%
  mutate(
    basement_type = case_when(
      bsmtarea > 0  ~ "Full",
      crwlarea > 0   ~ "Crawl",
      TRUE           ~ "None"
    ),
    stories = if_else(flrarea2 > 0, 2, 1)
  )

#--- 2. Base ‘constant’ for 1‐ or 2‐storey ---
#    Per the cost notes:
#      1‐storey: $51,500 if full basement, $45,500 if crawl
#      2‐storey: add $16,300 to the 1‐storey constant

data <- data %>%
  mutate(
    sty1_const = case_when(
      stories == 1 & basement_type == "Full"  ~ 51500,
      stories == 1 & basement_type == "Crawl" ~ 45500,
      TRUE                                    ~ 0
    ),
    sty2_const = case_when(
      stories == 2 & basement_type == "Full"  ~ 51500 + 16300,
      stories == 2 & basement_type == "Crawl" ~ 45500 + 16300,
      TRUE                                    ~ 0
    )
  )

#--- 3. Area‐based costs for main building ---
#    1‐storey area rate = $42.50
#    2‐storey first floor = $42.50, second floor = $33.40

data <- data %>%
  mutate(
    sty1_area_cost = if_else(stories == 1,
                             flrarea1 * 42.50,
                             0),
    sty2_area_cost = if_else(stories == 2,
                             flrarea1 * 42.50 + flrarea2 * 33.40,
                             0)
  )

#--- 4. Hot‐water heat cost ---
#    If HEATTYPE in {7,8}, add $4.40/sq ft (1st fl) + $3.30/sq ft (2nd fl).
#    Otherwise 0.

data <- data %>%
  mutate(
    hotwater_ind  = if_else(heattype %in% c(7,8), 1, 0),
    hotwater_cost = hotwater_ind * (flrarea1 * 4.40 + flrarea2 * 3.30)
  )

#--- 5. Fireplaces and baths ---
#    Fireplaces: $3,850 each
#    Baths: $4,000 each (assuming ‘BATHS’ is count of full baths)
#    (Adjust if partial baths need fractional cost)

data <- data %>%
  mutate(
    fireplace_cost = fireplcs * 3850,
    baths_cost     = baths     * 4000
  )

#--- 6. Sum up main‐structure raw cost ---
data <- data %>%
  mutate(
    main_raw_cost = sty1_const + sty2_const +
      sty1_area_cost + sty2_area_cost +
      hotwater_cost + fireplace_cost + baths_cost
  )

#--- 7. Main‐building class factor ---
#    Suppose MBMANCLS is a numeric code (40,41,42 => 0.507, etc.).
#    Adjust the mapping as needed for your specific codes:

data <- data %>%
  mutate(
    MB_factor = case_when(
      mbmancls %in% c(40,41,42)  ~ 0.507,
      mbmancls %in% c(50,51,52)  ~ 0.767,
      mbmancls %in% c(80,81,82)  ~ 0.783,
      mbmancls %in% c(90,91,92)  ~ 0.945,
      mbmancls %in% c(140,141,142) ~ 1.000,
      mbmancls %in% c(145,146,147) ~ 1.220,
      mbmancls %in% c(150,151,152) ~ 1.190,
      TRUE                       ~ 1.000  # fallback
    )
  ) %>%
  mutate(
    MB_RCN = main_raw_cost * MB_factor
  )

#--- 8. Garage cost ---
#    Garage type: attached, detached, basement, etc. 
#    If your data has codes in GAR_TYPE, adjust the if_else accordingly.
#    Then multiply by the garage quality class factor (GR_MANCL).

# First define a factor for GR_MANCL:
data <- data %>%
  mutate(
    GR_factor = case_when(
      gr_mancl %in% c(910,911) ~ 0.450,
      gr_mancl %in% c(920,921) ~ 1.000,
      gr_mancl %in% c(930,931) ~ 1.200,
      gr_mancl %in% c(940,941) ~ 1.420,
      TRUE                     ~ 1.000
    ),
    # Next define the raw garage cost by type:
    garage_raw_cost = case_when(
      gar_type == 1  ~ 11600 + 30 * gar_area,
      gar_type == 2  ~ 16300 + 30 * gar_area,
      gar_type == 3  ~ 7100 ,
      TRUE           ~ 0 
    ),
    GAR_RCN = garage_raw_cost * GR_factor
  )

#--- 9. (Optional) Separate Carport handling ---
#    If your data tracks a separate CP_AREA & CP_MANCL, do likewise:
data <- data %>%
  mutate(
    CP_factor = case_when(
      cp_mancl %in% c(910,911) ~ 0.450,
      cp_mancl %in% c(920,921) ~ 1.000,
      cp_mancl %in% c(930,931) ~ 1.200,
      cp_mancl %in% c(940,941) ~ 1.420,
      TRUE                     ~ 1.000
    ),
    # base carport cost:
    carport_raw_cost =  ifelse(cp_area==0,0,5850+cp_area*22),
    CP_RCN           = carport_raw_cost * CP_factor
  )

#--- 10. Final RCN: sum main building, garage, carport, etc. ---
data <- data %>%
  mutate(
    RCN = MB_RCN + GAR_RCN + CP_RCN
  )

# You now have a new column ‘RCN’ in the dataframe:
head(data[, c("MB_RCN","GAR_RCN","CP_RCN","RCN")])


data%<>%mutate(imprcnld = RCN * pctgood)%>%
  mutate(impr_ratio = imprrsid / imprcnld)

data%<>%mutate(imprcnld_adj = RCN * pctgood * median(impr_ratio))%>%
  mutate(mkfact1 = imprrsid / imprcnld_adj)%>%
  mutate(impr_ratio_adj = mkfact1)


ratio_statistics(data, impr_ratio_adj)

data <- data %>%
  mutate(
    mbmancls        = as.character(mbmancls),
    stories         = as.numeric(substr(mbmancls, nchar(mbmancls), nchar(mbmancls))),
    quality_factor  = as.numeric(substr(mbmancls, 1, nchar(mbmancls) - 1)),
    quality_cluster = case_when(
      quality_factor %in% c(4,5,15)   ~ "Older building codes",
      quality_factor %in% c(10,11,12,13,14,15) ~ "More modern standard",
      quality_factor %in% c(160,161,162)    ~ "After 1930 - custom",
      quality_factor %in% c(210,211,212)    ~ "After 1930 - estate custom",
      TRUE                                  ~ "Other"
    )
  )


data%>%
  mutate(Rank = rank(mkfact1), value = quality_factor)%>%group_by(splitlvl)%>%
  summarise(count = n(), MeanRank = mean(Rank))

kruskal.test(mkfact1~ splitlvl, data)

ratio_statistics(data, mkfact1,splitlvl)


data%<>%mutate(imprcnld_adj_split = case_when(
                splitlvl == 0 ~ imprcnld_adj * 0.99,
                splitlvl == 1 ~ imprcnld_adj * 1.09))%>%
      mutate(mkfact2 = imprrsid / imprcnld_adj_split)
  
ratio_statistics(data, mkfact2,splitlvl)


ratio_statistics(data, mkfact2, gar_type)

data%<>%mutate(imprcnld_adj_split_garage = case_when(
  gar_type == 0 ~ imprcnld_adj_split * 1.01,
  gar_type == 1 ~ imprcnld_adj_split * 0.99,
  gar_type == 2 ~ imprcnld_adj_split * 0.89,
  gar_type == 3 ~ imprcnld_adj_split * 1.09))%>%
  mutate(mkfact3 = imprrsid / imprcnld_adj_split_garage)


ratio_statistics(data, mkfact3, bedrooms)

data%<>%mutate(imprcnld_adj_split_garage_bedroom = case_when(
  bedrooms == 2 ~ imprcnld_adj_split_garage * 0.95,
  bedrooms == 3 ~ imprcnld_adj_split_garage * 1.02,
  bedrooms == 4 ~ imprcnld_adj_split_garage * 0.93))%>%
  mutate(mkfact4 = imprrsid / imprcnld_adj_split_garage_bedroom)

ratio_statistics(data, mkfact4, carptfl)

kruskal.test(mkfact4 ~ bedrooms, data = data)

data%<>%mutate(imprcnld_adj_split_garage_bedroom_carp = case_when(
  carptfl == 0 ~ imprcnld_adj_split_garage_bedroom * 1.01,
  carptfl == 1 ~ imprcnld_adj_split_garage_bedroom * 0.98))%>%
  mutate(mkfact5 = imprrsid / imprcnld_adj_split_garage_bedroom_carp)

ratio_statistics(data, mkfact5, familyrm)

data%<>%mutate(imprcnld_adj_split_garage_bedroom_carp_family = case_when(
  familyrm == 0 ~ imprcnld_adj_split_garage_bedroom_carp * 0.95,
  familyrm == 1 ~ imprcnld_adj_split_garage_bedroom_carp * 1.03,
  familyrm == 2 ~ imprcnld_adj_split_garage_bedroom_carp * 1.01))%>%
  mutate(mkfact6 = imprrsid / imprcnld_adj_split_garage_bedroom_carp_family)



kruskal_test_df <- data.frame(
"carport" = kruskal.test(mkfact6 ~ carptfl, data = data)$p.value,
"fireplace" = kruskal.test(mkfact6 ~ fireplcs, data = data)$p.value,
"bedrooms" = kruskal.test(mkfact6 ~ bedrooms, data = data)$p.value,
"baths" = kruskal.test(mkfact6 ~ baths, data = data)$p.value,
"basement" = kruskal.test(mkfact6 ~ baseste, data = data)$p.value,
"garage mc" = kruskal.test(mkfact6 ~ gr_mancl, data = data)$p.value,
"cp mc" = kruskal.test(mkfact6 ~ cp_mancl, data = data)$p.value,
"heat" = kruskal.test(mkfact6 ~ heattype, data = data)$p.value)%>%t()


data%<>%rename(Final_Land = genlandpow_final,
               Final_Impr = imprcnld_adj_split_garage_bedroom_carp_family)

data%<>%mutate(Final_Value = Final_Land + Final_Impr,
               ASR = Final_Value / saleprice)


data%>%ggplot(aes(x = Final_Value, y = saleprice))+geom_point()+
  geom_smooth(method = "lm", se = FALSE, color = 'grey')+
  labs ( x = "Cost Value", y = "Sale Price")

R_2 <- summary(lm(saleprice ~ Final_Value , data = data))$r.squared

ratio_statistics(data, ASR, neigh)

kruskal.test(ASR ~ neigh, data)

data%<>%mutate(Final_Value_Neigh_adjusted = 
                 case_when(
                  neigh == "713" ~  Final_Value / 1.01,
                  neigh == "715" ~  Final_Value / 1.02,
                  neigh == "716" ~  Final_Value / 1.00,
                  neigh == "731" ~  Final_Value / 0.89,
                  neigh == "732" ~  Final_Value / 1.00,
                  neigh == "734" ~  Final_Value / 1.22,
                  neigh == "736" ~  Final_Value / 0.97,
                  neigh == "737" ~  Final_Value / 0.97
                 ),
               ASR_adjusted = Final_Value_Neigh_adjusted/saleprice
)
ratio_statistics(data, ASR_adjusted,)

kruskal.test(ASR_adjusted ~ neigh, data)


g1 <- data%>%ggplot(aes(x = area1_2, y = ASR_adjusted)) + geom_point()+
  geom_smooth(method = "lm", se = FALSE, color = 'grey')+
  labs ( x = "Living Area", y = "ASR")

summary(lm(ASR_adjusted ~ area1_2, data = data))$r.squared

g2 <- data%>%ggplot(aes(x = lot_size, y = ASR_adjusted)) + geom_point()+
  geom_smooth(method = "lm", se = FALSE, color = 'grey')+
  labs (x = "Lot Size", y = "ASR")

summary(lm(ASR_adjusted ~ lot_size, data = data))$r.squared

g1 | g2

g3 <- data%>%ggplot(aes(x = as.factor(bedrooms), 
                  y = ASR_adjusted,
                  fill = as.factor(bedrooms))) + geom_boxplot()+
  labs ( x = "Bedrooms", y = "ASR")+theme(legend.position = "none")

g4 <- data%>%ggplot(aes(x = as.factor(splitlvl), 
                  y = ASR_adjusted,
                  fill = as.factor(splitlvl))) + geom_boxplot()+
  labs ( x = "Split Level", y = "ASR")+theme(legend.position = "none")

g5 <- data%>%ggplot(aes(x = as.factor(fireplcs), 
                  y = ASR_adjusted,
                  fill = as.factor(fireplcs))) + geom_boxplot()+
  labs ( x = "Fire Places", y = "ASR")+theme(legend.position = "none")

g6 <- data%>%ggplot(aes(x = as.factor(baseste), 
                  y = ASR_adjusted,
                  fill = as.factor(baseste))) + geom_boxplot()+
  labs ( x = "Basement", y = "ASR")+theme(legend.position = "none")

(g3 | g4) / (g5 | g6)

#### Archived ----
# 
# ## Checking the relationship between all the variables and the response variable 
# data%>%ggplot(aes(x = lot_size_adjusted, y = landresid))+geom_point()+geom_smooth(method = 'lm')+
#   labs(x = 'Adjusted Lot Size', y = 'Land Residual')
# data%>%ggplot(aes(x = width, y = landresid))+geom_point()+geom_smooth(method = 'lm')+
#   labs(x = 'Width', y = 'Land Residual')
# 
# data%>%ggplot(aes(x = depth, y = landresid))+geom_point()+geom_smooth(method = 'lm')+
#   labs(x = 'Depth', y = 'Land Residual')
# 
# 
# data%>%ggplot(aes(x = as.factor(corner), 
#                   y = landresid))+geom_boxplot()+
#   labs(x = 'Corner', y = 'Land Residual')
# 
# 
# data%>%group_by(corner)%>%count()
# 
# data%>%ggplot(aes(x = as.factor(culdesac), 
#                   y = landresid))+geom_boxplot()+
#   labs(x = 'cul-de-sac', y = 'Land Residual')
# 
# data%>%group_by(culdesac)%>%count()
# 
# data%>%ggplot(aes(x = as.factor(alley), 
#                   y = landresid))+geom_boxplot()+
#   labs(x = 'Alley', y = 'Land Residual')
# 
# data%>%group_by(alley)%>%count()
# 
# 
# data%>%ggplot(aes(x = as.factor(wooded), 
#                   y = landresid))+geom_boxplot()+
#   labs(x = 'Wooded Lot', y = 'Land Residual')
# 
# data%>%group_by(wooded)%>%count()
# 
# data%>%ggplot(aes(x = as.factor(pie_shpe), 
#                   y = landresid))+geom_boxplot()+
#   labs(x = 'Pie-Shape', y = 'Land Residual')
# 
# data%>%group_by(pie_shpe)%>%count()
# 
# data%>%ggplot(aes(x = as.factor(slope), 
#                   y = landresid))+geom_boxplot()+
#   labs(x = 'High Slope', y = 'Land Residual')
# 
# data%>%group_by(slope)%>%count()
# 
# 
# data%>%ggplot(aes(x = as.factor(abv_road), 
#                   y = landresid))+geom_boxplot()+
#   labs(x = 'Above Road', y = 'Land Residual')
# 
# data%>%group_by(abv_road)%>%count()
# 
# 
# ## Effect of Neighborhood 
# data%>%group_by(neigh)%>%summarise(N = n(),
#                                    Mean = mean(landresid),
#                                    Std = sd(landresid), .groups = 'drop')%>%View()
# 
# ## Corelation Table
# # data%>%select(lot_size_adjusted, width, corner, culdesac, alley, pie_shpe, slope)%>%cor()%>%View()
# 
# 
# ## Actual Model - First Attemp 
# data_model <- data%>%select(lot_size_adjusted, width, corner, culdesac, alley, pie_shpe, slope, landresid, neigh)%>%
#   mutate(neigh_715 = ifelse(neigh == "715",1,0),
#          neigh_736 = ifelse(neigh == "736",1,0),
#          neigh_731 = ifelse(neigh == "731",1,0),
#          neigh_737 = ifelse(neigh == "737",1,0),
#          neigh_734 = ifelse(neigh == "734",1,0),
#          neigh_732 = ifelse(neigh == "732",1,0),
#          neigh_713 = ifelse(neigh == "713",1,0),
#          slop = as.factor(slope),
#          pie_shpe = as.factor(pie_shpe),
#          alley = as.factor(alley),
#          culdesac = as.factor(culdesac),
#          corner = as.factor(corner))%>%select(-neigh)
#   
# full_model <- lm(landresid ~ ., data = data_model)
# final_model <- step(full_model, direction = "both")
# summary(final_model)
# 
# ## Stepwsie
# 
# step_1 <- lm(landresid ~ lot_size_adjusted + width + corner + culdesac + alley + 
#                pie_shpe + slope + neigh_715 + neigh_736 + neigh_731 + neigh_737 + 
#                neigh_734 + neigh_732 + neigh_713, data = data_model)
# summary(step_1)
# 
# 
# step_2 <- lm(landresid ~ lot_size_adjusted + width + corner + culdesac + alley + 
#                pie_shpe + neigh_715 + neigh_736 + neigh_731 + neigh_737 + 
#                neigh_734 + neigh_732 + neigh_713, data = data_model)
# summary(step_2)
# 
# step_3 <- lm(landresid ~ lot_size_adjusted + width + corner + culdesac + pie_shpe + 
#                neigh_715 + neigh_736 + neigh_731 + neigh_737 + neigh_734 + 
#                neigh_732 + neigh_713, data =data_model)
# summary(step_3)
# 
# step_4 <- lm(landresid ~ lot_size_adjusted + width + corner + pie_shpe + neigh_715 + 
#                neigh_736 + neigh_731 + neigh_737 + neigh_734 + neigh_732 + 
#                neigh_713, data =data_model)
# summary(step_4)
# 
# step_5<- lm(landresid ~ lot_size_adjusted + width + corner + pie_shpe + neigh_715 + 
#               neigh_736 + neigh_731 + neigh_737 + neigh_734 + neigh_713, data = data_model)
# summary(step_5)
# 
# step_6<- lm(landresid ~ lot_size_adjusted + width + corner + pie_shpe + neigh_715 + 
#               neigh_736 + neigh_731 + neigh_734 + neigh_713, data = data_model)
# summary(step_6)
# 
# step_7<- lm(landresid ~ lot_size_adjusted + width + corner + neigh_715 + 
#               neigh_736 + neigh_731 + neigh_734 + neigh_713, data = data_model)
# summary(step_7)
# 
# step_8 <- lm(landresid ~ width + corner + neigh_715 + neigh_736 + neigh_731 + 
#                neigh_734 + neigh_713, data = data_model)
# summary(step_8)
# 
# final_model <- step_8
# 
# vif(final_model)
# 
# ## Casewise Diagnosis
# std_residuals <- scale(resid(final_model))
# 
# 
# predicted <- predict(final_model)
# min(predicted)
# max(predicted)
# mean(predicted)
# sd(predicted)
# 
# 
# residuals <- residuals(final_model)
# min(residuals)
# max(residuals)
# mean(residuals)
# sd(residuals)
# 
# predicts_std <- scale(predicted)
# min(predicts_std)
# max(predicts_std)
# mean(predicts_std)
# sd(predicts_std)
# 
# residuals_std <- scale(residuals)
# min(residuals_std)
# max(residuals_std)
# mean(residuals_std)
# sd(residuals_std)
# 
# 
# 
# ## There are two data points with std residuals of  which sould be removed 
# data_model%<>%mutate(res_std = scale(residuals)[,1])%>%filter(abs(res_std)<2.5)
# data%<>%mutate(res_std = scale(residuals)[,1])%>%filter(abs(res_std)<2.5)
# 
# step_8_2<- lm(landresid ~  + width + corner + neigh_715 + neigh_736 + neigh_731 + 
#                 neigh_734 + neigh_713 , data = data_model)
# summary(step_8_2)
# 
# final_model <- step_8_2
# 
# vif(final_model)
# 
# predicted <- predict(final_model)
# 
# 
# residuals <- residuals(final_model)
# 
# predicts_std <- scale(predicted)
# 
# residuals_std <- scale(residuals)
# 
# plot(residuals)
# 
# resid_table <- data.frame('Predicted' = predicted,
#                           'Residual' = residuals,
#                           'Std Predicted' = predicts_std,
#                           'Std Residuals' = residuals_std)%>%t()%>%
#   as.data.frame()%>%
#   rowwise() %>%
#   summarise(Min = min(c_across(everything())),
#             Max = max(c_across(everything())),
#             Mean = mean(c_across(everything())),
#             sd = sd(c_across(everything())),
#             count = length(c_across(everything()))-4)
# 
# View(resid_table)
# 
# ## It is good now and so we can proceed
# 
# 
# ### Land Residual to Land Residual Prediction ratio for different neighborhoods
# 
# data %>%mutate(landresid_predict = predict(final_model),
#                ratio = landresid/landresid_predict,
#                rank = rank(ratio))%>%
#   arrange(ratio)%>%
#   group_by(neigh)%>%
#   summarise(N = n(),
#             Mean_Rank = mean(rank))%>%view()
# 
# data%<>%mutate(landresid_predict = predict(final_model),
#                ratio = landresid/landresid_predict,
#                rank = rank(ratio))
# 
# ## KW test for the ratio
# kruskal.test(ratio ~ neigh, data = data)
# 
# ## Find ratio statistics for ration between land residual and land residual rediction 
# round(cbind(data%>%group_by(neigh)%>%summarise(Mean = mean(ratio),
#                              LB_mean = mean(ratio) - 1.96 * sd(ratio)/sqrt(n()),
#                              UB_mean = mean(ratio) + 1.96 * sd(ratio)/sqrt(n()),
#                              Median = median(ratio),
#                              Min = min(ratio),
#                              Max = max(ratio),
#                              COD = (mean(abs(ratio-median(ratio)))/median(ratio)) * 100,
#                              CV = sd(ratio)/mean(ratio)*100),
#             data.frame(LB_mode = sort(data$ratio)[lower_index], 
#                        UB_mode = sort(data$ratio)[upper_index],
#                        Actual_coverage = (pbinom(upper_index, n, 0.5) - pbinom(lower_index, n, 0.5))*100)),2)%>%
#   View()
# 
# 
# data%>%mutate(ratio_og = landval/landresid)%>%
#   group_by(neigh)%>%
#   summarise(COD_og = (mean(abs(ratio_og-median(ratio_og)))/median(ratio_og)) * 100)
