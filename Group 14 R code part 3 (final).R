
install.packages("RKaggle")
install.packages("summarytools")
library(RKaggle)
library(tidyverse)
library(summarytools)

#Agriculture Crop Yield by Samuel Oti Attakorah 
#(https://www.kaggle.com/datasets/samuelotiattakorah/agriculture-crop-yield/data?select=crop_yield.csv)

df_list <- get_dataset("samuelotiattakorah/agriculture-crop-yield")
summary_df <- summary(df_list)
summary_df


# output:
#     Region           Soil_Type             Crop          
#  Length:1000000     Length:1000000     Length:1000000    
#  Class :character   Class :character   Class :character  
#  Mode  :character   Mode  :character   Mode  :character  
#                                                          
#                                                          
#                                                          
#   Rainfall_mm     Temperature_Celsius Fertilizer_Used
#  Min.   : 100.0   Min.   :15.00       Mode :logical  
#  1st Qu.: 324.9   1st Qu.:21.25       FALSE:500060   
#  Median : 550.1   Median :27.51       TRUE :499940   
#  Mean   : 550.0   Mean   :27.50                      
#  3rd Qu.: 774.7   3rd Qu.:33.75                      
#  Max.   :1000.0   Max.   :40.00                      
#  Irrigation_Used Weather_Condition  Days_to_Harvest
#  Mode :logical   Length:1000000     Min.   : 60.0  
#  FALSE:500509    Class :character   1st Qu.: 82.0  
#  TRUE :499491    Mode  :character   Median :104.0  
#                                     Mean   :104.5  
#                                     3rd Qu.:127.0  
#                                     Max.   :149.0  
#  Yield_tons_per_hectare
#  Min.   :-1.148        
#  1st Qu.: 3.418        
#  Median : 4.652        
#  Mean   : 4.649        
#  3rd Qu.: 5.879        
#  Max.   : 9.963        

freq(df_list$Fertilizer_Used)


# Frequencies  
# df_list$Fertilizer_Used  
# Type: Logical  
# 
# Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
# ----------- --------- --------- -------------- --------- --------------
#   FALSE    500060     50.01          50.01     50.01          50.01
# TRUE    499940     49.99         100.00     49.99         100.00
# <NA>         0                               0.00         100.00
# Total   1000000    100.00         100.00    100.00         100.00

freq(df_list$Soil_Type)

# Frequencies  
# df_list$Soil_Type  
# Type: Character  
# 
# Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
# ------------ --------- --------- -------------- --------- --------------
#   Chalky    166779     16.68          16.68     16.68          16.68
# Clay    166352     16.64          33.31     16.64          33.31
# Loam    166795     16.68          49.99     16.68          49.99
# Peaty    166283     16.63          66.62     16.63          66.62
# Sandy    167119     16.71          83.33     16.71          83.33
# Silt    166672     16.67         100.00     16.67         100.00
# <NA>         0                               0.00         100.00
# Total   1000000    100.00         100.00    100.00         100.00

ctable(
  x = df_list$Fertilizer_Used,
  y = df_list$Soil_Type
  )

# Cross-Tabulation, Row Proportions  
# Fertilizer_Used * Soil_Type  
# Data Frame: df_list  
# 
# ----------------- ----------- ---------------- ---------------- ---------------- ---------------- ---------------- ---------------- ------------------
#   Soil_Type           Chalky             Clay             Loam            Peaty            Sandy             Silt              Total
# Fertilizer_Used                                                                                                                                     
# FALSE                83541 (16.7%)    83502 (16.7%)    83049 (16.6%)    83331 (16.7%)    83479 (16.7%)    83158 (16.6%)    500060 (100.0%)
# TRUE                83238 (16.6%)    82850 (16.6%)    83746 (16.8%)    82952 (16.6%)    83640 (16.7%)    83514 (16.7%)    499940 (100.0%)
# Total               166779 (16.7%)   166352 (16.6%)   166795 (16.7%)   166283 (16.6%)   167119 (16.7%)   166672 (16.7%)   1000000 (100.0%)
# ----------------- ----------- ---------------- ---------------- ---------------- ---------------- ---------------- ---------------- ------------------

#boxplot showing yield by fertilizer usage
boxplot(df_list$Yield_tons_per_hectare ~ df_list$Fertilizer_Used,
        main = "Yield With Fertilized Soil",
        xlab = "Yield (Tons per Hectare)", ylab = "Fertilizer Used")

#boxplot showing yield by soil type
boxplot(df_list$Yield_tons_per_hectare ~ df_list$Soil_Type,
        main = "Yield of Different Soil Types",
        xlab = "Yield (Tons per Hectare)", ylab = "Soil Type")

#histogram yield distrobution
hist(df_list$Yield_tons_per_hectare,
     main = "Frequency of Yield",
     xlab = "Yield")

#barplot of crop type
barplot(prop.table(table(df_list$Crop)),
        ylab = "Proportion of Recorded Datapoints",
        main = "% of Observations")

#Scatterplot temperature influence on yield
ggplot(df_list)+
  aes(x = Yield_tons_per_hectare, y = Temperature_Celsius)+
  geom_point()

#Faceted scatterplot Rainfall influence on yields by region
ggplot(df_list)+
  aes(x = Yield_tons_per_hectare, y = Rainfall_mm)+
  geom_point()+
  facet_wrap(~ Region)+
  labs(title = "Yield vs Rainfall by Region",
       x = "Yield (Tons, per Hectare)",
       y = "Rainfall in mm")+
  theme_minimal()

#Faceted scatterplot Rainfall influence on yields by soil type
ggplot(df_list)+
  aes(x = Yield_tons_per_hectare, y = Rainfall_mm)+
  geom_point()+
  facet_wrap(~ Soil_Type)+
  labs(title = "Yield vs Rainfall by Soil Type",
       x = "Yield (Tons, per Hectare)",
       y = "Rainfall in mm")+
  theme_minimal()

#Color - Coded Correlation Matrix
install.packages("corrplot")
library(corrplot)
numeric_vars <- df_list %>%
  select(where(is.numeric))
cor_matrix <- cor(numeric_vars, use = "complete.obs")
corrplot(cor_matrix, 
         method = "color",     
         tl.col = "black",     
         tl.cex = 0.8,         
         number.cex = 0.7,     
         addCoef.col = "black" 
)

#regression model
model1 = lm(Yield_tons_per_hectare ~ Rainfall_mm + Temperature_Celsius + Days_to_Harvest, data=df_list)
summary(model1)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -3.7040 -0.7897 -0.0007  0.7904  3.6701 
# 
# Coefficients:
#                         Estimate   Std. Error   t value   Pr(>|t|)    
#   (Intercept)          1.354e+00   6.527e-03    207.389    <2e-16 ***
#   Rainfall_mm          4.992e-03   4.171e-06   1197.032    <2e-16 ***
#   Temperature_Celsius  2.013e-02   1.501e-04    134.100    <2e-16 ***
#   Days_to_Harvest     -3.183e-05   4.176e-05     -0.762     0.446    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.084 on 999996 degrees of freedom
# Multiple R-squared:  0.592,	Adjusted R-squared:  0.592 
# F-statistic: 4.836e+05 on 3 and 999996 DF,  p-value: < 2.2e-16


#T value for rainfall_mm indicates it as highly significant as well as Temperature_Celsius though less so
#PR(>|t|) indicates coefficient for Days_to_harvest has a significant probability of being 0, R's marking confirms Days_to_Harvest as insignificant
#Both Rainfall_mm and Temperature_Celsius are marked as 3 star values by R, indicating they are significant, 1mm of rainfall contributes to .004992 tonnes of crop yield per hectere
#R squared and adjusted R squared indicate 59.2% of variance in the dependent variable is explained by the independent variables this indicates moderate influence on the dependent variable from the independent variables.


#removing insignificant value 'Days_to_harvest' from model
model2 = lm(Yield_tons_per_hectare ~ Rainfall_mm + Temperature_Celsius, data=df_list)
summary(model2)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -3.7048 -0.7897 -0.0007  0.7903  3.6701 
# 
# Coefficients:
#                       Estimate    Std. Error  t value  Pr(>|t|)    
#   (Intercept)         1.350e+00   4.846e-03    278.7    <2e-16 ***
#   Rainfall_mm         4.992e-03   4.171e-06   1197.0    <2e-16 ***
#   Temperature_Celsius 2.013e-02   1.501e-04    134.1    <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.084 on 999997 degrees of freedom
# Multiple R-squared:  0.592,	Adjusted R-squared:  0.592 
# F-statistic: 7.254e+05 on 2 and 999997 DF,  p-value: < 2.2e-16

#no significant change in the model after 'Days_to_harvest' is removed further indicating no significance

#Checking for Correlation in independent variables
cor(df_list$Rainfall_mm, df_list$Temperature_Celsius)

# [1] -0.0001230056
# Value is not significantly different from 0, indicating no Correlation or collinearity


#CART Model

install.packages("rpart")
install.packages("rpart.plot")
install.packages("caret")

library(rpart)
library(rpart.plot)
library(caret)

df_list <- get_dataset("samuelotiattakorah/agriculture-crop-yield")

df_list$Crop <- as.factor(df_list$Crop)
df_list$Soil_Type <- as.factor(df_list$Soil_Type)
df_list$Fertilizer_Used <- as.logical(df_list$Fertilizer_Used)
df_list$Irrigation_Used <- as.logical(df_list$Irrigation_Used)

#Train Test Split
set.seed(123)
train_idx <- createDataPartition(df_list$Crop, p = 0.7, list = FALSE)
train_df <- df_list[train_idx, ]
test_df  <- df_list[-train_idx, ]


cart_model <- rpart(
  Crop ~ Rainfall_mm + Temperature_Celsius + Fertilizer_Used + Soil_Type + Irrigation_Used,
  data = train_df,
  method = "class",
  control = rpart.control(
    cp = 0.0001,       
    minsplit = 10,     
    maxdepth = 15      
  )
)

# ───── Plot Full Tree ─────
rpart.plot(
  cart_model,
  type = 2,
  extra = 104,
  under = TRUE,
  fallen.leaves = TRUE,
  box.palette = "BuGn",
  tweak = 1.2,
  cex = 0.55,
  shadow.col = "gray",
  main = "CART Tree – Predicting Crop Type"
)

#Evaluate Performance
pred <- predict(cart_model, test_df, type = "class")
conf_matrix <- confusionMatrix(pred, test_df$Crop)
print(conf_matrix)

#Show Variable Importance 
cat("\nVariable Importance:\n")
print(sort(cart_model$variable.importance, decreasing = TRUE))

#Example Prediction
new_data <- data.frame(
  Rainfall_mm = 850,
  Temperature_Celsius = 23,
  Fertilizer_Used = as.logical(1),
  Soil_Type = factor("Loam", levels = levels(df_list$Soil_Type)),
  Irrigation_Used = as.logical(1)
)

cat("\nPrediction for new data:\n")
print(predict(cart_model, new_data, type = "class"))


