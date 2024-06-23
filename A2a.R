#NSSO
library(dplyr)
setwd('E:\\JESIN\\DOCUMENTS\\scma\\A2a')
getwd()

# Load the dataset
data <- read.csv("NSSO68.csv")

# Subset data to state assigned
subset_data <- data %>%
  filter(state_1 == 'UP') %>%
  select(foodtotal_v, hhdsz, Regular_salary_earner, MPCE_MRP, MPCE_URP, Possess_ration_card, Education, No_of_Meals_per_day)
print(subset_data)

sum(is.na(subset_data$hhdsz))
sum(is.na(subset_data$Regular_salary_earner))
sum(is.na(subset_data$MPCE_MRP))
sum(is.na(subset_data$MPCE_URP))
sum(is.na(subset_data$Possess_ration_card))
sum(is.na(subset_data$Education))
sum(is.na(subset_data$No_of_Meals_per_day))



impute_with_mean <- function(data, columns) {
  data %>%
    mutate(across(all_of(columns), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
}

# Columns to impute
columns_to_impute <- c("Possess_ration_card")

# Impute missing values with mean
data <- impute_with_mean(data, columns_to_impute)

sum(is.na(data$Possess_ration_card))

# Fit the regression model
model <- lm(foodtotal_v~ hhdsz+Regular_salary_earner+MPCE_MRP+MPCE_URP+Possess_ration_card+Education+No_of_Meals_per_day, data = subset_data)

# Print the regression results
print(summary(model))


library(car)
# Check for multicollinearity using Variance Inflation Factor (VIF)
vif(model) # VIF Value more than 8 its problematic

# Extract the coefficients from the model
coefficients <- coef(model)

# Construct the equation
equation <- paste0("y = ", round(coefficients[1], 2))
for (i in 2:length(coefficients)) {
  equation <- paste0(equation, " + ", round(coefficients[i], 6), "*x", i-1)
}
# Print the equation
print(equation)










